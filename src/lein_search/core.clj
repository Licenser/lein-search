(ns lein-search.core
  (:use [clojure.contrib.duck-streams :only (reader writer with-out-writer)])
  (:use [clojure.contrib.str-utils :only (chomp str-join)])
  (:require [clojure.zip :as zip])
  (:import (java.io File PushbackReader) java.util.zip.GZIPInputStream java.net.URL))

;; TODO: check to see if leiningen.core/home-dir is defined; use it if so.
(def *lein-dir* (str (System/getProperty "user.home") "/.lein"))

;;; User input functions

(defn good-read-line []
  (binding [*in* (-> System/in java.io.InputStreamReader.
                     clojure.lang.LineNumberingPushbackReader.)]
    (read-line)))

(defn prompt-for-input [prompt]
  (print prompt)
  (flush)
  (chomp (good-read-line)))

(defn yes-or-no-prompt [question]
  (condp = (prompt-for-input (str question " (y/n) "))
      "y" true
      "n" false
      (recur question)))

(defn prompt-for-number [prompt]
  (try (Integer/parseInt (prompt-for-input prompt))
       (catch java.lang.NumberFormatException e nil)))

(defn choose-from-numbered-list-if-multiple
  "Return first item immediately if there is only one, otherwise prompt the user
with a numbered list of choices."
  [choices prompt formatter]
  (if (= 1 (count choices))
     (first choices)
     (do (println
          (str-join "\n"
                    (for [[n i] (map vector (iterate inc 1) choices)]
                      (str n ": " (formatter i)))))
         (loop []
           (let [v (prompt-for-number (str prompt ": "))]
             (if (or (nil? v) (nil? (nth choices (dec v) nil)))
               (recur)
               (nth choices (dec v))))))))

;;; Reading/writing clojure forms

(defn read-clj [f]
  (with-open [r (reader f)]
    (read (java.io.PushbackReader. r))))

(defn- project-clj-path [project]
  (str (:root project) "/project.clj"))

(defn read-project-clj [project]
  (read-clj (project-clj-path project)))

(defn write-project-clj [project forms]
  (with-out-writer (project-clj-path project)
    (pr forms)))


;;; Modifying defproject forms

(defn- zip-move-until
  "Return the next location for which pred returns true, moving using
the supplied zip move function each time"
  [pred move ziploc]
  (->> ziploc (iterate move) (take-while identity) (filter pred) first))

(defn- find-or-add-keyval
  "Given the clojure.zip location of a key-value list, return the location
 at which the value with the given key can be found, inserting it if necessary."
  [val-list key default]
  (let [first-form (-> val-list zip/down)]
    (if-let [marker (zip-move-until #(= key (zip/node %))
                                    zip/right
                                    first-form)]
      (zip/right marker)
      (-> first-form
          zip/rightmost
          (zip/insert-right default)
          (zip/insert-right key)
          zip/rightmost))))

(defn- update-dependency-list
  "Modify the project's dependency list of the given type by passing it through f.

 Adds a dependency list of that type if none currently exists."
  [project dep-type f]
  (let [dep-list-loc (find-or-add-keyval (zip/seq-zip project) dep-type [])]
    (-> dep-list-loc
        (zip/replace (f (zip/node dep-list-loc)))
        (zip/root))))

(defn add-artifact [project type artifact version]
  (update-dependency-list project type
                          (fn [deps]
                            (cons [(symbol artifact) version] deps))))

(defn update-artifact [project dep-type artifact new-version]
  (update-dependency-list project dep-type
                          (fn [deps]
                            (for [[a v] deps]
                              [a (if (= a (symbol artifact))
                                   new-version
                                   v)]))))



;;; Grokking version strings

(defn latest-stable [versions]
  (first (filter (partial re-find #"^(\d+).(\d+).(\d+)$") versions)))

(defn split-version [v]
  (if-let [[_ major minor patch suffix] (when v (re-find #"(\d+)\.(\d+)(?:\.(\d+))?(?:-(.*))?" v))]
    [(Integer/parseInt major) (Integer/parseInt minor) (Integer/parseInt (or patch "0")) (or suffix "")]
    [0 0 0 ""]))

(defn compare-versions [v1 v2]
  (let [vers1 (split-version v1)
        vers2 (split-version v2)
        version-comparison (.compareTo (subvec vers1 0 3) (subvec vers2 0 3))]
    (if (zero? version-comparison)
      (let [v1-suffix (last vers1)
            v2-suffix (last vers2)]
        (cond
         (= v1-suffix v2-suffix) 0
         (= v1-suffix "") 1
         (= v2-suffix "") -1
         (= v1-suffix "SNAPSHOT") 1
         (= v2-suffix "SNAPSHOT") -1
         :else (.compareTo v1-suffix v2-suffix)))
      version-comparison)))

(defn latest-stable [versions]
  (->> versions
       (sort-by identity compare-versions)
       (filter (partial re-find #"^(\d+).(\d+)(?:.(\d+))?$"))
       last))


;;; Clojars cache

(defn- read-clojars-index [url]
  (with-open [r (reader  (GZIPInputStream. (.openStream (URL. url))))]
    (let [r (PushbackReader. r)]
      (loop [result [] code (read r false false)]
        (if code
          (recur (conj result code) (read r false false))
          result)))))

(defn write-clojars-cache []
  (if (not (.exists (File. *lein-dir*))) (.mkdirs (File. *lein-dir*)))
  (with-out-writer (str  *lein-dir* "/clojars")
    (pr (read-clojars-index "http://clojars.org/repo/feed.clj.gz"))))

(defn read-clojars-cache []
  (read-clj (str *lein-dir* "/clojars")))

(defn find-clojar [what]
  (let [[group artifact] (if-let [match (re-find #"^([^/]+)/(.+)$" what)]
                           (next match)
                           [what what])]
    (->> (read-clojars-cache)
         (filter
          (fn [{artifact-id :artifact-id group-id :group-id}]
            (and
             (= artifact-id artifact)
             (= group-id group)))))))

(defn search-clojar [what]
  (let [p (re-pattern what)]
    (doall (filter
            (fn [{description :description artifact-id :artifact-id group-id :group-id}]
              (or
               (re-find p (or description ""))
               (re-find p artifact-id)
               (re-find p group-id)))
            (read-clojars-cache)))))

(defn clojars-artifact-name
  ([group-id article-id]
     (if (= group-id article-id)
       article-id
       (str group-id "/" article-id)))
  ([{artifact-id :artifact-id group-id :group-id}]
     (clojars-artifact-name group-id artifact-id)))
