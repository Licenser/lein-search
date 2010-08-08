(ns leiningen.update-repo
  "Updates the clojars.org repositories index."
  (:use clojure.contrib.duck-streams)
  (:import (java.io File InputStreamReader PushbackReader) java.util.zip.GZIPInputStream java.net.URL))

(def *lein-dir* (str (System/getProperty "user.home") "/.lein"))

(defn split-version [v]
  (if-let [[_ major minor patch suffix] (re-find #"(\d+)\.(\d+)(?:\.(\d+))?(?:-(.*))?" v)]
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

(defn read-index [url]
  (with-open [r (PushbackReader. (InputStreamReader. (GZIPInputStream. (.openStream (URL. url)))))]
      (loop [result [] code (read r false false)]
	(if code
	  (recur (conj result code) (read r false false))
	  result))))

(defn update-repo [project & args]
  (if (not (.exists (File. *lein-dir*))) (.mkdirs (File. *lein-dir*)))
  (with-open [w (writer (str  *lein-dir* "/clojars"))]
    (println "Getting the list of packages on clojars.org ...")
    (binding [*out* w] (pr (read-index "http://clojars.org/repo/feed.clj.gz")))))
