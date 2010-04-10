(ns leiningen.add
  "Interactively adds a dependency from clojars. With one parameter it will add the latest stable of the corresponding version with two arguments it will take the second argument as version."
  (:use (clojure.contrib duck-streams seq-utils str-utils)
	[leiningen.update-repo :only [*lein-dir*]]
	[leiningen.search :only [read-clj search-clojar artifact-name]]))


(defn good-read-line [] 
  (binding [*in* (-> System/in (java.io.InputStreamReader.) (clojure.lang.LineNumberingPushbackReader.))] (read-line)))
(defn add-artifact [project artifact version]
  (reverse 
   (first 
    (reduce 
     (fn [[form is-deps?] f] 
       (if is-deps? 
	 [(cons (cons [(symbol artifact) version] f) form) false]
	 [(cons f form) (= f :dependencies)])) ['() false] project))))

(defn find-clojar [what]
  (let [p (re-pattern what)]
    (let [[group  artifac] (if (= -1 (.indexOf what "/")) [what what] (next (re-find #"^([^/]+)/(.+)$" what)))]
    (doall (filter 
	    (fn [{artifact-id :artifact-id group-id :group-id}] 
	      (and
	       (= artifact-id artifac)
	       (= group-id group)))
	    (read-clj (str *lein-dir* "/clojars")))))))

(defn get-version [artifact]
  (if (= 1 (count (second artifact)))
    [(first artifact) (first (second artifact))]
    (loop [versions (second artifact)]
      (println 
       (first 
	(reduce 
	 (fn [[s i] v] 
	   [(str s (if (> i 1) "\n") i ": " (first artifact) " " v) 
	    (inc i)]) ["" 1] versions)))
      (print "Please select a version: ")
      (flush)
      (let [v (try (Integer/parseInt (chomp (good-read-line))) (catch java.lang.NumberFormatException e -1))]
	(if (or (nil? v) (nil? (nth versions (dec v) nil)))
	  (recur versions)
	  [(first artifact) (nth versions (dec v))])))))

(defn latest-stable [versions]
  (first (filter (partial re-find #"^(\d+).(\d+).(\d+)$") versions)))

(defn get-artifact-id [res]
  (if (> (count res) 1)
    (do
      (println
       (first 
	(reduce 
	 (fn [[s i] a] 
	   [(str s (if (> i 1) "\n") i ": " (first a) "(" (str-join ", " (second a)) ")") 
	    (inc i)]) ["" 1] res)))
      (print "Please select an artifact: ")
      (flush)
      (read-line)
      (let [a (try (Integer/parseInt (chomp (good-read-line))) (catch java.lang.NumberFormatException e -1))]
	(if (or (nil? a) (nil? (nth res (dec a) nil)))
	  (recur res)
	  (let [artifact (nth res (dec a))]
	    (get-version artifact)))))
    (get-version (first res))))

(defn add [project artifact & args]
  (let [version (first args)
	p (:root project)
	res  (first (find-clojar artifact))]
    (if (empty? res)
      (println "Sorry nothing on clojar that matches" artifact (if version ""))
      (if (and version (not-any? (partial = version) (:versions res)))
	(println (str "Println sorry ther is no version " version " for " (artifact-name res) ". Try: " (str-join ", " (:versions res))))
	(let [[a v] [(artifact-name res) (if version version (latest-stable (:versions res)))]
	      p (read-clj (str (:root project) "/project.clj"))]
	  (println "Adding:" a v)
	  (with-open [o (writer (str (:root project) "/project.clj"))]
	    (binding [*out* o]
	      (pr (add-artifact p a v)))))))))