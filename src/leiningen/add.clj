(ns leiningen.add
  "Interactively adds a dependency from clojars."
  (:use (clojure.contrib duck-streams seq-utils str-utils)
	[leiningen.update :only [*lein-dir*]]
	[leiningen.search :only [group-results read-clj find-clojar]]))

(defn add-artifact [project artifact version]
  (reverse 
   (first (reduce 
	   (fn [[form is-deps?] f] 
	     (if is-deps? 
	       [(cons (cons [(symbol artifact) version] f) form) false]
	       [(cons f form) (= f :dependencies)])) ['() false] project))))

(defn get-artifact-id [res]
   (println (first 
    (reduce 
     (fn [[s i] a] 
       [(str s (if (> i 1) "\n") i ": " (first a) "(" (str-join ", " (second a)) ")") 
	(inc i)]) ["" 1] res)))
   (print "Please select an artifact: ")
   (flush)
   (let [a (try (Integer/parseInt (chomp (read-line))) (catch java.lang.NumberFormatException e -1))]
     (if (or (nil? a) (nil? (nth res (dec a) nil)))
       (recur res)
       (let [artifact (nth res (dec a))]
	 (if (= 1 (count (second artifact)))
	   [(first artifact) (first (second artifact))]
	   (loop [versions (second artifact)]
	     (println (first 
		       (reduce 
			(fn [[s i] v] 
			  [(str s (if (> i 1) "\n") i ": " (first artifact) " " v) 
			   (inc i)]) ["" 1] versions)))
	     (print "Please select a version: ")
	     (flush)
	     (let [v (try (Integer/parseInt (chomp (read-line))) (catch java.lang.NumberFormatException e -1))]
	       (if (or (nil? v) (nil? (nth versions (dec v) nil)))
		 (recur versions)
		 [(first artifact) (nth versions (dec v))]))))))))

(defn add [project what & args]
  (let [p (:root project)]
    (let [[a v] (get-artifact-id (group-results (find-clojar what)))
	  p (read-clj (str (:root project) "/project.clj"))]
      (with-open [o (writer (str (:root project) "/project.clj"))]
	(binding [*out* o]
	  (pr (add-artifact p a v)))))))