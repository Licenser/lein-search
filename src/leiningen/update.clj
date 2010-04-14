(ns leiningen.update
  "lein update checks for newer versions of currently used dependencies and aks the user if they should be updated to the latest stable."
  (:use (clojure.contrib duck-streams seq-utils str-utils)
	[leiningen.add :only [latest-stable add-artifact find-clojar good-read-line]]
	[leiningen.search :only [read-clj]]
	[leiningen.update-repo :only [compare-versions]]))


(defn update-artifact [project type artifact version]
  (reverse 
   (first 
    (reduce 
     (fn [[form is-deps?] f] 
       (if is-deps?
	 [(cons (map (fn [[a v]] (if (= a (symbol artifact)) [a version] [a v])) f) form) false]
	 [(cons f form) (= f type)])) ['() false] project))))

(defn ask-for-update [artifact version new-version]
  (print (str "You are currently using "artifact" in version "version". Do you want to update to "new-version"? (y/n)"))
  (flush)
  (let [r (chomp (good-read-line))]
    (cond
     (= r "y") true
     (= r "n") false
     :else (recur artifact version new-version))))

(defn find-updates [[artifact version]]
  (let [res (first (find-clojar (str artifact)))
	latest (if res (latest-stable (:versions res)) "0.0.0")]
    (if (and res (< 0  (compare-versions latest version)))
      [artifact version latest]
      [artifact version nil])))
  
(defn update [project & args]
  (let [deps (:dependencies project)
	dev-deps (:dev-dependencies project)
	p (reduce (fn [p [artifact version new-version]]
		    (if (and new-version (ask-for-update artifact version new-version))
		      (update-artifact p :dev-dependencies (str artifact) new-version)
		      p))
		  (reduce (fn [p [artifact version new-version]]
			    (if (and new-version (ask-for-update artifact version new-version))
			      (update-artifact p :dependencies (str artifact) new-version)
			p))
			  (read-clj (str (:root project) "/project.clj"))
			  (map find-updates deps))
		  (map find-updates dev-deps))]
    (with-open [o (writer (str (:root project) "/project.clj"))]
      (binding [*out* o]
	(pr p)))))
    