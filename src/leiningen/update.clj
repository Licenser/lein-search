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

(defn yes-or-no-prompt [question]
  (print question " (y/n)")
  (flush)
  (let [r (chomp (good-read-line))]
    (cond
     (= r "y") true
     (= r "n") false
     :else (recur question))))

(defn ask-for-update [artifact version new-version]
  (yes-or-no-prompt (str "You are currently using "artifact" in version "version". Do you want to update to "new-version"?")))

(defn find-updates [[artifact version]]
  (let [res (first (find-clojar (str artifact)))
	latest (if res (latest-stable (:versions res)) "0.0.0")]
    (if (and res (< 0  (compare-versions latest version)))
      [artifact version latest]
      [artifact version nil])))
  
(defn update [project & args]
  (let [project-clj-path (str (:root project) "/project.clj")
        maybe-add-updates
        (fn [dep-type initial]
          (reduce (fn [p [artifact version new-version]]
                    (if (and new-version (ask-for-update artifact version new-version))
                      (update-artifact p dep-type (str artifact) new-version)
                      p))
                  initial
                  (map find-updates (dep-type project))))
	updated-project (->> (read-clj project-clj-path)
                             (maybe-add-updates :dependencies)
                             (maybe-add-updates :dev-dependencies))]
    (with-open [o (writer project-clj-path)]
      (binding [*out* o]
	(pr p)))))
    