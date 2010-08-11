(ns leiningen.add
  "Interactively adds a dependency from clojars.
  With one parameter it will add the latest stable of the corresponding version with two arguments it will take the second argument as version.
  If the first parameter is --dev or -d it will work exactly as without --dev just that it will add a dev dependency."
  (:use [clojure.contrib.str-utils :only (str-join)]
        lein-search.core))


(defn get-version [[artifact-name available-versions]]
  [artifact-name
   (choose-from-numbered-list-if-multiple available-versions
                                          "Please select a version"
                                          (fn [v] (str artifact-name " " v)))])

(defn get-artifact-id [res]
  (get-version
   (choose-from-numbered-list-if-multiple res
                                          "Please select an artifact"
                                          (fn [[name vers]]
                                            (str name " (" (str-join ", " vers) ")")))))

(defn add [project artifact & args]
  (let [dev (or (= artifact "--dev") (= artifact "-d"))
	artifact (if dev (first args) artifact)
	args (if dev (rest args) args)
	version (first args)
	p (:root project)
	res  (first (find-clojar artifact))]
    (if (empty? res)
      (println "Sorry; nothing on clojars that matches" artifact (if version ""))
      (if (and version (not-any? (partial = version) (:versions res)))
	(println "Sorry; there is no version" version "for" (clojars-artifact-name res) ". Try one of:" (str-join ", " (:versions res)))
	(let [[a v] [(clojars-artifact-name res) (if version version (latest-stable (:versions res)))]]
	  (println "Adding:" a v)
	  (write-project-clj project
                             (add-artifact (read-project-clj project)
                                           (if dev :dev-dependencies :dependencies) a v)))))))