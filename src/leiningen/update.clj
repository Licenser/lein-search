(ns leiningen.update
  "lein update checks for newer versions of currently used dependencies and aks the user if they should be updated to the latest stable."
  (:use (clojure.contrib duck-streams seq-utils str-utils)
	[leiningen.add :only [latest-stable add-artifact find-clojar good-read-line update-dependency-list prompt-for-input read-project-clj write-project-clj]]
	[leiningen.update-repo :only [compare-versions]]))


(defn update-artifact [project dep-type artifact new-version]
  (update-dependency-list project dep-type
                          (fn [deps]
                            (for [[a v] deps]
                              [a (if (= a (symbol artifact))
                               new-version
                               v)]))))

(defn yes-or-no-prompt [question]
  (condp = (prompt-for-input (str question " (y/n) "))
    "y" true
    "n" false
    (recur question)))

(defn ask-for-update [artifact version new-version]
  (yes-or-no-prompt (str "You are currently using "artifact" version "version". Do you want to update to "new-version"?")))

(defn find-updates [[artifact version]]
  (let [res (first (find-clojar (str artifact)))
	latest (if res (latest-stable (:versions res)) "0.0.0")]
    (if (and res (< 0  (compare-versions latest version)))
      [artifact version latest]
      [artifact version nil])))

(defn update [project & args]
  (let [maybe-add-updates
        (fn [dep-type initial]
          (reduce (fn [p [artifact version new-version]]
                    (if (and new-version (ask-for-update artifact version new-version))
                      (update-artifact p dep-type (str artifact) new-version)
                      p))
                  initial
                  (map find-updates (dep-type project))))]
    (write-project-clj project
                       (->> (read-project-clj project)
                            (maybe-add-updates :dependencies)
                            (maybe-add-updates :dev-dependencies)))))
