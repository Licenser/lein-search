(ns leiningen.update
  "lein update checks for newer versions of currently used dependencies and aks the user if they should be updated to the latest stable."
  (:use lein-search.core))


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
                  (map find-updates (dep-type project))))
        updated-project (doall (->> (read-project-clj project)
                                    (maybe-add-updates :dependencies)
                                    (maybe-add-updates :dev-dependencies)))]
    (when (yes-or-no-prompt "Overwrite project.clj?")
      (write-project-clj project updated-project))))
