(ns leiningen.search
  "Searches the indexed clojars.org repository. Giving -v as first argument
prints the versions instead of the description."
  (:use [clojure.contrib.str-utils :only (str-join re-sub)]
        lein-search.core
        leiningen.update-repo)
  (:import java.io.File))

(defn search [what & args]
  (if (map? what)
    ;; To be backwards-compatible with Leiningen 1.2 and prior, we
    ;; must accept and discard a project argument. Current Leiningen (1.3+)
    ;; inspects the arg list and only passes in the project if needed.
    (apply search args)
    (let [show-versions (= "-v" what)
          what (if show-versions (first args) what)]
      (if (.exists (File. (str *lein-dir* "/clojars")))
        (let [results (search-clojar what)]
          (println (format "Results for %s:" what))
          (doseq [{:keys [description versions artifact-id group-id]} results
                  :let [name (clojars-artifact-name group-id artifact-id)
                        versions-string (str-join ", " versions)
                        description (or description "No description given")
                        desc (re-sub #"\n\s*" " " description)]]
            (println (if show-versions
                       (format "%s: %s" name versions-string)
                       (format "%-40s - %s" name desc)))))
        ;; If there's no index found, fetch it and try again.
        (do (update-repo)
            (apply search what args))))))
