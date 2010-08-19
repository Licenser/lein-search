(ns leiningen.search
  "Searches the indexed clojars.org repository. Giving -v as first argument prints the versions instead of the description."
  (:use [clojure.contrib.str-utils :only (str-join re-sub)]
        lein-search.core)
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
        (let [m (search-clojar what)]
          (println "Results for " what ":")
          (if show-versions
            (println (str-join "\n" (map (fn [{versions :versions artifact-id :artifact-id group-id :group-id}] (str (clojars-artifact-name group-id artifact-id) ": " (str-join ", " versions))) m)))
            (println (str-join "\n" (map (fn [{description :description artifact-id :artifact-id group-id :group-id}] (format "%-40s - %s" (clojars-artifact-name group-id artifact-id) (re-sub #"\n\s*" " " (or description "No description given")))) m)))))
        (println "No repo index found, please run lein update first.")))))
