(ns leiningen.search
  "Searches the indexed clojars.org repository. Giving -v as first argument
prints the versions instead of the description."
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
          (println (format "Results for %s:" what))
          (if show-versions
            (doseq [{:keys [versions artifact-id group-id]} m
                    :let [name (clojars-artifact-name group-id artifact-id)
                          versions-string (str-join ", " versions)]]
              (println (format "%s: %s" name versions-string)))
            (doseq [{:keys [description artifact-id group-id]} m
                    :let [name (clojars-artifact-name group-id artifact-id)
                          desc (or description "No description given")
                          desc (re-sub #"\n\s*" " " desc)]]
              (println (format "%-40s - %s" name desc)))))
        (println "No repo index found, please run lein update first.")))))
