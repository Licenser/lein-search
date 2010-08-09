(ns leiningen.update-repo
  "Updates the clojars.org repositories index."
  (:use lein-search.util))


(defn update-repo [project & args]
  (println "Getting the list of packages on clojars.org ...")
  (write-clojars-cache))
