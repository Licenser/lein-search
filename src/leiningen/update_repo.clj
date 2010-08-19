(ns leiningen.update-repo
  "Updates the clojars.org repositories index."
  (:use lein-search.core))

(defn update-repo [& _]
  (println "Getting the list of packages on clojars.org ...")
  (write-clojars-cache))
