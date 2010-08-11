(ns lein-search.core-test
  (:use [lein-search.core] :reload-all)
  (:use [clojure.test]))

(deftest version-parsing
  (are [a b] (= a (split-version b))
       [0 1 2 ""] "0.1.2"
       [0 1 0 ""] "0.1"
       [1 2 3 "SNAPSHOT"] "1.2.3-SNAPSHOT"
       [1 2 0 "SNAPSHOT"] "1.2-SNAPSHOT"))

(deftest version-comparisons
  (are [a b c] (= a (compare-versions b c))
       1  "1.2" "1.1"
       -1 "1.1" "1.2"
       0  "1.1" "1.1"
       1  "0.1.1" "0.1"
       -1 "0.1"   "0.1.1"
       0  "0.1.1" "0.1.1"
       ;; ? "0.1.0" "0.1"
       ;; 1  "1.2" "1.2-RC2"
       0  "1.1-RC" "1.1-RC"
       0  "0.2-SNAPSHOT" "0.2-SNAPSHOT"
       1  "0.1"          "0.1-SNAPSHOT"
       -1 "0.1-SNAPSHOT" "0.1"))