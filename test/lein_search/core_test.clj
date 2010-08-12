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

(deftest adding-artifact-versions
  (is (= '(defproject "whatever" :dependencies [[a "1.2"]])
         (add-artifact '(defproject "whatever" :dependencies [])
                       :dependencies 'a "1.2")))
  (is (= '(defproject "whatever" :dev-dependencies [[a "1.2"]])
      (add-artifact '(defproject "whatever" :dev-dependencies [])
                    :dev-dependencies 'a "1.2")))
  (is (= '(defproject "whatever" :dev-dependencies [[a "1.2"]])
         (add-artifact '(defproject "whatever")
                       :dev-dependencies 'a "1.2"))))

(deftest updating-artifact-versions
  (is (= '(defproject "whatever" :dependencies [[a "1.2"]])
         (update-artifact '(defproject "whatever" :dependencies [[a "1.1"]])
                          :dependencies 'a "1.2")))
  (is (= '(defproject "whatever" :dev-dependencies [[a "1.2"]])
         (update-artifact '(defproject "whatever" :dev-dependencies [[a "1.1"]])
                          :dev-dependencies 'a "1.2"))))

(deftest picking-latest-stable-version
  (is (= "1.2.0" (latest-stable ["1.1.8" "1.2.0-RC" "1.2.0" "1.2.1-SNAPSHOT"])))
  (is (= "1.2" (latest-stable ["1.1" "1.2-RC" "1.2" "1.2-SNAPSHOT"]))))