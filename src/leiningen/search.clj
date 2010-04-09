(ns leiningen.search
  "Searches the indexed clojars.org repository."
  (:use (clojure.contrib duck-streams seq-utils str-utils)
	[leiningen.update :only [*lein-dir*]])
  (:import java.io.File))


(defn match-it [what e]
  (try
   (if (string? e)
     (if (re-find (re-pattern what) e)
       e
       nil)
     (if (re-find (re-pattern what) (first e))
       e
       (let [r (remove nil? (map (partial match-it what) (second e)))]
	 (if (empty? r)
	   nil
	   [(first e) r]))))
   (catch Throwable err (pr err) (println e))))

(defn format-mvn [p]
  (if (vector? p)
    (map (fn [#^String s]
	   (cond 
	    (= -1 (.indexOf s " "))
	    (str (first p) " " s)
	    (= -1 (.indexOf s "/"))
	    (str (first p) "/" s)
	    :else
	    (str (first p) "." s)))  (flatten (map format-mvn (second p))))
    p))

(defn format-results [r]
  (flatten (map  format-mvn r)))

(defn group-results [r]
  (vec (reduce (fn [groups [a v]] (update-in groups [(symbol a)] conj v)) {} (map #(re-split #" " %) (format-results r)))))
  

(defn read-clj [f]
  (with-open [r (reader f)]
    (read (java.io.PushbackReader. r))))

(defn find-clojar [what]
    (doall (remove nil? (map (partial match-it what)  (read-clj (str *lein-dir* "/clojars"))))))

(defn search [project what & args]
  (if (.exists (File. (str *lein-dir* "/clojars")))
    (let [m (find-clojar what)]
      (println (str "Results for " what ":"))
      (println (str-join "\n" (map (fn [[artifact versions]] (str artifact ": " (str-join ", " versions))) (group-results m)))))
  (println "No repo index found, please run lein update first.")))