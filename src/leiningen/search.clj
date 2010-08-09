(ns leiningen.search
  "Searches the indexed clojars.org repository. Giving -v as first argument prints the versions instead of the description."
  (:use (clojure.contrib duck-streams seq-utils str-utils)
	[leiningen.update-repo :only [*lein-dir* compare-versions]])
  (:import java.io.File))


(defn read-clj [f]
  (with-open [r (reader f)]
    (read (java.io.PushbackReader. r))))

(defn search-clojar [what]
  (let [p (re-pattern what)]
    (doall (filter
	    (fn [{description :description artifact-id :artifact-id group-id :group-id}] 
	      (or
	       (re-find p (or description ""))
	       (re-find p artifact-id)
	       (re-find p group-id)))
	    (read-clj (str *lein-dir* "/clojars"))))))

(defn artifact-name
  ([group-id article-id]
     (if (= group-id article-id)
       article-id
       (str group-id "/" article-id)))
  ([{artifact-id :artifact-id group-id :group-id}] 
     (artifact-name group-id artifact-id)))

(defn search [project what & args]
  (let [show-versions (= "-v" what)
	what (if show-versions (first args) what)]
    (if (.exists (File. (str *lein-dir* "/clojars")))
      (let [m (search-clojar what)]
	(println "Results for " what ":")
	(if show-versions
	  (println (str-join "\n" (map (fn [{versions :versions artifact-id :artifact-id group-id :group-id}] (str (artifact-name group-id artifact-id) ": " (str-join ", " versions))) m)))
	  (println (str-join "\n" (map (fn [{description :description artifact-id :artifact-id group-id :group-id}] (format "%-40s - %s" (artifact-name group-id artifact-id) (re-sub #"\n\s*" " " (or description "No description given")))) m)))))
  (println "No repo index found, please run lein update first."))))