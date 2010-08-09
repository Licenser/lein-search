(ns leiningen.add
  "Interactively adds a dependency from clojars.
  With one parameter it will add the latest stable of the corresponding version with two arguments it will take the second argument as version.
  If the first parameter is --dev or -d it will work exactly as without --dev just that it will add a dev dependency."
  (:use (clojure.contrib duck-streams seq-utils str-utils)
	[leiningen.update-repo :only [*lein-dir*]]
	[leiningen.search :only [read-clj search-clojar artifact-name]]))


(defn good-read-line [] 
  (binding [*in* (-> System/in (java.io.InputStreamReader.) (clojure.lang.LineNumberingPushbackReader.))] (read-line)))

(defn prompt-for-input [prompt]
  (print prompt)
  (flush)
  (chomp (good-read-line)))

(defn prompt-for-number [prompt]
  (try (Integer/parseInt (prompt-for-input prompt))
       (catch java.lang.NumberFormatException e nil)))

(defn update-dependency-list
  "Modify the project's dependency list of the given type by passing it through f"
  [project dep-type f]
  (->> project
       (reduce
        (fn [[form prev] n]
          (if (= prev dep-type)
            [(cons (vec (f n)) form) nil]
            [(cons n form) n]))
        [() nil])
       first
       reverse))

(defn add-artifact [project type artifact version]
  (update-dependency-list project type
                          (fn [deps]
                            (cons [(symbol artifact) version] deps))))

(defn find-clojar [what]
  (let [p (re-pattern what)]
    (let [[group  artifac] (if (= -1 (.indexOf what "/")) [what what] (next (re-find #"^([^/]+)/(.+)$" what)))]
    (doall (filter 
	    (fn [{artifact-id :artifact-id group-id :group-id}] 
	      (and
	       (= artifact-id artifac)
	       (= group-id group)))
	    (read-clj (str *lein-dir* "/clojars")))))))

(defn choose-from-numbered-list-if-multiple
  "Return first item immediately if there is only one, otherwise prompt the user
with a numbered list of choices."
  [choices prompt formatter]
  (if (= 1 (count choices))
     (first choices)
     (do (println
          (str-join "\n"
                    (for [[n i] (map vector (iterate inc 1) choices)]
                      (str n ": " (formatter i)))))
         (loop []
           (let [v (prompt-for-number (str prompt ": "))]
             (if (or (nil? v) (nil? (nth choices (dec v) nil)))
               (recur)
               (nth choices (dec v))))))))

(defn get-version [[artifact-name available-versions]]
  [artifact-name
   (choose-from-numbered-list-if-multiple available-versions
                                          "Please select a version"
                                          (fn [v] (str artifact-name " " v)))])

(defn latest-stable [versions]
  (first (filter (partial re-find #"^(\d+).(\d+).(\d+)$") versions)))

(defn get-artifact-id [res]
  (get-version
   (choose-from-numbered-list-if-multiple res
                                          "Please select an artifact"
                                          (fn [[name vers]]
                                            (str name " (" (str-join ", " vers) ")")))))

(defn add [project artifact & args]
  (let [dev (or (= artifact "--dev") (= artifact "-d"))
	artifact (if dev (first args) artifact)
	args (if dev (rest args) args)
	version (first args)
	p (:root project)
	res  (first (find-clojar artifact))]
    (if (empty? res)
      (println "Sorry; nothing on clojars that matches" artifact (if version ""))
      (if (and version (not-any? (partial = version) (:versions res)))
	(println "Sorry; there is no version" version "for" (artifact-name res) ". Try one of:" (str-join ", " (:versions res)))
	(let [[a v] [(artifact-name res) (if version version (latest-stable (:versions res)))]
	      p (read-clj (str (:root project) "/project.clj"))]
	  (println "Adding:" a v)
	  (with-open [o (writer (str (:root project) "/project.clj"))]
	    (binding [*out* o]
	      (pr (add-artifact p (if dev :dev-dependencies :dependencies) a v)))))))))