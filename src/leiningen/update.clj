(ns leiningen.update
  "Updates the clojars.org repositories index."
  (:use pl.danieljanus.tagsoup clojure.contrib.duck-streams)
  (:import (javax.xml.parsers SAXParserFactory) java.io.File))

(def *lein-dir* (str (System/getProperty "user.home") "/.lein"))

(defn nv-startparse [s ch]
  (let [spf (. SAXParserFactory newInstance)]
  (doto spf
  (.setValidating false)
  (.setSchema nil)
  (.setFeature "http://xml.org/sax/features/namespaces", false)
  (.setFeature "http://xml.org/sax/features/validation", false)
  (.setFeature "http://apache.org/xml/features/nonvalidating/load-dtd-grammar", false)
  (.setFeature "http://apache.org/xml/features/nonvalidating/load-external-dtd", false))
  (.. spf newSAXParser (parse s ch))))

(defn find-links [x]
  (filter #(not= (first %) \.)
	  (map 
	   #(:href (second %)) 
	   (filter #(and (vector? %) (= (first %) :a)) (tree-seq #(and (vector? %) (vector? (nth % 2))) #(nnext %) x)))))

  
(defn remove-slash [str]
  (second (re-find #"(.*)/$" str)))

(defn fetch-from 
  ([url]
     (if (= url "http://clojars.org/repo/org/clojure/") ;silly fix for a strange link in clojars, this links to a different page which is badish bad
       '()
       (remove nil? 
	       (pmap (fn [d]
		      (if (= (last d) \/) 
			(let [sub (fetch-from (str url d))]
			  (if (empty? sub)
			    (remove-slash d)
			    [(remove-slash d) sub]))))
		     (find-links (parse url))))))
  ([] (fetch-from "http://clojars.org/repo/")))

(defn update [project & args]
  (if (not (.exists (File. *lein-dir*))) (.mkdirs (File. *lein-dir*)))
  (with-open [w (writer (str  *lein-dir* "/clojars"))]
    (println "Getting the list of packages on clojars.org this can take a while ...")
    (binding [*out* w] (pr (fetch-from)))))
