(ns k-means.core
  (:gen-class)
  (:require [taoensso.timbre.profiling :as profiling :refer (p profile)])
  (:require [k-means.cluster :as cluster]
            [k-means.vector  :as vector]
            [clojure.string  :as string]))


(defn choose-initial-centroids
  "Randomly choose k centroids from vectors"
  ([l k] (choose-initial-centroids l k (list)))
  ([l k c]
    (cond
      (< k 1) c
      :else (recur l (dec k) (conj c (rand-nth l))))))

(defn still-moving?
  "Predicate to test if the centroids have not reached equlibrium"
  [t c]
  (not (= t c)))

(defn k-means
  "Perform the actual k-means"
  ([l k max-iterations]
    (k-means l k (p :choose-initial-centroids (choose-initial-centroids l k)) (vector/make-vector k) max-iterations))
  ([l k c g max-iterations]
    (let [t c g (p :cluster (cluster/cluster l c)) c (p :mean-centroids (cluster/mean-centroids g))]
      ; (println "Iteration" (- 100 max-iterations) c)
      (cond
        (> 0 max-iterations) g
        (still-moving? t c) ; Keep shifting centroids until equilibrium
          (recur l k c g (dec max-iterations))
        :else g))))

(defn k-means-p
  "Perform k-means using concurrent paradigms"
  ([l k max-iterations]
    (k-means-p l k (p :choose-initial-centroids (choose-initial-centroids l k)) (vector/make-list k) max-iterations))
  ([l k c g max-iterations]
    (let [t c g (p :cluster-parallel (cluster/cluster-p l c)) c (p :mean-centroids-parallel (cluster/mean-centroids-p g))]
      ; (println "Iteration" (- 100 max-iterations) c)
      (cond
        (> 0 max-iterations) g
        (still-moving? t c) ; Keep shifting centroids until equilibrium
          (recur l k c g (dec max-iterations))
        :else g))))

(def usage
  (->> ["Concurrent k-means for INB375 at QUT"
        "Harry Scells 2015"
        "Usage: k-means [action] [options]"
        ""
        "Actions:"
        "  testseq        Test the sequential algorithm with [k] clusters and [n] points"
        "  testpar        Test the parallel algorithm with [k] clusters and [n] points"
        "  seq            Run the sequential algorithm with [k] clusters on [dataset]"
        "  par            Run the parallel algorithm with [k] clusters on [dataset]"
        ""
        "Please refer to the README for more information."]
       (string/join \newline)))

(defn -main
  "Main function for k-means"
  [& args]
  (cond
    (not (= 3 (count args))) (println usage)
    (= (first args) "testseq")
      (let [c (profile :info :Arithmetic (k-means (vector/make-list-rand (Integer/parseInt (nth args 2))) (Integer/parseInt (second args)) 100)) => "Done"]
        (spit "k-means.txt" (vec c)))
    (= (first args) "testpar")
      (let [c (profile :info :Arithmetic (k-means-p (vector/make-list-rand (Integer/parseInt (nth args 2))) (Integer/parseInt (second args)) 100)) => "Done"]
        (spit "k-means.txt" (vec c)))
    (= (first args) "seq")
      (let [c (profile :info :Arithmetic (k-means (read-string (nth args 2)) (Integer/parseInt (second args)) 100)) => "Done"]
        (spit "k-means.txt" (vec c)))
    (= (first args) "par")
      (let [c (profile :info :Arithmetic (k-means-p (read-string (nth args 2)) (Integer/parseInt (second args)) 100)) => "Done"]
        (spit "k-means.txt" (vec c)))
    :else
      (println usage))
  (shutdown-agents))
