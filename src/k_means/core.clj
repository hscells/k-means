(ns k-means.core
  (:gen-class)
  (:require [taoensso.timbre.profiling :as profiling :refer (p profile)])
  (:require [k-means.cluster :as cluster]
            [k-means.vector :as vector]))


(defn choose-initial-centroids
  "Randomly choose k centroids from vectors"
  ([l k] (choose-initial-centroids l k (vector)))
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
      (println "Iteration" (- 100 max-iterations) c)
      (cond
        (> 0 max-iterations) g
        (still-moving? t c) ; Keep shifting centroids until equilibrium
          (recur l k c g (dec max-iterations))
        :else g))))

(defn k-means-p
  "Perform k-means using concurrent paradigms"
  ([l k max-iterations]
    (k-means-p l k (p :choose-initial-centroids (choose-initial-centroids l k)) (vector/make-vector k) max-iterations))
  ([l k c g max-iterations]
    (let [t c g (p :cluster-parallel (cluster/cluster-p l c)) c (p :mean-centroids (cluster/mean-centroids g))]
      (println "Iteration" (- 100 max-iterations) c)
      (cond
        (> 0 max-iterations) g
        (still-moving? t c) ; Keep shifting centroids until equilibrium
          (recur l k c g (dec max-iterations))
        :else g))))

(defn -main
  "Main function for k-means"
  [& args]
  (let [c (profile :info :Arithmetic (k-means-p (vector/make-vector-rand 40000) 10 25)) => "Done"]
    (spit "k-means.txt" c)) (shutdown-agents))
