(ns k-means.cluster
  "Clustering related methods"
  (:gen-class)
  (:require [taoensso.timbre.profiling :as profiling :refer (p profile)])
  (:require [k-means.vector :as vector]
            [clojure.core.reducers :as reducers]))

(def n-cpu (.availableProcessors (Runtime/getRuntime)))

(defn mean-2dvec
  [^ints v]
  (cond
    (empty? v) v
    :else
      (let [
        a (map first v)
        b (flatten (map rest v))
        x (float (/ (reduce + a) (count a)))
        y (float (/ (reduce + b) (count b)))]
        (list x y))))

(defn mean-centroids
  "Calculates the mean for a given centroid"
  ([g] (mean-centroids g []))
  ([g l]
    (cond
      (empty? g) l
      :else
        (recur (rest g) (conj l (p :mean-2dvec (mean-2dvec (first g))))))))

(defn mean-centroids-p
  "Calculates the mean for a given centroid"
  [g]
  (map #(p :mean-2dvec (mean-2dvec %)) g))

(defn distance
  "Implements euclidian distance between two vectors"
  ([^ints c1 ^ints c2]
    (->> (map - c1 c2) (map #(* % %)) (reducers/fold +))))

(defn min-index
  [c]
  (.indexOf c (apply min c)))

(defn min-distance
  "returns the index of the centroid c closest to vecotr v"
  [c v]
    (for [i c :let [d (distance i v)]] d))

(defn cluster
  "take a list of vectors and cluster on centroids c via distance"
  ([l c] (cluster l c (vector/make-vector (count c))))
  ([l c a]
    (cond
      (empty? l) a
      ; otherwise we continue until the list of vectors is empty
      (not-empty l)
        ; for the vector at the start of the list, choose a centroid which is closest
        (let [i (min-index (p :min-distance (min-distance c (first l))))]
          ; append the vector to the smallest distance
          (recur (rest l) c (assoc a i (conj (nth a i) (first l)))))
      :else
      a)))

(defn group-clusters
  "group a list of cluster groups into one cluster group"
  ([^ints l] (group-clusters l (vector/make-list (count l))))
  ([^ints l ^ints a]
    (cond
      (empty? l) a
      :else
        (recur (rest l) (pmap into (first l) a)))))

(defn cluster-p
  "take a list of vectors and cluster on centroids c via distance in parallel"
  [^ints l ^floats c]
    ; first split the list of observations into chunks and run the sequental cluster algorithm over each chunk
    (let [futures (doall (map #(future (cluster % c)) (partition-all (int (/ (count l) n-cpu)) l))) clusters (map deref futures)]
      ; secondly, join each sub-set of clusters into one large set
      (p :group-clusters (group-clusters clusters))))
