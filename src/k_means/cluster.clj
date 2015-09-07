(ns k-means.cluster
  "Clustering related methods"
  (:gen-class)
  (:require [taoensso.timbre.profiling :as profiling :refer (p profile)])
  (:require [k-means.vector :as vector]))

(defn mean
  "Retrieve the mean value of a list"
  [l]
  (float (/ (reduce + l) (count l))))

(defn mean-2dvec
  [v]
  (cond
    (empty? v) v
    :else
      (let [
        a (map first v)
        b (flatten (map rest v))
        x (float (/ (reduce + a) (count a)))
        y (float (/ (reduce + b) (count b)))]
        [x y])))


(defn mean-centroids
  "Calculates the mean for a given centroid"
  ([g] (mean-centroids g []))
  ([g l]
    (cond
      (empty? g) l
      :else
        (recur (rest g) (conj l (p :mean-2dvec (mean-2dvec (first g))))))))

(defn distance
  "Implements euclidian distance between two vectors"
  ([c1 c2]
    (->> (map - c1 c2) (map #(* % %)) (reduce +))))

(defn distance-p
  "Implements euclidian distance between two vectors"
  ([c1 c2]
    (->> (pmap - c1 c2) (pmap #(* % %)) (reduce +))))

(defn min-index
  [c]
  (.indexOf c (apply min c)))

(defn min-distance
  "returns the index of the centroid c closest to vecotr v"
  [c v]
    (for [i c :let [d (distance i v)]] d))

(defn min-distance-p
  "returns the index of the centroid c closest to vecotr v"
  [c v]
    (for [i c :let [d (distance-p i v)]] d))

(defn min-vector
  "Chooses the smallest vector in a set of vectors"
  [l]
  (map distance l))

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

(defn -dist-cluster
  "perform recursive clustering"
  ([l c] (-dist-cluster l c []))
  ([l c a]
    (cond
      (empty? l) a
      (not-empty l)
        (let [i (min-index (p :min-distance (min-distance-p c (first l))))]
          (recur (rest l) c (conj a (first l)))))))

(defn -group-clusters
  "group a list of cluster groups into one cluster group"
  ([l] (-group-clusters l (vector/make-vector (count l)) 0))
  ([l a i]
    (cond
      (empty? l) (pmap first a)
      :else
        (recur (rest l) (assoc a i (conj (nth a i) (nth (first l) i))) (inc i)))))

(defn cluster-p
  "take a list of vectors and cluster on centroids c via distance in parallel"
  ([l c] (cluster-p l c []))
  ([l c a]
    (let [clusters (for [i (partition-all (int (/ (count l) (count c))) l) :let [m (p :distance-cluster (future (cluster i c)))]] (deref m))]
      (-group-clusters clusters))))
