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

(defn min-index
  [c]
  (.indexOf c (apply min c)))

(defn min-distance
  "returns the index of the centroid c closest to vecotr v"
  [c v]
    (for [i c :let [d (distance i v)]] d))

(defn min-vector
  "Chooses the smallest vector in a set of vectors"
  [l]
  (map distance l))

(defn cluster
  "take a vector and cluster it via distance"
  ([g l c] (cluster g l c (vector/make-vector (count c))))
  ([g l c a]
    (cond
      ; g is groups of c, so create initial vectors of g
      (nil? g) (recur (vector/make-vector (count c)) l c (vector/make-vector (count c)))
      ; otherwise we continue until the list of vectors is empty
      (not-empty l)
        ; for the vector at the start of the list, choose a centroid which is closest
        (let [i (min-index (p :min-distance (min-distance c (first l))))]
          ; append the vector to the smallest distance
          (recur g (rest l) c (assoc a i (conj (nth a i) (first l)))))
      :else
      a)))
