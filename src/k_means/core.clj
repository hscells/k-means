(ns k-means.core
  (:gen-class))

(defn choose-initial-centroids
  "Randomly choose k centroids from vectors"
  ([l k] (choose-initial-centroids l k (vector)))
  ([l k c]
    (cond
      (< k 1) c
      :else (recur l (dec k) (conj c (rand-nth l))))))

(defn still-moving?
  "Predicate to test if the centroids have not reached equlibrium"
  [c]
  true)

(defn mean
  "Retrieve the mean value of a list"
  [l]
  (float (/ (reduce + l) (count l))))

(defn mean-centroids
  "Calculates the mean for a given centroid"
  [g]
  g)
; (defn centroid)

(defn distance
  "Implements euclidian distance between two vectors"
  ([[p1 p2] [q1 q2]]
    (Math/sqrt (+ (Math/pow (- q1 q2) 2) (Math/pow (- p1 p2) 2)))))
    ; (->> (map - c1 c2) (map #(* % %)) (reduce +))))) ; This is a better implementation for when this gets parallelised

(defn min-index
  [c]
  (.indexOf c (apply min c)))

(defn min-distance
  "returns the index of the centroid c closest to vecotr v"
  [c v]
    (for [i c :let [d (distance i v)]] d))

(defn cluster
  "take a vector and cluster it via distance"
  [g l c]
  (println g)
  (cond
    (empty? g) (recur (hash-set (range 0 (count c))) l c)
    (not-empty l) (recur (conj (first l) (get g (min-index (min-distance c (first l))))) (rest l) c)
    :else
    g))

(defn min-vector
  "Chooses the smallest vector in a set of vectors"
  [l]
  (map distance l))

(defn group
  "Group each vector based on the distance from each centroid"
  ([l c g]
    (cond
      (empty? l) g
      :else
        (recur (rest l) c ()))))

(defn k-means
  "Perform the actual k-means"
  ([l k]
    (k-means l k (choose-initial-centroids l k) (hash-set (range 0 k))))
  ([l k c g]
    (let [g (cluster g l c) c (mean-centroids g)]
      ; (println g)
      (cond
        (still-moving? c) ; Keep shifting centroids until equilibrium
          ; (recur nil k c g)
          g
        :else g))))

(defn -main
  "Main function for k-means"
  [& args]
  (k-means [[1 2] [3 2] [6 3] [1 8] [9 10] [1 4]] 3))
