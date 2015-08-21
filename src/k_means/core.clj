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
  [t c]
  (not (= t c)))

(defn mean
  "Retrieve the mean value of a list"
  [l]
  (float (/ (reduce + l) (count l))))

(defn mean-2dvec
  [v]
  (let [
    a (map first v)
    b (flatten (map rest v))
    x (float (/ (reduce + a) (count a)))
    y (float (/ (reduce + b) (count b)))]
    [x y]))

;; This is the next method which will be implemented...
(defn mean-centroids
  "Calculates the mean for a given centroid"
  ([g] (mean-centroids g []))
  ([g l]
    (cond
      (empty? g) l
      :else
        (recur (rest g) (conj l (mean-2dvec (first g)))))))

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

(defn make-vector
  "create a vector containing n cols"
  ([n] (make-vector n []))
  ([n c]
    (cond
      (<= n 0) c
      :else
        (recur (dec n) (conj c [])))))

(defn cluster
  "take a vector and cluster it via distance"
  [g l c]
  (cond
    (nil? g) (recur (make-vector (count c)) l c)
    (not-empty l)
      (let [i (min-index (min-distance c (first l)))]
        (recur (assoc g i (conj (nth g i) (first l))) (rest l) c))
    :else
    g))

(defn min-vector
  "Chooses the smallest vector in a set of vectors"
  [l]
  (map distance l))

(defn k-means
  "Perform the actual k-means"
  ([l k max-iterations]
    (k-means l k (choose-initial-centroids l k) (make-vector k) max-iterations))
  ([l k c g max-iterations]
    (let [t c g (cluster g l c) c (mean-centroids g)]
      (println t c (= t c))
      ; (println (still-moving? t c))
      (cond
        (> 0 max-iterations) g
        (still-moving? t c) ; Keep shifting centroids until equilibrium
          (recur l k c g (dec max-iterations))
        :else g))))

(defn -main
  "Main function for k-means"
  [& args]
  (let [c (k-means [[1 2] [3 2] [6 3] [1 8] [9 10] [1 4] [56 67] [44 53] [43 54] [43 64] [76 56]] 3 100)] (println c)))
