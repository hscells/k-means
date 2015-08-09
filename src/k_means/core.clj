(ns k-means.core
  (:gen-class))

(defn choose-initial-centroids
  "Randomly choose k centroids from vectors"
  ([l k] (choose-initial-centroids l k (vector)))
  ([l k c]
    (cond
      (< k 1) c
      :else (recur l (dec k) (conj c (rand-nth l))))))

; (defn centroid)

(defn distance
  "Implements euclidian distance between two vectors"
  ([[p1 p2] [q1 q2]]
    (Math/sqrt (+ (Math/pow (- q1 q2) 2) (Math/pow (- p1 p2) 2)))))
    ; (->> (map - c1 c2) (map #(* % %)) (reduce +)))))

; (defn group)

(defn k-means
  "Perform the actual k-means "
  ([l k]
    (k-means l k (choose-initial-centroids l k)))
  ([l k c]
    (println c)))

(defn -main
  "Main function for k-means"
  [& args]
  (k-means [[1 2] [3 2] [6 3] [1 8] [9 10] [1 4]] 2))
