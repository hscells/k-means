(ns k-means.vector
  "General vector related functions"
  (:gen-class)
  (:require [taoensso.timbre.profiling :as profiling :refer (p profile)]))

(defn make-vector-rand
  "create a vector containing n cols populated with random data"
  ([n] (make-vector-rand n []))
  ([n c]
  (cond
    (<= n 0) c
    :else
      (recur (dec n) (conj c [(rand-int 50000) (rand-int 50000)])))))

(defn make-vector
  "create a vector containing n cols"
  ([n] (make-vector n []))
  ([n c]
    (cond
      (<= n 0) c
      :else
        (recur (dec n) (conj c [])))))


(defn make-list-rand
  "create a vector containing n cols populated with random data"
  ([n] (make-vector-rand n '()))
  ([n c]
  (cond
    (<= n 0) c
    :else
      (recur (dec n) (conj c '((rand-int 50000) (rand-int 50000)))))))

(defn make-list
  "create a vector containing n cols"
  ([n] (make-vector n []))
  ([n c]
    (cond
      (<= n 0) c
      :else
        (recur (dec n) (conj c '())))))
