(ns k-means.core-test
  (:require [clojure.test :refer :all]
            [k-means.core :refer :all]))

(deftest mean-test
  (testing "means"
    (is (= 2.5 (mean [1 2 3 4])))))

(deftest distance-test
  (testing "distance"
    (is (= 5.0 (distance [2 -1] [-2 2])))))
