(ns k-means.core-test
  (:require [clojure.test     :refer :all]
            [k-means.cluster  :as cluster]
            [k-means.vector   :as vector]
            [k-means.core     :as core]))

(deftest distance-test
  (testing "distance"
    (is (= 25 (cluster/distance [2 -1] [-2 2])))))
