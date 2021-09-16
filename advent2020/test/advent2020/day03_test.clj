(ns advent2020.day03-test
  (:require [advent2020.day03 :refer [count-trees example]]
            [clojure.test :refer [deftest testing is]]))

(deftest day03-tests
  (testing "part 1"
    (is (= 7 (count-trees example 3 1))))

  (testing "part 2"
    (is (= 2 (count-trees example 1 1)))
    (is (= 7 (count-trees example 3 1)))
    (is (= 3 (count-trees example 5 1)))
    (is (= 4 (count-trees example 7 1)))
    (is (= 2 (count-trees example 1 2)))
    (is (->> (map #(apply count-trees example %) [[1 1] [3 1] [5 1] [7 1] [1 2]])
             (reduce *)
             (= 336)))))

