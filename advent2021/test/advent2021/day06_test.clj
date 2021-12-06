(ns advent2021.day06-test
  (:require [advent2021.day06 :refer [example count-fish]]
            [clojure.test :refer [deftest testing is]]))

(deftest day06-tests
  (testing "parts 1 and 2"
    (is (= 26 (count-fish example 18)))
    (is (= 5934 (count-fish example 80)))
    (is (= 26984457539 (count-fish example 256)))))

