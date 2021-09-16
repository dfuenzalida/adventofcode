(ns advent2020.day01-test
  (:require [advent2020.day01 :as day01]
            [clojure.test :refer [deftest testing is]]))

(def example
  [1721 979 366 299 675 1456])

(deftest day01-tests
  (testing "part 1"
    (is (= 514579 (day01/part-1 example))))

  (testing "part 2"
    (is (= 241861950 (day01/part-2 example)))))

