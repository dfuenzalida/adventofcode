(ns advent2021.day02-test
  (:require [advent2021.day02 :refer [example part-1 part-2 move-submarine
                                      move-submarine2 read-input]]
            [clojure.test :refer [deftest testing is]]))

(deftest day02-tests

  (testing "part 1"
    (is (= [15 10] (reduce move-submarine [0 0] (read-input example))))
    (is (= 150 (part-1 example))))

  (testing "part 2"
    (is (= [15 60 10] (reduce move-submarine2 [0 0 0] (read-input example))))
    (is (= 900 (part-2 example)))))

