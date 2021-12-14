(ns advent2021.day13-test
  (:require [advent2021.day13 :refer [bounce example part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(declare expected)

(deftest day13-tests

  (testing "part 1"
    (is (= [0 1 2 3 4 5 4 3 2 1 0] (map (partial bounce 5) (range 11))))
    (is (= 17 (part-1 example))))

  (testing "part 2"
    (is (= expected (part-2 example)))))

(def expected "#####
#   #
#   #
#   #
#####
     
     ")

