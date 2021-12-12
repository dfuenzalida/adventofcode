(ns advent2021.day11-test
  (:require [advent2021.day11 :refer [example example2 iterate-map
                                      read-input part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(declare small0 small1 small2)

(deftest day11-tests

  (testing "part 1"
    (let [[s0 s1 s2] (map read-input [small0 small1 small2])]
      (is (= [s0 s1 s2] (take 3 (iterate (comp first iterate-map) s0)))))

    (is (= 1656 (part-1 example))))

  (testing "part 2"
    (is (= 195 (part-2 example)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def small0 "11111
19991
19191
19991
11111")

(def small1 "34543
40004
50005
40004
34543")

(def small2 "45654
51115
61116
51115
45654")
       
