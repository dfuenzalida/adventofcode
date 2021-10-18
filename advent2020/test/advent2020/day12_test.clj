(ns advent2020.day12-test
  (:require [advent2020.day12 :refer [example mdistance move move2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day12-tests
  (testing "part 1"
    (is (= [17 -8] (take 2 (reduce move [0 0 0] example))))
    (is (= 25 (mdistance (reduce move [0 0 0] example)))))

  (testing "part 2"
    (is (= (move2 [0 0 10 1] ["F" 10]) [100 10 10 1]))
    (is (= (move2 [100 10 10 1] ["N" 3]) [100 10 10 4]))
    (is (= (move2 [100 10 10 4] ["F" 7]) [170 38 10 4]))
    (is (= (move2 [170 38 10 4] ["R" 90]) [170 38 4 -10]))
    (is (= 286 (mdistance (reduce move2 [0 0 10 1] example))))))
