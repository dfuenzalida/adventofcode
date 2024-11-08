(ns advent2022.day13-test
  (:require [advent2022.day13 :refer [example right-order? part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day13-tests

  (testing "part 1"
    (is (= true (right-order? [1 1 3 1 1] [1 1 5 1 1])))
    (is (= true (right-order? [[1] [2 3 4]] [[1] 4])))
    (is (= false (right-order? [9] [[8 7 6]])))
    (is (= true (right-order? [[4,4],4,4] [[4,4],4,4,4])))
    (is (= false (right-order? [7 7 7 7] [7 7 7])))
    (is (= true (right-order? [] [3])))
    (is (= false (right-order? [1,[2,[3,[4,[5,6,7]]]],8,9] [1,[2,[3,[4,[5,6,0]]]],8,9])))
    (is (= 13 (part-1 example))))

  (testing "part 2"
    (is (= 140 (part-2 example)))))

