(ns advent2021.day05-test
  (:require [advent2021.day05 :refer [example horz-vert-points line-points part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day05-tests

  (testing "part 1"
    (is (= #{[1 1] [1 2] [1 3]} (set (horz-vert-points [1 1 1 3]))))
    (is (= #{[7 7] [8 7] [9 7]} (set (horz-vert-points [9 7 7 7]))))
    (is (= 5 (part-1 example))))

  (testing "part 2"
    (is (= [[1 1] [2 2] [3 3]] (line-points [1 1 3 3])))
    (is (= 12 (part-2 example)))))

