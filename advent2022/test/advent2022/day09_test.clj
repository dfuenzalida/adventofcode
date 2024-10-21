(ns advent2022.day09-test
  (:require [advent2022.day09 :refer [example example2 move-tail touching? part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day09-tests

  (testing "part 1"
    (is (= true (touching? [0 0] [0 0])))
    (is (= true (touching? [1 1] [2 1])))
    (is (= true (touching? [1 1] [2 2])))
    (is (= false (touching? [1 1] [3 1])))
    (is (= true (touching? [4 1] [3 0])))

    (is (= [0 0] (move-tail [0 0] [0 0])))
    (is (= [0 0] (move-tail [1 0] [0 0])))
    (is (= [1 0] (move-tail [2 0] [0 0])))
    (is (= [3 1] (move-tail [4 2] [2 0])))

    (is (= 13 (part-1 example))))
  
  (testing "part 2"
    (is (= 1 (part-2 example)))
    (is (= 36 (part-2 example2)))))

