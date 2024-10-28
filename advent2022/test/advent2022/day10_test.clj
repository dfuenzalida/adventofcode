(ns advent2022.day10-test
  (:require [advent2022.day10 :refer [example2 part-1 part-2]]
            [clojure.test :refer [deftest testing is]]))

(deftest day10-tests

  (testing "part 1"
    (is (= (part-1 example2) 13140)))

  (testing "part 2"
    (is (= (part-2 example2)
           ["##..##..##..##..##..##..##..##..##..##.."
            "###...###...###...###...###...###...###."
            "####....####....####....####....####...."
            "#####.....#####.....#####.....#####....."
            "######......######......######......####"
            "#######.......#######.......#######....."]))))

