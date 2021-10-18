(ns advent2020.day11-test
  (:require [advent2020.day11 :refer [example iter-seats part-1 part-2 parse-input]]
            [clojure.test :refer [deftest testing is]]))

(declare example-iter1 example-iter2)

(deftest day11-tests
  (testing "part 1"
    (is (= (parse-input example-iter1) (iter-seats (parse-input example))))
    (is (= (parse-input example-iter2) (iter-seats (parse-input example-iter1))))
    (is (= 37 (part-1 example))))

  (testing "part 2"
    (is (= 26 (part-2 example)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example-iter1
  "#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##")

(def example-iter2
  "#.LL.L#.##
#LLLLLL.L#
L.L.L..L..
#LLL.LL.L#
#.LL.LL.LL
#.LLLL#.##
..L.L.....
#LLLLLLLL#
#.LLLLLL.L
#.#LLLL.##")
