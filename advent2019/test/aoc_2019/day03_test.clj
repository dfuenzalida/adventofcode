(ns aoc-2019.day03-test
  (:require [clojure.test :refer :all]
            [aoc-2019.day03 :refer :all]))

(deftest tests
  (testing "Day 03 part 1"

    (is (= 6
           (min-distance "R8,U5,L5,D3"
                         "U7,R6,D4,L4")))
    (is (= 159
           (min-distance "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                         "U62,R66,U55,R34,D71,R55,D58,R83")))
    (is (= 135
           (min-distance "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                         "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))))

  (testing "Day 03 part 2"

    (is (= 30
           (least-steps "R8,U5,L5,D3" "U7,R6,D4,L4")))

    (is (= 610
           (least-steps "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                        "U62,R66,U55,R34,D71,R55,D58,R83")))

    (is (= 410
           (least-steps "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
               "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")))))


