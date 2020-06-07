(ns aoc-2019.day16-test
  (:require [clojure.test :refer :all]
            [aoc-2019.day16 :refer :all]))

(deftest tests
  (testing "Day 16 part 1"

    (is (= [2 4 1 7 6 1 7 6]
           (solve-1 "80871224585914546619083218645595")))

    (is (= [7 3 7 4 5 4 1 8]
           (solve-1 "19617804207202209144916044189917")))

    (is (= [5 2 4 3 2 1 3 3]
           (solve-1 "69317163492948606335995924319873"))))

  )
