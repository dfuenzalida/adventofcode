(ns aoc-2019.day09-test
  (:require [clojure.test :refer :all]
            [aoc-2019.intcode :refer [execute]]))

(deftest tests
  (testing "Day 09 part 1"

    ;; Takes no input and produces a copy of itself as output
    (let [prog [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]]
      (is (= prog (->> (execute prog []) last))))

    ;; should output a 16-digit number:
    (let [prog [1102,34915192,34915192,7,4,7,99,0]]
      (is (= 16 (->> (execute prog []) last last str count))))

    ;; output is middle number
    (let [middle 1125899906842624
          prog   [104,middle,99]]
      (is (= middle (->> (execute prog []) last last)))))

  )
