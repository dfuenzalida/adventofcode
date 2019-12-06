(ns aoc-2019.day06-test
  (:require [clojure.test :refer :all]
            [aoc-2019.day06 :refer :all]))

(def example1
  (str "COM)B\n"
       "B)C\n"
       "C)D\n"
       "D)E\n"
       "E)F\n"
       "B)G\n"
       "G)H\n"
       "D)I\n"
       "E)J\n"
       "J)K\n"
       "K)L"))

(deftest tests
  (testing "Day 06 part 1"
    (is (= 42 (solve-1 (build-map example1)))))

  (testing "Day 06 part 2"
    (let [example2 (merge (build-map example1) {"YOU" "K", "SAN" "I"})]
      (is (= 4 (solve-2 example2 "YOU" "SAN")))))

  )
