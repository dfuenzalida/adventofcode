(ns aoc-2019.day12-test
  (:require [clojure.test :refer :all]
            [aoc-2019.day12 :refer :all]))

(def example1 ["<x=-1, y=0, z=2>"
               "<x=2, y=-10, z=-7>"
               "<x=4, y=-8, z=8>"
               "<x=3, y=5, z=-1>"])

(def example2 ["<x=-8, y=-10, z=0>"
               "<x=5, y=5, z=10>"
               "<x=2, y=-7, z=3>"
               "<x=9, y=-8, z=-3>"])

(deftest tests
  (testing "Day 12 part 1"

    (let [moons  (parse-input example1)
          moons' (-> (iterate simulate moons) (nth 10))
          energy (->> moons' (map energy) (reduce +))]
      (is (= 179 energy)))

    (let [moons  (parse-input example2)
          moons' (-> (iterate simulate moons) (nth 100))
          energy (->> moons' (map energy) (reduce +))]
      (is (= 1940 energy))))

  )

