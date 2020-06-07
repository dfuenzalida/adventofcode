(ns aoc-2019.day10-test
  (:require [clojure.test :refer :all]
            [aoc-2019.day10 :refer :all]))

(declare example1 example2 example3 example4 example5 example-part2)

(deftest tests
  (testing "Day 10 part 1 - Finding best place"
    (is (= [3 4] (best-location example1)))
    (is (= [5 8] (best-location example2)))
    (is (= [1 2] (best-location example3)))
    (is (= [6 3] (best-location example4)))
    (is (= [11 13] (best-location example5))))

  (testing "Day 10 part 1 - Counting visible asteroids"
    (is (= [8 [3 4]] (visible (coords example1) [3 4])))
    (is (= [33 [5 8]] (visible (coords example2) [5 8])))
    (is (= [35 [1 2]] (visible (coords example3) [1 2])))
    (is (= [41 [6 3]] (visible (coords example4) [6 3])))
    (is (= [210 [11 13]] (visible (coords example5) [11 13]))))

  (testing "Day 10 part 2"
    (is (= [8 3] (best-location example-part2)))

    (is (= 0.0   (angle [ 0 -1])))
    (is (= 90.0  (angle [ 1  0])))
    (is (= 180.0 (angle [ 0  1])))
    (is (= 270.0 (angle [-1  0])))

    (is (= 45.0  (angle [ 1 -1])))
    (is (= 135.0 (angle [ 1  1])))
    (is (= 225.0 (angle [-1  1])))
    (is (= 315.0 (angle [-1 -1])))

    (let [laser-targets (laser-list example5)]
      (is (= [[11 12] [12 1] [12 2]] (take 3 laser-targets)))
      (is (= [12 8] (nth laser-targets 9)))
      (is (= [16 0] (nth laser-targets 19)))
      (is (= [16 9] (nth laser-targets 49)))
      (is (= [10 16] (nth laser-targets 99)))
      (is (= [9 6] (nth laser-targets 198)))
      (is (= [8 2] (nth laser-targets 199)))
      (is (= [10 9] (nth laser-targets 200)))
      (is (= [11 1] (last laser-targets))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example1
  [".#..#"
   "....."
   "#####"
   "....#"
   "...##"]) ;; best: 8

(def example2 ["......#.#."
               "#..#.#...."
               "..#######."
               ".#.#.###.."
               ".#..#....."
               "..#....#.#"
               "#..#....#."
               ".##.#..###"
               "##...#..#."
               ".#....####"])

(def example3 ["#.#...#.#."
               ".###....#."
               ".#....#..."
               "##.#.#.#.#"
               "....#.#.#."
               ".##..###.#"
               "..#...##.."
               "..##....##"
               "......#..."
               ".####.###."])

(def example4 [".#..#..###"
               "####.###.#"
               "....###.#."
               "..###.##.#"
               "##.##.#.#."
               "....###..#"
               "..#.#..#.#"
               "#..#.#.###"
               ".##...##.#"
               ".....#.#.."])

(def example5 [".#..##.###...#######"
               "##.############..##."
               ".#.######.########.#"
               ".###.#######.####.#."
               "#####.##.#.##.###.##"
               "..#####..#.#########"
               "####################"
               "#.####....###.#.#.##"
               "##.#################"
               "#####.##.###..####.."
               "..######..##.#######"
               "####.##.####...##..#"
               ".#####..#.######.###"
               "##...#.##########..."
               "#.##########.#######"
               ".####.#.###.###.#.##"
               "....##.##.###..#####"
               ".#.#.###########.###"
               "#.#.#.#####.####.###"
               "###.##.####.##.#..##"])

(def example-part2 [".#....#####...#.."
                    "##...##.#####..##"
                    "##...#...#.#####."
                    "..#.....#...###.."
                    "..#.#.....#....##"])
