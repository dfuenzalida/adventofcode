(ns advent2020.day03
  (:require [clojure.string :refer [split-lines]]))

(def example
  ["..##......."
   "#...#...#.."
   ".#....#..#."
   "..#.#...#.#"
   ".#...##..#."
   "..#.##....."
   ".#.#.#....#"
   ".#........#"
   "#.##...#..."
   "#...##....#"
   ".#..#...#.#"])

(defn count-trees [grid dx dy]
  (let [xxs (mapv (partial into []) grid)
        height (count xxs)
        width  (count (first xxs))
        cs (for [i (rest (range 0 height dy))
                 :let [j (rem (* (/ i dy) dx) width)
                       c (get-in xxs [i j])]]
             c)]
    (->> (filter #{\#} cs) count)))

;; (count-trees example 3 1)

(defn read-input []
  (->> "resources/day03.txt" slurp split-lines))

(defn part-1 []
  (count-trees (read-input) 3 1))

;; (part-1)

;; (= 2 (count-trees example 1 1))
;; (= 7 (count-trees example 3 1))
;; (= 3 (count-trees example 5 1))
;; (= 4 (count-trees example 7 1))
;; (= 2 (count-trees example 1 2))

(comment
  (= 336 (->> (map #(apply count-trees example %) [[1 1] [3 1] [5 1] [7 1] [1 2]])
              (reduce *)))
  )
     
(defn part-2 []
  (let [grid (read-input)]
    (->> (map #(apply count-trees grid %) [[1 1] [3 1] [5 1] [7 1] [1 2]])
         (reduce *))))

;; (part-2)

