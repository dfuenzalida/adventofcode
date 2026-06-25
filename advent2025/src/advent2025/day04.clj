(ns advent2025.day04
  (:require [clojure.string :as s]
            [clojure.edn :as edn]))

(declare example)

(defn to-coords-set
  "Given a diagram, return a set of [x y] coordinates of the paper rolls"
  [lines]
  (let [grid (mapv (partial mapv str) lines)]
    (into (sorted-set)
          (for [i (range (count grid))
                j (range (count (first grid)))
                :when (= "@" (get-in grid [j i]))]
            [i j]))))

(defn fork-accesible?
  "Returns true if there are fewer than 4 paper rolls in the eight adjacent positions to [i j]"
  [coord-set [i j]]
  (> 4
     (count (for [dx (range -1 2) dy (range -1 2)
                  :when (not (and (zero? dx) (zero? dy)))
                  :let [x (+ i dx) y (+ j dy)]
                  :when (coord-set [x y])] ;; the coord-set contains the adjacent position
              true))))

(defn part-1 [input]
  (let [cs (to-coords-set (s/split-lines input))]
    (->> (filter (partial fork-accesible? cs) cs)
         count)))

;; (= 13 (part-1 example))
;; (->> "resources/day04.txt" slurp part-1)

(defn fixed-point
  "Returns the value of applying f repeatedly to x until the result is constant"
  [f x]
  (reduce
   (fn [a b] (if (= a b) (reduced a) b))
   (iterate f x)))

;; (fixed-point #(Math/sqrt %) 30)
;; (fixed-point #(bit-shift-right % 1) 255)

(defn remove-accesible
  "Given a coords-set cs, returns cs without it's fork-accesible points"
  [cs]
  (into (sorted-set)
        (remove (partial fork-accesible? cs) cs)))

(defn part-2 [input]
    (let [cs (->> input s/split-lines to-coords-set)
        fp (fixed-point remove-accesible cs)]
    (- (count cs) (count fp))))

;; (= 43 (part-2 example))
;; (->> "resources/day04.txt" slurp part-2)

(comment
  
  (->> example s/split-lines count)
  (->> example s/split-lines first count)
  (->> example s/split-lines (mapv (partial mapv str)))

  (let [cs (to-coords-set (s/split-lines example))]
    (->> (filter (partial fork-accesible? cs) cs)
         count))
  )

(def example
  "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")
