(ns advent2025.day07
  (:require [clojure.string :as s]
            [clojure.edn :as edn]
            [advent2025.shared :refer [fixed-point]]))

(declare example)

(defn read-input [s]
  (let [grid  (->> (s/split-lines s) (mapv (partial mapv str)))
        start (->> grid first (take-while (partial not= "S")) count)
        width (count (first grid)) height (count grid)]
    (->> (for [x (range width) y (range height)
               :when (= "^" (get-in grid [y x]))]
           [x y])
         (into (sorted-set))
         (vector width height start))))

;; (read-input example)

(defn iterate-beams [height splitters [split-count beams]]
  (reduce (fn [[count' beams'] [bx by]]
            (cond
              ;; beam went off the bottom
              (< height by)
              [count' (into beams' [[bx by]])]
              
              ;; beam meets a splitter: splits into two, also moving down
              (some #{[bx by]} splitters)
              (let [count' (inc count') ;; one new splitter was touched
                    beams' (-> (set beams')
                               (disj [bx by])
                               (into [[(dec bx) (+ 1 by)] [(inc bx) (+ 1 by)]])
                               )]
                [count' beams'])

              ;; beam continues going down
              :otherwise
              [count' (into beams' [[bx (inc by)]])]
              ))
          [split-count #{}]
          beams))

(defn part-1 [s]
  (let [[width height start splitters] (read-input s)]
    (first
     (fixed-point (partial iterate-beams height splitters) [0 #{[start 0]}]))))

;; (= 21 (-> example part-1))
;; (-> "resources/day07.txt" slurp part-1)

(defn iterate-beams2 [height splitters [split-count beams]]
  (reduce (fn [[count' beams'] [[bx by] kvc]]
            (cond
              ;; beam went off the bottom, won't move further
              (< height by)
              [count' (update beams' [bx by] (fnil (partial + kvc) 0))]

              ;; beam meets a splitter: splits into two, also moving down
              (some #{[bx by]} splitters)
              (let [count' (inc count')
                    beams' (-> beams'
                               (update [(dec bx) (+ 0 by)] (fnil (partial + kvc) 0))
                               (update [(inc bx) (+ 0 by)] (fnil (partial + kvc) 0)))]
                [count' beams'])

              ;; beam continues going down
              :otherwise
              [count' (update beams' [bx (inc by)] (fnil (partial + kvc) 0))]
              ))
          [split-count {}]
          beams))

(defn part-2 [s]
  (let [[width height start splitters] (read-input s)]
    (->> (fixed-point (partial iterate-beams2 height splitters) [0 {[start 0] 1}])
         second
         vals
         (reduce +))))

;; (= 40 (->> example part-2))
;; (time (->> "resources/day07.txt" slurp part-2))
;; => "Elapsed time: 6679.7901 msecs"

(comment

  (let [s example]
    (let [[width height start splitters] (read-input s)]
      (->> (iterate (partial iterate-beams2 height splitters) [0 {[start 0] 1}])
           (drop 16)
           first
           second
           #_vals
           #_(reduce +)
           )))
  
  (let [[width height start splitters] (read-input example)]
    (fixed-point (partial iterate-beams height splitters) [0 #{[start 0]}])
    #_(->> (iterate (partial iterate-beams height splitters) [0 #{[start 0]}])
         (take 15)
         last)
    )
  
  )

(def example
  ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............")
