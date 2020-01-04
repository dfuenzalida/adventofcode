(ns aoc-2019.day11
  (:require [aoc-2019.intcode :refer [execute]]))

(defn read-program []
  (->> (slurp "resources/input11.txt")
       (format "[%s]")
       (clojure.edn/read-string)))

;; (->> (execute (read-program) [0 0]) last)

(def left  {:up :left, :left :down, :down :right, :right :up})
(def right {:up :right, :right :down, :down :left, :left :up})
(def speed {:up [0 -1] :down [0 1] :left [-1 0] :right [1 0]})

;; Part 1
;; Loop until execution does not produce output, consume the paint color,
;; then the turn direction (then move one ahead). Keep track of angle and
;; position and current colors by position.

(defn paint [prog colors]
  (loop [prog prog, ip 0, relbase 0, direction :up, position [0 0], colors colors]
    (let [color (get colors position 0)
          [ip' relbase' prog' stdin' stdout'] (execute prog ip relbase [color])]
      (if (= stdin' [color]) ;; input not consumed (program terminated)
        colors ;; map from panel positions to color painted
        (let [[paint-color turn] stdout'
              colors' (assoc colors position paint-color)
              direction' (if (zero? turn) (left direction) (right direction))
              position'  (mapv + position (speed direction'))]
          (recur prog' ip' relbase' direction' position' colors'))))))

(defn solve-1 []
  (count (paint (read-program) {})))

;; (solve-1)
