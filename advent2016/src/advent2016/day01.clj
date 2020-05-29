(ns advent2016.day01
  (:require [clojure.string :refer [lower-case]]))

(defn read-input []
  (slurp "resources/day01.txt"))

(defn to-turn-steps [s]
  (let [dir   (-> (subs s 0 1) lower-case keyword)
        steps (-> (subs s 1) read-string)]
    [dir steps]))

(def deltas   {:up [0 1] :left [-1 0] :right [1 0] :down [0 -1]}) ;; cartesian plane
(def turn-left  {:up :left, :left :down, :down :right, :right :up})
(def turn-right {:up :right, :right :down, :down :left, :left :up})

(defn distance [x y]
  (let [abs (fn [n] (if (pos? n) n (* -1 n)))]
    (+ (abs x) (abs y))))

(defn walk [[dir x y] [turn steps]]
  (let [newdir  (if (= :l turn) (turn-left dir) (turn-right dir))
        [dx dy] (deltas newdir)
        newx (+ x (* steps dx))
        newy (+ y (* steps dy))]
    [newdir newx newy]))

(defn travel [input]
  (->> input
       (re-seq #"\w\d+")
       (map to-turn-steps)
       (reduce walk [:up 0 0])
       rest
       (apply distance)))

;; (= 5 (travel "R2, L3"))
;; (= 2 (travel "R2, R2, R2"))
;; (= 12 (travel "R5, L5, R5, R3"))

(defn part1 []
  (travel (read-input)))

;; (part1)

(defn coords-path [turn-steps]
  (loop [coords [], turn-steps turn-steps, dir :up, x 0, y 0]
    (if (seq turn-steps)
      (let [[turn steps] (first turn-steps)
            newdir  (if (= :l turn) (turn-left dir) (turn-right dir))
            [dx dy] (deltas newdir)
            newx (+ x (* steps dx))
            newy (+ y (* steps dy))
            new-coords (for [i (range 1 (inc steps))]
                         [(+ x (* i dx)) (+ y (* i dy))])]
        (recur (concat coords new-coords) (rest turn-steps) newdir newx newy))
      coords)))

(defn part2 [input]
  (->> input
       (re-seq #"\w\d+")
       (map to-turn-steps)
       coords-path
       (reduce (fn [cset c] (if (cset c) (reduced c) (conj cset c))) #{})
       (apply distance)))

;; (= 4 (part2 "R8, R4, R4, R8"))

;; (part2 (read-input))

