(ns advent2022.day08
  (:require [clojure.string :as string]))

(defn read-input [s]
  (->> (string/split-lines s)
       (mapv (partial mapv (comp read-string str)))))

(defn visible? [grid x y]
  (let [size  (count grid)
        height (nth (nth grid y) x)
        north (for [j (range 0 y)] (nth (nth grid j) x))
        south (for [j (range (inc y) size)] (nth (nth grid j) x))
        east  (for [i (range 0 x)] (nth (nth grid y) i))
        west  (for [i (range (inc x) size)] (nth (nth grid y) i))]
    ;; is the `height` greater than all the other numbers in any of the directions?
    (some true? (map #(every? (partial > height) %) [north south east west]))))

(defn part-1 [s]
  (let [grid (read-input s)
        size (count grid)]
    (->> (for [i (range 1 (- size 1))
               j (range 1 (- size 1))
               :when (visible? grid i j)]
           true)
         count
         (+ (* (dec size) 4))))) ;; (4 * (N-1)) trees on the edge always visible

;; (visible? (read-input example) 1 1)
;; (= 21 (part-1 example))
;; (part-1 (slurp "resources/day08.txt"))

(defn viewing-distance
  ([x ys] (viewing-distance x 0 ys))
  ([x total ys]
   (if-let [y (first ys)]
     (if (> x y)
       (recur x (inc total) (rest ys))
       (inc total))
     total)))

(defn scenic-score [grid x y]
  (let [size  (count grid)
        height (nth (nth grid y) x)
        north (for [j (range (dec y) -1 -1)] (nth (nth grid j) x))
        south (for [j (range (inc y) size)] (nth (nth grid j) x))
        east  (for [i (range (dec x) -1 -1)] (nth (nth grid y) i))
        west  (for [i (range (inc x) size)] (nth (nth grid y) i))]
    ;; scenic score is the product of the viewing distance in each direction
    (->> (map (partial viewing-distance height) [north south east west])
         (reduce *))))

;; (= 4 (-> (read-input example) (scenic-score 2 1)))
;; (= 8 (-> (read-input example) (scenic-score 2 3)))

(defn part-2 [s]
  (let [grid (read-input s)
        size  (count grid)]
    (->> (for [i (range 1 (- size 1))
               j (range 1 (- size 1))]
           (scenic-score grid i j))
         (reduce max))))

;; (= 1 (viewing-distance 5 [3]))
;; (= 1 (viewing-distance 5 [5 2 2]))
;; (= 2 (viewing-distance 5 [1 2]))
;; (= 2 (viewing-distance 5 [3 5 3]))

;; (= 8 (part-2 example))
;; (part-2 (slurp "resources/day08.txt"))

(comment
  ;; A grid of N by N has N^2 - (N-2)^2 = 4N - 4 elements in the edge

  (->> example
       (string/split-lines)
       (mapv (partial mapv (comp read-string str)))
       )

  ;; 9 is greater than all the others
  (every? (partial > 9) [5 4 3])
  )

(def example
  "30373
25512
65332
33549
35390")
