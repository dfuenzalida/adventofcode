(ns advent2021.day11
  (:require [clojure.string :refer [split-lines]]))

(declare example example2)

(def deltas [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]])

(defn neighbors [width height [y x]]
  (for [[dx dy] deltas
        :let    [x' (+ x dx) y' (+ y dy)]
        :when   (and (< -1 x' width) (< -1 y' height))]
    [y' x']))

(defn read-input [s]
  (->> s split-lines
       (mapv (partial mapv (comp read-string str)))))

;; Given a 2D array, a set of flashing points and a seq of points to increase
;; iterates the seq updating everything as required, returning the pair of
;; updated 2D array and set of points that did flash in the iteration

(defn iterate-map
  ([xss] (let [height (count xss)
               width  (count (first xss))]
           (iterate-map xss #{} (for [j (range height) i (range width)] [j i]))))
  ([xss flashset increasing]
   (if-let [p (first increasing)]
     (if (flashset p)
       (recur xss flashset (rest increasing))
       (let [v (get-in xss p)]
         (if (<= 9 v)
           (let [height (count xss)
                 width  (count (first xss))]
             (recur xss
                    (conj flashset p)
                    (concat (rest increasing) (neighbors width height p))))
           (recur (update-in xss p inc)
                  flashset
                  (rest increasing)))))
     ;; No more points: reset the flashing points before finishing the iteration
     (let [xss' (reduce (fn [m p] (assoc-in m p 0)) xss flashset)]
       [xss' flashset]))))

;; (-> example2 read-input iterate-map first iterate-map first clojure.pprint/pprint)
;; (-> example read-input iterate-map first clojure.pprint/pprint)

(defn part-1 [s]
  (-> (iterate
       (fn [[m ftot]] (let [[m' fset] (iterate-map m)] [m' (+ ftot (count fset))]))
       [(read-input s) 0])
      (nth 100)
      second))

;; (= 1656 (part-1 example))
;; (part-1 (slurp "resources/day11.txt"))

(defn part-2 [s]
  (let [m (read-input s)
        height (count m)
        width (count (first m))]
    (->> (iterate
          (fn [[m ftot]] (let [[m' fset] (iterate-map m)] [m' fset]))
          [(read-input s) #{}])
         (take-while #(not= (* width height) (count (second %))))
         count)))

;; (= 195 (part-2 example))
;; (part-2 (slurp "resources/day11.txt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")

(def example2 "11111
19991
19191
19991
11111")
