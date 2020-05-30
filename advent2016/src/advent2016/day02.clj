(ns advent2016.day02
  (:require [clojure.string :refer [split-lines lower-case]]))

(def keypad [[1 2 3] [4 5 6] [7 8 9]])
(def moves {:u [0 -1] :d [0 1] :l [-1 0] :r [1 0]})
(defn bound [x a b] (if (< x a) a (if (> x b) b x)))

(def example "ULL\nRRDDD\nLURDL\nUUUUD")

(defn input-to-lines [input]
  (->> (split-lines input)
       (mapv #(map (comp keyword lower-case str) %))))

(defn move [[x y] dir]
  (let [[dx dy] (moves dir)
        newx    (bound (+ x dx) 0 2)
        newy    (bound (+ y dy) 0 2)]
    [newx newy]))

(defn bath-code [lines kpad movef x0 y0]
  (loop [lines lines, result "", x x0, y y0]
    (if (seq lines)
      (let [line   (first lines)
            [x y]  (reduce movef [x y] line)
            digit  ((kpad y) x)
            result (str result digit)]
        (recur (rest lines) result x y))
      result)))

(defn read-input []
  (slurp "resources/day02.txt"))

(defn part1 []
  (-> (read-input) input-to-lines (bath-code keypad move 1 1) read-string))

;; (= 1985 (-> example input-to-lines (bath-code keypad move 1 1) read-string))

;; (part1)

(def keypad2 (mapv vec ["       "
                        "   1   "
                        "  234  "
                        " 56789 "
                        "  ABC  "
                        "   D   "
                        "       "]))

(defn move2 [[x y] dir]
  (let [[dx dy] (moves dir)
        newx    (+ x dx)
        newy    (+ y dy)
        newchar ((keypad2 newx) newy)]
    (if (= \space newchar) ;; move only if the next position is not space
      [x y]
      [newx newy])))

;; (= "5DB3" (-> example input-to-lines (bath-code keypad2 move2 1 3)))

(defn part2 []
  (-> (read-input) input-to-lines (bath-code keypad2 move2 1 3)))

;; (part2)
