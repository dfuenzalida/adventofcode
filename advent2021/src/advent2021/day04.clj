(ns advent2021.day04
  (:require [clojure.string :refer [blank? split-lines]]))

(declare example)

(defn parse-input [s] ;; s => [randos [board1 board2 ,,, ]]
  (let [input (->> s split-lines (partition-by blank?) (remove (comp empty? first))
                   (mapv #(mapv (comp read-string (partial format "[%s]")) %)))]
    [(ffirst input) (rest input)]))

(defn cols [xs]
  (for [i (range 5)]
    (for [j (range 5)]
      (get-in xs [j i]))))

(defn rows-and-cols [board]
  (concat board (cols board)))

(defn winning-board? [[randos board]]
  (let [rowcols (rows-and-cols board)]
    (when (some #(every? (set randos) %) rowcols)
      board)))

(defn score [[randos board]]
  (let [unmarked (->> (reduce concat board) (remove (set randos)) (reduce +))]
    (* unmarked (last randos))))

(defn part-1 [s]
  (let [[randos boards] (parse-input s)
        randseqs (->> (map #(take % randos) (range 5 (inc (count randos))))
                      (mapcat (partial repeat (count boards))))
        pairs (map vector randseqs (cycle boards))]
    (->> (filter winning-board? pairs)
         first
         score)))

;; (= 4512 (part-1 example))
;; (part-1 (slurp "resources/day04.txt"))

(defn part-2 [s]
  (let [[randos boards] (parse-input s)
        randseqs (map #(take % randos) (range 5 (inc (count randos))))]
    (loop [boards boards, randseqs randseqs]
      (if (= 1 (count boards))
        (let [pairs (map vector randseqs (cycle boards))]
          (->> (filter winning-board? pairs) first score))
        (let [randos  (first randseqs)
              boards' (remove #(winning-board? [randos %]) boards)]
          (recur boards' (rest randseqs)))))))

;; (= 1924 (part-2 example))
;; (part-2 (slurp "resources/day04.txt"))

(comment

  (let [board [[14 21 17 24  4] [10 16 15  9 19] [18  8 23 26 20] [22 11 13  6  5] [2  0 12  3  7]]
        rands [7,4,9,5,11,17,23,2,0,14,21,24]]
    (winning-board? [rands board]))

  ;; rows and cols
  (let [[_ boards] (parse-input example)
        board (first boards)]
    (concat board (cols board)))

  ;; growing sets of bingo numbers
  (let [[randos _] (parse-input example)]
    (map (comp set #(take % randos)) (range 5 (inc (count randos)))))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")
