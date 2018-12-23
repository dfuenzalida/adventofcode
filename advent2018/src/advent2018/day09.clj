(ns advent2018.day09)

(defn insert-at [board index marble]
  (let [index (inc (rem (dec index) (count board)))]
    [(into [] (concat (take index board)
                      [marble]
                      (drop index board)))
     index]))

;; (insert-at [0] 0 1)
;; (insert-at [0 1] 1 2)
;; (insert-at [0 2 1 3] 5 4)

;; board: [0 4 2 1 3]
;; player - current player
;; index - index of the current marble
;; marble - current marble
;; score-map - scores

(defn iter-game [[board player index marble score-map]]
  (let [[newboard newindex] (insert-at board (+ 2 index) marble)] ;; TODO case 23
    [newboard
     (rem (inc player) (count score-map))
     newindex
     (inc marble)
     score-map]))

;; (map (comp println first) (take 17 (iterate iter-game [[0] 1 0 1 (repeat 9 0)])))
