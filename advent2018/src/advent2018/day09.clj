(ns advent2018.day09)

(defn insert-at [board ^long index ^long marble] ;; returns [board index score]
  (let [index (inc (rem (dec index) (count board)))]
    (if (zero? (rem marble 23))
      (let [to-remove (rem (+ index (count board) -9) (count board))
            score     (+ marble (nth board to-remove))]
        [(into [] (concat (take to-remove board)
                          (drop (inc to-remove) board)))
         to-remove score])
      [(into [] (concat (take index board)
                        [marble]
                        (drop index board)))
       index 0])))

;; (insert-at [0] 0 1)
;; (insert-at [0 1] 1 2)
;; (insert-at [0 2 1 3] 5 4)

;; board: [0 4 2 1 3]
;; player - current player
;; index - index of the current marble
;; marble - current marble
;; score-map - scores

(defn iter-game [[board ^long player ^long index ^long marble score-map]]
  (println player board)
  (let [[newboard newindex score] (insert-at board (+ 2 index) marble)]
    [newboard
     (inc (rem player (count score-map)))
     newindex
     (inc marble)
     (if (zero? score)
       score-map
       (update-in score-map [player] + score))]))

(defn zero-scores [n]
  (into {} (map vector (range 1 (inc n)) (repeat n 0))))

(defn game-max-score [num-players last-marble]
  (->> (iterate iter-game [[0] 1 1 1 (zero-scores num-players)])
       (drop last-marble) first last vals (apply max)))

;; (game-max-score 9 25) => 32
;; (game-max-score 10 1618) => 8317
;; (game-max-score 13 7999) => 146373
;; (game-max-score 17 1104) => 2764
;; (game-max-score 21 6111) => 54718
;; (game-max-score 30 5807) => 37305
;; (time (game-max-score 459 72103)) => "Elapsed time: 1166720.191011 msecs"

