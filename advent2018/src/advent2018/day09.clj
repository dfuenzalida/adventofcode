(ns advent2018.day09)

(defn length [xs]
  (.size xs)
  ;; #?(:clj (.size xs)
  ;;    :cljs (.length xs))
  )

(defn insert-at [board ^long index ^long marble] ;; returns [board index score]
  (let [size (length board)
        index (inc (rem (dec index) size))]
    (if (zero? (rem marble 23))
      (let [to-remove (rem (+ index size -9) size)
            score     (+ marble (board to-remove))]
        [(into (subvec board 0 to-remove) (subvec board (inc to-remove)))
         to-remove score])
      [(-> (subvec board 0 index)
           (conj marble)
           (into (subvec board index)))
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
  ;; (println player board)
  (let [[newboard newindex score] (insert-at board (+ 2 index) marble)]
    ;; (when (pos? score)
    ;;   (println "adding" score "points to player" player))
    [newboard
     (inc (rem player (length score-map)))
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

;; After optimization (subvec)
;; (time (game-max-score 459 72103)) => "Elapsed time: 455356.42075 msecs"

;; (game-max-score 459 721)
