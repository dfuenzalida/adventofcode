(ns advent2025.day01
  (:require [clojure.string :as s]))

(declare example)

(defn read-input [s]
  (->> (s/split-lines s)
       (map (fn [s] [(first s) (read-string (subs s 1))]))))

(defn acc-zeros [[zeros crossed pos] x]
  (let [[dir n] x
        crossed (cond
                  ;;         full turns to the left + maybe one turn
                  (= dir \L) (+ crossed (quot n 100)
                                (if (and (pos? pos) (> (mod n 100) pos)) 1 0))
                  ;;         full turns to the right + maybe one turn
                  :otherwise (+ crossed (quot n 100)
                                (if (and (pos? pos) (> (+ (mod n 100) pos) 100)) 1 0)))
        pos     (cond
                  (= dir \L) (mod (+ pos 100 (* -1 n)) 100)
                  :otherwise (mod (+ pos n) 100))
        zeros   (if (zero? pos) (inc zeros) zeros)]
    [zeros crossed pos]))

(defn part-1 [xs]
  (->> (reduce acc-zeros [0 0 50] xs)
       first))

(defn part-2 [xs]
  (->> (reduce acc-zeros [0 0 50] xs)
       (take 2)
       (reduce +)))

;; (= [3 3 32] (part-1 (read-input example)))
;; (->> "resources/day01.txt" slurp read-input part-1)
;; (->> "resources/day01.txt" slurp read-input part-1 first (= 1191))
;; (reduce acc-zeros [0 0 50] [[\L 68] [\L 30] [\R 48]])
;; part 2: 7443 too high/ not 6252
;; (+ 1191 6724)
;; (+ 1191 6252)

;; (= 6 (part-2 (read-input example)))
;; (->> "resources/day01.txt" slurp read-input part-2)

;; (reductions acc-zeros [0 0 50] (read-input example))

(comment
  (->> (s/split-lines example)
       (map (fn [[dir d1 d2]] [dir (read-string (str d1 d2))]))
       (reduce acc-zeros [0 50])
       )

  (acc-zeros [0 0 50] [\L 68])
  (acc-zeros [0 0 52] [\R 48])
  
  (first "L1234")
  (subs "R1234" 1)

  (quot -7 3)
  )

(def example "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")
