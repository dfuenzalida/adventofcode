(ns advent2022.day02
  (:require [clojure.string :as string]))

(def char-to-hand
  {\A :rock \B :paper \C :scissors
   \X :rock \Y :paper \Z :scissors})

(defn read-input [s]
  (->> (string/split s #"\n")
       (map (juxt (comp char-to-hand first) (comp char-to-hand last)))))

(def score-chosen
  {:rock 1 :paper 2 :scissors 3})

(def what-beats
  {:rock :paper, :paper :scissors, :scissors :rock})

(def what-loses
  (into {} (map (juxt second first) what-beats)))

(defn score-outcome [a b] ;; b is player's choice
  (if (= a b)
    3
    (if (= b (what-beats a)) 6 0)))

(defn score [[a b]]
  (+ (get score-chosen b 0) (score-outcome a b)))

(defn part-1 [s]
  (->> s read-input (map score) (reduce +)))

;; (part-1 (slurp "resources/day02.txt"))

(defn needs-to-end [a b]
  (condp = b
    :rock (what-loses a)
    :paper a
    (what-beats a)))
  
(defn score2 [[a b]]
  (let [selected (needs-to-end a b)]
    (+ (get score-chosen selected 0) (score-outcome a selected))))

(defn part-2 [s]
  (->> s read-input (map score2) (reduce +)))

;; (part-2 example)
;; (part-2 (slurp "resources/day02.txt"))

(comment
  (read-input example)
  (= (score [:rock :paper]) 8)
  (= (score [:paper :rock]) 1)
  (= (score [:scissors :scissors]) 6)
  (= 15 (part-1 example))
  (= 12 (part-2 example))
  )

(def example "A Y
B X
C Z")
