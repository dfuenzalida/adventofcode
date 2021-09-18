(ns advent2020.day05
  (:require [clojure.string :refer [split-lines]]))

(defn split-range [[low high] c]
  (cond
    (#{\F \L} c) (let [high' (-> (+ high low) inc (/ 2) dec)]
                   [low high'])
    :else        (let [low' (-> (+ high low) inc (/ 2))]
                   [low' high])))

(defn row [s]
  (->> (reduce split-range [0 127] (take 7 s)) first))

(defn column [s]
  (->> (reduce split-range [0 7] (drop 7 s)) first))

(defn seat-id [s]
  (let [r (row s)
        c (column s)]
    (+ c (* 8 r))))

(defn read-input []
  (split-lines (slurp "resources/day05.txt")))

(defn part-1 []
  (->> (read-input)
       (map seat-id)
       (reduce max)))

;; (part-1)

(defn part-2 []
  (->> (read-input)
       (map seat-id)
       sort
       (partition 2 1)
       (filter (fn [[a b]] (= b (+ a 2))))
       first
       first
       inc))

;; (part-2)
;
;; (= 44 (row "FBFBBFFRLR"))
;; (= 5 (column "FBFBBFFRLR"))
;; (= 357 (seat-id "FBFBBFFRLR"))
;; (= 567 (seat-id "BFFFBBFRRR"))
;; (= 119 (seat-id "FFFBBBFRRR"))
;; (= 820 (seat-id "BBFFBBFRLL"))

;; (split-range [0 127] \F)
;; (split-range [0 63] \B)
;; (split-range [32 63] \F)
;; (split-range [32 47] \B)
