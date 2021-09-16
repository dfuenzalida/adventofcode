(ns advent2020.day01
  (:require [clojure.string :refer [split-lines]]))

(defn read-input []
  (->> "resources/day01.txt" slurp split-lines (mapv read-string)))

(defn part-1 [entries]
  (first
   (for [i entries
         j entries
         :when (and (not= i j) (= 2020 (+ i j)))]
     (* i j))))

;; (part-1 (read-input))

(defn part-2 [entries]
  (first
   (for [i entries
         j entries
         k entries
         :when (and (not= i j k) (= 2020 (+ i j k)))]
     (* i j k))))

;; (part-2 (read-input))
