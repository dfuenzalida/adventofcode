(ns advent2023.day04
  (:require [clojure.string :as s]
            [clojure.edn :as edn]))

(declare example)

(defn read-input [s]
  (->> s s/split-lines
       (mapcat #(re-seq #".*: (.*) \| (.*)" %))
       (map rest) ;; drop the whole matched string
       (map (fn [[winners nums]]
              (edn/read-string (format "[[%s] [%s]]" winners nums))))))

(defn worth [[winners nums]]
  (let [winset  (set winners)
        matches (count (filter winset nums))]
    (long (Math/pow 2 (dec matches)))))

(defn part-1 [s]
  (->> s read-input (map worth) (reduce +)))

;; (= 13 (part-1 example))
;; (->> "resources/day04.txt" slurp part-1)

(defn match-count [[winners nums]]
  (let [winset  (set winners)]
    (count (filter winset nums))))

(defn part-2 [s]
  (let [matchcounts (->> s read-input (mapv match-count))]
    (loop [i 0
           m (zipmap (range (count matchcounts)) (repeat 1))]
      (if (< i (count matchcounts))
        (let [matchcount (matchcounts i)
              copies     (m i)
              newcards   (zipmap (range (inc i) (+ i 1 matchcount)) (repeat copies))
              m' (merge-with + m newcards)]
          (recur (inc i) m'))
        (->> m vals (reduce +))))))

;; (= 30 (part-2 example))
;; (->> "resources/day04.txt" slurp part-2)

(comment
  (->> example read-input (map match-count))

  (zipmap (range 10) (repeat 1))

  )

(def example
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")
