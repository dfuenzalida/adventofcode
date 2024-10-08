(ns advent2022.day03
  (:require [clojure.string :as string]))

(defn read-input [s]
  (->> (string/split s #"\n")
       (map #(split-at (/ (count %) 2) %))))

(defn in-both [[a b]]
  (some (set a) b))

(defn priority [c]
  (cond
    (<= (int \a) (int c) (int \z)) (- (int c) (int \a) -1)
    :else (- (int c) (int \A) -1 -26)))

(defn part-1 [s]
  (->> s read-input (map in-both) (map priority) (reduce +)))

;; (part-1 (slurp "resources/day03.txt"))

(defn read-input2 [s]
  (->> (string/split s #"\n")
       (partition 3)))

(defn in-all-3 [[a b c]]
  (let [a-and-b (into #{} (filter (set a) b))]
    (some a-and-b c)))

(defn part-2 [s]
  (->> s read-input2 (map in-all-3) (map priority) (reduce +)))

;; (part-2 (slurp "resources/day03.txt"))

(comment
  
  (= (part-1 example) 157)
  (= (priority \a) 1)
  (= (priority \z) 26)
  (= (priority \A) 27)
  (= (priority \Z) 52)

  (= (part-2 example) 70)
  )

(def example
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")
