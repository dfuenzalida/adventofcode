(ns advent2016.day15
  (:require [clojure.string :refer [split-lines]]
            [clojure.edn :as edn]))

(defn parse-line [s]
  (let [extract (juxt last second)]
    (->> (re-seq #"\d+" s)
         extract
         (mapv edn/read-string))))

;; (parse-line "Disc #1 has 13 positions; at time=0, it is at position 11.")

(defn read-input []
  (->> (slurp "resources/day15.txt")
       split-lines
       (mapv parse-line)))

(def example-input
  [[4 5] [1 2]])

(defn current-pos [[initial total] t pos]
  (mod (+ initial pos t) total))

;; (zero? (current-pos [4 5] 5 1))

(defn valid? [xs t]
  (->> (map current-pos xs (repeat t) (rest (range)))
       (every? zero?)))

;; (valid? example-input 5)

(defn part1 [input]
  (->> (rest (range))
       (filter (partial valid? input))
       first))

;; (part1 example-input)
;; (part1 (read-input))

(defn part2 []
  (let [input (conj (read-input) [0 11])]
    (->> (rest (range))
         (filter (partial valid? input))
         first)))

;; (part2)
