(ns aoc-2019.day16
  (:require [clojure.string :refer [trim]]))

(defn pattern-for [n]
  (let [source  [0 1 0 -1]
        pattern (cycle (mapcat (partial repeat n) source))]
    (rest pattern)))

(defn iter-digit [xs n]
  (let [sum (->> (map * xs (pattern-for n)) (reduce +) Math/abs)]
    (rem sum 10)))

(defn iter-signal [xs]
  (map (partial iter-digit xs) (range 1 (inc (count xs)))))

(defn to-digits [x]
  (->> x str (map (comp read-string str))))

(defn read-input []
  (slurp "resources/input16.txt"))

(defn solve-1 [digits]
  (let [iterations (->> digits to-digits (iterate iter-signal))]
    (take 8 (nth iterations 100))))

;; Takes ~20 seconds on my machine
;; (time (solve-1 (clojure.string/trim (read-input))))

