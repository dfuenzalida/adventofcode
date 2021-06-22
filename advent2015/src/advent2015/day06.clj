(ns advent2015.day06
  (:require [clojure.string :refer [split-lines starts-with?]]))

(defn read-input []
  (slurp "resources/day06.txt"))

;; Apply a function `f` to the points in the rectangle a b c d 
(defn update-grid [m f a b c d]
  (let [coords (for [x (range a (inc c))
                     y (range b (inc d))]
                 [x y])]
    (reduce f m coords)))

(defn turn-on [m a b c d]
  (update-grid m #(assoc %1 %2 true) a b c d))

(defn turn-off [m a b c d]
  (update-grid m dissoc a b c d))

(defn toggle-at [m coord]
  (if (m coord) (dissoc m coord) (assoc m coord true)))

(defn toggle [s a b c d]
  (update-grid s toggle-at a b c d))

(defn process-line [m s]
  (let [[a b c d] (->> (re-seq #"\d+" s) (map read-string))]
    (cond
      (starts-with? s "turn on") (turn-on m a b c d)
      (starts-with? s "turn off") (turn-off m a b c d)
      :else (toggle m a b c d))))

(defn part-1 []
  (->> (read-input)
       split-lines
       (reduce process-line {})
       count))

;; (time (part-1))

(defn increase-by [n m coord]
  (let [val (get m coord 0)]
    (assoc m coord (+ n val))))

(defn decrease-by [m coord]
  (let [val    (get m coord 0)
        newval (max 0 (dec val))]
    (assoc m coord newval)))

(defn process-line2 [m s]
  (let [[a b c d] (->> (re-seq #"\d+" s) (map read-string))]
    (cond
      (starts-with? s "turn on") (update-grid m (partial increase-by 1) a b c d)
      (starts-with? s "turn off") (update-grid m decrease-by a b c d)
      :else (update-grid m (partial increase-by 2) a b c d))))

(defn part-2 []
  (->> (read-input)
       split-lines
       (reduce process-line2 {})
       vals
       (reduce +)))

;; (time (part-2))
