(ns advent2021.day13
  (:require [clojure.string :refer [blank? split split-lines]]))

(declare example)

(defn abs [x]
  (if (pos? x) x (* -1 x)))

(defn bounce [p x]
  (- p (abs (- p x))))

;; (map (partial bounce 5) (range 11))

(defn read-input [s]
  (let [[pts _ comms] (->> s split-lines (partition-by blank?))
        pts   (map #(read-string (format "[%s]" %)) pts)
        comms (map #(let [[dir n] (split % #"=")] [dir (read-string n)]) comms)]
    [pts comms]))

;; (read-input example)

(defn fold-paper [xs [cmd n]]
  (condp = cmd
    "fold along x" (->> (map (fn [[x y]] [(bounce n x) y]) xs) set)
    #_otherwise    (->> (map (fn [[x y]] [x (bounce n y)]) xs) set)))

(defn part-1 [s]
  (let [[pts comms] (read-input s)]
    (->> (fold-paper pts (first comms))
         count)))

;; (= 17 (part-1 example))
;; (part-1 (slurp "resources/day13.txt"))

(defn fold-paper2 [[width height xs] [cmd n]]
  (condp = cmd
    "fold along x" [n height (->> (map (fn [[x y]] [(bounce n x) y]) xs) set)]
    #_otherwise    [width n (->> (map (fn [[x y]] [x (bounce n y)]) xs) set)]))

(defn part-2 [s]
  (let [[pts comms] (read-input s)
        [w h pts]   (reduce fold-paper2 [0 0 pts] comms)]
    (->> (for [y (range h) x (range w)]
           (if (pts [x y]) "#" " "))
         (partition w)
         (interpose ["\n"])
         (reduce concat)
         (reduce str))))

;; (println (part-2 example)) ;; => prints a 5x5 square
;; (println (part-2 (slurp "resources/day13.txt")))

(comment
  (let [[pts comms] (read-input example)]
    (reduce fold-paper2 [0 0 pts] comms))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")
