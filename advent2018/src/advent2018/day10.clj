(ns advent2018.day10
  (:require [clojure.string :as s]))

(def example-input
  "position=< 9,  1> velocity=< 0,  2>
position=< 7,  0> velocity=<-1,  0>
position=< 3, -2> velocity=<-1,  1>
position=< 6, 10> velocity=<-2, -1>
position=< 2, -4> velocity=< 2,  2>
position=<-6, 10> velocity=< 2, -2>
position=< 1,  8> velocity=< 1, -1>
position=< 1,  7> velocity=< 1,  0>
position=<-3, 11> velocity=< 1, -2>
position=< 7,  6> velocity=<-1, -1>
position=<-2,  3> velocity=< 1,  0>
position=<-4,  3> velocity=< 2,  0>
position=<10, -3> velocity=<-1,  1>
position=< 5, 11> velocity=< 1, -2>
position=< 4,  7> velocity=< 0, -1>
position=< 8, -2> velocity=< 0,  1>
position=<15,  0> velocity=<-2,  0>
position=< 1,  6> velocity=< 1,  0>
position=< 8,  9> velocity=< 0, -1>
position=< 3,  3> velocity=<-1,  1>
position=< 0,  5> velocity=< 0, -1>
position=<-2,  2> velocity=< 2,  0>
position=< 5, -2> velocity=< 1,  2>
position=< 1,  4> velocity=< 2,  1>
position=<-2,  7> velocity=< 2, -2>
position=< 3,  6> velocity=<-1, -1>
position=< 5,  0> velocity=< 1,  0>
position=<-6,  0> velocity=< 2,  0>
position=< 5,  9> velocity=< 1, -2>
position=<14,  7> velocity=<-2,  0>
position=<-3,  6> velocity=< 2, -1>")

(defn parse-input [lines]
  (->> lines
       (map #(re-seq #".*=<(.*),(.*)> .*=<(.*),(.*)>" %))
       (map first)
       (map rest)
       (map #(map read-string %))))

(defn move [points]
  (for [[x y dx dy] points]
    [(+ x dx) (+ y dy) dx dy]))

(defn graph [points]
  (let [points-set                (set (map (juxt first second) points))
        [min-x max-x min-y max-y] (for [projfn [first second] redfn [min max]]
                                    (apply redfn (map projfn points-set)))
        width                     (- max-x min-x)
        lines                     (->>
                                   (for [y (range min-y (inc max-y))
                                         x (range min-x (inc max-x))
                                         :let [s (if (points-set [x y]) "#" ".")]]
                                     s)
                                   (partition (inc width))
                                   (map #(apply str %)))]
    (println)
    (dorun
     (map println lines))))

(def example-points
  (->> example-input s/split-lines parse-input))

(defn read-input []
  (->> (slurp "resources/day10.txt") s/split-lines parse-input))

(defn find-extremes [points]
    (let [points-set (set (map (juxt first second) points))]
      (for [projfn [first second] redfn [min max]]
        (apply redfn (map projfn points-set)))))

;; Found that the points group in a smaller are each time until they start expanding again
(defn find-msg [points time]
  (let [[min-x max-x _ _] (find-extremes points)
        curr-width (- max-x min-x)
        [min-x max-x _ _] (find-extremes (move points))
        next-width (- max-x min-x)]
    (if (> next-width curr-width)
      (do
        (println time)
        (graph points))
      (recur (move points) (inc time)))))

;; (find-msg example-points 0) => prints "HI" at second 3
;; (find-msg (read-input) 0) => prints the time and msg, about ~10K seconds in

