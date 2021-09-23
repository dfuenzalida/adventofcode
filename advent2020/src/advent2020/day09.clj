(ns advent2020.day09
  (:require [clojure.string :refer [split-lines]]))

(declare example)

(defn valid? [[xs x]]
  (->> (for [i xs
             j xs
             :when (and (not= i j)
                        (= x (+ i j)))]
         true)
       first))

(defn find-invalid [n xs] ;; find invalid number in a window of size `n`
  (->> xs
       (partition (inc n) 1)
       (map (juxt butlast last))
       (drop-while valid?)
       first
       second))

;; (find-invalid 5 example)

(defn part-1 []
  (->> (slurp "resources/day09.txt") split-lines (map read-string)
       (find-invalid 25)))

;; (part-1)

(defn find-weak-pair [size xs] ;; naive, but it worked
  (let [invalid (find-invalid size xs)
        subnums (->> (for [i (range 0 (count xs))
                           j (range (inc i) (count xs))
                           :let [subnums (subvec xs i j)]
                           :when (= invalid (reduce + subnums))]
                       subnums)
                     first)
        min-sn  (reduce min subnums)
        max-sn  (reduce max subnums)]
    [min-sn max-sn]))

;; (= [15 47] (find-weak-pair 5 example))

(defn part-2 []
  (->> (slurp "resources/day09.txt") split-lines (mapv read-string)
       (find-weak-pair 25)
       (reduce +)))

;; (part-2)

(comment
  (->> example
       (partition 6 1)
       (map (juxt butlast last))
       (drop-while valid?)
       first
       second)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example
  [35
   20
   15
   25
   47
   40
   62
   55
   65
   95
   102
   117
   150
   182
   127
   219
   299
   277
   309
   576])
