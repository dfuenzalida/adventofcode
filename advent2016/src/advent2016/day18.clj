(ns advent2016.day18
  (:require [clojure.string :refer [split-lines]]))

;; sequences of chars that lead to a trap
(def trap-map {[\^ \^ \.] \^
               [\. \^ \^] \^
               [\^ \. \.] \^
               [\. \. \^] \^})

(defn evolve [^String s]
  (let [row (str \. s \.) ;; adding "safe" boundaries
        triplets (partition 3 1 row)]
    (->> (map #(get trap-map % \.) triplets)
         (reduce str))))

;; (->> (iterate evolve ".^^.^.^^^^") (take 10) (mapv println))

;; (->> (iterate evolve ".^^.^.^^^^") (take 10) (mapcat seq) (filter #{\.}) count (= 38))

(defn read-input []
  (->> (slurp "resources/day18.txt") split-lines first))

(defn part1 []
  (->> (iterate evolve (read-input))
       (take 40)
       (mapcat seq)
       (filter #{\.})
       count))

;; (part1)

(defn part2 []
  (->> (iterate evolve (read-input))
       (take 400000)
       (mapcat seq)
       (filter #{\.})
       count))

;; (time (part2)) ;; => "Elapsed time: 110095.081073 msecs"

