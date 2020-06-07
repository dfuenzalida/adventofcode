(ns aoc-2019.day06
  (:require [clojure.string :refer [split split-lines]]))

(defn read-input []
  (slurp "resources/input06.txt"))

(defn build-map [content]
  (->> (split-lines content)
       (mapv #(split % #"\)"))
       (map (juxt second first))
       (into {})))

(defn count-orbits [m k] ;; iterates key > value > key until nil
  (dec (count (take-while some? (iterate (partial m) k)))))

(defn solve-1 [m]
  (->> (into #{} (concat (keys m) (vals m)))
       (map (partial count-orbits m))
       (reduce +)))

;; (solve-1 (build-map (read-input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn path-up [m k]
  (take-while some? (iterate (partial m) k)))

(defn common-parent [m k1 k2]
  (let [p1 (path-up m k1)
        s2 (set (path-up m k2))]
    (some s2 p1)))

(defn solve-2 [m you san]
  (let [common (common-parent m you san)
        comm-o (count-orbits m common)
        you-o  (count-orbits m you)
        san-o  (count-orbits m san)]
    (+ (- you-o comm-o 1) (- san-o comm-o 1))))

;; (solve-2 (build-map (read-input)) "YOU" "SAN")
