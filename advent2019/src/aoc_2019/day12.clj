(ns aoc-2019.day12
  (:require [clojure.string :refer [split-lines]]))

(defn parse-input [lines] ;; parse to a 6-entry vector (3 pos, 3 speeds)
  (let [vecs (mapv #(->> (re-seq #"-?\d+" %) (mapv read-string)) lines)]
    (mapv #(into % [0 0 0]) vecs)))

(defn read-input []
  (->> (slurp "resources/input12.txt") split-lines))

(defn sign [a b]
  (cond (< a b) 1 (> a b) -1 :else 0))

(defn accelerate [xs [x y z dx dy dz]]
  (let [[dx' dy' dz'] (->> (map (fn [[a b c]] [(sign x a) (sign y b) (sign z c)]) xs)
                           (cons [dx dy dz])
                           (reduce (fn [[a b c] [d e f]] [(+ a d) (+ b e) (+ c f)])))]
    [x y z dx' dy' dz']))

(defn move [[x y z dx dy dz]]
  [(+ x dx) (+ y dy) (+ z dz) dx dy dz])

(defn simulate [moons]
  (->> (mapv (partial accelerate moons) moons)
       (mapv move)))

(defn energy [[a b c d e f]]
  (let [pot (->> (map #(Math/abs %) [a b c]) (reduce +))
        kin (->> (map #(Math/abs %) [d e f]) (reduce +))]
    (* pot kin)))

(defn solve-1 []
  (let [moons  (parse-input (read-input))
        moons' (-> (iterate simulate moons) (nth 1000))]
    (->> moons' (map energy) (reduce +))))

;; (solve-1)

