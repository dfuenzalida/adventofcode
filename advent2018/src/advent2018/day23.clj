(ns advent2018.day23
  (:require [clojure.edn :as edn]
            [clojure.string :as s]))

(def example "pos=<0,0,0>, r=4
pos=<1,0,0>, r=1
pos=<4,0,0>, r=3
pos=<0,2,0>, r=1
pos=<0,5,0>, r=3
pos=<0,0,3>, r=1
pos=<1,1,1>, r=1
pos=<1,1,2>, r=1
pos=<-1,3,1>, r=1")

(def example-input
  (s/split-lines example))

(defn parse [lines]
  (let [numbers (mapv #(re-seq #"-?\d+" %) lines)]
    (mapv #(mapv edn/read-string %) numbers)))

(defn distance [[^long a ^long b ^long c _]
                [^long e ^long f ^long g _]]
  (+ (Math/abs (- a e)) (Math/abs (- b f)) (Math/abs (- c g))))

(defn close? [[_ _ _ radius :as p1] p2]
  (<= (distance p1 p2) radius))

(defn part1 [input]
  (let [points    (parse input)
        strongest (last (sort-by last points))
        in-range  (filter (partial close? strongest) points)]
    (count in-range)))

;; (part1 example-input)

;; (time (part1 (->> (slurp "resources/day23.txt") s/split-lines)))
;; "Elapsed time: 23.048196 msecs"

(def example2 "pos=<10,12,12>, r=2
pos=<12,14,12>, r=2
pos=<16,12,12>, r=4
pos=<14,14,14>, r=6
pos=<50,50,50>, r=200
pos=<10,10,10>, r=5")

(def example-input2
  (s/split-lines example2))

;; (let [points (parse example-input2)
;;       points (map (fn [[x y z r]] [(/ x r) (/ y r) (/ z r)]) points)]
;;   (reduce (fn [[a b c] [d e f]] [(+ a d) (+ b e) (+ c f)]) [0.0 0.0 0.0] points))
