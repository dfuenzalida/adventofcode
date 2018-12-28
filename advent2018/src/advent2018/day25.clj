(ns advent2018.day25
  (:require [clojure.edn :as edn]
            [clojure.string :as s]))

(defn distance [[^long a ^long b ^long c ^long d]
                [^long e ^long f ^long g ^long h]]
  (+ (Math/abs (- a e)) (Math/abs (- b f))
     (Math/abs (- c g)) (Math/abs (- d h))))

(defn close? [p1 p2]
  (<= (distance p1 p2) 3))

(defn same-const? [point const]
  (when (some (partial close? point) const)
    const))

;; Takes a Set of consts: #{[p1 p2 ...]} and a 4d-point, returns a Set of consts
(defn group-consts [consts point]
  (let [best-consts (filter (partial same-const? point) consts)]
    (if best-consts
      (let [mega-const (conj (reduce into best-consts) point)
            consts     (reduce disj consts best-consts)]
        (conj consts mega-const))
      (conj consts [point]))))

(def example-4 [[-1,2,2,0] [0,0,2,-2] [0,0,0,-2] [-1,2,0,0]
                [-2,-2,-2,2] [3,0,2,-1] [-1,3,2,2]
                [-1,0,-1,0] [0,2,1,-2] [3,0,0,0]])

(def example-1 [[0,0,0,0] [3,0,0,0] [0,3,0,0] [0,0,3,0] [0,0,0,3]
                [0,0,0,6] [9,0,0,0] [12,0,0,0]
                [6,0,0,0] ;; <== this extra point causes 2 constellations to merge
                ])

;; (count (reduce group-consts #{} example-4)) => 4
;; (count (reduce group-consts #{} example-1)) => 1

(defn read-input []
  (->> (slurp "resources/day25.txt")
       s/split-lines
       (map #(str "[" % "]"))
       (map edn/read-string)))

;; (time (count (reduce group-consts #{} (read-input))))
;; "Elapsed time: 662.154208 msecs"


