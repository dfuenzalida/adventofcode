(ns advent2022.day15
  (:require [clojure.string :as string]))

(defn distance [[a b] [c d]]
  (+ (abs (- a c)) (abs (- b d))))

(defn read-input [s]
  (->> (re-seq #"-?\d+" s)
       (map read-string)
       (partition 2)
       (partition 2)))

(defn margins [xfn redfn y pairs]
  (->> (map (fn [[[sx sy] [bx by]]]
              (let [dist (distance [sx sy] [bx by])]
                (xfn sx (- dist (distance [0 y] [0 sy])))))
            pairs)
       (reduce redfn)))

(defn xranges [y pairs]
  (for [[[sx sy] [bx by]] pairs
        :let [dist (distance [sx sy] [bx by])
              difx (- dist (distance [0 y] [0 sy]))]
        :when (pos? difx)
        ]
    [(- sx difx) (+ sx difx)]))

;; (margins - min 10 (read-input example))
;; (margins + max 10 (read-input example))
;; (->> (xranges 10 (read-input example)) sort)

;; Given a point, is there any sensor in the distmap that is close enough to it?
(defn close-enough? [[px py] distmap]
  (->> (map (fn [[[sx sy] dist]] (>= dist (distance [sx sy] [px py]))) distmap)
       (some true?)))

(defn part-1-v1 [s y] ;; SLOW VERSION that checks every X coordinate
  (let [pairs (read-input s)
        distmap (->> (map (fn [[[a b] [c d]]] [[a b] (distance [a b] [c d])]) pairs) (into {}))
        beacons (->> (map second pairs) (into #{}))
        left    (margins - min y pairs)
        right   (margins + max y pairs)]
    (->> (for [i (range left (inc right))
               :when (and (not (beacons [i y]))
                          (close-enough? [i y] distmap))]
           true)
         count)))

(defn part-1 [s y]
  (let [ranges (->> s read-input (xranges y) (reduce concat) sort)]
    (- (last ranges) (first ranges))))

;; (= 26 (part-1 example 10))

;; (time (part-1-v1 (slurp "resources/day15.txt") 2000000))
;; "Elapsed time: 4330.879232 msecs"
;; (time (part-1 (slurp "resources/day15.txt") 2000000))
;; "Elapsed time: 0.920181 msecs"

(defn reduce-intervals [[a b] [c d]]
  (cond
    (<= a c d b) [a b]
    (<= c a b d) [c d]
    (<= a c b d) [a d]
    (<= c a d b) [c b]
    :else (reduced [[a b] [c d]])))

(defn find-signal [s maxrange]
  (let [input (read-input s)]
    (first
     (for [y (range maxrange)
           :let [intervals (->> (xranges y input) sort (reduce reduce-intervals))]
           ;; if there's a vector, there's a gap and that's the location we look for
           :when (vector? (first intervals))
           :let [[a b] (first intervals)]]
       [(inc b) y]))))

;; (= [14 11] (find-signal example 20))

(defn part-2 [s maxrange]
  (let [[x y] (find-signal s maxrange)]
    (+ (* x 4000000) y)))

;; (= 56000011 (part-2 example 20))
;; (time (find-signal (slurp "resources/day15.txt") 4000000))
;; "Elapsed time: 62979.752477 msecs"

(comment

  (let [ranges (->> (slurp "resources/day15.txt") read-input (xranges 2000000) sort)
        [from to] (reduce reduce-intervals ranges)]
    (- to from))

  (->> example read-input (xranges 11) sort (reduce reduce-intervals))
  
  (let [s example y 10]
    (let [pairs (read-input s)
          distmap (->> (map (fn [[[a b] [c d]]] [[a b] (distance [a b] [c d])]) pairs) (into {}))
          beacons (->> (map second pairs) (into #{}))
          left    (margins - min y pairs)
          right   (margins + max y pairs)]
      [left right]))
  
  ;; left range for y=10
  (let [y 10
        pairs (read-input example)]
    (->>
     (map (fn [[[sx sy] [bx by]]]
            (let [dist (distance [sx sy] [bx by])]
              ;; can go to the left as much unused distance in the Y axis
              (- sx (- dist (distance [0 y] [0 sy])))))
          pairs)
     (reduce min)
     ))

  )


(def example
  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3")
