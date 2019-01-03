(ns advent2018.day06
  (:require [clojure.string :as s]))

(def example-input
  [[1, 1] [1, 6] [8, 3] [3, 4] [5, 5] [8, 9]])

(defn bounding-box [points]
  (let [top    (reduce min (map second points))
        left   (reduce min (map first points))
        bottom (reduce max (map second points))
        right  (reduce max (map first points))]
    [[left top] [right bottom]]))

;; (bounding-box example-input)

(defn inner-points [points] ;; points not in the bounding box
  (let [[[left top] [right bottom]] (bounding-box points)]
    (->> points
         (filter (fn [[x _]] (nil? (#{left right} x))))
         (filter (fn [[_ y]] (nil? (#{top bottom} y)))))))

(defn boundary-points [points] ;; points IN the bounding box
  (let [[[left top] [right bottom]] (bounding-box points)]
    (set (concat (filter (fn [[x _]] (#{left right} x)) points)
                 (filter (fn [[_ y]] (#{top bottom} y)) points)))))

;; (inner-points example-input)

(defn distance [[^long a ^long b] [^long c ^long d]]
  (+ (Math/abs (- a c)) (Math/abs (- b d))))

(defn closest [coords x y] ;; returns the closest coord (or nil if 2+ are closer)
  (let [dist-coord (map (fn [c] [(distance c [x y]) c]) coords)
        dist-freqs (group-by first dist-coord)
        min-dist   (reduce min (keys dist-freqs))
        closer-pts (get dist-freqs min-dist)]
    (when (= 1 (count closer-pts))
      (-> closer-pts first second))))

(defn outer-points [points]
  (let [[[left top]
         [right bottom]] (bounding-box points)
        outline-points   (concat (map #(vector % top) (range left (inc right)))
                                 (map #(vector % bottom) (range left (inc right)))
                                 (map #(vector left %) (range top (inc bottom)))
                                 (map #(vector right %) (range top (inc bottom))))]
    (set outline-points)))

;; (closer example-input 6 0)

(defn part1 [input]
  (let [[[x0 y0] [x1 y1]] (bounding-box input)
        outline-pts       (outer-points input)
        closest-out       (->> (map (fn [[x y]] (closest input x y)) outline-pts)
                               (filter some?)
                               set)
        boundary-pts      (boundary-points input)
        closest-grid      (for [x (range x0 (inc x1))
                                y (range y0 (inc y1))]
                            [(closest input x y) [x y]])
        grid-by-closest   (-> (group-by first closest-grid)
                              (dissoc nil))]
    
    (->> (reduce dissoc grid-by-closest closest-out) ;; coord -> [closest pts]
         vals
         (map count)
         (reduce max))))

;; (part1 example-input) => 17

(defn read-input []
  (->> (slurp "resources/day06.txt")
       s/split-lines
       (map #(str "[" % "]"))
       (map read-string)))

;; (time (part1 (read-input)))

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn part2 [input]
  (let [[[left top] [right bottom]] (bounding-box input)]
    (->> (for [x (range left (inc right))
               y (range top (inc bottom))]
           (reduce + (for [p2 input]
                       (distance [x y] p2))))
         (filter #(< % 10000))
         count)))

;; (time (part2 (read-input)))

