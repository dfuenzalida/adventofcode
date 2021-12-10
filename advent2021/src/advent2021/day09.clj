(ns advent2021.day09
  (:require [clojure.string :refer [split-lines]]))

(declare example)

(defn read-input [s]
  (->> s split-lines
       (mapv #(mapv (comp read-string str) %))))

(defn make-height-map [xss]
  (let [height (count xss)
        width (count (first xss))]
    (->> (for [j (range height) i (range width)]
           [[i j] (get-in xss [j i])])
         (reduce merge {}))))

(defn low-point? [m [x y]]
  (let [height (m [x y])
        deltas [[-1 0] [1 0] [0 -1] [0 1]]
        points (map (fn [[dx dy]] [(+ x dx) (+ y dy)]) deltas)
        heights (->> (map m points) (remove nil?))]
    (every? (partial < height) heights)))

;; (true? (-> example read-input make-height-map (low-point? [1 0])))

(defn part-1 [s]
  (let [m  (->> s read-input make-height-map)
        xs (filter (partial low-point? m) (keys m))]
    (->> (map (comp inc m) xs)
         (reduce +))))

;; (= 15 (part-1 example))
;; (part-1 (slurp "resources/day09.txt"))

(defn grow
  "Given a map and point, return a seq of neighour points that have a greater value under 9"
  [m [x y]]
  (let [height (m [x y])
        deltas [[-1 0] [1 0] [0 -1] [0 1]]
        points (map (fn [[dx dy]] [(+ x dx) (+ y dy)]) deltas)]
    (->> (remove (comp nil? m) points)
         (filter #(and (<= height (m %)) (< (m %) 9))))))

(defn basin-points
  "Takes a map and points, expands the set of points using `grow` until reaching the largest set for the map"
  [m points]
  (let [points' (->> (mapcat (partial grow m) points) (concat points) set)]
    (if (= points points')
      points'
      (recur m points'))))

(defn part-2 [s]
  (let [m  (->> s read-input make-height-map)
        xs (filter (partial low-point? m) (keys m))
        bs (map #(basin-points m [%]) xs)]
    (->> (map count bs) sort reverse (take 3) (reduce *))))

;; (= 1134 (part-2 example))
;; (part-2 (slurp "resources/day09.txt"))

(comment

  (let [m (-> example read-input make-height-map)]
    (->> (basin-points m #{[2 2]}) count))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example "2199943210
3987894921
9856789892
8767896789
9899965678")
