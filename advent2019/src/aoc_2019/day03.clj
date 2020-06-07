(ns aoc-2019.day03
  (:require [clojure.edn :as edn]
            [clojure.string :refer [split split-lines]]))

(defn read-input []
  (split-lines (slurp "resources/input03.txt")))

(def deltas-for-dir {\U [0 1], \D [0 -1], \L [-1 0], \R [1 0]})

(defn manh-dist [[a b]]
  (+ (Math/abs a) (Math/abs b)))

(defn segments [line] ;; list of 4 coords: [[a b c d] ... ]
  (loop [x 0, y 0, result [], movs (split line #",")]
    (if (seq movs)
      (let [mov     (first movs)
            [dx dy] (deltas-for-dir (first mov))
            dist    (edn/read-string (subs mov 1))
            xdx     (+ x (* dist dx))
            ydy     (+ y (* dist dy))]
        (recur xdx
               ydy
               (conj result [(min x xdx) (min y ydy) (max x xdx) (max y ydy)])
               (rest movs)))
      result)))

(defn intersect [[a b c d] [e f g h]] ;; for lines [a b] to [c d] and [e f] to [g h]
  (when (and (= a c) (= f h) (<= e a g) (<= b f d))
    [a f]))

(defn intersections [line1 line2]
  (rest ;; discard [0 0]
   (for [ps1 (segments line1)
         ps2 (segments line2)
         :let [isection (or (intersect ps1 ps2) (intersect ps2 ps1))]
         :when isection]
     isection)))

(defn min-distance [p1 p2]
  (->> (intersections p1 p2)
       (map manh-dist)
       (reduce min)))

(defn solve-1 []
  (let [[p1 p2] (read-input)]
    (min-distance p1 p2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn path-to-movs [path]
  (let [movs (split path #",")]
    (mapcat (fn [mov]
              (let [[dx dy] (deltas-for-dir (first mov))
                    dist    (edn/read-string (subs mov 1))]
                (repeat dist [dx dy]))) movs)))

;; for each intersection point [x y] => number of steps since start of path

(defn steps-map [path isections]
  (loop [x 0, y 0, steps 1, result {}, movs (path-to-movs path)]
    ;; (println [x y])
    (if (seq movs)
      (let [[dx dy] (first movs)
            x'      (+ x dx)
            y'      (+ y dy)]
        (if (isections [x' y'])
          (recur x' y' (inc steps) (merge-with min {[x' y'] steps} result) (rest movs))
          (recur x' y' (inc steps) result (rest movs))))
      result)))

(defn least-steps [p1 p2]
  (let [iss (set (intersections p1 p2))]
    (->> (merge-with + (steps-map p1 iss) (steps-map p2 iss))
         vals
         (reduce min))))

(defn solve-2 []
  (let [[p1 p2] (read-input)]
    (least-steps p2 p1)))

