(ns advent2020.day11
  (:require [clojure.string :refer [split-lines]]))

(declare example)

(def deltas
  [[1 0] [-1 0] [0 1] [0 -1] [-1 -1] [1 1] [-1 1] [1 -1]])

(defn parse-input [s]
  (let [source (->> s split-lines (mapv vec))]
    (->> (for [j (range (count source))
               i (range (count (first source)))
               :let [v (get-in source [j i])]
               :when (some #{\L \#} [v])]
           [[j i] v])
         (reduce merge {}))))

(defn neighbors [m [j i]]
  (let [current (get m [j i])
        neighs  (->> (map (fn [[dj di]] (get m [(+ j dj) (+ i di)])) deltas)
                     (remove nil?)
                     frequencies)
        nextval (cond
                  (and (= \L current) (nil? (get neighs \#))) \#
                  (and (= \# current) (<= 4 (get neighs \# 0))) \L
                  :else current)]
    [[j i] nextval]))

;; (neighbors (parse-input example) [0 0])

(defn iter-seats [m]
  (->> (map #(neighbors m %) (keys m))
       (reduce merge {})))

;; (iter-seats (iter-seats (parse-input example)))

(defn part-1 [s]
  (let [m (parse-input s)]
    (->> (iterate iter-seats m)
         (partition 2 1)
         (take-while (fn [[a b]] (not= a b)))
         last
         second
         vals
         (filter #{\#})
         count)))

;; (= 37 (part-1 example))

;; (part-1 (slurp "resources/day11.txt"))

;; find the first non-null element of `m` at `[j i]`
;; in the `[dj di]` direction up to a distance of `size`
(defn trace [m j i size [dj di]]
  (let [steps (max (if (neg? dj) j (- size j)) (if (neg? di) i (- size i)))]
    (->> (for [n (range 1 (inc steps))
               :let [v (get m [(+ j (* n dj)) (+ i (* n di))])]]
           v)
         (drop-while nil?)
         first)))

(defn neighbors2 [m [j i]]
  (let [size    (->> m keys (apply max-key first) first)
        current (get m [j i])
        neighs  (->> (map (partial trace m j i size) deltas)
                     (remove nil?)
                     frequencies)
        nextval (cond
                  (and (= \L current) (nil? (get neighs \#))) \#
                  (and (= \# current) (<= 5 (get neighs \# 0))) \L
                  :else current)]
    [[j i] nextval]))

(defn iter-seats2 [m]
  (->> (map #(neighbors2 m %) (keys m))
       (reduce merge {})))

(defn part-2 [s]
  (let [m (parse-input s)]
    (->> (iterate iter-seats2 m)
         (partition 2 1)
         (take-while (fn [[a b]] (not= a b)))
         last
         second
         vals
         (filter #{\#})
         count)))

;; (time (= 26 (part-2 example)))

;; (time (part-2 (slurp "resources/day11.txt")))
;; "Elapsed time: 741425.620645 msecs"

(comment
  (let [m (parse-input example)]
    (->> m keys (apply max-key first) #_first))

  (let [m (parse-input example)]
    (->> m keys (apply max-key second) #_second))

  (let [m (parse-input example)]
    (->> (iterate iter-seats m)
         (partition 2 1)
         (take-while (fn [[a b]] (not= a b)))
         last
         second
         vals
         (filter #{\#})
         count))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example
  "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")
