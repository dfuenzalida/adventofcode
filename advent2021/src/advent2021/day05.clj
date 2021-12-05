(ns advent2021.day05)

(declare example)

(defn parse-input [s]
  (->> (re-seq #"\d+" s)
       (map read-string)
       (partition 4)))

(defn horizontal-or-vertical? [[a b c d]]
  (or (= a c) (= b d)))

(defn horz-vert-points [[a b c d]]
  (for [i (range (min a c) (inc (max a c)))
        j (range (min b d) (inc (max b d)))]
    [i j]))

;; (= #{[1 1] [1 2] [1 3]} (set (horz-vert-points [1 1 1 3])))
;; (= #{[7 7] [8 7] [9 7]} (set (horz-vert-points [9 7 7 7])))

(defn part-1 [s]
  (->> (parse-input s)
       (filter horizontal-or-vertical?)
       (mapcat horz-vert-points)
       frequencies
       (remove (comp #{1} second))
       (map second)
       count))

;; (= 5 (part-1 example))
;; (part-1 (slurp "resources/day05.txt"))

(defn abs [x] ;; to avoid using Java interop
  (if (pos? x) x (* -1 x)))

(defn diagonal? [[a b c d]]
  (= (abs (- a c)) (abs (- b d))))

(defn horizontal-vertical-or-diagonal? [line]
  (or (horizontal-or-vertical? line) (diagonal? line)))

(defn line-points [[a b c d]]
  (if (horizontal-or-vertical? [a b c d])
    (horz-vert-points [a b c d])
    (let [dist (abs (- a c))
          dx   (if (> c a) 1 -1)
          dy   (if (> d b) 1 -1)]
      (for [i (range (inc dist))]
        [(+ a (* i dx)) (+ b (* i dy))]))))

;; (= [[1 1] [2 2] [3 3]] (line-points [1 1 3 3]))

(defn part-2 [s]
  (->> (parse-input s)
       (filter horizontal-vertical-or-diagonal?)
       (mapcat line-points)
       frequencies
       (remove (comp #{1} second))
       (map second)
       count))

;; (= 12 (part-2 example))
;; (part-2 (slurp "resources/day05.txt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")
