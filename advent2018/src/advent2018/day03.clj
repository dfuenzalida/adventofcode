(ns advent2018.day03)

(def example-input ["#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2"])

(defn points-for [claim]
  (let [nums (->> claim
                  (re-seq #"#(.+) @ (.+),(.+): (.+)x(.+)")
                  first
                  rest)
        [claim-id left top width height] (map read-string nums)]
    (for [x (range left (+ left width))
          y (range top (+ top height))]
      [x y])))

;; (points-for (last example-input))

(defn part-1 [input]
  (->> input
       (mapcat points-for)
       frequencies
       vals
       (filter #(< 1 %))
       count))

;; (part-1 example-input) => 4

(defn read-input []
  (let [input-file (slurp "resources/day03.txt")]
    (.split #"\n" input-file)))

;; (part-1 (read-input))

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn delete-nth [xs n]
  (concat (take n xs)
          (drop (inc n) xs)))

;; just the points in the outline of a rectangle
(defn outline-for [claim]
  (let [nums (->> claim
                  (re-seq #"#(.+) @ (.+),(.+): (.+)x(.+)")
                  first
                  rest)
        [claim-id left top width height] (map read-string nums)]
    (set
     (concat
      (for [x (range left (+ left width))] [x top])
      (for [x (range left (+ left width))] [x (+ -1 top height)])
      (for [y (range top (+ top height))] [left y])
      (for [y (range top (+ top height))] [(+ -1 left width) y])
      ))))

;; (outline-for "#1 @ 2,2: 2x2")
;; (count (outline-for "#1 @ 0,0: 3x3")) => 8 ;; (3x3 grid minus the center point)

(defn part-2 [input]
  (first
   (filter some?
           (for [n (range (count input))]
             (let [most-points (set (mapcat points-for (delete-nth input n)))
                   claim-points (set (points-for (nth input n)))]
               (when-not (seq (filter claim-points most-points))
                 n))))))

;; (part-1 (drop 1 example-input))
;; (part-2 example-input)
;; (part-2 (read-input))
