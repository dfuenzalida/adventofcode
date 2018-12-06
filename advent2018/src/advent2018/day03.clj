(ns advent2018.day03)

(def example-input ["#1 @ 1,3: 4x4"
                    "#2 @ 3,1: 4x4"
                    "#3 @ 5,5: 2x2"])

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

;; find the missing claim

(defn points-and-claims-for [claim]
  (let [nums (->> claim
                  (re-seq #"#(.+) @ (.+),(.+): (.+)x(.+)")
                  first
                  rest)
        [claim-id left top width height] (map read-string nums)]
    (for [x (range left (+ left width))
          y (range top (+ top height))]
      {[x y] [claim-id]})))

(defn claims-for [claim]
  (->> claim
       (re-seq #"#(.+) @ .*")
       first
       rest
       first
       read-string))

;; (claims-for (first example-input))

;; map points to => [[x y] claim-id]
;; group-by first
;; filter kv pairs with values of more than 1 elem
;; map second to find the values
;; put all values into a set -> set of claims that overlap

(defn overlapping-claims [input]
  (->> input
       (mapcat points-and-claims-for)
       (apply merge-with into)
       (map second)
       (filter #(> (count %) 1))
       (reduce into #{})))

;; (overlapping-claims example-input)

(defn part-2 [input]
  (let [all-claims (set (map claims-for input))
        ovr-claims (overlapping-claims input)]
    (first
     (for [claim all-claims
           :when (not (ovr-claims claim))]
       claim))))

;; (part-2 example-input)
;; (part-2 (read-input))
