(ns advent2015.day14)

(defn read-input []
  (->> (slurp "resources/day14.txt")
       (re-seq #"\d+")
       (map clojure.edn/read-string)
       (partition 3)))

(defn make-timeline [speed endurance wait]
  (cycle (concat (repeat endurance speed) (repeat wait 0))))

(defn distance-after [time xs]
  (->> (take time xs) (reduce + 0)))

;; (->> (make-timeline 14 10 127) (distance-after 1000))

(defn part-1 []
  (->> (read-input)
       (map (partial apply make-timeline))
       (map (partial distance-after 2503))
       (reduce max)))

;; (part-1)

;; For part 2 we use accumulated distance using `reductions` instead
;; and take vertical slices of the different timelines to find the
;; winner index[es] for each second. Then we use `frequencies` to find
;; the most frequent winner.

(defn acc-distance-after [time xs]
  (->> (take time xs) (reductions +)))

(defn max-indexes [xs] ;; returns the indexes of the greater value in xs
  (first
   (reduce (fn [[indexes maxval] [i x]]
             (cond
               (= x maxval) [(into indexes [i]) maxval]
               (> x maxval) [[i] x]
               :else [indexes maxval]))
           [[] 0]
           (map-indexed vector xs))))

;; (max-indexes [1 9 9 3 4 5 2 9])

(defn part-2 []
  (->> (read-input)
       (map (partial apply make-timeline))
       (map (partial acc-distance-after 2503))
       (apply map vector) ;; "vertical slices"
       (map max-indexes)
       (reduce into [])
       frequencies
       vals
       (reduce max)))

;; (part-2)

(comment
  (->> [[14 10 127] [16 11 162]]
       (map (partial apply make-timeline))
       (map (partial acc-distance-after 1000))
       (apply map vector)
       (map max-indexes)
       (reduce into [])
       frequencies)
  )
