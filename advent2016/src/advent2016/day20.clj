(ns advent2016.day20)

(defn read-input []
  (->> (slurp "resources/day20.txt")
       (re-seq #"\d+")
       (map clojure.edn/read-string)
       (partition 2)
       (mapv vec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Starting with MAX = 1, loop though the intervals looking
;; for intervals [a b] so that one of the following are true:
;; * (<= 0 a b MAX) /contained interval/  => MAX stays the same
;; * (<= 0 a MAX b) /overlapping interval/  => MAX = b
;; * (= (inc MAX) a) /adjacent interval/ => MAX = b
;; If a whole interation does not change the MAX, we end with MAX+1

(defn upper-bound [xs]
  (loop [xs xs, remaining [], upper 1, continue? false]
    (let [inter (first xs)]
      (if inter
        (let [[a b]  inter
              upper' (cond
                       (<= 0 a b upper) upper ;; contained
                       (<= 0 a upper b) b     ;; overlapped
                       (= (inc upper) a) b    ;; adjacent
                       :else nil)]
          (if upper'
            (recur (rest xs) remaining upper' true)
            (recur (rest xs) (conj remaining inter) upper continue?)))
        (if continue?
          (recur remaining [] upper false)
          (inc upper))))))

;; (= 3 (upper-bound [[5 8] [0 2] [4 7]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn part-1 []
  (upper-bound (read-input)))

;; (part-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2
;;
;; For every pair of intervals [a b] and [c d], they can overlap or not in the following
;; patterns: (>= a c b d), (>= c a d b), (>= a c d b), (>= c a b d) or not overlap at all.
;;
;; Loop through the list of intervals by checking if a first interval overlaps any other.
;; If they overlap, remove both intervals from the list, adding a new interval that combines
;; them as per the rules above (eg. (<= c a d b) becomes [c b]).
;;
;; If an overlap happened, we start over with the updated list of intervals. Otherwise
;; we're done, we subtract the widths of all intervals from 2^32.

(defn overlap [[a b] [c d]]
  (cond
    (<= a c b d) [a d]
    (<= c a d b) [c b]
    (<= a c d b) [a b]
    (<= c a b d) [c d]
    (= (inc b) c) [a d]
    (= (inc d) a) [c b]
    :else nil))

;; (overlap [1 2] [1 2])

(defn overlaps [xs]
  (for [i (range (count xs))
        j (range (inc i) (count xs))
        :let [inter1 (xs i)
              inter2 (xs j)
              over   (overlap inter1 inter2)]
        :when over]
    [over inter1 inter2]))

;; (overlaps [[0 5] [4 10] [15 20]])

(defn size [[a b]]
  (- b a -1))

(defn reduce-intervals [xs]
  (loop [xs xs]
    (let [olaps (overlaps xs)]
      (if (empty? olaps) ;; no overlaps in the last iteration, we're done
        (->> xs (map size) (reduce - (bit-shift-left 1 32)))
        ;; for every triplet in olaps, collect the first items (interval union)
        ;; collect the tails (to be removed) and create the next interval version
        (let [_       (println "reducing..." (count olaps))
              unions  (set (map first olaps))
              rem-set (set (mapcat rest olaps))
              xs'     (->> (remove rem-set xs)
                           (concat unions)
                           (into []))]
          (recur xs'))))))

;; (reduce-intervals [[5 8] [0 2] [4 7]])

(defn part-2 []
  (reduce-intervals (read-input)))

;; (part-2)
