(ns advent2021.day14
  (:require [clojure.string :refer [blank? split split-lines]]))

(declare example)

(defn read-input [s]
  (let [[starter & rules] (->> s split-lines (remove blank?))
        rules (->> (map #(let [[k v] (split % #" -> ")]
                           {(vec k) (first v)}) rules)
                   (reduce merge {}))]
    [starter rules]))

;; When AB is expanded into AXB two things happen:
;; - every occurrence of AB dissapears and the same number of AX and XB pairs appear
;; - the numbrer of X chars increases by the previous number of AB pairs, but As and Bs stay the same

(defn make-pairmap [s]
  (->> s (partition 2 1) (map vec) frequencies))

;; (->> example read-input first make-pairmap)
;; (->> example read-input first frequencies)

(defn expand [expmap [pairmap charmap]]
  (let [tuples   (map
                  (fn [[[k1 k2] pcount]]
                    (if-let [c (expmap [k1 k2])]
                      [{[k1 c] pcount, [c k2] pcount} {c pcount}]
                      [{[k1 k2] pcount} {}]))
                  pairmap)
        pairmap' (reduce (partial merge-with +) {} (map first tuples))
        charmap' (reduce (partial merge-with +) charmap (map second tuples))]
    [pairmap' charmap']))

(defn part-1 [s n]
  (let [[starter expmap] (read-input s)
        pairmap-and-freq [(make-pairmap starter) (frequencies starter)]
        iters (iterate (partial expand expmap) pairmap-and-freq)
        freqs (->> (nth iters n) second (sort-by second))]
    (- (second (last freqs)) (second (first freqs)))))

;; (= 1588 (part-1 example 10))
;; (part-1 (slurp "resources/day14.txt") 10)

;; (= 2188189693529 (part-1 example 40))
;; (time (part-1 (slurp "resources/day14.txt") 40))
;; => "Elapsed time: 38.90599 msecs"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")
