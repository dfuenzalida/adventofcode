(ns advent2021.day03
  (:require [clojure.string :refer [split-lines]]))

(declare example)

(defn digit-freqs [s]
  (let [lines (->> s split-lines)]
    (for [i (->> lines first count range)]
      (->> lines
           (map #(nth % i))
           frequencies))))

(defn digits-from-freqs [sortfn freqs]
  (->> freqs
       (map (partial sort-by second sortfn))
       (map ffirst)))

(defn from-bin [xs]
  (->> xs reverse
       (map-indexed (fn [i c] [(bit-shift-left 1 i) c]))
       (remove (comp #{\0} second))
       (map first)
       (reduce +)))

;; (= [\1 \0 \1 \1 \0] (->> example digit-freqs (digits-from-freqs >)))
;; (= 22 (from-bin [\1 \0 \1 \1 \0]))

(defn part-1 [s]
  (let [gamma   (->> s digit-freqs (digits-from-freqs >) from-bin)
        epsilon (->> s digit-freqs (digits-from-freqs <) from-bin)]
    (* gamma epsilon)))

;; (= 198 (part-1 example))
;; (part-1 (slurp "resources/day03.txt"))

(defn rating [xs sortfn tie]
  (loop [xs xs, i 0]
    (if (= 1 (count xs))
      (from-bin (first xs))
      (let [bits        (map #(nth % i) xs)
            freqs       (frequencies bits)
            mostfreqbit (->> freqs (sort-by second sortfn) ffirst)
            tied?       (->> freqs vals set count (= 1))]
        (if tied?
          (recur (filter #(= tie (nth % i)) xs) (inc i))
          (recur (filter #(= mostfreqbit (nth % i)) xs) (inc i)))))))

;; (= 23 (-> example split-lines (rating > \1)))
;; (= 10 (-> example split-lines (rating < \0)))

(defn part-2 [s]
  (let [oxygen (-> s split-lines (rating > \1))
        c02rat (-> s split-lines (rating < \0))]
    (* oxygen c02rat)))

;; (= 230 (part-2 example))
;; (part-2 (slurp "resources/day03.txt"))

(comment

  (let [lines (->> example split-lines)]
    (for [i (->> lines first count range)]
      (->> lines
           (map #(nth % i))
           frequencies)))

  (->> example
       digit-freqs
       (map (partial sort-by second >))
       (map ffirst)
       )

  (sort-by second < [[:a 3] [:b 2] [:c 1]])
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")
