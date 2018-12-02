(ns advent2018.day02)

(defn letter-counts [s]
  (->> s frequencies vals (apply hash-set)))

(defn checksum [ws]
  (let [counts (map letter-counts ws)
        twos   (count (filter #(some #{2} %) counts))
        threes (count (filter #(some #{3} %) counts))]
    (* twos threes)))

;; (letter-counts "bababc")

(def ex-part-1 ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"])

;; (checksum ex-part-1)

(defn read-input []
  (let [input-file (slurp "resources/day02.txt")]
    (.split #"\n" input-file)))

;; Part 1:
;; (checksum (read-input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ex-part-2 ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"])

(defn diff-by-1? [a b]
  (->> (map = a b)
       (filter false?)
       count
       (= 1)))

;; (diff-by-1? "hello" "Hello")

(defn common-letters [ws]
  (let [[w1 w2] (first
                 (for [x ws, y ws, :when (diff-by-1? x y)]
                   [x y]))]    
    (->> (map (fn [a b] [(= a b) a]) w1 w2)
         (filter first)
         (map second)
         (apply str))))

;; (common-letters ex-part-2) => fgij
;; (common-letters (read-input))

