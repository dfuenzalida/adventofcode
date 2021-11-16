(ns advent2020.day14
  (:require [clojure.string :refer [split-lines starts-with?]]))

(declare example-mask example example2)

(defn masks-for [s]
  (let [mask1s (->> s (map {\X 1 \1 1 \0 0}) (reduce str))
        mask0s (->> s (map {\X 0 \1 1 \0 0}) (reduce str))]
    [(Long/parseLong mask0s 2) (Long/parseLong mask1s 2)]))

(defn to-binary [val width]
  (->> val Long/toBinaryString
       (format (str "%" width "s")) (replace {\space \0}) (reduce str)))

(defn mask-value [mask val]
  (let [val-str (to-binary val 36)
        new-val (->> (map (fn [m v] (get {\1 1 \0 0} m v)) mask val-str)
                     (reduce str))]
    (Long/parseLong new-val 2)))

;; (= 73 (mask-value example-mask 11))
;; (= 101 (mask-value example-mask 101))
;; (= 64 (mask-value example-mask 0))

(defn parse-line [[mask m] line]
  (if-let [newmask (->> (re-seq #"mask = (.+)$" line) first second)]
    [newmask m]
    (let [[pos val] (->> (re-seq #"mem\[(\d+)\] = (\d+)" line) first rest (map read-string))]
            [mask (assoc m pos (mask-value mask val))])))

(defn part-1 [input]
  (->> (reduce parse-line ["" {}] (split-lines input))
       second
       vals
       (reduce +)))

;; (= 165 (part-1 example))
;; (part-1 (slurp "resources/day14.txt"))

(defn from-binary [s]
  (Long/parseLong s 2))

(defn decode-mask [mask address]
  (let [address-bits (to-binary address 36)]
    (->> (map (fn [a bm] (condp = bm \0 a \1 \1 \X \X)) address-bits mask)
         (reduce str))))

;; (decode-mask "000000000000000000000000000000X1001X" 42)

(defn addresses-for [mask address]
  (let [masked (decode-mask mask address)
        numxs (count (filter #{\X} mask))
        fmat  (str "%" numxs "s")
        fmask (->> (replace {\X "%s"} masked) (reduce str))]
    (for [i (range (bit-shift-left 1 numxs))] ;; 0 to 2^(n-1)
      (->> (Long/toBinaryString i)
           (format fmat) ;; make sure the output is always numxs bits
           (replace {\space \0})
           (apply format fmask)
           from-binary))))

;; (= [26 27 58 59] (addresses-for "000000000000000000000000000000X1001X" 42))
;; (= [16 17 18 19 24 25 26 27] (addresses-for "00000000000000000000000000000000X0XX" 26))

(defn parse-line2 [[mask m] line]
  (if-let [newmask (->> (re-seq #"mask = (.+)$" line) first second)]
    [newmask m]
    (let [[pos val] (->> (re-seq #"mem\[(\d+)\] = (\d+)" line) first rest (map read-string))
          addresses (addresses-for mask pos)
          m'        (reduce #(assoc %1 %2 val) m addresses)]
      [mask m'])))

(defn part-2 [input]
  (->> (reduce parse-line2 ["" {}] (split-lines input))
       second
       vals
       (reduce +)))

;; (= 208 (part-2 example2))
;; (time (part-2 (slurp "resources/day14.txt")))
;; => "Elapsed time: 733.682177 msecs"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example-mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")

(def example
  "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0")

(def example2
  "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1")

(comment

  (->> example split-lines first (re-seq #"= (.+)$") first second)
  (->> example split-lines second (re-seq #"mem\[(\d+)\] = (\d+)") first rest (map read-string))

  (reduce parse-line ["" {}] (split-lines example))

  ;; How many Xs max?
  (->> (slurp "resources/day14.txt") split-lines
       (map #(filter #{\X} %))
       (map count)
       (reduce max))

  ;; print a few masked values for part-2
  (let [mask "X01X0001"
        numxs (count (filter #{\X} mask))
        fmat  (str "%" numxs "s")
        fmask (->> (replace {\X "%s"} mask) (reduce str))]
    (for [i (range (bit-shift-left 1 numxs))] ;; 0 to 2^(n-1)
      (->> (Long/toBinaryString i)
           (format fmat) ;; make sure the output is always numxs bits
           (replace {\space \0})
           (apply format fmask))
      ))
  )

