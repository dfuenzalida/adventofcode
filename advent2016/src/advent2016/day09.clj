(ns advent2016.day09)

(defn decompressed-length [input]
  (loop [total 0, input input]
    (if (empty? input)
      total
      (if (= \( (first input))
        (let [[whole len times] (first (re-seq #"\((\d+)x(\d+)\)" input))
              [len times]       (map read-string [len times])
              rep-start         (count whole)
              rep-size          (* len times)]
          (recur (+ total rep-size) (subs input (+ rep-start len))))
        (recur (inc total) (subs input 1))))))

;; (= "ADVENT" (decompress "ADVENT"))
;; (= "ABBBBBC" (decompress "A(1x5)BC"))
;; (= "XYZXYZXYZ" (decompress "(3x3)XYZ"))
;; (= "ABCBCDEFEFG" (decompress "A(2x2)BCD(2x2)EFG"))
;; (= "(1x3)A" (decompress "(6x1)(1x3)A"))
;; (= "X(3x3)ABC(3x3)ABCY" (decompress "X(8x2)(3x3)ABCY"))

(defn part-1 []
  (-> "resources/day09.txt" slurp decompressed-length dec #_not_sure_why_off_by_1))

;; (part-1)

(defn decompressed-length2 [input]
  (loop [total 0, input input]
    (if (empty? input)
      total
      (if (= \( (first input))
        (let [[whole len times] (first (re-seq #"\((\d+)x(\d+)\)" input))
              [len times]       (map read-string [len times])
              rep-start         (count whole)
              repetition        (subs input rep-start (+ rep-start len))
              input             (subs input (+ rep-start len))]
          (recur (+ total (* times (decompressed-length2 repetition))) input))
        (recur (inc total) (subs input 1))))))

;; (= (decompressed-length2 "(3x3)XYZ") (count "XYZXYZXYZ"))
;; (= (decompressed-length2 "X(8x2)(3x3)ABCY") (count "XABCABCABCABCABCABCY"))
;; (= (decompressed-length2 "(27x12)(20x12)(13x14)(7x10)(1x12)A") 241920)
;; (= (decompressed-length2 "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN") 445)

(defn part-2 []
  (-> "resources/day09.txt" slurp decompressed-length2 dec))

;; (part-2)

