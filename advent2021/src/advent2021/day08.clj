(ns advent2021.day08
  (:require [clojure.string :refer [split-lines]]))

(declare example example2)

(defn read-input [s]
  (->> s split-lines
       (mapv #(partition-all 10 (re-seq #"[a-g]+" %)))))

(defn part-1 [s]
  (->> s read-input
       (mapcat second)
       (map count)
       (filter #{2 3 4 7}) ;; numbers 1, 4, 7 and 8 have 2, 3, 4 or 7 segments
       count))

;; (= 26 (part-1 example2))
;; (part-1 (slurp "resources/day08.txt"))

(defn key-for [& xs]
  (->> xs set sort (reduce str)))

(defn make-segment-map [xs] ;; create a map with the strings to digits
  (let [freqs  (->> (reduce str xs) frequencies (map (fn [[a b]] [b a])) (reduce merge {}))
        ;; deduce the segments
        [bot-l top-l bot-r] [(freqs 4) (freqs 6) (freqs 9)]
        s2digs (->> (filter #(= 2 (count %)) xs) first)
        top-r  (->> (remove #{bot-r} s2digs) first)
        s3digs (->> (filter #(= 3 (count %)) xs) first)
        top    (->> (remove #{bot-r top-r} s3digs) first)
        s4digs (->> (filter #(= 4 (count %)) xs) first)
        middle (->> (remove #{bot-r top-l top-r} s4digs) first)
        bottom (->> (remove #{bot-l top-l bot-r top-r top middle} "abcdefg") first)]
    (zipmap [(key-for top top-l top-r bot-l bot-r bottom) ;; 0
             (key-for top-r bot-r) ;; 1
             (key-for top top-r middle bot-l bottom) ;; 2
             (key-for top top-r middle bot-r bottom) ;; 3
             (key-for top-l top-r middle bot-r) ;; 4
             (key-for top top-l middle bot-r bottom) ;; 5
             (key-for top top-l middle bot-l bot-r bottom) ;; 6
             (key-for top top-r bot-r) ;; 7
             "abcdefg" ;; 8 (every segment)
             (key-for top top-l top-r middle bot-r bottom)] ;; 9
            (range 0 10))))

(defn lookup-segment [m s]
  (->> s set sort (reduce str) m))

(defn digits-to-num [nums]
  (reduce #(+ (* 10 %1) %2) 0 nums))

(defn compute-line [[seqs digits]]
  (let [digit-map (make-segment-map seqs)]
    (->> (map (partial lookup-segment digit-map) digits)
         digits-to-num)))

(defn part-2 [s]
  (->> s read-input
       (map compute-line)
       (reduce +)))

;; (= 5353 (part-2 example))
;; (= 8 (-> example read-input ffirst make-segment-map (lookup-segment "fdgacbe")))

;; (time (part-2 (slurp "resources/day08.txt")))
;; => "Elapsed time: 32.389484 msecs"

(comment
  (let [[seqs digits] (->> example read-input first)
        digit-map     (make-segment-map seqs)]
    (->> (map (partial lookup-segment digit-map) digits)
         digits-to-num)) ;; => 5353
  )

;; (lookup-segment (make-segment-map example) "ba")

;; (->> example read-input ffirst (reduce str) frequencies)

;; (->> example read-input ffirst (reduce str) frequencies (filter (comp #{9} second)) ffirst)
;; => bottom-right is \b

;; DEDUCTIONS based on the example
;; => the segment that is used 4 times across all digits is the :bottom-left vertical bar => g
;; => the segment that is used 6 times across all digits is the :top-left vertical bar => e
;; => the segment that is used 9 times across all digits is the :bottom-right vertical bar => b
;; (3 out of 7 segments)
;; => from the 2-letter sequence, the one that is not the :bottom-right is the :top-right => a
;; (4 out of 7 segments)
;; => from the 3-letter sequence, the one that is not :top-right or :bottom-right is the :top bar => d
;; (5 out of the 7 segments)
;; => from the 4-letter sequence, the one that is not :top-left, :top_right or :bottom-right is the :middle bar => f
;; (6 out of 7 segments)
;; => the remaining bar is the :bottom-bar => c

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example
  "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

(def example2
  "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")
