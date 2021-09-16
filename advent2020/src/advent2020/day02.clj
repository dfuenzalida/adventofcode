(ns advent2020.day02
  (:require [clojure.string :refer [split-lines]]))

;; 9-11 d: dddpddsnzdkqpdddk

(defn parse-line [line] ;; "a-b x: zzzzz" -> [a b \x zzzz]
  (let [[l h c password] (->> line (re-seq #"(\d+)-(\d+) (\w): (\w+)") first next)]
    [(read-string l) (read-string h) (first c) password]))

(defn read-input []
  (->> "resources/day02.txt" slurp split-lines (mapv parse-line)))

(defn valid1? [[l h c password]]
  (<= l (get (frequencies password) c 0) h))

;; (true? (->> "1-3 a: abcde" parse-line valid1?))
;; (false? (->> "1-3 b: cdefg" parse-line valid1?))
;; (true? (->> "2-9 c: ccccccccc" parse-line valid1?))

(defn part-1 []
  (->> (read-input) (filter valid1?) count))

;; (part-1)

(defn valid2? [[i j c password]]
  (let [x (= c (nth password (dec i)))
        y (= c (nth password (dec j)))]
    (= (set [x y]) #{true false})))

;; (true? (valid2? [1 3 \a "abcde"]))
;; (false? (valid2? [1 3 \b "cdefg"]))
;; (false? (valid2? [2 9 \c "ccccccccc"]))

(defn part-2 []
  (->> (read-input) (filter valid2?) count))

;; (part-2)
