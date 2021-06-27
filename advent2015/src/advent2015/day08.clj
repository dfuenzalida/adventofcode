(ns advent2015.day08
  (:require [clojure.string :refer [split-lines]]))

(defn read-input []
  (->> (slurp "resources/day08.txt") split-lines))

(def hex
  {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7
   \8 8 \9 9 \a 10 \b 11 \c 12 \d 13 \e 14 \f 15})

(defn to-char [a b]
  (->> (+ (* 16 (hex a)) (hex b)) char))

(defn unescape [s]
  (loop [in (rest (butlast s)) out []]
    (if-let [c (first (seq in))]
      (cond
        (= \\ c) (let [[x y z] (take 3 (rest in))]
                   (cond
                     (= \\ x) (recur (drop 2 in) (conj out \\))
                     (= \" x) (recur (drop 2 in) (conj out \"))
                     (= \x x) (recur (drop 4 in) (conj out (to-char y z)))))
        :else (recur (rest in) (conj out c)))
      out)))

(defn part-1 []
  (let [input         (read-input)
        source-length (->> input (map count) (reduce +))
        unesc-length  (->> input (map (comp count unescape)) (reduce +))]
    (- source-length unesc-length)))

;; (part-1)

(defn escape [s]
  (loop [in s, out []]
    (if-let [c (first (seq in))]
      (cond
        (= \\ c) (recur (rest in) (into out [\\ \\]))
        (= \" c) (recur (rest in) (into out [\\ \"]))
        :else (recur (rest in) (into out [c])))
      (->> (reduce str out) (format "\"%s\"")))))

(defn part-2 []
  (let [input          (read-input)
        source-length  (->> input (map count) (reduce +))
        escaped-length (->> input (map (comp count escape)) (reduce +))]
    (- escaped-length source-length)))

;; (part-2)

