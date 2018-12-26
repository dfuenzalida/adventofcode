(ns advent2018.day12
  (:require [clojure.string :as s :refer [starts-with?]]))

(def example "initial state: #..#.#..##......###...###

...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #")

(defn parse-input [input]
  (let [lines (s/split-lines input)
        state (->> lines first (re-seq #"[#.]+") first)
        rules (into {} (map #(vec (nfirst (re-seq #"(.+) => (.+)" %))) (drop 2 lines)))]
    [state rules]))

(def example-input
  (parse-input example))

;; (defn remap-rule [[pattern value]]
;;   (let [mapper {\# :plant \. :empty}]
;;     [(mapv mapper pattern) (mapper (first value))]))

;; (remap-rule (first (second example-input)))

(defn print-state [state]
  (println state))

(defn simulate [offset state rules iters]
  ;; (print-state state)
  (if (pos? iters)
    (let [state  (str ".." state "...")
          pieces (map #(apply str %) (partition 5 1 state))]
      (recur offset
             (apply str (map #(get rules % ".") pieces))
             rules
             (dec iters)))
    [offset state]))

;; (let [[state rules] example-input] (simulate 0 state rules 21))

(defn compute-sum [input iters]
  ;; (println "============================================================")
  (let [offset         -20
        padding        (apply str (repeat (Math/abs offset) \.))
        [state rules]  (parse-input input)
        state          (str padding state padding)
        [_ state] (simulate 0 state rules iters)]
    (->> (map vector (range offset (count state)) state)
         (filter (fn [[i c]] (= c \#)))
         (map first)
         (reduce +))))

;; (compute-sum example 20) => 325

;; Part 1
;; (compute-sum (slurp "resources/day12.txt") 20)

;; Part 2
(defn part-2 []
  (let [sum200 (compute-sum (slurp "resources/day12.txt") 200)
        sum300 (compute-sum (slurp "resources/day12.txt") 300)
        diff   (- sum300 sum200)]
    (-> 50000000000
        (- 200)
        (/ 100)
        (* diff)
        (+ sum200))))

;; (part-2)
    
