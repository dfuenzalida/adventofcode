(ns advent2020.day08
  (:require [clojure.string :refer [split-lines]]))

(declare example)

(defn parse-line [s]
  (let [[_ op amount] (first (re-seq #"(\w+) (.*)" s))]
    [op (read-string amount)]))

(defn read-input [s]
  (->> s split-lines (mapv parse-line)))

;; (read-input example)

(defn exec [program]
  (loop [ip 0, acc 0, ips #{}]
    (cond
      (some ips [ip]) [:loop acc]
      (>= ip (count program)) [:terminated acc]
      :else (let [[instr amount] (nth program ip)
                  ips'           (conj ips ip)]
              (case instr
                "acc" (recur (inc ip) (+ acc amount) ips')
                "jmp" (recur (+ ip amount) acc ips')
                "nop" (recur (inc ip) acc ips'))))))

;; (= 5 (second (exec (read-input example)))

(defn part-1 []
  (exec (read-input (slurp "resources/day08.txt"))))

;; (part-1)

(defn flip [program n]
  (let [[instr amount] (nth program n)
        instr' ({"nop" "jmp", "jmp" "nop", "acc" "acc"} instr)]
    (assoc-in program [n] [instr' amount])))

;; (flip [["nop" 10] ["jmp" 11] ["acc" 12]] 0)

(defn part-2 [program]
  (->> (for [i (range (count program))]
         (exec (flip program i)))
       (filter (comp #{:terminated} first))
       first
       second))

;; (= 8 (part-2 (read-input example)))
;; (part-2 (read-input (slurp "resources/day08.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")
