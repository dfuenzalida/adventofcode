(ns advent2022.day11
  (:require [clojure.string :as string]))

;; reads 6 lines of text, returns a key-value pair with the monkey attributes
(defn read-entries [xs]
  (let [[[id] [& items] [op arg] [testarg] [id-true] [id-false]]
        (->> (map #(re-seq #"[\d\*\+]+" %) xs)
             (map #(str "[" (reduce str (interpose " " %)) "]"))
             (map read-string))]
    [id {:items (into [] items) :op op :arg arg :testarg testarg :id-true id-true :id-false id-false :inspected 0}]))

(defn read-input [s]
  (->> s string/split-lines (partition-by empty?) (remove #(= 1 (count %))) (map read-entries) (into (sorted-map))))

(defn inspect-items [state n]
  (let [monkey-state (get state n)
        {:keys [items op arg testarg id-true id-false]} monkey-state]
    (if-let [item (first items)]
      (let [op     ({'+ + '* *} op)
            arg    (first (filter some? [arg item]))
            level  (int (/ (op item arg) 3))
            target (if (zero? (mod level testarg)) id-true id-false)
            state' (-> state
                       (update-in [n :items] subvec 1)
                       (update-in [n :inspected] inc)
                       (update-in [target :items] conj level))]
        (recur state' n))
      state)))

(defn inspect-round [state]
  (reduce inspect-items state (keys state)))

(defn part-1 [s]
  (->> (read-input s)
       (iterate inspect-round)
       (drop 20)
       first
       vals
       (map :inspected)
       (sort >)
       (take 2)
       (reduce *)))

;; (= 10605 (part-1 example))
;; (part-1 (slurp "resources/day11.txt"))

(defn inspect-items2 [nmod state n]
  (let [monkey-state (get state n)
        {:keys [items op arg testarg id-true id-false]} monkey-state]
    (if-let [item (first items)]
      (let [op     ({'+ +' '* *'} op)
            arg    (first (filter some? [arg item]))
            level  (mod (op item arg) nmod) ;; keep it manageable
            target (if (zero? (mod level testarg)) id-true id-false)
            state' (-> state
                       (update-in [n :items] subvec 1)
                       (update-in [n :inspected] inc)
                       (update-in [target :items] conj level))]
        (recur nmod state' n))
      state)))

(defn inspect-round2 [nmod state]
  (reduce (partial inspect-items2 nmod) state (keys state)))

(defn find-inspected [s rounds nmod]
  (->> (read-input s)
       (iterate (partial inspect-round2 nmod))
       (drop rounds)
       first
       vals
       (map :inspected)))

(defn part-2 [s]
  (let [nmod (->> s read-input vals (map :testarg) (reduce *))]
    (->> (find-inspected s 10000 nmod)
         (sort >)
         (take 2)
         (reduce *))))

;; (= 2713310158 (part-2 example))
;; (part-2 (slurp "resources/day11.txt"))

(comment

  (->> (read-input example)
       vals
       (map :testarg)
       (reduce *))

  (let [nmod 11]
    (and
     (= [2 4 3 6] (find-inspected example 1 nmod))
     ;; (= [99 97 8 103] (find-inspected example 20 nmod))
     ))

  (->> (read-input example)
       (iterate inspect-round)
       (drop 20)
       first
       clojure.pprint/pprint)

  )

(def example
  "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")
