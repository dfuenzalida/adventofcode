(ns advent2022.day13
  (:require [clojure.string :as string]))

(defn read-input [s]
  (->> (string/split-lines s)
       (remove empty?)
       (map read-string)
       (partition 2)))

(defn right-order? [a b]
  ;; (println "comparing" a b)
  (cond
    (and (number? a) (number? b)) (if (= a b) nil (< a b))
    (and (coll? a) (coll? b))     (let [ca (count a) cb (count b)
                                        res (->> (map right-order? a b) (remove nil?) first)]
                                    (if (nil? res)
                                      (if (= ca cb) nil (< ca cb))
                                      res))
    (and (number? a) (coll? b))   (right-order? [a] b)
    (and (coll? a) (number? b))   (right-order? a [b])
    :else nil))

;; (= true (right-order? [1 1 3 1 1] [1 1 5 1 1]))
;; (= true (right-order? [[1] [2 3 4]] [[1] 4]))
;; (= false (right-order? [9] [[8 7 6]]))
;; (= true (right-order? [[4,4],4,4] [[4,4],4,4,4]))
;; (= false (right-order? [7 7 7 7] [7 7 7]))
;; (= true (right-order? [] [3]))
;; (= false (right-order? [1,[2,[3,[4,[5,6,7]]]],8,9] [1,[2,[3,[4,[5,6,0]]]],8,9]))

(defn part-1 [s]
  (let [input (read-input s)]
    (->> (filter (fn [i] (let [[a b] (nth input i)]
                           (right-order? a b)))
                 (range (count input)))
         (map inc)
         (reduce +))))

;; (= 13 (part-1 example))
;; (part-1 (slurp "resources/day13.txt"))

(defn right-order-comparator [a b]
  (let [res (right-order? a b)]
    (condp = res nil 0 true -1 false 1)))

(defn part-2 [s]
  (let [packets (->> (read-input s)
                     (reduce concat)
                     (concat [[[2]] [[6]]])
                     (sort-by identity right-order-comparator))
        pred    #{[[2]] [[6]]}]
    (->> (filter (fn [i] (pred (nth packets i))) (range (count packets)))
         (map inc)
         (reduce *))))

;; (= 140 (part-2 example))
;; (part-2 (slurp "resources/day13.txt"))

(comment

  (let [packets (->> (read-input example)
                     (reduce concat)
                     (concat [[[2]] [[6]]])
                     (sort-by identity right-order-comparator))
        pred    #{[[2]] [[6]]}]
    (->> (filter (fn [i] (pred (nth packets i))) (range (count packets)))
         (map inc)
         (reduce *)))
  
  (->>
   (read-input example)
   last)

  (->> example
       string/split-lines
       (remove empty?)
       (map read-string)
       (partition 2)
       )
  
  )

(def example
  "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")
