(ns advent2015.day10)

(def count-and-first
  (juxt count first))

(defn look-and-say [x]
  (->> (map (comp read-string str) x)
       (partition-by identity)
       (map count-and-first)
       (reduce into []))) ;; cannot be lazy (eg. concat) because it will blow the stack

;; (look-and-say "111221")

(defn iter [x n]
  (->> (iterate look-and-say x)
       (drop n)
       first
       count))

(defn part-1 []
  (iter "1223334444" 40))

;; (part-1)

(defn part-2 []
  (iter "1223334444" 50))

;; (part-2)

