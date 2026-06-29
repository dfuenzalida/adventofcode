(ns advent2025.day06
  (:require [clojure.string :as s]
            [clojure.edn :as edn]))

(declare example)

(defn read-input [s]
  (let [items (->> (s/split-lines s)
                   (mapv #(edn/read-string (str "[" % "]"))))
        nums  (butlast items)
        ops   (last items)]
    [ops nums]))

;; (read-input example)

(defn part-1 [input]
    (let [[ops nums] input
          ops        (map {'+ + '* *} ops)] ;; replace symbol with fn
      (->> (map
            (fn [n] (reduce (nth ops n) (map #(nth % n) nums)))
            (range (count ops)))
           (reduce +))))

;; (= 4277556 (->> example read-input part-1))
;; (->> "resources/day06.txt" slurp read-input part-1)

(defn read-input2 [s]
  (let [input (s/split-lines s)
        width (count (first input))]
    (->> (map
          (fn [n] (reduce str (map #(nth % n) input)))
          (range (dec width) -1 -1)) ;; (seq of width-1 down to 0)
         (reduce str)
         (re-seq #"(\d)+|\+|\*")
         (mapv (comp edn/read-string first)) ;; parse each matching string
         (partition-by symbol?)
         (partition 2)
         (map (fn [[nums [op]]] [op nums])))))

(defn part-2 [input] ;; [[op nums] ... ]
  (->>  (map (fn [[op nums]]
               (let [op ({'+ + '* *} op)]
                 (reduce op nums)))
             input)
        (reduce +)))

;; (read-input2 example)
;; (= 3263827 (->> example read-input2 part-2))
;; (->> "resources/day06.txt" slurp read-input2 part-2)

(comment
  (range 5 -1 -1)

  (mapv (comp edn/read-string first) (re-seq #"(\d)+|\+|\*" "456+123*567+"))
  (partition-by symbol? [12 34 56 '+ 45 68 '*])

  (let [input (s/split-lines example)
        width (count (first input))]
    (->> (map
          (fn [n] (reduce str (map #(nth % n) input)))
          (range (dec width) -1 -1))
         (reduce str)
    ))

  )

(comment
  (->> example
       s/split-lines
       (mapv #(edn/read-string (str "[" % "]"))))

  (let [[ops nums] (read-input example)
        ops        (map {'+ + '* *} ops)] ;; replace symbol with fn
    (map
     (fn [n] (reduce (nth ops n) (map #(nth % n) nums)))
     (range (count ops))))
  
  )


(def example
  "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  ")
