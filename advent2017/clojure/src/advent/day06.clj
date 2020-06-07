(ns advent.day06
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example-banks [0 2 7 0])

(defn distribute
  [xs idx cnt]
  (let [idx (mod idx (count xs))]
    (if (pos? cnt)
      (recur
       (assoc-in xs [idx] (inc (xs idx)))
       (inc idx)
       (dec cnt))
      xs)))

(defn reallocate
  [xs prevs i] ;; banks as a vector, previous vectors, iterations
  (if (prevs xs)
    [(count prevs) i]
    (let [max-val (apply max xs)
          max-idx (first (first (filter
                                 (comp #{max-val} second)
                                 (map-indexed vector xs))))
          new-xs  (distribute (assoc-in xs [max-idx] 0)
                              (inc max-idx)
                              max-val)]
      (recur new-xs (into prevs [xs]) (inc i)))))

;; (reallocate example-banks #{} 0) => 5

(def input1 [4 10 4 1 8 4 9 14 5 1 14 15 0 15 3 5])

;; (reallocate input1 #{} 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn reallocate2
  [xs prevs i] ;; banks as a vector, previous vectors map, iterations
  (if (prevs xs)
    (- i (prevs xs))
    (let [max-val (apply max xs)
          max-idx (first (first (filter
                                 (comp #{max-val} second)
                                 (map-indexed vector xs))))
          new-xs  (distribute (assoc-in xs [max-idx] 0)
                              (inc max-idx)
                              max-val)]
      (recur new-xs (into prevs {xs i}) (inc i)))))

;; (reallocate2 example-banks {} 0) => 4
;; (reallocate2 input1 {} 0)
