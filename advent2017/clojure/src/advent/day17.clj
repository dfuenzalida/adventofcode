(ns advent.day17
  (:gen-class))

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn iter-spinlock [state steps pos n]
  (if (pos? n)
    (let [i         (count state)
          nextpos   (inc (mod (+ pos steps) i))
          nextstate (concat
                     (take nextpos state)
                     [i]
                     (drop nextpos state))]
      (recur nextstate steps nextpos (dec n)))
    state))

;; (iter-spinlock [0] 3 0 9)
;; (def example-seq (iter-spinlock [0] 3 0 2017))

;; (take 10 (drop-while #(not= % 2017) example-seq))

;;(def part-a-seq (iter-spinlock [0] 394 0 2017))
;; (take 10 (drop-while #(not= % 2017) part-a-seq))

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn iter-spinlock2 [state steps pos i n]
  (if (pos? n)
    (let [nextpos   (mod (+ pos steps) i)
          nextstate (if (zero? nextpos) i state)]
      (recur nextstate steps (inc nextpos) (inc i) (dec n)))
    state))

;; (iter-spinlock2 [0] 394 0 1 50000000)
