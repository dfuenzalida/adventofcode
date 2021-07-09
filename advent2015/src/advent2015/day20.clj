(ns advent2015.day20)

(defn divisors [^Long n]
  (loop [i 1, divs #{1}]
    (if (< n (* i i))
      divs
      (if (zero? (mod n i))
        (recur (inc i) (into divs [i (/ n i)]))
        (recur (inc i) divs)))))

(defn sum-divs [^Long n]
  [n (reduce + 0 (divisors n))])

(defn part-1 []
  (->> (map sum-divs (rest (range)))
       (drop-while #(> 3213210 (second %)))
       ffirst))

;; takes ~1 minute for a number around 3M
;; (time (part-1))

(defn sum-divs2 [^Long n]
  ;; "... each Elf will stop after delivering presents to 50 houses"
  ;; => if N is divided by D, don't count it if D * 50 > N
  (let [valid-divs (filter #(< n (* 50 %)) (divisors n))]
    [n (* 11 (reduce + 0 valid-divs))]))

(defn part-2 []
  (->> (map sum-divs2 (rest (range)))
       (drop-while #(> 32132100 (second %)))
       ffirst))

;; takes about ~95 seconds:
;; (time (part-2))

