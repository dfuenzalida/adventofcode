(ns advent2018.day14)

(defn iter [[recipes pos1 pos2]]
  (let [r1   (nth recipes pos1)
        r2   (nth recipes pos2)
        sum  (+ r1 r2)
        d1   (int (/ sum 10))
        d2   (rem sum 10)
        rcps (into recipes (if (pos? d1) [d1 d2] [d2]))
        pos1 (rem (+ pos1 1 r1) (count rcps))
        pos2 (rem (+ pos2 1 r2) (count rcps))]
    [rcps pos1 pos2]))
  
;; (iter [[3 7] 0 1])
;; (iter [[3 7 1 0] 0 1])

;; 15 iterations
;; (first (drop 14 (iterate iter [[3 7] 0 1])))

(defn recipes-after [n]
  (let [num-recs (+ 10 n)
        all-recs (iterate iter [[3 7] 0 1])
        filterfn (fn [[r _ _]] (< (.size r) num-recs))
        recipe   (ffirst (drop-while filterfn all-recs))]
    (subvec recipe n (+ 10 n))))

;; (apply str (recipes-after 2018)) => "5941429882"

(defn ending? [ending]
  (let [req-size (.size ending)]
    (fn [[r _ _]] (or (= ending (subvec r (- (.size r) req-size)))
                      (= ending (subvec r (- (.size r) req-size 1) (dec (.size r))))))))

;; ((ending? [4 5 6]) [[1 2 3 4 5 6] nil nil]) => true
;; ((ending? [4 5 6]) [[1 2 3 4 5 6 9] nil nil]) => true

(defn part2 [ending]
  (let [req-size (.size ending)
        all-recs (iterate iter [[3 7] 0 1])
        all-recs (drop-while (fn [[r _ _]] (< (.size r) (inc req-size))) all-recs)
        filterfn (ending? ending)]
    (-> (filter filterfn all-recs)
        ffirst
        (.size)
        (- req-size))))

;; (part2 [5 1 5 8 9]) ;; => 9
;; (part2 [5 9 4 1 4]) ;; => 2018

;; (time (part2 [? ? ? ? ? ?]))
;; "Elapsed time: 387381.290379 msecs"

