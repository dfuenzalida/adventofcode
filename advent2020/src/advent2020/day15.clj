(ns advent2020.day15)

;; [0, 3, 6,,, 0, (4-1), (5-2),,, ]

;; fn that takes a starting sequence and returns a lazy-sequence that
;; - keeps a map of N -> [A B] where A and B are the 2 more recent occurrences of N
;; - keeps track of the last emitted number

(defn occurrences-map [xs] ;; [a b c ,,,] -> {a 1, b 2, c 3}
  (->> (map-indexed (fn [i x] [x (inc i)]) xs)
       (reduce merge {})))

;; (occurrences-map '(a b c))

(defn gen-sequence
  ([init-seq]
   (let [lastnum    (last init-seq)
         occurences (occurrences-map (butlast init-seq))]
     (concat (butlast init-seq)
             (gen-sequence lastnum (count init-seq) occurences))))
  ([lastnum index m]
   (cons lastnum
         (lazy-seq
          (if-let [prev-pos (get m lastnum)]
            (gen-sequence (- index prev-pos) (inc index) (assoc m lastnum index))
            (gen-sequence 0 (inc index) (assoc m lastnum index)))))))

 ;; (= [0 3 6 0 3 3 1 0 4 0] (->> (gen-sequence [0 3 6]) (take 10)))

;; Part 1 - pretty much instantaneous

;; (= 436 (nth (gen-sequence [0 3 6]) (dec 2020)))
;; (= 1 (nth (gen-sequence [1 3 2]) (dec 2020)))
;; (= 10 (nth (gen-sequence [2 1 3]) (dec 2020)))
;; (= 27 (nth (gen-sequence [1 2 3]) (dec 2020)))
;; (= 78 (nth (gen-sequence [2 3 1]) (dec 2020)))
;; (= 438 (nth (gen-sequence [3 2 1]) (dec 2020)))
;; (= 1836 (nth (gen-sequence [3 1 2]) (dec 2020)))

;; Part 2 - take ~20 seconds in my laptop

;; (time (= 175594 (nth (gen-sequence [0 3 6]) (dec 30000000))))
;; => "Elapsed time: 20590.63701 msecs"

;; (time (= 362 (nth (gen-sequence [3 1 2]) (dec 30000000))))
;; => "Elapsed time: 21527.241776 msecs"

