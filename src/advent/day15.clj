(ns advent.day15
  (:gen-class))

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn mk-gen [factor modulo]
  (fn [n] (mod (* factor n) modulo)))

(defn iterator [gen-a seed-a gen-b seed-b]
  (loop [a       seed-a
         b       seed-b
         matches 0
         n       40000000]
    (if (pos? n)
      (let [val-a (gen-a a)
            val-b (gen-b b)]
        (if (= (bit-and 0xffff val-a) (bit-and 0xffff val-b))
          (recur val-a val-b (inc matches) (dec n))
          (recur val-a val-b matches (dec n))))
      matches)))

;; (iterator (mk-gen 16807 2147483647) 65
;;           (mk-gen 48271 2147483647) 8921)

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ex-seq-a
  (->> (iterate (mk-gen 16807 2147483647) 65)
       (drop 1)
       (filter #(zero? (mod % 4)))))

(def ex-seq-b
  (->> (iterate (mk-gen 48271 2147483647) 8921)
       (drop 1)
       (filter #(zero? (mod % 8)))))

(defn count-matches [seq-a seq-b matches n]
  (if (pos? n)
    (let [a (first seq-a) b (first seq-b)]
      (if (= (bit-and 0xffff a) (bit-and 0xffff b))
        (recur (rest seq-a) (rest seq-b) (inc matches) (dec n))
        (recur (rest seq-a) (rest seq-b) matches (dec n))))
    matches))

;; (count-matches ex-seq-a ex-seq-b 0 (* 5 1000 1000))

(def seq-a2
  (->> (iterate (mk-gen 16807 2147483647) 783)
       (drop 1)
       (filter #(zero? (mod % 4)))))

(def seq-b2
  (->> (iterate (mk-gen 48271 2147483647) 325)
       (drop 1)
       (filter #(zero? (mod % 8)))))

;; *very slow*, took ~2 minutes on the REPL in my laptop
;; (count-matches seq-a2 seq-b2 0 (* 5 1000 1000))
