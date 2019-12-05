(ns aoc-2019.day04)

(defn valid? [n]
  (and (<= 100000 n 999999)
       (loop [digit (mod n 10), n (quot n 10), repeated 0]
         (if (zero? n)
           (pos? repeated)
           (let [digit' (mod n 10)]
             (cond
               (< digit' digit) (recur digit' (quot n 10) repeated)
               (= digit' digit) (recur digit' (quot n 10) (inc repeated))
               :else false))))))

(defn solve-1 []
  (count (filter valid? (range 193651 (inc 649729)))))

(defn valid2? [n]
  (and (<= 100000 n 999999)
       (loop [digit (mod n 10), n (quot n 10), repeated {}]
         (if (zero? n)
           (->> repeated vals (some #{1}) some?) ;; one repetition
           (let [digit' (mod n 10)]
             (cond
               (< digit' digit) (recur digit' (quot n 10) repeated)
               (= digit' digit) (recur digit' (quot n 10)
                                       (merge-with + repeated {digit 1}))
               :else false))))))

(defn solve-2 []
  (count (filter valid2? (range 193651 (inc 649729)))))

