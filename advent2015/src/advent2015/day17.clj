(ns advent2015.day17)

(def example
  [20, 15, 10, 5, 5])

(defn to-bit-vector [wide n]
  (let [digits (-> (reduce str "" (repeat wide 0))
                   (str (Long/toBinaryString n)))]
    (mapv {\0 0 \1 1} (subs digits (- (count digits) wide)))))

;; bruteforce approach, simple yet effective
(defn count-perms [xs goal] ;; xs - seq of bucket sizes
  (let [c (count xs)]
    (count
     (for [i (range 1 (bit-shift-left 1 c))
           :let [ys  (to-bit-vector c i)
                 sum (reduce + 0 (map * xs ys))]
           :when (= sum goal)]
       true))))

(defn read-input []
  (->> (slurp "resources/day17.txt")
       clojure.string/split-lines
       (mapv clojure.edn/read-string)))

(defn part-1 []
  (let [input (read-input)]
    (count-perms input 150)))

;; (time (part-1)) ;; ~13 secs on my laptop

(defn min-num-buckets [xs goal]
  (let [c (count xs)]
    (reduce
     min
     (for [i (range 1 (bit-shift-left 1 c))
           :let [ys  (to-bit-vector c i)
                 sum (reduce + 0 (map * xs ys))]
           :when (= sum goal)]
       (count (filter #{1} ys))))))

;; (min-num-buckets example 25)

(defn count-perms2 [xs goal num-buckets]
  (let [c (count xs)]
    (count
     (for [i (range 1 (bit-shift-left 1 c))
           :let [ys  (to-bit-vector c i)
                 sum (reduce + 0 (map * xs ys))]
           :when (and (= sum goal)
                      (= (count (filter #{1} ys))
                         num-buckets))]
       true))))

(defn part-2 []
  (let [input (read-input)
        min-b (min-num-buckets input 150)]
    (count-perms2 input 150 min-b)))

;; (part-2)

