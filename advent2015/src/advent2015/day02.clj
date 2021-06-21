(ns advent2015.day02)

(defn read-input []
  (->> (slurp "resources/day02.txt")
       (re-seq #"\d+")
       (map clojure.edn/read-string)
       (partition 3)))

(defn areas [a b c]
  [(* a b) (* a c) (* b c)])

(defn areas-and-smallest [[l w h]]
  (let [[a b c]  (areas l w h)
        smallest (reduce min [a b c])]
    (+ a a b b c c smallest)))

(defn part-1 []
  (->> (read-input)
       (map areas-and-smallest)
       (reduce +)))

;; (part-1)

(defn ribbon [[l w h]]
  (let [[a b] (->> (sort [l w h]) (take 2))]
    (+ a a b b (* l w h))))

;; (= 34 (ribbon [2 3 4]))
;; (= 14 (ribbon [1 1 10]))

(defn part-2 []
  (->> (read-input)
       (map ribbon)
       (reduce +)))

;; (part-2)
