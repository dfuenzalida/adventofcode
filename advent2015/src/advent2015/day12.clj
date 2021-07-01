(ns advent2015.day12)

(defn read-input []
  (slurp "resources/day12.txt"))

(defn part-1 []
  (->> (read-input)
       (re-seq #"-?\d+")
       (map clojure.edn/read-string)
       (reduce + 0)))

;; (part-1)

(defn sum-of [x]
  (cond
    (number? x) x
    (vector? x) (->> (map sum-of x) (reduce + 0))
    (map? x) (if (some #{"red"} (vals x))
               0
               (->> (vals x) (map sum-of) (reduce + 0)))
    :else 0))

(defn part-2 []
  (->> (clojure.string/replace (read-input) ":" "")
       clojure.edn/read-string
       sum-of))

;; (part-2)
