(ns advent2025.shared)

(defn fixed-point
  "Returns the value of applying f repeatedly to x until the result is constant"
  [f x]
  (reduce
   (fn [a b] (if (= a b) (reduced a) b))
   (iterate f x)))
