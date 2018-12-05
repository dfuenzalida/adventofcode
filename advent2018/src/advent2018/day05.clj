(ns advent2018.day05
  (:require [clojure.string :as s]))

(defn react? [a b]
  (and (not= a b)
       (= (s/upper-case (str a b)) (s/upper-case (str b a)))))

(def react2? (memoize react?))

;; (react? \A \a)

(defn reduce-polimer [input]
  (loop [s (vec input), i 0]
    (if (>= i (dec (count s)))
      (apply str s)
      (let [[a b] (subvec s i (+ i 2))]
        (if (react2? a b)
          (recur (into (subvec s 0 i) (subvec s (+ i 2))) (max 0 (+ -2 i)))
          (recur s (inc i)))))))

(defn reduce-polimer2 [input]
  (loop [s input, i 0]
    (if (>= i (dec (.length s)))
      s
      (let [a (.substring s i (inc i))
            b (.substring s (inc i) (+ i 2))]
        (if (react2? a b)
          (recur (str (.substring s 0 i) (.substring s (+ i 2))) 0)
          (recur s (inc i)))))))

;; (reduce-polimer "aB")
;; (reduce-polimer "abBA")
;; (= "dabCBAcaDA" (reduce-polimer "dabAcCaCBAcCcaDA"))
;; (= 10 (count (reduce-polimer "dabAcCaCBAcCcaDA")))
;;
;;  (time (count (reduce-polimer (s/trim (slurp "resources/day05.txt")))))
;; "Elapsed time: 276899.992513 msecs" ;; AFTER POSITION OPTIMIZATION
;; 10972
