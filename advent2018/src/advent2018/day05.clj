(ns advent2018.day05
  (:require [clojure.string :as s]))

(defn react? [a b]
  (and (not= a b)
       (= (s/upper-case (str a b)) (s/upper-case (str b a)))))

(def react2? (memoize react?))

(defn reduce-polimer2 [^String input]
  (loop [s input, i 0]
    (if (>= i (dec (.length s)))
      s
      (let [a (.substring s i (inc i))
            b (.substring s (inc i) (+ i 2))]
        (if (react2? a b)
          (recur (str (.substring s 0 i) (.substring s (+ i 2))) (max 0 (+ -2 i)))
          (recur s (inc i)))))))

;; (reduce-polimer2 "aB")
;; (reduce-polimer2 "abBA")
;; (= "dabCBAcaDA" (reduce-polimer2 "dabAcCaCBAcCcaDA"))
;; (= 10 (count (reduce-polimer2 "dabAcCaCBAcCcaDA")))
;;
;;  (time (count (reduce-polimer (s/trim (slurp "resources/day05.txt")))))
;; "Elapsed time: 276899.992513 msecs" ;; AFTER POSITION OPTIMIZATION
;; 10972

;; (time (count (reduce-polimer2 (s/trim (slurp "resources/day05.txt")))))
;; "Elapsed time: 761.113723 msecs" ;; POSITION OPT + ^String type hint
;; 10972

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn smallest-reduction [^String input]
  (let [input   (s/trim input)
        letters (->> input s/upper-case (map str) set)
        lengths (for [letter letters]
                  {letter
                   (count
                    (reduce-polimer2 (-> input
                                         (.replaceAll letter "")
                                         (.replaceAll (s/lower-case letter) ""))))})
        len-map (reduce merge {} lengths)]
    (->> len-map (map second) (apply min))))

;; (smallest-reduction "dabAcCaCBAcCcaDA") => 4
;; (time (smallest-reduction (s/trim (slurp "resources/day05.txt"))))
