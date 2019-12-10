(ns aoc-2019.day07
  (:require [aoc-2019.day05 :refer [execute]]))

(defn read-program []
  (->> (slurp "resources/input07.txt")
       (format "[%s]")
       (clojure.edn/read-string)))

(defn generate-phases []
  (let [values (set (range 5))]
    (for [a values
          b (disj values a)
          c (disj values a b)
          d (disj values a b c)
          e (disj values a b c d)]
      [a b c d e])))

(defn execute-phases [program phases]
  (loop [input 0, phases phases]
    (if (seq phases)
      (let [phase  (first phases)
            output (->> (execute program [phase input]) last last)]
        (recur output (rest phases)))
      input)))

(defn solve-1 []
  (let [prog (read-program)
        best (apply max-key (partial execute-phases prog) (generate-phases))]
    (execute-phases prog best)))

;; (solve-1)

(comment

  )

;; (count (set (generate-phases)))

;; Input is [phase-setting, input-from-previous-phase]
