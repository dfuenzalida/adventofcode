(ns advent2018.day01)

(def example1
  [+1, -2, +3, +1])

;; (reduce + 0 example1)

(defn read-input []
  (let [input-file (slurp "resources/day01.txt")]
    (read-string (format "[%s]" input-file))))

;; (reduce + 0 (read-input))

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn conj-or-find [xset xs]
  (when (seq xs)
    (let [x (first xs)]
      (if (xset x)
        x
        (recur (conj xset x) (rest xs))))))

;; (conj-or-find #{} [1 2 3 4 5 2 3 4]) => 2

;; (conj-or-find #{} (reductions + (cycle example1))) => 2
;; (conj-or-find #{} (reductions + (cycle [+3, +3, +4, -2, -4]))) => 10
;; (conj-or-find #{} (reductions + (cycle [-6, +3, +8, +5, -6]))) => 5
;; (conj-or-find #{} (reductions + (cycle [+7, +7, -2, -7, -4]))) => 14

;; (conj-or-find #{} (reductions + (cycle (read-input))))

