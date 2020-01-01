(ns aoc-2019.day07
  (:require [aoc-2019.intcode :refer [execute]]))

(defn read-program []
  (->> (slurp "resources/input07.txt")
       (format "[%s]")
       (clojure.edn/read-string)))

(defn gen-phases [phases]
  (let [values (set phases)]
    (for [a values
          b (disj values a)
          c (disj values a b)
          d (disj values a b c)
          e (disj values a b c d)]
      [a b c d e])))

(defn exec-phases [program phases]
  (loop [input 0, phases phases]
    (if (seq phases)
      (let [phase  (first phases)
            output (->> (execute program [phase input]) last last)]
        (recur output (rest phases)))
      input)))

(defn solve-1 []
  (let [prog (read-program)
        phases (range 5)
        best (apply max-key (partial exec-phases prog) (gen-phases phases))]
    (exec-phases prog best)))

;; (solve-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For each set of phase values, loop through the amps, expecting each phase
;; to take input and emit input. If the last amp does not read from stdin,
;; return the largest emitted value.
;;
;; The return value of `execute` is the instruction pointer, program memory,
;; stdin and stout. The value of stdout is used to build the stdin of the next
;; phase.

(defn loop-amps [program phases]
  (loop [i      0
         amps   (vec (repeat 5 [0 program]))
         input  [0]
         phases phases
         signal 0]
    (let [[ip prog] (nth amps i)
          stdin (if (seq phases) (into [(first phases)] input) input)
          [ip' prog' in' out'] (execute prog ip stdin)]
      (if (and (= 4 i) (= stdin in'))
        signal ;; last amp and didn't consume input? we return the last best signal
        (recur (mod (inc i) 5)
               (assoc-in amps [i] [ip' prog' in'])
               out'
               (rest phases)
               (if (and (= 4 i) (seq out')) ;; sent output on last amp? compute max
                 (let [sig' (first out')] (max signal sig'))
                 signal))))))

(defn solve-2 []
  (let [program (read-program)]
    (->> (gen-phases (range 5 10))
         (map (partial loop-amps program))
         (reduce max))))

;; (solve-2)
