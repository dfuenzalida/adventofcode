(ns advent.day08
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example ["b inc 5 if a > 1"
              "a inc 1 if b < 5"
              "c dec -10 if a >= 1"
              "c inc -20 if c == 10"])

(defn parse-line [s]
  (let [[reg op amt _ reg2 cmp cmp-val] (clojure.string/split s #" ")]
    [reg op (Integer. amt) reg2 cmp (Integer. cmp-val)]))

(def fns
  {"inc" + "dec" - "<" < "<=" <= "==" = "!=" not= ">=" >= ">" >})

(defn process [lines m]
  (if (seq lines)
    (let [[reg op amt reg2 cmp cmp-val] (parse-line (first lines))]
      (if ((fns cmp) (get m reg2 0) cmp-val)
        (recur (rest lines)
               (assoc m reg ((fns op) (get m reg 0) amt)))
        (recur (rest lines) m)))
    m))

(defn read-input []
  (-> (slurp "src/input8.txt")
      (clojure.string/split #"\n")))

;; (apply max (vals (process example {}))) => 1
;; (apply max (vals (process (read-input) {})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn process2 [lines m max-val]
  (if (seq lines)
    (let [[reg op amt reg2 cmp cmp-val] (parse-line (first lines))]
      (if ((fns cmp) (get m reg2 0) cmp-val)
        (let [new-val ((fns op) (get m reg 0) amt)]
          (recur (rest lines)
                 (assoc m reg new-val)
                 (max new-val max-val)))
        (recur (rest lines) m max-val)))
    [m max-val]))

;; (last (process2 example {} 0)) => 10
;; (last (process2 (read-input) {} 0))
