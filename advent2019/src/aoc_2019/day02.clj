(ns aoc-2019.day02)

(defn read-input []
  (->> (slurp "resources/input02.txt")
       (format "[%s]")
       (clojure.edn/read-string)))

(def opcode-to-fn {1 +, 2 *})

(defn execute [input]
  (loop [ip 0, data input]
    (if-let [func (opcode-to-fn (get data ip))]
      (let [a (->> ip (+ 1) (get data) (get data))
            b (->> ip (+ 2) (get data) (get data))
            c (->> ip (+ 3) (get data))]
        (recur (+ 4 ip) (assoc-in data [c] (func a b))))
      data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn solve-1 []
  (-> (read-input)
      (assoc-in [1] 12)
      (assoc-in [2] 2)
      execute
      first))

;; Execute the program with all 100x100 options of params, find the right combo

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn solve-2 []
  (let [input (read-input)]
    (first
     (for [a (range 100)
           b (range 100)
           :when (= 19690720
                    (-> input (assoc-in [1] a) (assoc-in [2] b) execute first))]
       (+ (* 100 a) b)))))

