(ns advent.day23
  (:gen-class))

(def input [["set" "b" 65]      ;; 0
            ["set" "c" "b"]
            ["jnz" "a" 2]
            ["jnz" 1 5]
            ["mul" "b" 100]
            ["sub" "b" -100000] ;; 5
            ["set" "c" "b"]
            ["sub" "c" -17000]
            ["set" "f" 1]
            ["set" "d" 2]
            ["set" "e" 2]       ;; 10
            ["set" "g" "d"]
            ["mul" "g" "e"]
            ["sub" "g" "b"]
            ["jnz" "g" 2]
            ["set" "f" 0]       ;; 15
            ["sub" "e" -1]
            ["set" "g" "e"]
            ["sub" "g" "b"]
            ["jnz" "g" -8]
            ["sub" "d" -1]      ;; 20
            ["set" "g" "d"]
            ["sub" "g" "b"]
            ["jnz" "g" -13]
            ["jnz" "f" 2]
            ["sub" "h" -1]      ;; 25
            ["set" "g" "b"]
            ["sub" "g" "c"]
            ["jnz" "g" 2]
            ["jnz" 1 3]
            ["sub" "b" -17]     ;; 30
            ["jnz" 1 -23]])

(defn step-prog [prog regs ip]
  (let [[op reg val] (get prog ip)]
    (condp = op
      "set" [(assoc regs reg (get regs val val)) (inc ip)]
      "sub" [(update-in regs [reg] - (get regs val val)) (inc ip)]
      "mul" [(update-in regs [reg] * (get regs val val)) (inc ip)]
      "jnz" [regs (+ ip (if (zero? (get regs reg reg)) 1 val))]
      [regs ip :unknown])))

(defn run-program-mul-op [prog regs]
  (loop [prog prog regs regs ip 0 mult-count 0]
    ;; (println [ip mult-count])
    (if (or (neg? ip)
            (>= ip (count prog)))
      [mult-count regs ip]
      (let [[regs' ip'] (step-prog prog regs ip)]
        (recur prog
               regs'
               ip'
               (+ mult-count
                  (if (= "mul" (first (get prog ip))) 1 0)))))))


(def orig-regs (zipmap (map str "abcdefghi") (repeat 0)))

;; (step-prog input orig-regs 0)
;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (run-program-mul-op input orig-regs)

;; part 2
;; (let [b 106500]
;;   (count
;;    (filter not-empty
;;            (for [x (range b (inc (+ b 17000)) 17)]
;;              (for [i (range 2 x)
;;                    :when (zero? (mod x i))]
;;                true)))))

