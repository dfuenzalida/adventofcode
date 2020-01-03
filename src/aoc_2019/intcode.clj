(ns aoc-2019.intcode)

(defn add [ip relbase op1 op2 op3 program stdin stdout]
  (let [prog' (assoc-in program [op3] (+ op1 op2))]
    [(+ ip 4) relbase prog' stdin stdout])) ;; new-ip, new-program, stdin, stdout

(defn mult [ip relbase op1 op2 op3 program stdin stdout]
  (let [prog' (assoc-in program [op3] (* op1 op2))]
    [(+ ip 4) relbase prog' stdin stdout]))

(defn read-input [ip relbase op1 op2 op3 program stdin stdout]
  (let [prog' (assoc-in program [op1] (first stdin))] ;; reads from a direct address
    [(+ ip 2) relbase prog' (rest stdin) stdout]))

(defn write-output [ip relbase op1 op2 op3 program stdin stdout]
  [(+ ip 2) relbase program stdin (conj stdout op1)])

(defn jump-if-true [ip relbase op1 op2 op3 program stdin stdout]
  (if (not= 0 op1)
    [op2 relbase program stdin stdout]
    [(+ ip 3) relbase program stdin stdout]))

(defn jump-if-false [ip relbase op1 op2 op3 program stdin stdout]
  (if (zero? op1)
    [op2 relbase program stdin stdout]
    [(+ ip 3) relbase program stdin stdout]))

(defn less-than [ip relbase op1 op2 op3 program stdin stdout]
  (let [newval (if (< op1 op2) 1 0)
        prog'  (assoc-in program [op3] newval)]
    [(+ ip 4) relbase prog' stdin stdout]))

(defn equals [ip relbase op1 op2 op3 program stdin stdout]
  (let [newval (if (= op1 op2) 1 0)
        prog'  (assoc-in program [op3] newval)]
    [(+ ip 4) relbase prog' stdin stdout]))

(defn change-relbase [ip relbase op1 op2 op3 program stdin stdout]
  [(+ ip 2) (+ relbase op1) program stdin stdout])

(def opcode-to-fn
  {1 add, 2 mult, 3 read-input, 4 write-output, 5 jump-if-true,
   6 jump-if-false, 7 less-than, 8 equals, 9 change-relbase})

(defn get-op [program addr mode opcode ip relbase]
  (cond
    (= 2 mode) (as-> (+ ip addr) $ (get program $ 0) (+ relbase $) (get program $ 0))
    (and (zero? mode) (not= 3 opcode)) (as-> (+ ip addr) $ (get program $ 0) (get program $ 0))
    ;; defaults to mode 1 OR mode 0 + opcode 3
    :else (get program (+ ip addr) 0)))

(defn execute
  ([program stdin] (execute (zipmap (range) program) 0 stdin))
  ([program ip stdin]
   (loop [ip ip, relbase 0, program program, stdin stdin, stdout []]
     (let [instruction (get program ip 0)
           opcode (mod instruction 100)
           c      (mod (quot instruction 100) 10)  ;; mode for 1st param
           b      (mod (quot instruction 1000) 10) ;; mode for 2nd param
           func   (opcode-to-fn opcode)
           op1    (get-op program 1 c opcode ip relbase)
           op2    (get-op program 2 b opcode ip relbase)
           op3    (as-> (+ ip 3) $ (get program $ 0))]
       (if func
         (if (and (= 3 opcode) (= [] stdin))
           [ip program stdin stdout]
           (let [[ip' relbase' prog' stdin' stdout'] (func ip relbase op1 op2 op3 program stdin stdout)]
             (recur ip' relbase' prog' stdin' stdout')))
         [ip program stdin stdout])))))

(comment
  ;; Tests for problem 9

  ;; should return `prog` - OK
  (let [prog [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]]
    (->> (execute prog []) last (= prog)))

  ;; should be 16 - OK
  (let [prog [1102,34915192,34915192,7,4,7,99,0]]
    (->> (execute prog []) last last str count (= 16)))

  ;; output is middle number
  (let [prog [104,1125899906842624,99]]
    (->> (execute prog []) last last (= 1125899906842624)))

  ;; output number at address 1985
  (->> (execute {0 109, 1 50, 2 109, 3 69, 4 204, 5 -34, 85 42} 0 []) last)

  ;; not 203
  (let [prog (->> (slurp "resources/input09.txt")
                  (format "[%s]")
                  (clojure.edn/read-string))]
    (->> (execute prog [1]) last))

  ;; end comment
  )
