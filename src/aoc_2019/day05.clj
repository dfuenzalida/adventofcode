(ns aoc-2019.day05)

(defn read-program []
  (->> (slurp "resources/input05.txt")
       (format "[%s]")
       (clojure.edn/read-string)))

(defn add [ip op1 op2 op3 program stdin stdout]
  (let [prog' (assoc-in program [op3] (+ op1 op2))]
    [(+ ip 4) prog' stdin stdout])) ;; new-ip, new-program, stdin, stdout

(defn mult [ip op1 op2 op3 program stdin stdout]
  (let [prog' (assoc-in program [op3] (* op1 op2))]
    [(+ ip 4) prog' stdin stdout]))

(defn read-input [ip op1 op2 op3 program stdin stdout]
  (let [prog' (assoc-in program [op1] (first stdin))] ;; reads from a direct address
    [(+ ip 2) prog' (rest stdin) stdout]))

(defn write-output [ip op1 op2 op3 program stdin stdout]
  [(+ ip 2) program stdin (conj stdout op1)])

(defn jump-if-true [ip op1 op2 op3 program stdin stdout]
  (if (not= 0 op1)
    [op2 program stdin stdout]
    [(+ ip 3) program stdin stdout]))

(defn jump-if-false [ip op1 op2 op3 program stdin stdout]
  (if (zero? op1)
    [op2 program stdin stdout]
    [(+ ip 3) program stdin stdout]))

(defn less-than [ip op1 op2 op3 program stdin stdout]
  (let [newval (if (< op1 op2) 1 0)
        prog'  (assoc-in program [op3] newval)]
    [(+ ip 4) prog' stdin stdout]))

(defn equals [ip op1 op2 op3 program stdin stdout]
  (let [newval (if (= op1 op2) 1 0)
        prog'  (assoc-in program [op3] newval)]
    [(+ ip 4) prog' stdin stdout]))

(def opcode-to-fn
  {1 add, 2 mult, 3 read-input, 4 write-output,
   5 jump-if-true, 6 jump-if-false, 7 less-than, 8 equals})

(defn execute
  ([program stdin] (execute program 0 stdin))
  ([program ip stdin]
   (loop [ip ip, program program, stdin stdin, stdout []]
     (let [instruction (get program ip)
           opcode (mod instruction 100)
           c      (mod (quot instruction 100) 10)  ;; mode for 1st param
           b      (mod (quot instruction 1000) 10) ;; mode for 2nd param

           func  (opcode-to-fn opcode)
           op1   (if (and (zero? c) (#{1 2 4 5 6 7 8} opcode))
                   (->> (+ ip 1) (get program) (get program))
                   (->> (+ ip 1) (get program)))
           op2   (if (zero? b)
                   (->> (+ ip 2) (get program) (get program))
                   (->> (+ ip 2) (get program)))
           op3   (->> (+ ip 3) (get program))]
       (if func
         (if (and (= 3 opcode) (= [] stdin))
           [ip program stdin stdout]
           (let [[ip' prog' stdin' stdout'] (func ip op1 op2 op3 program stdin stdout)]
             (recur ip' prog' stdin' stdout')))
         [ip program stdin stdout])))))

(defn solve-1 []
  (->> (execute (read-program) [1]) last last))

(defn solve-2 []
  (->> (execute (read-program) [5]) last last))

;; (solve-1)
;; (solve-2)

