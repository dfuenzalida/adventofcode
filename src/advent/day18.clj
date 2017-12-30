(ns advent.day18
  (:gen-class))

(def example ["set a 1"
              "add a 2"
              "mul a a"
              "mod a 5"
              "snd a"
              "set a 0"
              "rcv a"
              "jgz a -1"
              "set a 1"
              "jgz a -2"])

(defn read-input []
  (-> (slurp "src/input18.txt")
      (clojure.string/split #"\n")))

(defn long-or-str [s]
  (try (Long. s) (catch Exception e s)))

(defn parse-line [s]
  (let [[c r v] (into [] (.split s " "))]
    [c (long-or-str r) (when v (long-or-str v))]))

(def example-program
  (mapv parse-line example))

(defn run-program [prog i state freq]
  (if (or (neg? i) (>= i (count prog) i))
    [freq state]
    (let [[cmd reg val] (get prog i)]
      (condp = cmd
        "snd" (let [f (get state reg 0)
                    f (if (zero? f) freq f)]
                (recur prog (inc i) state f))
        "set" (recur prog (inc i)
                     (assoc state reg (get state val val)) freq)
        "add" (recur prog (inc i)
                     (assoc state reg (+ (get state reg 0) (get state val val))) freq)
        "mul" (recur prog (inc i)
                     (assoc state reg (* (get state reg 0) (get state val val))) freq)
        "mod" (recur prog (inc i)
                     (assoc state reg (mod (get state reg) (get state val val))) freq)
        "jgz" (let [regv (get state reg 0)]
                (recur prog (+ i (if (pos? regv) (get state val val) 1)) state freq))
        "rcv" (let [f (get state reg 0)]
                (if (zero? f)
                  (recur prog (inc i) state freq)
                  [freq state]))
        (recur prog (inc i) state freq)))))

;; (run-program example-program 0 {} 0)

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (run-program (mapv parse-line (read-input)) 0 {} 0)

(def input-program
  (mapv parse-line (read-input)))

(defn next-state [state reg val f]
  (assoc state reg (f (get state reg 0)
                      (get state val val))))
  
(defn step-program [prog ip state queue] ;; returns [ip state sent queue running?]
  (if (or (neg? ip) (>= ip (count prog)))
    [ip state nil queue false]
    (let [[op reg val] (get prog ip)]
      (condp = op
        "set" [(inc ip) (next-state state reg val (comp second vector)) nil queue true]
        "add" [(inc ip) (next-state state reg val +) nil queue true]
        "mul" [(inc ip) (next-state state reg val *) nil queue true]
        "mod" [(inc ip) (next-state state reg val mod) nil queue true]
        "jgz" (let [regv (get state reg reg)]
                [(+ ip (if (pos? regv) (get state val val) 1)) state nil queue true])

        "snd" [(inc ip) state (get state reg reg) queue true]

        "rcv" (if (empty? queue)
                [ip state nil [] false]
                [(inc ip)
                 (assoc state reg (first queue))
                 nil
                 (into [] (rest queue)) true])))))

(defn run-programs [prog s0 i0 q0 s1 i1 q1 nsends0 nsends1]
  (let [[i0' st0' sent0 q0' run0] (step-program prog i0 s0 q0)
        [i1' st1' sent1 q1' run1] (step-program prog i1 s1 q1)]
    (if (= [false false] [run0 run1])
      [nsends0 nsends1 [i0' st0' q0'] [i1' st1' q1']]
      (recur
       prog
       st0' i0' (if sent1 (conj q0' sent1) q0')
       st1' i1' (if sent0 (conj q1' sent0) q1')
       (if sent0 (inc nsends0) nsends0)
       (if sent1 (inc nsends1) nsends1)))))
                   
;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (run-programs input-program {"p" 0 :p 0} 0 [] {"p" 1 :p 1} 0 [] 0 0)

