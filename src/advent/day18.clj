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

(defn parse-line [s]
  (let [[c r v] (into [] (.split s " "))]
    [c r (when v (try (Long. v) (catch Exception e v)))]))

(def example-program
  (mapv parse-line example))

(defn run-program [prog i state freq]
  ;;(println i "/" (get prog i) "- " state freq)
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

