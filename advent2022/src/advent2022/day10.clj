(ns advent2022.day10
  (:require [clojure.string :as string]))

(defn read-input [s]
  (->> (string/split-lines s)
       (map #(read-string (str "[:" % "]")))))

(defn execute [state]
  (let [{:keys [clock x wait inst insts]} state
        [op arg] inst]
    (merge state
           {:clock (inc clock)}
           (cond
             (pos? wait) {:wait (dec wait)}
             :else       (let [[op' arg'] (first insts)]
                           {:x     (+ x (if (= :addx op) arg 0))
                            :wait  (if (= :addx op') 1 0)
                            :inst  [op' arg']
                            :insts (rest insts)})))))

(defn part-1 [s]
  (let [insts (read-input s)
        state {:clock 0 :x 1 :wait 0 :inst nil :insts insts}
        iters (iterate execute state)
        cycles [20 60 100 140 180 220]
        strengths (map #(* % (:x (nth iters %))) cycles)]
    (reduce + strengths)))

;; (= (part-1 example2) 13140)
;; (part-1 (slurp "resources/day10.txt"))

(defn draw [clock x]
  (if (<= (dec x) (mod (dec clock) 40) (inc x)) "#" "."))

(defn part-2 [s]
  (let [insts (read-input s)
        state {:clock 0 :x 1 :wait 0 :inst nil :insts insts}
        iters (->> (iterate execute state) rest (take 240))]
    (->> (map (fn [{:keys [clock x]}] (draw clock x)) iters)
         (partition 40)
         (mapv (partial apply str)))))

;; (do (println) (clojure.pprint/pprint (part-2 example2)))
;; (do (println) (clojure.pprint/pprint (part-2 (slurp "resources/day10.txt"))))

(comment

  (read-input example)
  
  (->>
   (iterate execute {:clock 0 :x 1 :wait 0 :insts [[:noop] [[:noop] [:addx 41] [:noop]]]})
   (take 7)
   rest
   #_clojure.pprint/pprint)

  )

(def example
  "noop
addx 3
addx -5")

(def example2
  "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")

