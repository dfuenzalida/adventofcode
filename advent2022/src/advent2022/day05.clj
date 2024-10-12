(ns advent2022.day05
  (:require [clojure.string :as string]))

(defn load-state [state stacks crates]
  (if-let [crate (first crates)]
    (condp = crate
      ;; skip spaces
      \space (recur state (rest stacks) (rest crates))
      ;; otherwise consume the crate
      (let [stack  (first stacks)
            state' (update state stack str crate)]
        (recur state' (rest stacks) (rest crates))))
    state))

(defn read-steps [steps]
  (->> (reduce str steps)
       (re-seq #"\d+")
       (partition 3)
       (map (fn [[n from to]] [(read-string n) from to]))))

(defn movements [steps]
  (->> (map (fn [[n from to]] (repeat n [from to])) steps)
       (reduce concat)))

(defn moves-reducer [state [from to]]
  (if-let [top-from (first (get state from))]
    (-> state
        (update from rest)
        (update to (partial into [top-from])))
    state))

(defn load-all [s]
  (let [lines   (string/split s #"\n")
        [drawing _ steps] (partition-by empty? lines)
        stacks  (->> drawing last (re-seq #"\S+"))
        crates  (->> (butlast drawing)
                     (map #(str % " ")) ;; make lines of size multiple of 4
                     (reduce str)
                     (re-seq #".{4}") ;; break into strings of size 4
                     (map second)) ;; second char is crate name or space
        state  (load-state (zipmap stacks (repeat "")) (cycle stacks) crates)]
    [state stacks steps]))

(defn part-1 [s]
  (let [[state stacks steps] (load-all s)
        moves  (movements (read-steps steps))
        state' (reduce moves-reducer state moves)]
    (->> (map #(get state' %) stacks)
         (map first)
         (reduce str))))

;; (= (part-1 example) "CMZ")
;; (part-1 (slurp "resources/day05.txt"))

(defn moves-reducer2 [state [n from to]]
  (if-let [top-from (take n (get state from))]
    (-> state
        (update from (partial drop n))
        (update to (partial concat top-from)))
    state))

(defn part-2 [s]
  (let [[state stacks steps] (load-all s)
        moves  (read-steps steps)
        state' (reduce moves-reducer2 state moves)]
    (->> (map #(get state' %) stacks)
         (map first)
         (reduce str))))

;; (moves-reducer2 {:a "ABC" :b "XYZ"} [2 :a :b])
;; (= (part-2 example) "MCD")
;; (part-2 (slurp "resources/day05.txt"))

(comment
  (let [lines   (string/split example #"\n")
        [drawing _ steps] (partition-by empty? lines)
        stacks  (->> drawing last (re-seq #"\S+"))
        crates  (->> (butlast drawing)
                     (map #(str % " ")) ;; make lines of size multiple of 4
                     (reduce str)
                     (re-seq #".{4}") ;; break into strings of size 4
                     (map second)) ;; second char is crate name or space

        state  (load-state (zipmap stacks (repeat "")) (cycle stacks) crates)
        moves  (movements (read-steps steps))
        ]
    #_steps
    #_(zipmap stacks (repeat ""))
    #_[stacks crates] ;; => [("1", "2", "3"), (\space ...)
    ;; state
    ;; (read-steps steps)
    state'
    )

  (->>
   (string/split example #"\n")
   (partition-by empty?))
  
  ) ;; end comment

(def example
"    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")
