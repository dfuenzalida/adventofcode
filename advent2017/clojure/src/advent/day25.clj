(ns advent.day25
  (:gen-class))

;; tape:   just the positions of the 1s
;; states: {:state {val-at-curr-slot [val-to-write movement :next-state]}}

(def ex-states {:a {0 [1  1 :b]  ;; write 1, move right, next state is :b
                    1 [0 -1 :b]} ;; write 0, move left, next state is :b
                :b {0 [1 -1 :a]
                    1 [1  1 :a]}})

(defn write [tape slot val]
  (if (zero? val)
    (disj tape slot)
    (conj tape slot)))

(defn read [tape slot] ;; tape contains only 1s
  (if (tape slot) 1 0))

(defn iter [steps tape state slot states]
  (if (pos? steps)
    (let [curr-val (read tape slot)
          [new-val movement new-state] (get-in states [state curr-val])]
      (recur (dec steps)
             (write tape slot new-val)
             new-state
             (+ slot movement)
             states))
    [tape state slot]))

;; example
;; (count (first (iter 6 #{} :a 0 ex-states)))

(def input-states {:a {0 [1  1 :b]
                       1 [0 -1 :c]}
                   :b {0 [1 -1 :a]
                       1 [1 -1 :d]}
                   :c {0 [1  1 :d]
                       1 [0  1 :c]}
                   :d {0 [0 -1 :b]
                       1 [0  1 :e]}
                   :e {0 [1  1 :c]
                       1 [1 -1 :f]}
                   :f {0 [1 -1 :e]
                       1 [1  1 :a]}})

;; part a
;; (count (first (iter 12172063 #{} :a 0 input-states)))
