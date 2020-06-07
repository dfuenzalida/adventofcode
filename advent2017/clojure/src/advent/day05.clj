(ns advent.day05
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example-offsets [0 3 0 1 -3])

(defn iter-offsets [pos offsets steps]
  (if (>= pos (count offsets))
    steps ;; we escaped!
    (let [curr    (offsets pos)
          new-pos (+ pos curr)]
      (recur new-pos
             (assoc-in offsets [pos] (inc curr))
             (inc steps)))))

;; (iter-offsets 0 example-offsets 0) => 5

(def part1-offsets
  (mapv #(Integer. %)
        (->
         (slurp "src/input5.txt")
         (clojure.string/split #"\n"))))

;; (iter-offsets 0 part1-offsets 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn iter-offsets2 [pos offsets steps]
  (if (>= pos (count offsets))
    steps ;; we escaped!
    (let [curr    (offsets pos)
          new-pos (+ pos curr)
          change  (if (>= curr 3) dec inc)]
      (recur new-pos
             (assoc-in offsets [pos] (change curr))
             (inc steps)))))

;; (iter-offsets2 0 example-offsets 0) => 10

;; (iter-offsets2 0 part1-offsets 0)
