(ns advent.day11
  (:gen-class))

(def problem-input
  (let [text (slurp "src/input11.txt")
        text (.substring text 0 (dec (.length text)))] ;; \n at the end
    (clojure.string/split text #",")))

;; (frequencies problem-input)

(defn distance [m]
  (let [n (get m "n" 0) ne (get m "ne" 0) nw (get m "nw" 0)
        s (get m "s" 0) se (get m "se" 0) sw (get m "sw" 0)
        vects [(Math/abs (- n s))
               (Math/abs (- ne sw))
               (Math/abs (- nw se))]]
    (reduce + (drop 1 (sort vects))))) ;; sum the 2 largest numbers

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (-> problem-input frequencies distance)

(def zero
  {"n" 0 "s" 0 "ne" 0 "se" 0 "nw" 0 "sw" 0})

(def moves
  (reductions (fn [m k] (update-in m [k] inc)) zero problem-input))

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (reduce max (map distance moves))
