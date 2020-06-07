(ns advent.day09
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn clean [s]
  (-> s
      (clojure.string/replace #"!." "")
      (clojure.string/replace #"<.*?>" "")))

(defn score [s nesting total]
  (if (seq s)
    (condp = (first s)
      \{ (recur (rest s) (inc nesting) (+ total nesting))
      \} (recur (rest s) (dec nesting) total)
      (recur (rest s) nesting total))
    total))

;; (score (clean "{{<a!>},{<a!>},{<a!>},{<ab>}}") 1 0) => 3

(defn read-input []
  (slurp "src/input9.txt"))

;; (score (clean (read-input)) 1 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn cancel [s]
  (clojure.string/replace s #"!." ""))

(def garbage-size
  (let [garbage-seq (->> (read-input) cancel (re-seq #"<.*?>"))]
    (-
     (reduce +
             (map count garbage-seq))
     (* 2 (count garbage-seq)))))

