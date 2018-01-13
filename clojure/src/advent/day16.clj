(ns advent.day16
  (:gen-class))

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def problem-input
  (let [text (slurp "src/input16.txt")
        text (.substring text 0 (dec (.length text)))] ;; \n at the end
    (clojure.string/split text #",")))

(defn spin [^String s ^Integer n]
  (let [ls (.length s)]
    (str (.substring s (- ls n))
         (.substring s 0 (- ls n)))))

(defn exchange [^String s ^Integer a ^Integer b]
  (let [table {b (.charAt s a) a (.charAt s b)}]
    (apply str (map #(get table % (.charAt s %)) (range (.length s))))))

(defn partner [^String s ^String a ^String b]
  (let [table {(first b) (first a) (first a) (first b)}]
    (apply str (map #(get table % %) s))))

(defn reductor [^String s ^String op]
  (let [operand (.charAt op 0)
        params  (into [] (.split (.substring op 1) "/"))]
    (condp = operand
      \s (spin s (Integer. (params 0)))
      \x (exchange s (Integer. (params 0)) (Integer. (params 1)))
      \p (partner s (params 0) (params 1))
      s)))

;;(reduce reductor "abcde" ["s1" "x3/4" "pe/b"])
;;(reduce reductor "abcdefghijklmnop" problem-input)

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def original "abcdefghijklmnop")

(defn count-steps [^String s ^Long i]
  (let [s2 (reduce reductor s problem-input)]
    (if (= original s2)
      i
      (recur s2 (inc i)))))

;; I suspected iterating would eventually return the same string
(def identity-steps
  (count-steps "abcdefghijklmnop" 1))

;; (iterator "abcdefghijklmnop" (mod 1000000000 identity-steps))
