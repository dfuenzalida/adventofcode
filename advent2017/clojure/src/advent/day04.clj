(ns advent.day04
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def system-passphrases
  (-> "src/day4.txt"
      slurp
      (clojure.string/split #"\n")))

(defn valid? [s]
  (->> (clojure.string/split s #" ")
       frequencies
       vals
       (into #{})
       (= #{1})))

;; (count (filter valid? system-passphrases))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn valid2? [s]
  (->> (clojure.string/split s #" ")
       (map frequencies)
       frequencies
       vals
       (into #{})
       (= #{1})))

;; (valid2? "abcde fghij") => true
;; (valid2? "abcde xyz ecdab") => false
;; (valid2? "iiii oiii ooii oooi oooo") => true
;; (valid2? "oiii ioii iioi iiio") => false

;; (count (filter valid2? system-passphrases))
