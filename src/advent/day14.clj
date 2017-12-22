(ns advent.day14
  (require [advent.day10 :as day10])
  (:gen-class))

(def hex-to-bin
  {\0 "0000" \1 "0001" \2 "0010" \3 "0011" \4 "0100" \5 "0101" \6 "0110"
   \7 "0111" \8 "1000" \9 "1001" \a "1010" \b "1011" \c "1100" \d "1101"
   \e "1110" \f "1111"})

(defn hex-to-binary [s]
  (apply str (map hex-to-bin s)))

(defn count-used-squares [input]
  (->> (map #(str input "-" %) (range 128))
       (map day10/knot-hash)
       (map hex-to-binary)
       (apply str)
       (filter #{\1})
       count))

;; (count-used-squares "flqrgnkx")
