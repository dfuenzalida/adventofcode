(ns advent2016.day05)

(defonce digester
  (java.security.MessageDigest/getInstance "MD5"))

(defn md5 [s]
  (->> (.digest digester (.getBytes s))
       (map (partial format "%02x"))
       (apply str)))

(defn five-zeroes? [s]
  (.startsWith s "00000"))

;; (five-zeroes? (md5 "abc3231929"))
;; (five-zeroes? (md5 "abc5017308"))

(defn part-1 [input]
  (loop [pass "", i 0]
    (let [curr (md5 (str input i))]
      (if (five-zeroes? curr)
        (let [pass (str pass (subs curr 5 6))]
          (if (= 8 (count pass))
            pass
            (recur pass (inc i))))
        (recur pass (inc i))))))

;; (part-1  "abc") ;; takes ~5 minutes

(defn update-string [s i c] ;; replace in `s` the char at `i` with `c`
  (str (subs s 0 i) c (subs s (inc i))))

(defn part-2 [input]
  (let [valid-digits (set "01234567")]
    (loop [pass "________", i 3231920]
      (let [curr (md5 (str input i))]
        (if (five-zeroes? curr)
          (let [position (->> (subs curr 5 6) first valid-digits)
                index    (when position (read-string (str position)))
                newdigit (subs curr 6 7)]
            (if (and position (= "_" (subs pass index (inc index))))
              (let [pass  (update-string pass index newdigit)]
                (if (neg? (.indexOf pass "_"))
                  pass
                  (do
                    (println i "\t" pass)
                    (recur pass (inc i)))))
              (recur pass (inc i))))
          (recur pass (inc i)))))))

;; (part-2 "abc")

