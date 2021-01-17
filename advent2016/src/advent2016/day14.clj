(ns advent2016.day14)

(set! *warn-on-reflection* true)

(defonce ^java.security.MessageDigest$Delegate digester
  (java.security.MessageDigest/getInstance "MD5"))

(defn md5 [^String s]
  (->> (.digest digester (.getBytes s))
       (map (partial format "%02x"))
       (reduce str)))

;; (md5 "abc18")
;; (->> (re-seq #"(.)\1\1" "abcccdefgggh") #_(map second)) ;;  => ("c" "g")

(defn hash-for [salt n]
  (md5 (str salt n)))

(def hash-for-m
  (memoize hash-for))

(defn valid-key? [hash-fn ^String salt ^Long n]
  (let [first-hash (hash-fn salt n)]
    (when-let [c (->> (re-seq #"(.)\1\1" first-hash) first second)] ;; FIXME assumes only one match
      (let [charseq (str c c c c c)]
        (->> (map (partial hash-fn salt) (range (+ 1 n) (+ 1001 n)))
             (some (fn [^String snd-hash] (.contains snd-hash charseq))))))))

(defn part1 [salt]
  (->> (range)
       (filter (partial valid-key? hash-for-m salt))
       (take 64)
       last))

;; (time (part1 "abc")) ;; about 2-3 seconds

(defn stretch-for [salt n]
  (->> (str salt n) (iterate md5) (drop (inc 2016)) first))

(def stretch-for-m
  (memoize stretch-for))

(defn part2 [salt]
  (->> (range)
       (filter (partial valid-key? stretch-for-m salt))
       (take 64)
       last))

;; (valid-key2? "abc" 10)
;; (time (part2 "abc"))
;; "Elapsed time: 1473957.949449 msecs" ;; TODO improve performance

