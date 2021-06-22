(ns advent2015.day04)

(defonce digester
  (java.security.MessageDigest/getInstance "MD5"))

(defn md5 [s]
  (->> (.digest digester (.getBytes s))
       (map (partial format "%02x"))
       (apply str)))

(defn index-and-hashes [i s]
  [i (md5 (str s i))])

(defn coin-for [s]
  (->> (map-indexed index-and-hashes (repeat s))
       (filter #(.startsWith (second %) "00000"))
       ffirst))

;; (coin-for "abcdef")

(defn coin2-for [s]
  (->> (map-indexed index-and-hashes (repeat s))
       (filter #(.startsWith (second %) "000000"))
       ffirst))

;; (coin2-for "abcdef")
