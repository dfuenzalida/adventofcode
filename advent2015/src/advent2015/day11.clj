(ns advent2015.day11)

(set! *warn-on-reflection* true)

(defn has-3-ascending [^String s]
  (->> (partition 3 1 s)
       (filter (fn [xs]
                 (let [[a b c] (map long xs)]
                   (and (= 1 (- b a)) (= 2 (- c a))))))
       seq boolean))

(defn no-banned-letters [^String s]
  (boolean (not (some #{\i \o \l} s))))

(defn two-non-overlapping-pairs [^String s]
  (boolean (re-matches #".*(.)\1.*(.)\2.*" s)))

(def from-az
  (zipmap "0123456789abcdefghijklmnop" "abcdefghijklmnopqrstuvwxyz"))

(def to-az
  (zipmap "abcdefghijklmnopqrstuvwxyz" "0123456789abcdefghijklmnop"))

(defn pass-to-num [^String s]
  (let [pl #(Long/parseLong % 26)]
    (->> (map to-az s)
         (reduce str "")
         pl)))

(defn num-to-pass [^Long n]
  (->> (Long/toString n 26)
       (map from-az)
       (reduce str "")))

(def next-pass
  (comp num-to-pass inc pass-to-num))

(defn good-pass? [s]
  (and (no-banned-letters s) (has-3-ascending s) (two-non-overlapping-pairs s)))

(defn next-pass [s]
  (->> (iterate next-pass s)
       rest
       (filter good-pass?)
       first))

;; (time (next-pass "partone")) ;; around ~400ms

;; (time (next-pass "parttwo")) ;; around ~20sec

