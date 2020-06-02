(ns advent2016.day07
  (:require [clojure.string :refer [join]]))

(defn to-groups [line]
  (->> (re-matches #"(\w+)\[(\w+)\](\w+)" (str line)) next))

(defn line-groups [line]
  (let [groups-map (->> (map vector (re-seq #"[a-z]+" line) (range))
                        (group-by (comp odd? second)))
        abbas      (map first (groups-map false))
        not-abbas  (map first (groups-map true))]
    [abbas not-abbas]))

(defn abba? [s]
  (when-let [groups (re-matches #"\w*(\w)(\w)\2\1\w*" (str s))]
    (let [[_ a b] groups]
      (not= a b))))

(defn valid? [[abbas not-abbas]]
  (and (some abba? abbas)
       (every? #(not= true (abba? %)) not-abbas)))

;; (= true (valid? (line-groups "abba[mnop]qrst")))
;; (= true (not (valid? (line-groups "abcd[bddb]xyyx"))))
;; (= true (not (valid? (line-groups "aaaa[qwer]tyui"))))
;; (= true (valid? (line-groups "ioxxoj[asdfgh]zxcvbn")))

(defn read-input []
  (clojure.string/split-lines (slurp "resources/day07.txt")))

(defn part-1 []
  (->> (read-input)
       (map line-groups)
       (filter valid?)
       count))

;; (part-1)

(defn to-bab [[a b _]]
  (str b a b))

(defn aba? [[a b c]]
  (and (= a c) (not= a b)))

(defn valid2? [line]
  (let [[outside inside] (map (partial join ",") (line-groups line))
        abas             (->> (partition 3 1 outside)
                              (map (partial apply str))
                              (filter #(neg? (.indexOf % ",")))
                              (filter aba?))
        babs             (map to-bab abas)]
    (some #(.contains inside %) babs))) ;; any babs is contained in `inside`?

;; (= true (valid2? "aba[bab]xyz"))
;; (not= true (valid2? "xyx[xyx]xyx"))
;; (= true (valid2? "aaa[kek]eke"))
;; (= true (valid2? "zazbz[bzb]cdb"))

(defn part-2 []
  (->> (read-input)
       (filter valid2?)
       count))

;; (part-2)

