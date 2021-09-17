(ns advent2020.day04
  (:require [clojure.string :refer [split-lines]]))

(declare example)

(defn parse-line [line]
  (->> line
       (re-seq #"(\w+):(\S+)") ;; select keys and values
       (map (comp vec next))   ;; drop whole match and form [key val] pairs
       (reduce merge {})))     ;; dump into map

(defn parse-input [lines]
  (->> lines
       (partition-by empty?)         ;; groups of lines
       (map (partial interpose " ")) ;; interpose space
       (map (partial reduce str))    ;; join groups as single string
       (remove empty?)               ;; remove empty strings
       (map parse-line)))

;; (parse-input (split-lines example))

(defn valid1? [m]
  (-> (select-keys m ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"])
      count
      (= 7)))

;; (->> example split-lines parse-input (map valid1?))

(defn part-1 [input]
  (->> input split-lines parse-input (filter valid1?) count))

;; (part-1 (slurp "resources/day04.txt"))

(defn year-range [low high]
  (fn [s]
    (and (re-matches #"\d{4}" s)
         (<= low (read-string s) high))))

(defn valid-height? [s]
  (when-let [[_ amount unit] (first (re-seq #"(\d+)(cm|in)$" s))]
    (case unit
      "cm" (<= 150 (read-string amount) 193)
      "in" (<= 59 (read-string amount) 76))))

(def validation-map
  {"byr" (year-range 1920 2002)
   "iyr" (year-range 2010 2020)
   "eyr" (year-range 2020 2030)
   "hgt" valid-height?
   "hcl" (partial re-matches #"#[0-9a-f]{6}")
   "ecl" #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
   "pid" (partial re-matches #"\d{9}")
   })

(defn valid2? [m]
  (and (valid1? m)
       (let [values (->> (merge-with #(%1 %2) validation-map m) vals)]
         (every? (comp true? boolean) values))))

(defn part-2 []
  (->> "resources/day04.txt" slurp split-lines parse-input (filter valid2?) count))

;; (part-2)

(def example
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(comment
  (->> example
       split-lines
       (partition-by empty?) ;; groups of lines
       (map (partial interpose " ")) ;; interpose space
       (map (partial reduce str)) ;; join groups as single string
       (remove empty?))

  (->> "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
       (re-seq #"(\w+):(\S+)")
       (map (comp vec next))
       (reduce merge {}))
  )
