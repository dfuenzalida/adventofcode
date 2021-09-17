(ns advent2020.day04-test
  (:require [advent2020.day04 :refer [example part-1 parse-input valid1? valid2? year-range]]
            [clojure.string :refer [split-lines]]
            [clojure.test :refer [deftest testing is]]))

(declare valid-passports invalid-passports)

(deftest day04-tests
  (testing "part 1"
    (is (= [true false true false] (->> example split-lines parse-input (map valid1?))))
    (is (= 2 (part-1 example))))

  (testing "part 2"
    (is (true? ((year-range 1920 2010) "2000")))

    (is (every? true? (->> valid-passports split-lines parse-input (map valid2?))))
    (is (every? false? (->> invalid-passports split-lines parse-input (map valid2?))))
    )
  )

(def valid-passports
  "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")

(def invalid-passports
  "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007")
