(ns advent2018.day04
  (:require [clojure.string :as str]))

(def example-data
"[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up")

(defn process-input [input]
  (->> input
       (re-seq #"(.*)")
       (map first)
       (filter seq)
       sort))

;; (last (process-input example-data))
;; (->> "resources/day04.txt" slurp process-input sort last)

(defn format-digit [n]
  (str (if (< n 10) "0" "") n))

(defn format-hh-mm [hh mm]
  (str (format-digit hh) ":" (format-digit mm)))

(defn inc-hh-mm [s]
  (let [[hh mm] (->> (re-seq #"(.*):(.*)" s) first rest (map #(str "1" %)) (map read-string) (map #(mod % 100)))
        inc-hh  (if (>= mm 59) 1 0)]
    (format-hh-mm (mod (+ hh inc-hh) 24) (mod (inc mm) 60))))

(defn hhmm-desc [s]
  (->> s (re-seq #"\[(.*) (.*)\] (.*)") first (drop 2)))

(defn guard-from [s]
  (->> s (re-seq #"Guard #(\d*) (.*)") first second read-string))

;; (inc (guard-from "Guard #1867 begins shift"))

;; (hhmm-desc "[1518-11-23 00:57] wakes up")
;; (hhmm-desc "[1518-06-22 23:58] Guard #1867 begins shift")

;; [[10 "00:05"], [10 "00:06"] ,,, [10 "00:24"]]
(defn sleep-minutes [events]
  (loop [events events
         zzz-entries []
         guard -1
         hhmm  "23:58"
         asleep false]
    (if (seq events)
      (let [[evt-hhmm evt-desc] (hhmm-desc (first events))
            next-hh-mm          (inc-hh-mm hhmm)]
        ;; (println hhmm "->" evt-hhmm "-" evt-desc)
        (cond

          (and (= hhmm evt-hhmm) (str/starts-with? evt-desc "Guard"))
          (let [new-guard (guard-from evt-desc)]
            (recur
             (rest events) zzz-entries new-guard next-hh-mm false))

          (= [hhmm "falls asleep"] [evt-hhmm evt-desc])
          (recur
           (rest events) (conj zzz-entries [guard hhmm]) guard next-hh-mm true)

          (= [hhmm "wakes up"] [evt-hhmm evt-desc])
          (recur
           (rest events) zzz-entries guard next-hh-mm false)

          :otherwise
          (recur
           events (if asleep (conj zzz-entries [guard hhmm]) zzz-entries) guard next-hh-mm asleep)))

      zzz-entries)))

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Find the guard that has the most minutes asleep. What minute does that guard spend asleep the most?

(defn solve-part-1 [events]

  (let [sleep-events (sleep-minutes (process-input events))

        ;; find the guard that sleeps the most
        sleepy-guard (->> sleep-events (map first) frequencies (sort-by second) last first)

        ;; find the hour that the guard above was more frequently sleeping
        sleepy-freq (->> sleep-events
                         (filter (fn [[g e]] (= g sleepy-guard)))
                         (map second)
                         frequencies
                         (sort-by second)
                         last
                         first)

        sleepy-minute (->> sleepy-freq
                           (re-seq #"..:(.*)")
                           first
                           last
                           (Integer.))]

    (* sleepy-guard sleepy-minute)))

;; (solve-part-1 example-data)
;; (solve-part-1 (->> "resources/day04.txt" slurp))

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Of all guards, which guard is most frequently asleep on the same minute?

(defn solve-part-2 [events]
  (let [sleep-events (sleep-minutes (process-input events))
        [guard ts]   (->> sleep-events
                          frequencies
                          (sort-by second)
                          last
                          first)
        minute       (->> ts
                          (re-seq #"..:(.*)")
                          first
                          last
                          (Integer.))]
    (* guard minute)))

;; (solve-part-2 example-data) => 4455
;; (solve-part-2 (slurp "resources/day04.txt"))


