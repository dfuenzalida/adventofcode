(ns advent2021.day12)

(declare example example2 example3)

(defn read-input [s]
  (->> (re-seq #"\w+" s)
       (partition 2)
       (map (fn [[a b]] {a [b], b [a]})) ;; make the graph undirected
       (reduce (partial merge-with into) {})))

(defn lowercase? [s]
  (= s (clojure.string/lower-case s)))

(defn visited-lowercase [xs]
  (set (filter lowercase? xs)))

(defn expand [m xs]
  (let [by-end    (group-by (comp (partial = "end") last) xs)
        ended     (get by-end true [])
        expanding (get by-end false [])]
    (->> (mapcat
          (fn [path]
            (let [lnode (last path)
                  optns (->> (get m lnode)
                             (remove #{"start"})
                             (remove (visited-lowercase path)))]
              (map #(conj (vec path) %) optns)))
          expanding)
         (concat ended))))

(defn find-count [s expandfn]
  (let [m     (read-input s)
        iters (iterate (partial expandfn m) [["start"]])]
    (->> (map (fn [a b] [(= a b) a]) iters (rest iters))
         (drop-while (comp false? first))
         first second count)))

(defn part-1 [s]
  (find-count s expand))

;; (= 10 (part-1 example))
;; (= 19 (part-1 example2))
;; (= 226 (part-1 example3))
;; (part-1 (slurp "resources/day12.txt"))

(defn upto-one-repeat-lowercase? [path]
  (let [ffreq (->> (remove #{"start"} path)
                   (filter lowercase?)
                   frequencies vals frequencies)
        repeated (get ffreq 2 0)]
    (and (<= repeated 1)
         (-> ffreq (dissoc 1) (dissoc 2) empty?))))

;; (upto-one-repeat-lowercase? ["start" "a" "b" "a" "c" "c"])

(defn expand2 [m xs]
  (let [by-end    (group-by (comp (partial = "end") last) xs)
        ended     (get by-end true [])
        expanding (get by-end false [])]
    (->> (mapcat
          (fn [path]
            (let [lnode (last path)
                  optns (->> (get m lnode) (remove #{"start"}))]
              (->> (map #(conj (vec path) %) optns)
                   (filter upto-one-repeat-lowercase?))))
          expanding)
         (concat ended))))

(defn part-2 [s]
  (find-count s expand2))

;; (= 36 (part-2 example))
;; (= 103 (part-2 example2))
;; (= 3509 (part-2 example3))

;; (time (part-2 (slurp "resources/day12.txt")))
;; => "Elapsed time: 10148.259232 msecs"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(def example2 "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc")

(def example3 "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW")

