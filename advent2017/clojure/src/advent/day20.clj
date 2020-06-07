(ns advent.day20
  (:gen-class))

(def example-input ["p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>"
                    "p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>"])

(defn parse-line [s]
  (let [[_ p v a] (first (re-seq #"p=<(.*?)>, v=<(.*?)>, a=<(.*?)>" s))
        [p v a]   [(.trim p) (.trim v) (.trim a)]
        p         (->> (clojure.string/split p #",") (mapv #(Long. %)))
        v         (->> (clojure.string/split v #",") (mapv #(Long. %)))
        a         (->> (clojure.string/split a #",") (mapv #(Long. %)))]
    {:p p :v v :a a}))

(defn index-of-max
  "Returns the index of the value that yields the max value for f(x)"
  [f xs]
  (first
   (apply max-key (comp f second) (map-indexed vector xs))))

(defn read-input []
  (-> (slurp "src/input20.txt")
      (clojure.string/split #"\n")))

(defn acceleration [{[ax ay az] :a}]
  (+ (Math/abs ax) (Math/abs ay) (Math/abs az)))

(defn min-acceleration [p]
  (* -1 (acceleration p)))

(def input-parts (map parse-line (read-input)))

;; part 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (index-of-max min-acceleration input-parts)

(defn move [{:keys [p v a]}]
  {:p (mapv + p v a)
   :v (mapv + v a)
   :a a})

(defn find-survivors [parts n]
  (if (neg? n)
    (count parts)
    (let [collisions    (->> parts
                             (map :p)
                             frequencies
                             (filter (fn [[k v]] (> v 1)))
                             (map first)
                             (into #{}))
          not-colliding (filter
                         #(not (some collisions [(:p %)]))
                         parts)]
      (recur (map move not-colliding) (dec n)))))

(def ex-colliding
  (map parse-line ["p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>"
                   "p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>"
                   "p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>"
                   "p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>"]))

;; (find-survivors ex-colliding 5) => 1

;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (find-survivors input-parts 1000)

