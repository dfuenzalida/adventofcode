(ns advent.day07
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example-input
  ["pbga (66)"
   "xhth (57)"
   "ebii (61)"
   "havc (66)"
   "ktlj (57)"
   "fwft (72) -> ktlj, cntj, xhth"
   "qoyq (66)"
   "padx (45) -> pbga, havc, qoyq"
   "tknk (41) -> ugml, padx, fwft"
   "jptl (61)"
   "ugml (68) -> gyxo, ebii, jptl"
   "gyxo (61)"
   "cntj (57)"])

(defn parents [[k vs]]
  (reduce merge {}
          (map #(hash-map % k) vs)))

(defn parents-map [m]
  (reduce merge (map parents m)))

(defn parse-line [s]
  (let [[_ parent weight] (first (re-seq #"(\w+) \((\d+)\)" s))
        [_ s2]            (first (re-seq #".* -> (.*)" s))
        children          (when s2 (clojure.string/split s2 #", "))]
    [parent (Integer. weight) children]))

(defn lines-to-children-map [lines]
  (->> lines
       (map parse-line)
       (map (juxt first last)) ;; name and children
       (filter last) ;; leave only nodes with children
       (into {})
       parents-map))

(defn find-root [lines]
  (let [data-map (lines-to-children-map lines)]
    (first
     (filter #(nil? (data-map %))
             (vals data-map)))))

(def example-root-name
  (find-root example-input))

(defn read-input []
  (-> (slurp "src/input7.txt")
      (clojure.string/split #"\n")))

(def root-name
  (find-root (read-input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn load-map [lines f]
  (->> lines
       (map parse-line)
       (map f)
       (filter last) ;; we don't want nil values in the hashmap
       (into {})))

(def ex-weight-map
  (load-map example-input (juxt first second)))

(def ex-children-map
  (load-map example-input (juxt first last)))

(defn weight [weight-map children-map node]
  (+ (weight-map node)
     (reduce +
             (map (partial weight weight-map children-map)
                  (children-map node)))))

;; (weight ex-weight-map ex-children-map "ugml") => 251

(defn unbalanced [weight-map children-map node]
  (if (<= (count (into #{} (map (partial weight weight-map children-map)
                                (children-map node)))) 1)
    node ;; the current node is not balanced, all children have same weight
    
    (let [unb-child (->> (map (juxt identity
                                    (partial weight weight-map children-map))
                              (children-map node))
                         (group-by second)
                         (filter #(= 1 (count (second %))))
                         first second first first)]
      (unbalanced weight-map children-map unb-child))))

;; example input

(def ex-unbalanced
  (unbalanced ex-weight-map ex-children-map example-root-name))

(def ex-parent-map
  (lines-to-children-map example-input))

(def ex-desired-tree-weight
  (->> ex-unbalanced
       ex-parent-map
       ex-children-map
       (filter #(not= ex-unbalanced %))
       first
       (weight ex-weight-map ex-children-map)))

(def ex-new-weight
  (+ (ex-weight-map ex-unbalanced)
     (- ex-desired-tree-weight
        (weight ex-weight-map ex-children-map ex-unbalanced))))

;; actual input

(def large-weight-map
  (load-map (read-input) (juxt first second)))

(def large-children-map
  (load-map (read-input) (juxt first last)))

(def large-unbalanced
  (unbalanced large-weight-map large-children-map root-name))

(def large-parent-map
  (lines-to-children-map (read-input)))

(def large-desired-tree-weight
  (->> large-unbalanced
       large-parent-map
       large-children-map
       (filter #(not= large-unbalanced %))
       first
       (weight large-weight-map large-children-map)))

;; finally
(def large-new-weight
  (+ (large-weight-map large-unbalanced)
     (- large-desired-tree-weight
        (weight large-weight-map large-children-map large-unbalanced))))
