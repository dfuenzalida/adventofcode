(ns advent2020.day16-test
  (:require [advent2020.day16 :refer [example example2 parse-range parse-ticket
                                      not-in-any-range? part-1 parse-input cycle1
                                      within-ranges? build-index-map]]
            [clojure.string :refer [blank? split-lines]]
            [clojure.test :refer [deftest testing is]]))

(deftest day16-tests

  (testing "part 1"
    (let [expected [{"class" [[1 3] [5 7]] "row" [[6 11] [33 44]], "seat" [[13 40] [45 50]]}
                    [7 1 14] [[7 3 47] [40 4 50] [55 2 20] [38 6 12]]]
          expected2 [{"class" [[0 1] [4 19]], "row" [[0 5] [8 19]], "seat" [[0 13] [16 19]]}
                     [11 12 13] [[3 9 18] [15 1 5] [5 14 9]]]]
      (is (= expected (parse-input example)))
      (is (= expected2 (parse-input example2)))
      (is (= ["class" [[1 3] [5 7]]] (parse-range "class: 1-3 or 5-7")))
      (is (= [7 1 14] (parse-ticket "7,1,14")))))


  (testing "part 2"
    (let [[ranges _ _] (parse-input example)
          ranges (mapcat second ranges)]
      (is (within-ranges? ranges 2))
      (is (not-in-any-range? ranges -1)))

    (is (= 71 (part-1 example)))
    (is (= [:b :c :a] (cycle1 [:a :b :c])))


    (let [[ranges mytix tix] (parse-input example2)
          expected {"seat" 2, "class" 1, "row" 0}]
      (is (= expected (build-index-map ranges (into tix [mytix]))))))
  )

