(ns advent2020.day17-test
  (:require [advent2020.day17 :refer [cubes-iterate glider-set parse-input
                                      cubes-iterate4 glider-set4 parse-input4]]
            [clojure.test :refer [deftest testing is]]))

(deftest day17-tests

  (testing "part 1"
    (is (= glider-set (set (parse-input ".#.\n..#\n###"))))
    (is (= 112 (-> (iterate cubes-iterate glider-set) (nth 6) count))))

  (testing "part 2"
    (is (= 848 (-> (iterate cubes-iterate4 glider-set4) (nth 6) count))))
  )


