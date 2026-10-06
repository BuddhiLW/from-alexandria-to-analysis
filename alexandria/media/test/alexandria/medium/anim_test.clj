(ns alexandria.medium.anim-test
  (:require [alexandria.medium.anim :as a]
            [clojure.test :refer [deftest is testing]]))

(defn- close? [x y] (< (Math/abs (- (double x) (double y))) 1e-9))

(deftest rate-functions-start-at-rest-and-end-at-one
  (doseq [f [a/linear a/smooth a/smoothstep a/rush-into a/rush-from a/double-smooth]]
    (is (close? 0 (f 0)))
    (is (close? 1 (f 1)))
    (is (close? 0.5 (a/smooth 0.5)))))

(deftest there-and-back-returns
  (is (close? 0 (a/there-and-back 1)))
  (is (close? 1 (a/there-and-back 0.5)))
  (is (close? 1 (a/there-and-back-with-pause 0.5))))

(deftest play-and-lag
  (testing "play runs only inside its part of the step"
    (is (zero? (a/play 0.1 0.2 0.4)))
    (is (close? 1 (a/play 0.5 0.2 0.4))))
  (testing "LaggedStart: the first starts at a, the last ends at b"
    (let [fs (a/lagged 0.3 4 0.5 0 1)]
      (is (> (first fs) (last fs)))
      (is (= [0 0 0 0] (map #(Math/round (double %)) (a/lagged 0 4 0.5 0 1))))
      (is (every? #(close? 1 %) (a/lagged 1 4 0.5 0 1))))))

(deftest create-and-fade
  (is (= 1 (:stroke-dashoffset (a/create 0))))
  (is (zero? (:stroke-dashoffset (a/create 1))))
  (is (= 0 (:opacity (a/fade 0))))
  (is (= 1 (:opacity (a/fade 1 [0 1])))))
