(ns alexandria.pascal.points-test
  (:require [alexandria.pascal.points :as pp]
            [clojure.test :refer [deftest is testing]]))

(def ok #{:grade/proved :grade/numeric})

(deftest grades
  (doseq [{:keys [label grade]} (pp/graded)]
    (is (ok grade) label)))

(deftest pascal-and-fermat-examples
  (testing "Pascal, 24 Aug. 1654: two points against three, 11 to 5"
    (let [{:keys [favourable total n rows]} (pp/fermat-at 2 3)]
      (is (= 4 n))
      (is (= [11.0 16] [favourable total]))
      (is (= "aabb" (first (first (filter #(= "aabb" (first %)) rows)))))))
  (testing "Pascal, 29 July 1654: 48, 56 and 44 pistoles of 64"
    (let [g (pp/pascal-grid-at)]
      (is (= 0.75 (g [1 2])))
      (is (= 0.875 (g [1 3])))
      (is (= 0.6875 (g [2 3])))))
  (testing "Traite, Usage: 2 against 4, base of six cells, 26 to 6"
    (let [{:keys [sum-a sum-b cells]} (pp/triangle-at 2 4)]
      (is (= [1.0 5.0 10.0 10.0 5.0 1.0] (subvec cells 0 6)))
      (is (= [26.0 6.0] [sum-a sum-b])))))

(deftest three-methods-agree-in-range
  (doseq [[ab [f p t]] @pp/agreement-cache]
    (is (== f p t) (pr-str ab))))

(deftest huygens
  (testing "Prop. I in numbers: 3 or 7 with equal chances is worth 5"
    (is (== 5 (:x (pp/huygens-tie :a {:a 3 :b 7 :p 1 :q 1 :x 0})))))
  (testing "the price solves back to the chances"
    (let [m (pp/huygens-tie :x {:a 10 :b 0 :p 1 :q 1 :x 7.5})]
      (is (< (Math/abs (- (:q m) (/ 1.0 3))) 1e-6)))))
