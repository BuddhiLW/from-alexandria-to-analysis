(ns alexandria.euclid.pythagoras-test
  (:require [alexandria.euclid.application :as ap]
            [alexandria.euclid.pythagoras :as py]
            [clojure.test :refer [deftest is testing]]))

(defn- close? [x y] (< (Math/abs (- (double x) (double y))) 1e-9))

(deftest every-pythagoras-step-is-proved
  (let [gs (py/grades)]
    (is (= 8 (count gs)))
    (doseq [{:keys [id grade]} gs]
      (is (= :grade/proved grade) (str id)))))

(deftest the-tie-re-solves-from-each-source
  (testing "c moved at a right angle: a forward, I.47"
    (let [[a b c A] (subvec ((py/tie 5.0 4.0 3.0 90.0 0.0 3.0) nil) 8 12)]
      (is (close? 5.0 a)) (is (= [4.0 90.0] [b A])) (is (close? 3.0 c))))
  (testing "a moved: b and A held, c by the inverse"
    (let [[a b c A] (subvec ((py/tie 7.0 4.0 3.0 90.0 0.0 3.0) nil) 0 4)]
      (is (close? 7.0 a)) (is (= [4.0 90.0] [b A]))
      (is (close? (* a a) (+ (* b b) (* c c))))))
  (testing "a moved below the branch: held where c = b cos A"
    (let [[a b c A] (subvec ((py/tie 1.0 4.0 3.0 60.0 0.0 3.0) nil) 0 4)]
      (is (close? c (max 0.5 (* b (Math/cos (/ (* A Math/PI) 180))))))
      (is (close? (* a a) (- (+ (* b b) (* c c)) (* 2 b c (Math/cos (/ (* A Math/PI) 180))))))))
  (testing "B dragged: c and A read off it"
    (let [[a _ c A] (subvec ((py/tie 5.0 4.0 3.0 90.0 1.0 3.0) nil) 16 20)]
      (is (close? (Math/sqrt 10) c))
      (is (close? (/ (* 180 (Math/atan2 3 1)) Math/PI) A))
      (is (close? (* a a) (- 26 (* 2 4 1)))))))

(deftest the-figure-names-the-case
  (let [right (py/named ((py/figure 4.0 3.0 90.0) nil))
        obtuse (py/named ((py/figure 4.0 3.0 120.0) nil))
        acute (py/named ((py/figure 4.0 3.0 60.0) nil))]
    (is (close? 25.0 (:a2 right))) (is (close? 25.0 (:sum right)))
    (is (= :right (py/kind (:cos right))))
    (is (= :obtuse (py/kind (:cos obtuse))))
    (is (< (:sum obtuse) (:a2 obtuse)))
    (is (close? (:a2 obtuse) (- (:sum obtuse) (:rect2 obtuse))))
    (is (= :acute (py/kind (:cos acute))))
    (is (> (:sum acute) (:a2 acute)))
    (is (= "II.12" (get-in py/names [:obtuse :prop])))))

(deftest every-application-step-is-proved
  (let [gs (ap/grades)]
    (is (= 7 (count gs)))
    (doseq [{:keys [id grade]} gs]
      (is (= :grade/proved grade) (str id)))))

(deftest the-application-tie
  (testing "VI.28 at a = 10, S = 21: y = 3"
    (is (close? 3.0 (nth ((ap/tie 10.0 21.0 9.0 -1.0) nil) 6))))
  (testing "S past VI.27's bound is held at (a/2)^2"
    (is (= [10.0 25.0 5.0 -1.0] (subvec ((ap/tie 10.0 30.0 3.0 -1.0) nil) 4 8))))
  (testing "I.44: k = 0, y = S/a"
    (is (close? 2.1 (nth ((ap/tie 10.0 21.0 3.0 0.0) nil) 6))))
  (testing "the names"
    (is (= [:ellipse :parabola :hyperbola] (map ap/kind [-0.5 0.0 0.5])))))
