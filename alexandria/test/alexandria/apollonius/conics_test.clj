(ns alexandria.apollonius.conics-test
  (:require [alexandria.apollonius.conics :as conics]
            [clojure.test :refer [deftest is testing]]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

(defn- close? [a b] (< (Math/abs (- (double a) (double b))) 1e-9))

(deftest every-claim-is-proved-by-emmy
  (let [g (conics/graded)]
    (is (= #{:apollonius/I.11-13 :apollonius/III.45 :apollonius/III.48
             :apollonius/III.51-52 :apollonius/dandelin}
           (set (keys g))))
    (doseq [[id rows] g {:keys [label grade]} rows]
      (is (= :grade/proved grade) (str id " " label)))))

(deftest the-sign-of-the-excess-names-the-section
  (let [k 0.6 tilt (conics/parabola-tilt k)]
    (is (= :parabola (conics/kind (conics/excess k tilt))))
    (is (= :ellipse (conics/kind (conics/excess k (- tilt 0.2)))))
    (is (= :hyperbola (conics/kind (conics/excess k (+ tilt 0.2)))))
    (testing "the horizontal plane cuts a circle: c = -1, p = d"
      (is (close? -1 (conics/excess k 0)))
      (is (close? (conics/parameter k 0 1) (conics/transverse k 0 1))))))

(deftest every-point-of-the-section-lies-on-the-cone-and-the-symptoma
  (doseq [theta [0.3 (conics/parabola-tilt 0.6) 1.3] phi [0.2 1.1 2.0]]
    (let [k 0.6 h 1
          [X Y Z] (mapv double ((conics/section-at k theta h) phi))
          s (/ (- Z h) (Math/sin theta))]
      (is (close? (+ (* X X) (* Y Y)) (* k k Z Z)) "on the cone")
      (is (close? (- (* k h) (* s (Math/cos theta))) X) "in the plane")
      (is (close? (* Y Y) (+ (* (conics/parameter k theta h) s) (* (conics/excess k theta) s s)))
          "y^2 = p x + c x^2"))))

(deftest the-projection-is-orthographic
  (is (= [1.0 2.0] (mapv double (conics/project 0 0 [1 5 2]))))
  (let [[x y] (mapv double ((conics/cone-figure 1 0 0) [0 1]))]
    (is (close? 1 x)) (is (close? 1 y))))

(deftest the-proofs-are-shelf-data
  (let [res (conics/proof)]
    (is (r/ok? res))
    (doseq [id [:apollonius/I.11-13 :apollonius/focal :apollonius/dandelin]]
      (is (seq (get-in res [:ok id :steps])) (str id))
      (is (every? (fn [{:keys [claim why stage]}] (and (string? claim) (string? why) (keyword? stage)))
                  (get-in res [:ok id :steps]))))
    (is (every? string? (get-in res [:ok :apollonius/preface :passages])))))

(deftest a-wrong-claim-fails
  (is (= :grade/fails
         (conics/grade-claim {:env :focal :claim '(= (square (length S P)) (square (+ a (* e x))))}))))
