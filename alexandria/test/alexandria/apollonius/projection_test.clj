(ns alexandria.apollonius.projection-test
  "Polar coordinates as a projection: Emmy's derivation is graded proved
   step by step, the forward and inverse maps compose to the identity on
   numbers too, and the section of the cone by the plane, dropped onto the
   base, is the focal conic."
  (:require [alexandria.apollonius.projection :as pj]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [emmy.env :as e]))

(defn- close? [a b] (< (Math/abs (- (double a) (double b))) 1e-9))

(deftest every-step-is-proved
  (let [gs (pj/grades)]
    (is (= 11 (count gs)))
    (doseq [{:keys [label grade]} gs]
      (is (= :grade/proved grade) label))))

(deftest derivation-reads-off-l-and-e
  (let [d @pj/derivation]
    (testing "Emmy's l and e are h tan alpha and tan tau tan alpha"
      (is (e/zero? (e/simplify (e/- (:ell d) (e/* 'h (e/tan 'alpha))))))
      (is (e/zero? (e/simplify (e/- (:e d) (e/* (e/tan 'tau) (e/tan 'alpha)))))))
    (testing "F is linear in rho"
      (is (e/zero? (:second d))))))

(deftest maps-are-inverse-on-numbers
  (doseq [alpha [0.2 0.6 1.0] tau [0.0 0.3 1.2] h [0.3 1.0 2.4]]
    (let [[l ecc] (pj/forward alpha tau h)
          [h' tau'] (pj/inverse alpha l ecc)]
      (is (close? h h'))
      (is (close? tau tau')))))

(deftest the-section-projects-to-the-focal-conic
  (testing "the root of the elimination at theta is l / (1 - e cos theta), and the point is on both surfaces"
    (doseq [[alpha tau h] [[0.6 0.3 1.2] [0.6 0.95 1.2] [0.4 1.2 0.8]]
            theta (range -3.0 3.1 0.4)
            :let [c (Math/cos theta)
                  rho (:root (pj/solve-linear-in (pj/elimination alpha tau h c)))
                  [l ecc] (pj/forward alpha tau h)]]
      (is (close? rho (/ l (- 1 (* ecc c)))))
      (is (close? (pj/cone-z alpha rho) (pj/plane-z tau h rho c))))))

(deftest equations-render-as-tex
  (let [qs (mapcat :eqs (pj/equations))]
    (is (= 14 (count qs)))
    (doseq [q qs]
      (is (str/includes? (e/->TeX q) " = ")))))
