(ns alexandria.noether.conservation-test
  (:require [alexandria.noether.conservation :as c]
            [clojure.test :refer [deftest is testing]]
            [emmy.env :as e]))

(deftest noether-identities-are-proved
  (testing "every symmetry the Lagrangian keeps gives an identity Emmy reduces to 0"
    (is (= [:grade/proved :grade/proved :grade/proved :grade/proved]
           (mapv :grade (c/graded))))))

(deftest a-broken-symmetry-leaks
  (testing "a strength k(t) leaks energy at -k'/r; an x-only potential W leaks J at y W'(x)"
    (is (not (e/zero? (e/simplify (c/energy-leak)))))
    (is (not (e/zero? (e/simplify (c/torque-leak)))))))

(deftest the-wrong-charge-is-not-conserved
  (testing "for a central force, the linear momentum p_x is not a Noether charge: its identity does not close"
    (let [res (e/with-literal-functions [x y V]
                (let [Lf (c/L-central 'm V)
                      path (e/up x y)
                      px (fn [t] (nth (((e/partial 2) Lf) ((emmy.mechanics.lagrange/Gamma path) t)) 0))
                      psi (e/- (((emmy.mechanics.lagrange/Lagrange-equations Lf) path) 't))]
                  (e/simplify (e/+ ((e/D px) 't) (nth psi 0)))))]
      ;; dp_x/dt + psi_x = dL/dx, the x-force, which is not identically zero
      (is (not (e/zero? res))))))
