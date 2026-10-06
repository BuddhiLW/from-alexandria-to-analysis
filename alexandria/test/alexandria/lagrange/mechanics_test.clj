(ns alexandria.lagrange.mechanics-test
  (:require [alexandria.lagrange.functions :as f]
            [alexandria.lagrange.mechanics :as mech]
            [clojure.test :refer [deftest is testing]]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

(defn- close? [a b tol] (< (Math/abs (- (double a) (double b))) tol))

(deftest mechanique-analytique-grades
  (let [gs (mech/graded)]
    (is (= :grade/numeric (:grade (first (filter #(= :double-pendulum (:id %)) gs)))))
    (is (every? #{:grade/proved} (map :grade (remove #(= :double-pendulum (:id %)) gs))))))

(deftest a-wrong-conic-fails
  (testing "an orbit with the wrong parameter p does not solve Kepler's equations"
    (let [res (e/simplify ((emmy.mechanics.lagrange/Euler-Lagrange-operator
                            (emmy.mechanics.lagrange/L-Kepler-polar 1 1))
                           (e/up 0 (e/up 2 0) (e/up 0 1) (e/up 0 0))))]
      (is (not (e/zero? (first res)))))))

(deftest pendulum-phase-curves-keep-energy
  (testing "each phase curve is a level set of the energy, to the integrator's accuracy (relative 1e-4)"
    (doseq [orbit (mech/pendulum-portrait)]
      (let [es (map (fn [[th om]] (- (* 0.5 om om) (* 9.81 (Math/cos th)))) orbit)
            scale (max 1.0 (Math/abs (double (first es))))]
        (is (< (/ (- (apply max es) (apply min es)) scale) 1e-4))))))

(deftest fonctions-analytiques-grades
  (is (= [:grade/proved :grade/proved :grade/numeric] (map :grade (f/graded)))))

(deftest the-mean-point-is-lagranges-u
  (let [u (f/mean-point e/cube 0.0 1.0)]
    (is (close? u (Math/sqrt (/ 1 3.0)) 1e-6))
    (is (close? u (f/cube-mean-point 0.0 1.0) 1e-6))))

(deftest the-remainder-u-lies-between
  (doseq [n [1 2 3 4] x [0.5 1.0 2.0]]
    (is (< 0 (f/remainder-u n x) x) (str n " " x))))

(deftest the-preface-is-quoted
  (let [p (get-in (mech/proof) [:ok :preface])]
    (is (r/ok? (mech/proof)))
    (is (re-find #"On ne trouvera point de Figures dans cet Ouvrage" (:original p)))
    (is (= 4 (count (get-in (mech/proof) [:ok :lagrange/kepler :steps]))))
    (is (= 6 (count (get-in (f/proof) [:ok :lagrange/mean-value :steps]))))))
