(ns alexandria.copernicus.orbits-test
  "Copernicus' shelf: the heliocentric flow by RK4, its identity with
   Ptolemy's epicycle, the synodic-sidereal tie and his V.1 periods."
  (:require [alexandria.copernicus.orbits :as co]
            [alexandria.raster :as raster]
            [alexandria.raster.step :as step]
            [clojure.test :refer [deftest is testing]]))

(defn- close? [a b tol] (< (Math/abs (- (double a) (double b))) tol))

(defn- rates [a P n]
  (let [wP (/ 1 P)
        pts (step/run (co/flow 1 wP) 0.004 [1 0 a 0 0] n)]
    (map (fn [s] (nth (mapv double ((co/rate 1 wP) s)) 2)) pts)))

(deftest graded
  (is (every? #{:grade/proved :grade/numeric} (map :grade (co/graded)))))

(deftest retrograde-from-a-moving-earth
  (testing "Mars (a = 1.52, P = 1.88 yr) retrogrades near opposition and runs forward elsewhere"
    (let [rs (rates 1.5237 1.8809 1000)]
      (is (some neg? rs)) (is (some pos? rs))
      (is (neg? (first rs)) "at opposition (t = 0) the planet runs backwards")))
  (testing "Venus (inner) retrogrades near inferior conjunction"
    (is (neg? (first (rates 0.7233 0.6152 10)))))
  (testing "a planet slower than Kepler's law allows on an inner orbit loses its loops"
    (is (every? pos? (rates 0.5 3.0 1000))))
  (testing "lambda' at the nearest point is the closed form (a wP - wE)/(a - 1)"
    (let [[v] (raster/value (fn [a wE wP] ((co/regime a wE wP) 0)) 1.5237 1 (/ 1 1.8809))]
      (is (close? v (* (/ 360 365.25) (/ (- (/ 1.5237 1.8809) 1) 0.5237)) 1e-6))
      (is (neg? v)))))

(deftest synodic-tie
  (testing "every source solves back to a consistent triple"
    (let [out (binding [raster/*simplify?* false] (raster/value (fn [E P S] ((co/tie E P S) 0)) 1 1.8809 2.135))]
      (doseq [[E P S] (partition 3 out)]
        (is (close? (/ 1 S) (Math/abs (- (/ 1 E) (/ 1 P))) 1e-6)))
      (is (close? (nth out 7) 1.8809 1e-3) "S -> P on the outer side recovers Mars")))
  (testing "inner side: Venus' synodic 1.599 yr gives P = 0.615"
    (let [[_ P' _] (drop 6 (binding [raster/*simplify?* false] (raster/value (fn [E P S] ((co/tie E P S) 0)) 1 0.6 1.5987)))]
      (is (close? P' 0.6152 2e-3)))))

(deftest copernicus-periods
  (let [rows (co/table)
        by (into {} (map (juxt :id identity)) rows)]
    (is (= 5 (count rows)))
    (is (close? (:S-days (by :mars)) 779.94 0.1) "Mars' synodic period, 779d 56'")
    (is (close? (:S-days (by :venus)) 583.92 0.1))
    (testing "the order of the distances Ptolemy's ratios fix once the epicycle is the Earth's orbit"
      (is (= [:mercury :venus :mars :jupiter :saturn] (map :id (sort-by :a rows)))))
    (testing "and it is the order of the periods"
      (is (= (map :id (sort-by :a rows)) (map :id (sort-by :P rows)))))))
