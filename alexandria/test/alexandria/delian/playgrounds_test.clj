(ns alexandria.delian.playgrounds-test
  (:require [alexandria.delian.playgrounds :as pg]
            [clojure.test :refer [deftest is testing]]))

(defn- row [f & params] (mapv double ((apply f params) [0])))
(defn- close? [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest the-means-are-in-continued-proportion
  (doseq [[a b] [[1 2] [0.6 4.8] [2 1] [1.3 3.7]]]
    (let [{:keys [x y ax xy yb cube-x ratio cube-a]} (zipmap pg/means-scalars (row pg/means-figure a b 2 1))]
      (is (close? ax xy 1e-6)) (is (close? xy yb 1e-6))
      (is (close? cube-x (* ratio cube-a) 1e-6))
      (testing "Theta is on all three loci"
        (let [{:keys [r1 r2 r3]} (zipmap pg/means-scalars (row pg/means-figure a b x y))]
          (is (every? #(close? % 0 1e-6) [r1 r2 r3])))))))

(deftest the-means-cases
  (is (= :delian (pg/means-kind 2)))
  (is (= :cube (pg/means-kind 8)))
  (is (= :cube (pg/means-kind 0.125)))
  (is (= :equal (pg/means-kind 1)))
  (is (= :solid (pg/means-kind 3))))

(deftest the-loci-curves
  (let [g (pg/means-curves 1 2)
        [x1 y1 x2 y2 x3 y3] (mapv double (g [1.5]))]
    (is (close? (* x1 x1) (* 1 y1) 1e-9))
    (is (close? (* y2 y2) (* 2 x2) 1e-9))
    (is (close? (* x3 y3) 2 1e-9))))

(deftest the-conchoid-keeps-its-interval
  (doseq [[d l phi] [[1 2 60] [1 1 30] [1.5 0.8 120]]]
    (let [{:keys [RQ RQ']} (zipmap pg/conchoid-scalars (row pg/conchoid-figure d l phi 2.2))]
      (is (close? RQ l 1e-9)) (is (close? RQ' l 1e-9))))
  (is (= :loop (pg/conchoid-kind 1 2)))
  (is (= :cusp (pg/conchoid-kind 1 1)))
  (is (= :smooth (pg/conchoid-kind 1.5 0.8)))
  (testing "the neusis preset lands Q near the given line"
    (let [{:keys [d l phi c]} (:neusis pg/conchoid-presets)
          {:keys [gap]} (zipmap pg/conchoid-scalars (row pg/conchoid-figure d l phi c))]
      (is (< (Math/abs gap) 0.02)))))

(deftest the-cissoid-proportion
  (doseq [u [0.3 1.0 1.2 2.0]]
    (let [{:keys [r1 r2 r3 ratio mean]} (zipmap pg/cissoid-scalars (row pg/cissoid-figure 1.2 u))]
      (is (close? r1 r2 1e-9)) (is (close? r2 r3 1e-9))
      (is (close? (Math/cbrt ratio) mean 1e-9))))
  (testing "the Delian preset puts the extremes in ratio 2"
    (let [{:keys [r u]} (:delian pg/cissoid-presets)
          {:keys [ratio]} (zipmap pg/cissoid-scalars (row pg/cissoid-figure r u))]
      (is (close? ratio 2 0.01))
      (is (= :delian (pg/cissoid-kind r u ratio))))))
