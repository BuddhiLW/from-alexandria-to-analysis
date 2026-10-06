(ns alexandria.leibniz.calculus-test
  (:require [alexandria.formula :as formula]
            [alexandria.grade :as grade]
            [alexandria.leibniz.calculus :as calc]
            [alexandria.library :as library]
            [clojure.test :refer [deftest is testing]]
            [desargues.board.construction :as c]
            [hive-dsl.result :as r]))

(defn- close? [a b] (< (Math/abs (- (double a) (double b))) 1e-9))
(defn- pt [f params state] (mapv double ((apply f params) state)))

(deftest every-check-grades-as-expected
  (let [grades (calc/graded)
        numeric (take-last 3 grades)]
    (is (every? #(= :grade/numeric (:grade %)) numeric))
    (is (every? #(= :raster (:source %)) numeric) "the numeric grades record raster as their source")
    (is (every? #{:grade/proved} (map :grade (drop-last 3 grades))))))

(deftest the-shelf-claims-are-proved
  (let [shelf (:ok (library/shelves))]
    (doseq [id [:leibniz/transmutation :leibniz/nova-methodus-product
                :leibniz/nova-methodus-quotient :leibniz/nova-methodus-power]
            claim (:claims (shelf id))]
      (is (= :grade/proved
             (get-in (grade/grade :symbolic [(c/check claim {})]) [:ok :adt/variant]))
          (str id)))))

(deftest one-value-two-notations
  (let [{:keys [tex leibniz newton]} (calc/renderings (calc/rule 'product))]
    (is (= "d\\overline{x\\,v} = x\\,dv + v\\,dx" leibniz))
    (is (= "\\dot{\\overline{x\\,v}} = x\\,\\dot{v} + v\\,\\dot{x}" newton))
    (is (string? tex)))
  (testing "the frozen formula is the same value under both renderings"
    (let [f (calc/frozen (calc/rule 'quotient))]
      (is (= (formula/thaw f) (formula/thaw (calc/frozen (calc/rule 'quotient)))))
      (is (= '(= (differential (/ v y)) (/ (- (* y dv) (* v dy)) (* y y))) (formula/thaw f))))))

(deftest a-wrong-rule-fails
  (is (not= :grade/proved
            (calc/rule-grade {:expr '(= (differential (* x v)) (* dx dv)) :env {}}))))

(deftest the-series-in-exact-rationals
  (is (= 1 (calc/partial-sum 1)))
  (is (= 2/3 (calc/partial-sum 2)))
  (is (= 13/15 (calc/partial-sum 3)))
  (is (every? (fn [{:keys [error bound]}] (< error bound)) (calc/series-table (range 1 50))))
  (testing "alternating: the sums fall on both sides of pi/4"
    (is (> (calc/partial-sum 5) (/ Math/PI 4) (calc/partial-sum 6)))))

(deftest figures
  (testing "the characteristic triangle's corners lie on the circle"
    (let [[x y] (pt calc/triangle [0.4 0.3] [1 1])]
      (is (close? (* y y) (- (* 2 x) (* x x))))))
  (testing "the tangent meets the y-axis at z"
    (is (close? (double (calc/circle-z 0.4)) (second (pt calc/tangent-line [0.4] [0])))))
  (testing "a sector is carried onto its strip"
    (is (= [0.0 0.0] (pt calc/transmute [8 0] [3 0])))
    (let [[x y] (pt calc/transmute [8 1] [3 1])]
      (is (close? 0.6875 x))
      (is (close? (double (calc/circle-z 0.6875)) y)))))

(deftest the-proofs-are-shelf-data
  (let [res (calc/proof)]
    (is (r/ok? res))
    (is (= #{:leibniz/transmutation :leibniz/arithmetical-quadrature :leibniz/nova-methodus}
           (set (keys (:ok res)))))
    (is (every? (fn [{:keys [claim why stage]}] (and (string? claim) (string? why) (keyword? stage)))
                (mapcat :steps (vals (:ok res)))))))
