(ns alexandria.archimedes.method-test
  (:require [alexandria.archimedes.method :as method]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]))

(defn- close? [a b] (< (Math/abs (- (double a) (double b))) 1e-9))
(defn- end-of [s t e] (mapv double ((method/hang s) [t e])))

(deftest the-balance-is-proved
  (is (= :grade/numeric (:grade (last (method/graded)))) "the quadrature is numeric")
  (is (= :raster (:source (last (method/graded)))) "and its number came from raster")
  (is (every? #{:grade/proved} (map :grade (butlast (method/graded))))))

(deftest areas
  (let [{:keys [ABC ACF segment]} (method/areas)]
    (is (= 1 ABC))
    (is (= 4 ACF))
    (is (= :raster (:source segment)))
    (is (close? 4/3 (:value segment)))))

(deftest slices-are-carried-to-h
  (testing "in place at the start"
    (is (= [0.3 0.0] (end-of 0 0.3 0)))
    (is (close? 0.91 (second (end-of 0 0.3 1)))))
  (testing "hung with the middle at H at the end, the length kept"
    (let [[_ y0] (end-of 1 0.3 0) [_ y1] (end-of 1 0.3 1)]
      (is (close? 4 (/ (+ y0 y1) 2)))
      (is (close? 0.91 (- y1 y0))))))

(deftest the-proposition-is-shelf-data
  (let [p (get-in (method/proof) [:ok :archimedes/method-1])]
    (is (r/ok? (method/proof)))
    (is (= 9 (count (:steps p))))
    (is (every? string? (:passages (:letter p))))
    (is (string? (:disclaimer p)))))
