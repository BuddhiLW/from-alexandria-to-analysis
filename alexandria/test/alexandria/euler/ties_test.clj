(ns alexandria.euler.ties-test
  (:require [alexandria.euler.ties :as ties]
            [alexandria.raster :as raster]
            [clojure.test :refer [deftest is testing]]
            [emmy.env :as e]))

(def ok #{:grade/proved :grade/numeric})

(deftest every-tie-is-graded
  (let [gs (ties/grades)]
    (doseq [k [:basel :e :cis :cut] {:keys [label grade]} (gs k)]
      (testing (str k " " label) (is (ok grade))))
    (is (every? #(= :grade/proved (:grade %)) (concat (:basel gs) (:cut gs))))
    (is (= :grade/proved (:grade (ties/slope-grade))))))

(deftest the-error-series-is-emmys
  (is (= [0 1/2 -11/24 7/16] @ties/error-coefficients)))

(deftest round-trips-by-raster
  (testing "Basel: n -> b -> n"
    (doseq [n [1.0 7.0 40.0 400.0]]
      (is (< (Math/abs (- n (raster/value ties/basel-inverse (raster/value ties/basel-forward n)))) 1e-9))))
  (testing "e: n -> eps -> n"
    (doseq [n [1 3 20 80]]
      (is (< (Math/abs (- 1 (/ (ties/e-inverse (raster/value ties/e-error (double n))) n))) 1e-7))))
  (testing "cis: x -> (cos, sin) -> x on both halves"
    (doseq [x [-2.9 -1.0 0.3 2.5]]
      (is (< (Math/abs (- x (raster/value ties/x-of-cos (Math/cos x) (Math/sin x)))) 1e-9))
      (is (< (Math/abs (- x (raster/value ties/x-of-sin (Math/sin x) (Math/cos x)))) 1e-9)))))

(deftest truncation-counts
  (let [{:keys [V E F]} (ties/truncated-counts {:V 8 :E 12 :F 6} (repeat 8 3))]
    (is (= [24 36 14] [V E F]) "the truncated cube")
    (is (= 2 (+ (- V E) F))))
  (let [{:keys [V E F]} (ties/truncated-counts {:V 5 :E 8 :F 5} [4])]
    (is (= [8 12 6] [V E F]) "the square pyramid's apex cut off: a frustum")))

(deftest derivations-render
  (doseq [[_ steps] (ties/equations) {:keys [eqs]} steps q eqs]
    (is (string? (e/->TeX q)))))
