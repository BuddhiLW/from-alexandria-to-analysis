(ns alexandria.ctx.riemann-test
  (:require [alexandria.ctx :as ctx]
            [alexandria.ctx.riemann :as riemann]
            [alexandria.ctx.surfaces]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.properties :refer [defprop-total]]
            [emmy.generic]))

(defn near? [eps a b]
  (<= (Math/abs (- (double a) (double b))) eps))

(defn point-near? [eps p q]
  (every? true? (map #(near? eps %1 %2) p q)))

(def plane
  (ctx/make :riemann/metric
            {:metric-components (fn [_] [[1.0 0.0] [0.0 1.0]])
             :embedding (fn [[u v]] [u v 0.0])}))

(deftest geodesic-endpoints-test
  (testing "generic metric geodesic reaches endpoints and has linear arc length"
    (let [p [0.0 0.0]
          q [0.2 -0.15]
          g (ctx/geodesic plane p q)
          d (ctx/distance plane p q)]
      (is (point-near? 1.0e-8 p (g 0.0)))
      (is (point-near? 1.0e-5 q (g 1.0)))
      (doseq [t [0.2 0.5 0.8]]
        (is (near? 1.0e-4 (* t d) (ctx/distance plane p (g t))))))))

(defspec cylinder-distance-is-unrolled-plane 40
  (prop/for-all [u0 (gen/choose -1.0 1.0)
                 du (gen/choose -1.0 1.0)
                 v0 (gen/choose -2.0 2.0)
                 v1 (gen/choose -2.0 2.0)]
    (let [radius 1.7
          u1 (+ u0 du)
          c (ctx/make :surface/cylinder {:radius radius})
          expected (Math/sqrt (+ (riemann/sqr (* radius du))
                                 (riemann/sqr (- v1 v0))))]
      (near? 1.0e-9 expected (ctx/distance c [u0 v0] [u1 v1])))))

(defspec curvature-closed-forms-random-test 8
  (prop/for-all [u (gen/double* {:min 0.35 :max 2.6 :NaN? false :infinite? false})
                 v (gen/double* {:min -2.5 :max 2.5 :NaN? false :infinite? false})]
    (let [R 2.0
          r 0.5
          sphere (ctx/make :riemann/metric
                           {:metric-components (fn [p]
                                                 (let [theta (nth p 0)]
                                                   [[(emmy.generic/square R) 0.0]
                                                    [0.0 (emmy.generic/square (emmy.generic/* R (emmy.generic/sin theta)))]]))})
          torus (ctx/make :surface/torus {:major R :minor r})]
      (and (near? 5.0e-3 (/ 1.0 (* R R)) (ctx/curvature sphere [u v]))
           (near? 1.0e-12 -1.0 (ctx/curvature (ctx/make :surface/pseudosphere {}) [u (Math/abs v)]))
           (near? 1.0e-12 (/ (Math/cos v) (* r (+ R (* r (Math/cos v)))))
                  (ctx/curvature torus [u v]))
           (near? 1.0e-12 0.0 (ctx/curvature (ctx/make :surface/cylinder {:radius R}) [u v]))))))

(deftest meet-geodesic-circle-on-cylinder-test
  (let [radius 2.0
        c (ctx/make :surface/cylinder {:radius radius})
        line {:geodesic [[0.0 0.0] [1.0 0.0]]}
        circle {:circle [[0.5 0.0] 0.4]}
        xs (sort (map (comp first :point) (ctx/meet c line circle)))
        expected [(- 0.5 (/ 0.4 radius)) (+ 0.5 (/ 0.4 radius))]]
    (is (= 2 (count xs)))
    (is (point-near? 1.0e-3 expected xs))))

(defn spherical-triangle-area [R a b c]
  (* R R (- (+ a b c) Math/PI)))

(deftest gauss-bonnet-small-triangle-test
  (testing "Gauss-Bonnet on a spherical geodesic triangle: ∫K dA + exterior = 2π"
    (let [R 3.0
          K (/ 1.0 (* R R))
          angles [(/ Math/PI 2.0) (/ Math/PI 2.0) (/ Math/PI 2.0)]
          area (spherical-triangle-area R (angles 0) (angles 1) (angles 2))
          exterior (reduce + (map #(- Math/PI %) angles))]
      (is (near? 1.0e-12 (* 2.0 Math/PI) (+ (* K area) exterior))))))

(defprop-total distance-totality
  (fn [[u v]]
    (ctx/distance plane [0.0 0.0] [u v]))
  (gen/tuple (gen/choose -0.25 0.25) (gen/choose -0.25 0.25))
  {:num-tests 5
   :pred (fn [d]
           (and (number? d) (not (Double/isNaN d)) (not (neg? d))))})
