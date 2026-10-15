(ns alexandria.structure.curvature-test
  (:require [alexandria.ctx :as ctx]
            [alexandria.ctx.hyperbolic]
            [alexandria.ctx.plane]
            [alexandria.ctx.sphere]
            [alexandria.raster :as raster]
            [alexandria.structure.curvature :as kc]
            [alexandria.structure.curvature-contract :as c :refer [defspace-form-contract]]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

;; Every curvature in [-2 2] is an instance.
(defspace-form-contract space-form c/space-form-gen c/chart-point)

;; A symbolic K, evaluated at numbers, is the same geometry (the blend).
(defspec symbolic-K-agrees 30
  (prop/for-all [K (gen/fmap #(/ % 50.0) (gen/choose -100 100)) p c/chart-point q c/chart-point]
    (let [blend (binding [raster/*simplify?* false]
                  (raster/value (fn [k] (kc/distance (kc/space-form k) p q)) K))]
      (c/close? blend (kc/distance (kc/space-form K) p q) 1e-5))))

(deftest named-cases
  (testing "the three values of one thing"
    (is (= 0 (kc/curvature kc/euclid)))
    (is (= 1 (kc/curvature kc/sphere)))
    (is (= -1 (kc/curvature kc/lobachevsky))))
  (testing "malli refuses a non-curvature"
    (is (thrown? Exception (kc/space-form "flat")))))

(deftest law-of-cosines-exact-at-K-0
  (testing "K = 0: the versine law vers(a - b) + sn a sn b (1 - cos C) is c^2/2 with c^2 = a^2 + b^2 - 2ab cos C, Emmy-exact"
    (is (e/zero? (e/simplify (e/- (e/+ (kc/vers 0 (e/- 'a 'b)) (e/* (kc/sn 0 'a) (kc/sn 0 'b) (e/- 1 (e/cos 'C))))
                                  (e/divide (e/- (e/+ (e/square 'a) (e/square 'b)) (e/* 2 'a 'b (e/cos 'C))) 2))))))
  (testing "K = -1: cosh c = cosh a cosh b - sinh a sinh b cos C (numbers)"
    (doseq [[a b C] [[0.3 0.7 1.1] [1.5 0.4 2.6] [2.0 2.0 0.3]]]
      (is (c/close? (Math/cosh (kc/law-of-cosines-side kc/lobachevsky a b C))
                    (- (* (Math/cosh a) (Math/cosh b)) (* (Math/sinh a) (Math/sinh b) (Math/cos C))) 1e-9))))
  (testing "K = 1: cos c = cos a cos b + sin a sin b cos C"
    (doseq [[a b C] [[0.3 0.7 1.1] [1.5 0.4 2.6] [1.0 1.2 0.3]]]
      (is (c/close? (Math/cos (kc/law-of-cosines-side kc/sphere a b C))
                    (+ (* (Math/cos a) (Math/cos b)) (* (Math/sin a) (Math/sin b) (Math/cos C))) 1e-9)))))

(deftest gauss-bonnet-octant
  (testing "the octant of the unit sphere: three right angles, area 4 pi / 8"
    (let [t (kc/triangle kc/sphere [0 0] [1 0] [0 1])]
      (is (c/close? (:S t) (* 1.5 Math/PI) 1e-9))
      (is (c/close? (kc/area kc/sphere (:S t)) (/ Math/PI 2) 1e-9))))
  (testing "Lobachevsky: area = pi - S, positive"
    (let [t (kc/triangle kc/lobachevsky [0.6 0.1] [-0.4 0.5] [-0.3 -0.6])]
      (is (pos? (kc/area kc/lobachevsky (:S t))))
      (is (c/close? (kc/area kc/lobachevsky (:S t)) (- Math/PI (:S t)) 1e-12)))))

(deftest raster-compiles-a-curvature-slider
  (testing "distance with K a kernel argument, through alexandria.raster"
    (doseq [K [-1.0 -0.3 0.4 1.0]]
      (is (c/close? (binding [raster/*simplify?* false]
                      (raster/value (fn [k] (kc/distance (kc/space-form k) [0.1 0.2] [0.4 -0.3])) K))
                    (kc/distance (kc/space-form K) [0.1 0.2] [0.4 -0.3]) 1e-6)))))

(def tangent (gen/tuple (gen/fmap #(/ % 100.0) (gen/choose -60 60)) (gen/fmap #(/ % 100.0) (gen/choose -60 60))))

(defn- ctx-law-holds?
  "The law of cosines of ctx's SpaceForm holds on ctx's own distances and
   angle for the triangle reached by tangents ta tb tc."
  [cx ta tb tc]
  (let [G (kc/of-context cx)
        [P Q R] (map #(ctx/point cx %) [ta tb tc])
        a (ctx/distance cx Q R) b (ctx/distance cx R P) cc (ctx/distance cx P Q)
        A (ctx/angle cx P Q R)]
    (or (r/err? A)
        (some #(< (double %) 0.02) [a b cc (:ok A)])
        (c/close? (kc/law-of-cosines-side G b cc (:ok A)) a 1e-5))))

(doseq [[nm cx] [['plane (ctx/make :plane {})] ['sphere (ctx/make :sphere {:radius 1})] ['hyperbolic (ctx/make :hyperbolic {})]]]
  (eval `(defspec ~(symbol (str "ctx-" nm "-is-a-space-form")) 40
           (prop/for-all [ta# tangent tb# tangent tc# tangent]
             (#'ctx-law-holds? ~cx ta# tb# tc#)))))
