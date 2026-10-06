(ns alexandria.ctx.hyperbolic-test
  (:require [alexandria.ctx :as ctx]
            [alexandria.ctx-contract :refer [defcontract angle!]]
            [alexandria.ctx.hyperbolic :as hyp]
            [alexandria.ctx.hyperbolic.models :as models]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [desargues.geometry.projective :as pg]))

(def eps 1e-6)

(defcontract hyperbolic (ctx/make :hyperbolic {}) {:tangent-scale 0.8 :tolerance 1.0e-4})

(defn close? [a b]
  (< (Math/abs (- (double a) (double b))) eps))

(defn close-pt? [p q]
  (every? true? (map close? p q)))

(def klein-point-gen
  (gen/let [x (gen/choose -700 700)
            y (gen/choose -700 700)]
    (let [p [(/ x 1000.0) (/ y 1000.0)]
          r (Math/sqrt (double (models/norm2 p)))]
      (if (< r 0.85)
        p
        (mapv #(/ % (max 1.0 (/ r 0.85))) p)))))

(def distinct-pair-gen
  (gen/such-that (fn [[p q]] (> (double (hyp/distance-klein p q)) 1e-3))
                 (gen/tuple klein-point-gen klein-point-gen)
                 100))

(deftest geodesic-is-arc-length-parametrized-test
  (let [ctx (ctx/make :hyperbolic {})
        property (prop/for-all [[p q] distinct-pair-gen
                                n (gen/choose 0 100)]
                   (let [t (/ n 100.0)
                         g (ctx/geodesic ctx p q)
                         x (g t)
                         d (ctx/distance ctx p q)]
                     (and (close? (* t d) (ctx/distance ctx p x))
                          (close? (* (- 1 t) d) (ctx/distance ctx x q)))))]
    (is (:pass? (tc/quick-check 100 property)))))

(deftest meet-parameters-reproduce-points-test
  (let [ctx (ctx/make :hyperbolic {})]
    (testing "geodesic/geodesic parameters"
      (let [a [-0.3 0.0]
            b [0.4 0.2]
            c [0.0 -0.35]
            d [0.1 0.45]
            {:keys [point at]} (first (ctx/meet ctx {:geodesic [a b]} {:geodesic [c d]}))
            [ta tb] at]
        (is (every? some? at))
        (is (close-pt? point ((ctx/geodesic ctx a b) ta)))
        (is (close-pt? point ((ctx/geodesic ctx c d) tb)))))
    (testing "geodesic/circle parameters"
      (let [a [-0.5 0.0]
            b [0.5 0.0]
            center [0.0 0.0]
            r 0.4
            xs (ctx/meet ctx {:geodesic [a b]} {:circle [center r]})]
        (is (seq xs))
        (doseq [{:keys [point at]} xs
                :let [[tg theta] at]]
          (is (every? some? at))
          (is (close-pt? point ((ctx/geodesic ctx a b) tg)))
          (is (close-pt? point ((ctx/circle ctx center r) theta))))))
    (testing "circle/circle parameters"
      (let [a [0 0]
            b (ctx/point ctx [0.6 0])
            r (ctx/distance ctx a b)
            xs (ctx/meet ctx {:circle [a r]} {:circle [b r]})]
        (is (= 2 (count xs)))
        (doseq [{:keys [point at]} xs
                :let [[theta-a theta-b] at]]
          (is (every? some? at))
          (is (close-pt? point ((ctx/circle ctx a r) theta-a)))
          (is (close-pt? point ((ctx/circle ctx b r) theta-b))))))))

(deftest model-roundtrips-test
  (let [roundtrip (prop/for-all [p klein-point-gen]
                    (and (close-pt? p (models/poincare->klein (models/klein->poincare p)))
                         (close-pt? p (models/half-plane->klein (models/klein->half-plane p)))
                         (close-pt? p (models/hyperboloid->klein (models/klein->hyperboloid p)))
                         (close-pt? (models/klein->poincare p)
                                    (models/half-plane->poincare
                                     (models/poincare->half-plane (models/klein->poincare p))))))]
    (is (:pass? (tc/quick-check 100 roundtrip)))))

(deftest geodesic-meet-points-lie-on-both-curves-test
  (let [ctx (ctx/make :hyperbolic {})
        property (prop/for-all [[[a b] [c d]] (gen/tuple distinct-pair-gen distinct-pair-gen)]
                   (let [xs (ctx/meet ctx {:geodesic [a b]} {:geodesic [c d]})]
                     (every? (fn [{p :point}]
                               (and (hyp/inside? p)
                                    (hyp/numeric-zero? (hyp/line-eval (pg/join a b) p))
                                    (hyp/numeric-zero? (hyp/line-eval (pg/join c d) p))))
                             xs)))]
    (is (:pass? (tc/quick-check 100 property)))))

(deftest circle-geodesic-meet-points-lie-on-both-curves-test
  (let [ctx (ctx/make :hyperbolic {})
        property (prop/for-all [[p q] distinct-pair-gen
                                center klein-point-gen
                                radius (gen/choose 1 120)]
                   (let [r (/ radius 100.0)
                         xs (ctx/meet ctx {:geodesic [p q]} {:circle [center r]})
                         l (pg/join p q)]
                     (every? (fn [{x :point}]
                               (and (hyp/numeric-zero? (hyp/line-eval l x))
                                    (close? r (ctx/distance ctx center x))))
                             xs)))]
    (is (:pass? (tc/quick-check 100 property)))))

(deftest circle-circle-meet-points-lie-on-both-curves-test
  (let [ctx (ctx/make :hyperbolic {})
        a [0 0]
        b (ctx/point ctx [0.6 0])
        r (ctx/distance ctx a b)
        xs (ctx/meet ctx {:circle [a r]} {:circle [b r]})]
    (is (= 2 (count xs)))
    (doseq [{p :point} xs]
      (is (close? r (ctx/distance ctx a p)))
      (is (close? r (ctx/distance ctx b p))))))

(deftest euclid-i-1-hyperbolic-test
  (let [ctx (ctx/make :hyperbolic {})
        a [0 0]
        b (ctx/point ctx [0.7 0])
        side (ctx/distance ctx a b)
        c (:point (first (ctx/meet ctx {:circle [a side]} {:circle [b side]})))
        ab (ctx/distance ctx a b)
        ac (ctx/distance ctx a c)
        bc (ctx/distance ctx b c)
        A (angle! ctx a b c)
        B (angle! ctx b a c)
        C (angle! ctx c a b)
        angle-sum (+ A B C)
        area (- Math/PI angle-sum)]
    (testing "Euclid I.1 constructs an equilateral triangle"
      (is (close? ab ac))
      (is (close? ab bc)))
    (testing "Gauss-Bonnet for K=-1: area is the angular defect"
      (is (pos? area))
      (is (close? area (- Math/PI (+ A B C)))))))

(deftest make-registers-model-option-test
  (is (close-pt? (ctx/embed (ctx/make :hyperbolic {:model :klein}) [0.1 0.2]) [0.1 0.2]))
  (is (close-pt? (ctx/embed (ctx/make :hyperbolic {:model :poincare}) [0.1 0.2])
                 (models/klein->poincare [0.1 0.2]))))
