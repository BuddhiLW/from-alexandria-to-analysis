(ns alexandria.ctx.sphere-test
  (:require [alexandria.ctx :as ctx]
            [alexandria.ctx-contract :refer [defcontract close? angle!]]
            [alexandria.ctx.sphere]
            [alexandria.measure :as measure]
            [clojure.test :refer [deftest is]]
            [emmy.expression :as x]
            [emmy.generic :as g]
            [emmy.simplify :as simplify]
            [emmy.structure :as s]))

(defcontract sphere (ctx/make :sphere {}) {:radius 1.0})

(deftest euclid-i1-on-sphere-has-positive-excess
  (let [ctx (ctx/make :sphere {:radius 1.0})
        a (ctx/point ctx (s/up 0 0))
        b (ctx/point ctx (s/up 0.7 0))
        side (ctx/distance ctx a b)
        xs (ctx/meet ctx {:circle [a side]} {:circle [b side]})
        c (:point (first xs))
        ab (ctx/distance ctx a b)
        bc (ctx/distance ctx b c)
        ca (ctx/distance ctx c a)
        A (angle! ctx a b c)
        B (angle! ctx b c a)
        C (angle! ctx c a b)
        excess (- (+ A B C) Math/PI)
        integral (measure/geodesic-triangle-integral ctx a b c)]
    (is (= 2 (count xs)))
    (is (close? ab bc 1.0e-5))
    (is (close? bc ca 1.0e-5))
    (is (pos? excess))
    (is (close? excess integral 5.0e-4))))

(deftest symbolic-distance-stays-symbolic-and-substitutes
  (let [ctx (ctx/make :sphere {:radius 2.0})
        p (s/up 'theta 'phi)
        q (s/up 0 0)
        expr (ctx/distance ctx p q)
        substituted (simplify/simplify-expression (-> expr (x/substitute 'theta 0.4) (x/substitute 'phi 0.2)))
        numeric (ctx/distance ctx (s/up 0.4 0.2) q)]
    (is (not (number? expr)))
    (is (= (g/freeze numeric) (g/freeze substituted)))))
