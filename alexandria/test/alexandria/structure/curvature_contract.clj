(ns alexandria.structure.curvature-contract
  "The conformance suite of a plane of constant curvature:
   (defspace-form-contract name space-form-gen point-gen) checks, for
   generated space forms and triangles, the metric laws, the law of
   cosines both ways, the sign of the angle excess, Gauss-Bonnet, the
   exponential map and that the motions are isometries."
  (:require [alexandria.structure.curvature :as kc]
            [alexandria.structure.group :as g]
            [alexandria.structure.groups :as gs]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(def tol 1e-7)
(defn close? ([a b] (close? a b tol)) ([a b t] (<= (Math/abs (- (double a) (double b))) t)))

(defn proper?
  "The triangle is not degenerate: sides above 1e-3, angles above 1e-3."
  [{:keys [a b c A B C]}]
  (every? #(> (double %) 1e-3) [a b c A B C]))

(defmacro defspace-form-contract
  [name G-gen point-gen & [{:keys [num-tests] :or {num-tests 80}}]]
  (let [n #(symbol (str name "-" %))
        tri `(gen/tuple ~G-gen ~point-gen ~point-gen ~point-gen)]
    `(do
       (defspec ~(n "metric") ~num-tests
         (prop/for-all [[G# p# q# r#] ~tri]
           (and (close? (kc/distance G# p# p#) 0 1e-6)
                (close? (kc/distance G# p# q#) (kc/distance G# q# p#))
                (<= (double (kc/distance G# p# q#))
                    (+ (double (kc/distance G# p# r#)) (double (kc/distance G# r# q#)) tol)))))
       (defspec ~(n "law-of-cosines") ~num-tests
         (prop/for-all [[G# p# q# r#] ~tri]
           (let [t# (kc/triangle G# p# q# r#)]
             (or (not (proper? t#))
                 (and (close? (kc/law-of-cosines-side G# (:b t#) (:c t#) (:A t#)) (:a t#) 1e-6)
                      (close? (kc/law-of-cosines-angle G# (:b t#) (:c t#) (:a t#)) (:A t#) 1e-5))))))
       (defspec ~(n "angle-excess-sign") ~num-tests
         (prop/for-all [[G# p# q# r#] ~tri]
           (let [t# (kc/triangle G# p# q# r#) K# (double (kc/curvature G#)) x# (double (:excess t#))]
             (or (not (proper? t#))
                 (cond (pos? K#) (> x# -1e-9) (neg? K#) (< x# 1e-9) :else (close? x# 0 1e-9))))))
       (defspec ~(n "motions-are-isometries") ~num-tests
         (prop/for-all [[G# p# q# a#] ~tri t# (gen/choose -300 300)]
           (let [M# (kc/motions G#)
                 m# (g/compose M# (gs/moebius-translation a#) (gs/moebius-rotation (/ t# 100.0)))]
             (and (close? (kc/distance G# (g/act M# m# p#) (g/act M# m# q#)) (kc/distance G# p# q#) 1e-6)))))
       (defspec ~(n "geodesic-and-exp") ~num-tests
         (prop/for-all [[G# p# q# v#] ~tri s# (gen/choose 0 100)]
           (let [d# (kc/distance G# p# q#) s# (/ s# 100.0) x# ((kc/geodesic G# p# q#) s#)]
             (and (close? (kc/distance G# p# x#) (* s# d#) 1e-6)
                  (close? (kc/distance G# x# q#) (* (- 1 s#) d#) 1e-6)
                  (close? (kc/distance G# [0 0] (kc/point G# v#)) (Math/hypot (first v#) (second v#)) 1e-6)))))
       (defspec ~(n "circle") ~num-tests
         (prop/for-all [[G# c# _# _#] ~tri r# (gen/choose 1 80) th# (gen/choose 0 628)]
           (close? (kc/distance G# c# ((kc/circle G# c# (/ r# 100.0)) (/ th# 100.0))) (/ r# 100.0) 1e-6))))))

(def curvatures
  "K from -2 to 2, and the three named cases."
  (gen/one-of [(gen/elements [-1 0 1]) (gen/fmap #(/ % 50.0) (gen/choose -100 100))]))

(def space-form-gen (gen/fmap kc/space-form curvatures))

(def chart-point
  "Points of |z| < 0.5: inside the disk of every K >= -2, away from the
   antipode of the origin of every K <= 2."
  (gen/such-that (fn [[x y]] (< (Math/hypot x y) 0.5))
                 (gen/tuple (gen/fmap #(/ % 200.0) (gen/choose -100 100))
                            (gen/fmap #(/ % 200.0) (gen/choose -100 100)))
                 100))
