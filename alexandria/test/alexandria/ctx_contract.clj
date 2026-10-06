(ns alexandria.ctx-contract
  (:require [alexandria.ctx :as ctx]
            [alexandria.measure :as measure]
            [clojure.test :refer [is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [emmy.env :as e]
            [emmy.structure :as s]
            [hive-test.properties :refer [defprop-total]]
            [hive-dsl.result :as r]))

(def tol 1.0e-5)
(defn close? ([a b] (close? a b tol)) ([a b t] (<= (Math/abs (- (double a) (double b))) t)))
(defn point-close? [ctx p q] (close? (ctx/distance ctx p q) 0.0 1.0e-4))
(defn between? [x lo hi] (<= lo (double x) hi))
(def scalar (gen/choose -80 80))
(def small-scalar (gen/choose -35 35))
(defn ->tangent [[u v]] (s/up (/ u 100.0) (/ v 100.0)))
(def tangent-gen (gen/fmap ->tangent (gen/tuple scalar scalar)))
(def small-tangent-gen (gen/fmap ->tangent (gen/tuple small-scalar small-scalar)))
(def radius-gen (gen/fmap #(/ % 100.0) (gen/choose 10 120)))

;; Degenerate inputs are outside the domain of angle, meet and the triangle
;; integral: coincident points span no geodesic, collinear triples bound no
;; triangle. The shared generators exclude them, so every context's contract
;; samples only the domain; the port's own Err for degenerate input is
;; tested separately (degenerate-angle-is-err).

(def min-separation
  "Smallest tangent-length separation the contract generators produce."
  0.05)

(def min-twice-area
  "Smallest |cross product| of a generated triangle's tangent edges: keeps
   the three vertices off a common geodesic through the origin chart."
  1.0e-3)

(defn- tangent-gap [t u]
  (Math/hypot (- (double (nth t 0)) (double (nth u 0)))
              (- (double (nth t 1)) (double (nth u 1)))))

(defn origin "The zero tangent." [] (s/up 0 0))

(defn separated?
  "True when every pair of tangents is at least min-separation apart."
  [& ts]
  (every? (fn [[t u]] (>= (tangent-gap t u) min-separation))
          (for [[i t] (map-indexed vector ts) [j u] (map-indexed vector ts) :when (< i j)] [t u])))

(defn non-collinear?
  "True when the tangents a b c are not on one line in the tangent chart."
  [a b c]
  (let [[ax ay] (map double a) [bx by] (map double b) [cx cy] (map double c)]
    (>= (Math/abs (- (* (- bx ax) (- cy ay)) (* (- by ay) (- cx ax))))
        min-twice-area)))

(defn distinct-pair-gen
  "Two tangents from g, separated."
  [g]
  (gen/such-that (fn [[a b]] (separated? a b)) (gen/tuple g g) 200))

(defn triangle-gen
  "Three tangents from g, pairwise separated and not collinear."
  [g]
  (gen/such-that (fn [[a b c]] (and (separated? a b c) (non-collinear? a b c)))
                 (gen/tuple g g g) 200))

(defn wedge-gen
  "Two tangents from g that, with the origin, form a proper triangle."
  [g]
  (gen/such-that (fn [[b c]] (and (separated? (origin) b c) (non-collinear? (origin) b c)))
                 (gen/tuple g g) 200))

(defn angle!
  "The angle as a number for test arithmetic; fails the test on Err."
  [ctx v p q]
  (let [res (ctx/angle ctx v p q)]
    (is (r/ok? res) (pr-str res))
    (:ok res)))

(def gauss-bonnet-tolerance-scale 6.0e-4)

(defn embed-coords [ctx p]
  (vec (ctx/embed ctx p)))

(defn left-of-geodesic? [ctx a b p]
  (let [ea (embed-coords ctx a)
        eb (embed-coords ctx b)
        ep (embed-coords ctx p)]
    (if (= 3 (count ea))
      (let [[ax ay az] ea
            [bx by bz] eb
            [px py pz] ep
            nx (- (* ay bz) (* az by))
            ny (- (* az bx) (* ax bz))
            nz (- (* ax by) (* ay bx))]
        (pos? (+ (* nx px) (* ny py) (* nz pz))))
      (let [[ax ay] ea
            [bx by] eb
            [px py] ep]
        (pos? (- (* (- bx ax) (- py ay)) (* (- by ay) (- px ax))))))))

(defn on-geodesic? [ctx a b m]
  (let [p (:point m)
        t (first (:at m))
        d (ctx/distance ctx a b)]
    (and (point-close? ctx p ((ctx/geodesic ctx a b) t))
         (close? (+ (double (ctx/distance ctx a p))
                    (double (ctx/distance ctx p b)))
                 d 1.0e-4))))

(defn on-circle? [ctx c r m idx]
  (let [p (:point m)
        t (nth (:at m) idx)]
    (and (close? (ctx/distance ctx c p) r 1.0e-4)
         (point-close? ctx p ((ctx/circle ctx c r) t)))))

(defmacro defcontract [name ctx-form opts]
  (let [n #(symbol (str name "-" %))
        num-tests (or (:num-tests opts) 50)
        tolerance (or (:tolerance opts) 1.0e-4)]
    `(do
       (def ~(symbol (str name "-ctx")) ~ctx-form)
       (defprop-total ~(n "total-point") #(ctx/point ~(symbol (str name "-ctx")) %) tangent-gen {:num-tests ~num-tests})
       (defspec ~(n "distance-symmetric-triangle") ~num-tests
         (prop/for-all [ta# tangent-gen tb# tangent-gen tc# tangent-gen]
           (let [ctx# ~(symbol (str name "-ctx"))
                 a# (ctx/point ctx# ta#)
                 b# (ctx/point ctx# tb#)
                 c# (ctx/point ctx# tc#)
                 ab# (ctx/distance ctx# a# b#)
                 ba# (ctx/distance ctx# b# a#)]
             (and (close? ab# ba# ~tolerance)
                  (<= (double ab#) (+ (double (ctx/distance ctx# a# c#))
                                      (double (ctx/distance ctx# c# b#))
                                      ~tolerance))))))
       (defspec ~(n "point-reaches-tangent-length") ~num-tests
         (prop/for-all [t# tangent-gen]
           (let [ctx# ~(symbol (str name "-ctx"))
                 p# (ctx/point ctx# t#)]
             (close? (ctx/distance ctx# (ctx/point ctx# (s/up 0 0)) p#)
                     (Math/sqrt (+ (* (double (nth t# 0)) (double (nth t# 0)))
                                   (* (double (nth t# 1)) (double (nth t# 1)))))
                     ~tolerance))))
       (defspec ~(n "geodesic-endpoints-arclength") ~num-tests
         (prop/for-all [[ta# tb#] (distinct-pair-gen tangent-gen) t0# (gen/choose 0 100)]
           (let [ctx# ~(symbol (str name "-ctx"))
                 a# (ctx/point ctx# ta#)
                 b# (ctx/point ctx# tb#)
                 t# (/ t0# 100.0)
                 g# (ctx/geodesic ctx# a# b#)
                 p# (g# t#)
                 d# (ctx/distance ctx# a# b#)]
             (and (point-close? ctx# a# (g# 0))
                  (point-close? ctx# b# (g# 1))
                  (close? (ctx/distance ctx# a# p#) (* t# (double d#)) ~tolerance)))))
       (defspec ~(n "circle-points-at-distance") ~num-tests
         (prop/for-all [tc# tangent-gen r# radius-gen a# (gen/choose 0 628)]
           (let [ctx# ~(symbol (str name "-ctx"))
                 c# (ctx/point ctx# tc#)
                 r# (min r# 1.0)
                 p# ((ctx/circle ctx# c# r#) (/ a# 100.0))]
             (close? (ctx/distance ctx# c# p#) r# ~tolerance))))
       (defspec ~(n "angle-range") ~num-tests
         (prop/for-all [[tv# tp# tq#] (triangle-gen tangent-gen)]
           (let [ctx# ~(symbol (str name "-ctx"))
                 v# (ctx/point ctx# tv#)
                 p# (ctx/point ctx# tp#)
                 q# (ctx/point ctx# tq#)
                 a# (angle! ctx# v# p# q#)]
             (between? a# 0.0 Math/PI))))
       (defspec ~(n "degenerate-angle-is-err") ~num-tests
         (prop/for-all [[tv# tq#] (distinct-pair-gen tangent-gen)]
           (let [ctx# ~(symbol (str name "-ctx"))
                 v# (ctx/point ctx# tv#)
                 q# (ctx/point ctx# tq#)
                 at-p# (ctx/angle ctx# v# v# q#)
                 at-q# (ctx/angle ctx# v# q# v#)]
             (and (r/err? at-p#) (r/err? at-q#)
                  (= :ctx/degenerate-angle (get-in at-p# [:error :alexandria/error]))
                  (= :p (get-in at-p# [:error :coincides]))
                  (= :q (get-in at-q# [:error :coincides]))))))
       (defspec ~(n "meet-i1-circles") 20
         (prop/for-all [r# radius-gen]
           (let [ctx# ~(symbol (str name "-ctx"))
                 a# (ctx/point ctx# (s/up 0 0))
                 b# (ctx/point ctx# (s/up (min r# 1.0) 0))
                 side# (ctx/distance ctx# a# b#)
                 xs# (ctx/meet ctx# {:circle [a# side#]} {:circle [b# side#]})]
             (and (= 2 (count xs#))
                  (left-of-geodesic? ctx# a# b# (:point (first xs#)))
                  (every? #(and (on-circle? ctx# a# side# % 0)
                                (on-circle? ctx# b# side# % 1)) xs#)))))
       (defspec ~(n "meet-geodesic-geodesic-on-both") 20
         (prop/for-all [[ta# tb#] (wedge-gen small-tangent-gen)]
           (let [ctx# ~(symbol (str name "-ctx"))
                 a# (ctx/point ctx# (s/up 0 0))
                 b# (ctx/point ctx# ta#)
                 c# (ctx/point ctx# tb#)
                 xs# (ctx/meet ctx# {:geodesic [a# b#]} {:geodesic [a# c#]})]
             (boolean
              (some #(and (point-close? ctx# a# (:point %))
                          (on-geodesic? ctx# a# b# %)
                          (on-geodesic? ctx# a# c# (update % :at (fn [[u# v#]] [v# u#])))) xs#)))))
       (defspec ~(n "meet-geodesic-circle-on-both") 20
         (prop/for-all [ta# (gen/such-that #(separated? (origin) %) small-tangent-gen 200)]
           (let [ctx# ~(symbol (str name "-ctx"))
                 a# (ctx/point ctx# (s/up 0 0))
                 b# (ctx/point ctx# ta#)
                 r# (ctx/distance ctx# a# b#)
                 xs# (ctx/meet ctx# {:geodesic [a# b#]} {:circle [a# r#]})]
             (boolean
              (some #(and (point-close? ctx# b# (:point %))
                          (on-geodesic? ctx# a# b# %)
                          (on-circle? ctx# a# r# % 1)) xs#)))))
       (defspec ~(n "gauss-bonnet-small-triangles") 30
         (prop/for-all [[ta# tb# tc#] (triangle-gen small-tangent-gen)]
           (let [ctx# ~(symbol (str name "-ctx"))
                 a# (ctx/point ctx# ta#)
                 b# (ctx/point ctx# tb#)
                 c# (ctx/point ctx# tc#)
                 A# (angle! ctx# a# b# c#)
                 B# (angle! ctx# b# c# a#)
                 C# (angle! ctx# c# a# b#)
                 excess# (- (+ A# B# C#) Math/PI)
                 integral# (measure/geodesic-triangle-integral ctx# a# b# c#)
                 tol# (+ ~tolerance (* gauss-bonnet-tolerance-scale
                                       (+ (Math/abs (double integral#))
                                          (Math/abs (double excess#)))))]
             (close? excess# integral# tol#)))))))
