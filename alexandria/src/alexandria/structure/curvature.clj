(ns alexandria.structure.curvature
  "Domain stratum: the plane of constant curvature K, one value object for
   Euclid (K = 0), the sphere (K > 0) and Lobachevsky's plane (K < 0).

   Model: the conformal disk chart, points z = [x y], metric
   ds = 2|dz| / (1 + K|z|^2). K = -1 is Poincare's disk (|z| < 1), K = +1
   the unit sphere seen by stereographic projection, K = 0 the plane at
   half scale. Its motions are alexandria.structure.groups/moebius-disk K.

   The curvature-K trigonometry (each a function of K and a length):
     cs  cos(sqrt K r)            1      cosh(sqrt(-K) r)
     sn  sin(sqrt K r)/sqrt K     r      sinh(sqrt(-K) r)/sqrt(-K)
     tn  tan(sqrt K r)/sqrt K     r      tanh(sqrt(-K) r)/sqrt(-K)
     vers (1 - cs r)/K = 2 sn(r/2)^2, r^2/2 at K = 0
   and their inverses asn atn arc-vers.

   On it: distance, angle, geodesic, point (exponential map at the
   origin), circle, the law of cosines (side from two sides and the angle,
   angle from three sides), the angle sum, its excess and the area
   excess / K (Gauss-Bonnet).

   K is an Emmy value. A number K picks its case exactly (so K = -1 on
   symbols gives cosh and sinh for TeX); a symbolic K (a slider compiled
   by raster, or a symbol for TeX) gives the branch-free blend of the two
   curved cases, continuous through K = 0, error O(1e-12).

   Relation to alexandria.ctx: a ctx Surface is a chart for constructions
   (meet, circle, embed); this is the metric structure those contexts
   share when their curvature is constant. `of-context` reads the
   SpaceForm of ctx.plane, ctx.sphere and ctx.hyperbolic, and the
   conformance suite checks the law of cosines on their own distances and
   angles. Contract: test/alexandria/structure/curvature_contract.clj."
  (:require [alexandria.ctx :as ctx]
            [alexandria.structure.arith :as a]
            [alexandria.structure.group :as g]
            [alexandria.structure.groups :as gs]
            [emmy.env :as e]
            [malli.core :as m]))

;; ---------------------------------------------------------------------------
;; The value object

(def K-schema
  "A curvature: a real number or an Emmy symbolic value."
  [:fn {:error/message "curvature must be a number or an Emmy symbolic value"}
   (fn [K] (or (number? K) (symbol? K) (seq? K) (e/numerical? K)))])

(defrecord SpaceForm [K])

(defn space-form
  "The plane of constant curvature K (validated with malli)."
  [K]
  (m/assert K-schema K)
  (->SpaceForm K))

(def euclid "K = 0." (space-form 0))
(def sphere "K = +1." (space-form 1))
(def lobachevsky "K = -1." (space-form -1))

(defn curvature "K of the space form." [G] (:K G))

(defn motions "The group of motions of the space form, acting on its chart." [G]
  (gs/moebius-disk (:K G)))

;; ---------------------------------------------------------------------------
;; Curvature-K trigonometry

(def ^:private blend-eps "Keeps sqrt|K| away from 0 in the blended form." 1e-12)

(defn- by-sign
  "f+ of sqrt K when K > 0, f0 when K = 0, f- of sqrt(-K) when K < 0; for a
   symbolic K the blend of the guarded g+ and g- (default f+ and f-),
   weighted by the sign of K: both branches are evaluated, so each must
   stay finite where the other holds."
  ([K f+ f0 f-] (by-sign K f+ f0 f- f+ f-))
  ([K f+ f0 f- g+ g-]
   (if (number? K)
     (cond (pos? K) (f+ (e/sqrt K))
           (zero? K) (f0)
           :else (f- (e/sqrt (e/- 0 K))))
     (let [mag (e/+ (e/abs K) blend-eps)
           u (e/sqrt mag)
           s (e/divide K mag)]
       (e/+ (e/* (e/divide (e/+ 1 s) 2) (g+ u))
            (e/* (e/divide (e/- 1 s) 2) (g- u)))))))

(defn- atanh* "atanh x = log((1 + x)/(1 - x))/2, |x| < 1." [x]
  (e/* 0.5 (e/log (e/divide (e/+ 1 x) (e/- 1 x)))))
(defn- asin* "asin x = 2 atan(x / (1 + sqrt(1 - x^2))), |x| <= 1." [x]
  (e/* 2 (e/atan (e/divide x (e/+ 1 (e/sqrt (e/- 1 (e/square x))))))))

(defn cs "cos_K r." [K r]
  (by-sign K #(e/cos (e/* % r)) (constantly 1) #(e/cosh (e/* % r))))

(defn sn "sin_K r." [K r]
  (by-sign K #(e/divide (e/sin (e/* % r)) %) (constantly r) #(e/divide (e/sinh (e/* % r)) %)))

(defn tn "tan_K r." [K r]
  (by-sign K #(e/divide (e/tan (e/* % r)) %) (constantly r) #(e/divide (e/tanh (e/* % r)) %)))

(defn atn "The inverse of tn: arctan_K x." [K x]
  (by-sign K #(e/divide (e/atan (e/* % x)) %) (constantly x) #(e/divide (atanh* (e/* % x)) %)
           #(e/divide (e/atan (e/* % x)) %)
           ;; |sqrt(-K) x| held below 1: scaled by min(1, c / |sqrt(-K) x|)
           #(let [y (e/* % x)]
              (e/divide (atanh* (e/* y (a/at-most 1 (e/divide 0.999999999999 (e/+ (e/abs y) 1e-300))))) %))))

(defn asn "The inverse of sn on [0, pi/(2 sqrt K)]: arcsin_K x." [K x]
  (by-sign K #(e/divide (asin* (e/* % x)) %) (constantly x) #(e/divide (a/asinh* (e/* % x)) %)
           #(e/divide (asin* (a/clamp (e/* % x) -1 1)) %)
           #(e/divide (a/asinh* (e/* % x)) %)))

(defn vers "versin_K r = (1 - cs r)/K = 2 sn(r/2)^2." [K r]
  (e/* 2 (e/square (sn K (e/divide r 2)))))

(defn arc-vers "The inverse of vers on lengths in [0, pi/sqrt K]." [K v]
  (e/* 2 (asn K (e/sqrt (a/at-least (e/divide v 2) 0)))))

;; ---------------------------------------------------------------------------
;; The chart

(defn- modulus [z] (e/sqrt (e/+ (a/norm2 z) 1e-300)))

(defn to-origin
  "The motion taking p to 0, applied to z: (z - p)/(1 + K conj(p) z)
   (the inverse of moebius-translation p in (motions G))."
  [G p z]
  (a/c-div (a/c- z p) (a/c+ a/one (a/cscale (:K G) (a/c* (a/cconj p) z)))))

(defn from-origin
  "The motion taking 0 to p, applied to w: (w + p)/(1 - K conj(p) w)."
  [G p w]
  (a/c-div (a/c+ w p) (a/c- a/one (a/cscale (:K G) (a/c* (a/cconj p) w)))))

(defn distance "The geodesic distance from p to q: 2 atn_K |T_p(q)|." [G p q]
  (e/* 2 (atn (:K G) (modulus (to-origin G p q)))))

(defn angle
  "The angle in [0 pi] at v between the geodesics to p and to q (the chart
   is conformal: the Euclidean angle at 0 after moving v there)."
  [G v p q]
  (let [[ux uy :as u] (to-origin G v p) [wx wy :as w] (to-origin G v q)]
    (a/angle2 (e/abs (e/- (e/* ux wy) (e/* uy wx))) (e/+ (e/* ux wx) (e/* uy wy)))))

(defn point
  "The point at the end of the geodesic from the origin with initial
   velocity tangent [u v] (|tangent| = its length): the exponential map."
  [G [u v]]
  (let [r (modulus [u v])]
    (a/cscale (e/divide (tn (:K G) (e/divide r 2)) r) [u v])))

(defn geodesic
  "The geodesic from p to q as (fn [t] point), t = 0 at p, 1 at q, the
   distance from p proportional to t."
  [G p q]
  (let [w (to-origin G p q)
        d (distance G p q)
        n (modulus w)]
    (fn [t] (from-origin G p (a/cscale (e/divide (tn (:K G) (e/divide (e/* t d) 2)) n) w)))))

(defn circle "The circle of radius r about c, as (fn [theta] point)." [G c r]
  (fn [theta] (from-origin G c (a/cscale (tn (:K G) (e/divide r 2)) (a/cis theta)))))

;; ---------------------------------------------------------------------------
;; Trigonometry of the triangle

(defn law-of-cosines-side
  "The side c opposite the angle C between sides a and b:
   vers c = vers(a - b) + sn a sn b (1 - cos C)."
  [G a b C]
  (let [K (:K G)]
    (arc-vers K (e/+ (vers K (e/- a b)) (e/* (sn K a) (sn K b) (e/- 1 (e/cos C)))))))

(defn law-of-cosines-angle
  "The angle C opposite c, from the three sides:
   1 - cos C = (vers c - vers(a - b)) / (sn a sn b)."
  [G a b c]
  (let [K (:K G)]
    (a/acos* (e/- 1 (e/divide (e/- (vers K c) (vers K (e/- a b)))
                              (e/+ (e/* (sn K a) (sn K b)) 1e-300))))))

(defn triangle
  "The triangle with vertices p q r: {:a :b :c} the sides opposite each
   vertex, {:A :B :C} the angles, :S their sum, :excess S - pi."
  [G p q r]
  (let [A (angle G p q r) B (angle G q r p) C (angle G r p q)
        S (e/+ A B C)]
    {:a (distance G q r) :b (distance G r p) :c (distance G p q)
     :A A :B B :C C :S S :excess (e/- S Math/PI)}))

(defn area
  "The area of a triangle of angle sum S, by Gauss-Bonnet: (S - pi)/K
   (K /= 0; at K = 0 the angles do not determine the area)."
  [G S]
  (e/divide (e/- S Math/PI) (:K G)))

(defn angle-of-parallelism
  "Lobachevsky's Pi(d) for K < 0: the angle between the perpendicular of
   length d to a line and the limiting parallel, 2 atan e^(-sqrt(-K) d).
   pi/2 at K = 0 (Euclid's single parallel)."
  [G d]
  (let [K (:K G)]
    (if (and (number? K) (zero? K))
      (/ Math/PI 2)
      (e/* 2 (e/atan (e/exp (e/- 0 (e/* (e/sqrt (e/- 0 K)) d))))))))

;; ---------------------------------------------------------------------------
;; The contexts of alexandria.ctx with constant curvature

(defn of-context
  "The SpaceForm sharing ctx's constant curvature (read at ctx's origin)."
  [c]
  (space-form (ctx/curvature c (ctx/point c [0 0]))))
