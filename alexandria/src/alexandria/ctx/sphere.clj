(ns alexandria.ctx.sphere
  "Closed-form sphere context.

   Reuse searched before writing: Emmy already provides the S2 chart
   (emmy.calculus.manifold/S2-spherical), its metric
   (emmy.calculus.metric/S2-metric), and generic arithmetic/vector operations
   (emmy.generic and emmy.structure). This namespace only supplies the
   Alexandria Surface port operations in terms of those primitives."
  (:require [alexandria.ctx :as ctx]
            [emmy.calculus.manifold :as manifold]
            [emmy.calculus.metric :as metric]
            [emmy.env :as e]
            [emmy.generic :as g]
            [emmy.structure :as s]))

(def eps 1.0e-9)
(defn- sq [x] (g/* x x))
(defn- clamp [x lo hi]
  (if (number? x)
    (max lo (min hi x))
    x))
(defn- near-zero? [x] (< (Math/abs (double x)) eps))
(defn- norm3 [v] (g/sqrt (g/dot-product v v)))
(defn- unit3 [v] (let [n (norm3 v)] (g// v n)))
(defn- antipode3 [v] (g/* -1 v))
(defn- R [{:keys [radius]}] radius)
(defn- theta [p] (nth p 0))
(defn- phi [p] (nth p 1))
(defn- ->p [theta phi] (s/up theta phi))

(defn- embed-unit [p]
  (let [th (theta p) ph (phi p)]
    (s/up (g/* (g/sin th) (g/cos ph))
          (g/* (g/sin th) (g/sin ph))
          (g/cos th))))

(defn- coords [v]
  (let [[x y z] v]
    (->p (g/acos (clamp z -1.0 1.0)) (g/atan y x))))

(defn- scaled-metric [radius]
  (fn [v w] (g/* (sq radius) (metric/S2-metric v w))))

(defn- tangent->point [radius tangent]
  (let [u (nth tangent 0) v (nth tangent 1)
        len (g/sqrt (g/+ (sq u) (sq v)))]
    (if (near-zero? len) (->p 0 0) (->p (g// len radius) (g/atan v u)))))

(defn- angular-distance [a b]
  (g/acos (clamp (g/dot-product a b) -1.0 1.0)))

(defn- slerp-unit [a b t]
  (let [omega (angular-distance a b)]
    (if (near-zero? omega)
      a
      (let [so (g/sin omega)]
        (g/+ (g/* (g// (g/sin (g/* (g/- 1 t) omega)) so) a)
             (g/* (g// (g/sin (g/* t omega)) so) b))))))

(defn- gc-normal [p q] (unit3 (g/cross-product (embed-unit p) (embed-unit q))))

(defn- tangent-at [v p]
  (let [n (embed-unit v) x (embed-unit p)]
    (unit3 (g/- x (g/* (g/dot-product x n) n)))))

(defn- angle-at [v p q]
  (let [a (tangent-at v p) b (tangent-at v q)]
    (g/acos (clamp (g/dot-product a b) -1.0 1.0))))

(defn- oriented-param [basis-u basis-v x]
  (g/atan (g/dot-product x basis-v) (g/dot-product x basis-u)))

(defn- gc-param [p q x]
  (let [u (embed-unit p)
        n (gc-normal p q)
        v (unit3 (g/cross-product n u))
        omega (angular-distance (embed-unit p) (embed-unit q))]
    (g// (oriented-param u v x) omega)))

(defn- circle-param [c x]
  (let [n (embed-unit c)
        ref (if (< (Math/abs (double (nth n 2))) 0.9) (s/up 0 0 1) (s/up 1 0 0))
        u (unit3 (g/cross-product ref n))
        v (g/cross-product n u)]
    (oriented-param u v x)))

(defn- result [p ta tb] {:point p :at [ta tb]})

(defn- meet-gc-gc [a b c d]
  (if (or (near-zero? (angular-distance (embed-unit a) (embed-unit b)))
          (near-zero? (angular-distance (embed-unit c) (embed-unit d))))
    []
    (let [x (unit3 (g/cross-product (gc-normal a b) (gc-normal c d)))
          y (antipode3 x)]
      [(result (coords x) (gc-param a b x) (gc-param c d x))
       (result (coords y) (gc-param a b y) (gc-param c d y))])))

(defn- meet-circle-circle [ctx c1 r1 c2 r2]
  (let [R (R ctx) n1 (embed-unit c1) n2 (embed-unit c2)
        k1 (g/cos (g// r1 R)) k2 (g/cos (g// r2 R))
        d (g/dot-product n1 n2) den (g/- 1 (sq d))]
    (if (near-zero? den)
      []
      (let [a (g// (g/- k1 (g/* d k2)) den)
            b (g// (g/- k2 (g/* d k1)) den)
            base (g/+ (g/* a n1) (g/* b n2))
            dir (unit3 (g/cross-product n1 n2))
            h2 (g/- 1 (g/dot-product base base))]
        (if (< (double h2) (- eps))
          []
          (let [h (g/sqrt (max 0.0 (double h2)))
                x (unit3 (g/+ base (g/* h dir)))
                y (unit3 (g/- base (g/* h dir)))
                one (result (coords x) (circle-param c1 x) (circle-param c2 x))
                two (result (coords y) (circle-param c1 y) (circle-param c2 y))]
            [one two]))))))

(defn- meet-gc-circle [ctx a b c r]
  (if (near-zero? (angular-distance (embed-unit a) (embed-unit b)))
    []
    (let [n (gc-normal a b) m (embed-unit c) k (g/cos (g// r (R ctx)))
          den (g/- 1 (sq (g/dot-product n m)))]
      (if (near-zero? den)
        []
        (let [base (g/* (g// k den) (g/- m (g/* (g/dot-product n m) n)))
              dir (unit3 (g/cross-product n m))
              h2 (g/- 1 (g/dot-product base base))]
          (if (< (double h2) (- eps))
            []
            (let [h (g/sqrt (max 0.0 (double h2)))
                  xs [(unit3 (g/+ base (g/* h dir))) (unit3 (g/- base (g/* h dir)))]]
              (mapv (fn [x] (result (coords x) (gc-param a b x) (circle-param c x))) xs))))))))

(defrecord Sphere [radius elliptic?]
  ctx/Surface
  (metric [_] (scaled-metric radius))
  (point [_ tangent] (tangent->point radius tangent))
  (geodesic [_ p q]
    (let [a (embed-unit p) b (embed-unit q)] (fn [t] (coords (slerp-unit a b t)))))
  (distance [_ p q]
    (let [d (g/* radius (angular-distance (embed-unit p) (embed-unit q)))]
      (if elliptic? (min d (g/- (g/* e/pi radius) d)) d)))
  (circle [_ center r]
    (fn [alpha]
      (let [c (embed-unit center)
            ref (if (< (Math/abs (double (nth c 2))) 0.9) (s/up 0 0 1) (s/up 1 0 0))
            u (unit3 (g/cross-product ref c))
            v (g/cross-product c u)
            rho (g// r radius)]
        (coords (g/+ (g/* (g/cos rho) c)
                     (g/* (g/sin rho) (g/+ (g/* (g/cos alpha) u) (g/* (g/sin alpha) v))))))))
  (-angle [_ v p q] (angle-at v p q))
  (meet [this curve-a curve-b]
    (let [{ga :geodesic ca :circle} curve-a {gb :geodesic cb :circle} curve-b]
      (cond
        (and ga gb) (apply meet-gc-gc (concat ga gb))
        (and ca cb) (meet-circle-circle this (first ca) (second ca) (first cb) (second cb))
        (and ga cb) (meet-gc-circle this (first ga) (second ga) (first cb) (second cb))
        (and ca gb) (mapv (fn [m] (update m :at (fn [[a b]] [b a])))
                          (meet-gc-circle this (first gb) (second gb) (first ca) (second ca)))
        :else [])))
  (curvature [_ _p] (g// 1 (sq radius)))
  (embed [_ p] (g/* radius (embed-unit p))))

(defmethod ctx/make :sphere [_ opts]
  (map->Sphere {:radius (double (or (:radius opts) (:R opts) 1.0))
                :elliptic? (boolean (:elliptic? opts))}))

(def emmy-S2-chart manifold/S2-spherical)
(def emmy-S2-metric metric/S2-metric)
