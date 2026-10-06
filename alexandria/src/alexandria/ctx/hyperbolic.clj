(ns alexandria.ctx.hyperbolic
  "Hyperbolic plane Surface implementation, curvature -1.

   The Klein model is Cayley-Klein geometry inside the absolute conic
   x²+y²=1. We reuse desargues.geometry.projective for homogeneous
   coordinates, joins, meets, incidence, conics and cross-ratio, and Emmy for
   generic arithmetic. Model conversion functions live in
   alexandria.ctx.hyperbolic.models."
  (:require [alexandria.ctx :as ctx]
            [alexandria.ctx.hyperbolic.models :as models]
            [desargues.geometry.projective :as pg]
            [emmy.calculus.coordinate :as coord]
            [emmy.calculus.form-field :as ff]
            [emmy.calculus.manifold :as m]
            [emmy.env :as e]))

(def absolute-conic
  "The unit-circle absolute x²+y²-w²=0."
  (pg/conic [1 0 1 0 0 -1]))

(def eps 1e-9)

(defn sq [x] (e/* x x))
(defn dot [[x y] [u v]] (e/+ (e/* x u) (e/* y v)))
(defn subp [p q] (mapv e/- p q))
(defn addp [p q] (mapv e/+ p q))
(defn scalep [a p] (mapv #(e/* a %) p))
(defn norm2 [p] (dot p p))
(defn norm [p] (e/sqrt (norm2 p)))

(defn numeric-zero? [x]
  (< (Math/abs (double x)) eps))

(defn inside? [p]
  (< (double (norm2 p)) 1.0))

(defn boundary-intersections
  "Intersections of the projective chord through p and q with the absolute.
   Returned in affine Klein coordinates."
  [p q]
  (let [d (subp q p)
        a (norm2 d)
        b (e/* 2 (dot p d))
        c (e/- (norm2 p) 1)
        disc (e/- (sq b) (e/* 4 a c))
        s (e/sqrt disc)
        t1 (e// (e/- (e/- b) s) (e/* 2 a))
        t2 (e// (e/+ (e/- b) s) (e/* 2 a))]
    [(addp p (scalep t1 d)) (addp p (scalep t2 d))]))

(defn distance-klein
  "Cayley-Klein distance, 1/2 |log cross-ratio|."
  [p q]
  (if (every? numeric-zero? (subp p q))
    0
    (let [[a b] (boundary-intersections p q)
          cr (pg/cross-ratio a b p q)]
      (e/* 1/2 (e/abs (e/log cr))))))

(defn hyperboloid-dot [[X Y T] [U V W]]
  (e/- (e/* T W) (e/* X U) (e/* Y V)))

(defn distance-hyperboloid [p q]
  (e/acosh (hyperboloid-dot (models/klein->hyperboloid p)
                            (models/klein->hyperboloid q))))

(defn geodesic-point
  "Arc-length proportional point on the Klein chord from p to q."
  [p q t]
  (let [d (distance-hyperboloid p q)]
    (if (numeric-zero? d)
      p
      (let [P (models/klein->hyperboloid p)
            Q (models/klein->hyperboloid q)
            denom (e/sinh d)
            a (e// (e/sinh (e/* (e/- 1 t) d)) denom)
            b (e// (e/sinh (e/* t d)) denom)]
        (models/hyperboloid->klein
         (mapv e/+ (scalep a P) (scalep b Q)))))))

(defn geodesic-parameter
  "Signed arc-length parameter of x on the oriented geodesic from p to q."
  [p q x]
  (let [d (distance-hyperboloid p q)]
    (if (numeric-zero? d)
      0
      (let [pq (subp q p)
            px (subp x p)
            orientation (dot px pq)
            s (distance-hyperboloid p x)]
        (e// (if (neg? (double orientation)) (e/- s) s) d)))))

(defn exp-origin
  "Exponential map at the origin, represented in Klein coordinates."
  [[u v]]
  (let [r (e/sqrt (e/+ (sq u) (sq v)))]
    (if (and (number? r) (zero? r))
      [0 0]
      (let [s (e// (e/tanh r) r)]
        [(e/* s u) (e/* s v)]))))

(defn line-coeff [[a b c]] [a b c])

(defn line-eval [l p]
  (let [[a b c] (line-coeff l)
        [x y] p]
    (e/+ (e/* a x) (e/* b y) c)))

(defn circle-data [center r]
  (let [[cx cy] center
        ch (models/klein->hyperboloid center)
        C (e/cosh r)
        [Xc Yc Tc] ch]
    {:center center :r r
     :A (e/+ (sq Xc) (sq C))
     :B (e/+ (sq Yc) (sq C))
     :D (e/* 2 Xc Yc)
     :E (e/* -2 Xc Tc)
     :F (e/* -2 Yc Tc)
     :G (e/- (sq Tc) (sq C))}))

(defn circle-eval [circle p]
  (let [{:keys [A B D E F G]} (circle-data (first circle) (second circle))
        [x y] p]
    (e/+ (e/* A (sq x)) (e/* B (sq y)) (e/* D x y)
         (e/* E x) (e/* F y) G)))

(defn circle-param [center r theta]
  (let [[X Y T] (models/klein->hyperboloid center)
        rho (e/sinh r)
        ch (e/cosh r)
        radial (let [s (e/sqrt (e/- (sq T) 1))]
                 (if (and (number? s) (zero? s)) [1 0 0]
                     [(e// (e/* T X) s) (e// (e/* T Y) s) s]))
        tangent [(e/- Y) X 0]
        tn (e/sqrt (e/+ (sq (first tangent)) (sq (second tangent))))
        tangent (if (and (number? tn) (zero? tn)) [0 1 0] (mapv #(e// % tn) tangent))
        dir (mapv e/+ (scalep (e/cos theta) radial) (scalep (e/sin theta) tangent))
        h (mapv e/+ (scalep ch [X Y T]) (scalep rho dir))]
    (models/hyperboloid->klein h)))

(defn circle-parameter
  "Angle theta whose circle-param value is x, for the oriented circle basis."
  [center r x]
  (let [[X Y T] (models/klein->hyperboloid center)
        h (models/klein->hyperboloid x)
        rho (e/sinh r)
        ch (e/cosh r)
        radial (let [s (e/sqrt (e/- (sq T) 1))]
                 (if (and (number? s) (zero? s)) [1 0 0]
                     [(e// (e/* T X) s) (e// (e/* T Y) s) s]))
        tangent [(e/- Y) X 0]
        tn (e/sqrt (e/+ (sq (first tangent)) (sq (second tangent))))
        tangent (if (and (number? tn) (zero? tn)) [0 1 0] (mapv #(e// % tn) tangent))
        u (scalep (e// 1 rho)
                  (mapv e/- h (scalep ch [X Y T])))
        c (e/- (hyperboloid-dot u radial))
        s (e/- (hyperboloid-dot u tangent))]
    (e/atan s c)))

(defn with-parameters
  "Attach protocol curve parameters for point x on curves a and b."
  [a b x]
  (let [param (fn [curve]
                (cond
                  (:geodesic curve) (let [[p q] (:geodesic curve)]
                                      (geodesic-parameter p q x))
                  (:circle curve) (let [[c r] (:circle curve)]
                                    (circle-parameter c r x))))]
    {:point x :at [(param a) (param b)]}))

(defn meet-geodesics [a b]
  (let [p (pg/dehomog (pg/meet (pg/join (first a) (second a))
                               (pg/join (first b) (second b))))]
    (if (inside? p)
      [(with-parameters {:geodesic a} {:geodesic b} p)]
      [])))

(defn solve-quadratic [a b c]
  (cond
    (numeric-zero? a) [(e// (e/- c) b)]
    :else (let [disc (e/- (sq b) (e/* 4 a c))]
            (if (and (number? disc) (< disc (- eps)))
              []
              (let [s (e/sqrt disc)]
                [(e// (e/- (e/- b) s) (e/* 2 a))
                 (e// (e/+ (e/- b) s) (e/* 2 a))])))))

(defn meet-geodesic-circle [[p q :as geodesic] [c r :as circle]]
  (let [d (subp q p)
        f (fn [t] (circle-eval [c r] (addp p (scalep t d))))
        f0 (f 0) f1 (f 1) f2 (f 2)
        a (e// (e/- f2 (e/* 2 f1) (e/- f0)) 2)
        b (e/- f1 f0 a)
        roots (solve-quadratic a b f0)]
    (->> roots
         (map (fn [t]
                (let [x (addp p (scalep t d))]
                  (with-parameters {:geodesic geodesic} {:circle circle} x))))
         (filter #(inside? (:point %)))
         vec)))

(defn meet-circles [[c1 r1 :as circle1] [c2 r2 :as circle2]]
  (let [[X1 Y1 T1] (models/klein->hyperboloid c1)
        [X2 Y2 T2] (models/klein->hyperboloid c2)
        C1 (e/cosh r1)
        C2 (e/cosh r2)
        n1 [(e/- X1) (e/- Y1) T1]
        n2 [(e/- X2) (e/- Y2) T2]
        edot (fn [u v] (reduce e/+ (map e/* u v)))
        cross (fn [[a b c] [d f g]]
                [(e/- (e/* b g) (e/* c f))
                 (e/- (e/* c d) (e/* a g))
                 (e/- (e/* a f) (e/* b d))])
        d (cross n1 n2)
        g11 (edot n1 n1)
        g12 (edot n1 n2)
        g22 (edot n2 n2)
        det (e/- (e/* g11 g22) (sq g12))]
    (if (numeric-zero? det)
      []
      (let [alpha (e// (e/- (e/* C1 g22) (e/* C2 g12)) det)
            beta (e// (e/- (e/* C2 g11) (e/* C1 g12)) det)
            p0 (mapv e/+ (scalep alpha n1) (scalep beta n2))
            qa (hyperboloid-dot d d)
            qb (e/* 2 (hyperboloid-dot p0 d))
            qc (e/- (hyperboloid-dot p0 p0) 1)
            ss (solve-quadratic qa qb qc)]
        (->> ss
             (map (fn [s]
                    (let [h (mapv e/+ p0 (scalep s d))
                          x (models/hyperboloid->klein h)]
                      (with-parameters {:circle circle1} {:circle circle2} x))))
             (filter #(inside? (:point %)))
             (sort-by (fn [{[x y] :point}]
                        (- (* (- x (first c1)) (- (second c2) (second c1)))
                           (* (- y (second c1)) (- (first c2) (first c1))))))
             vec)))))

(defn angle-at [v p q]
  (let [vh (models/klein->hyperboloid v)
        ph (models/klein->hyperboloid p)
        qh (models/klein->hyperboloid q)
        proj (fn [x]
               (let [a (hyperboloid-dot vh x)]
                 (mapv e/- x (scalep a vh))))
        u (proj ph) w (proj qh)]
    (e/acos (e// (e/- (hyperboloid-dot u w))
                 (e/sqrt (e/* (e/- (hyperboloid-dot u u))
                              (e/- (hyperboloid-dot w w))))))))

(def klein-metric
  "Emmy metric on R2-rect coordinates restricted to the Klein disk."
  (let [coordsys (m/with-coordinate-prototype m/R2-rect '[x y])
        [x y] (coord/coordinate-functions coordsys)
        [dx dy] (ff/coordinate-system->oneform-basis coordsys)]
    (fn [v w]
      (let [r2 (e/+ (sq x) (sq y))
            denom (e/- 1 r2)]
        (e/+ (e// (e/+ (e/* (dx v) (dx w))
                        (e/* (dy v) (dy w)))
                    denom)
             (e// (e/* (e/+ (e/* x (dx v)) (e/* y (dy v)))
                       (e/+ (e/* x (dx w)) (e/* y (dy w))))
                   (sq denom)))))))

(defrecord HyperbolicPlane [model]
  ctx/Surface
  (metric [_] klein-metric)
  (point [_ tangent] (exp-origin tangent))
  (geodesic [_ p q] (fn [t] (geodesic-point p q t)))
  (distance [_ p q] (distance-klein p q))
  (circle [_ center r] (fn [theta] (circle-param center r theta)))
  (-angle [_ v p q] (angle-at v p q))
  (meet [_ curve-a curve-b]
    (cond
      (and (:geodesic curve-a) (:geodesic curve-b))
      (meet-geodesics (:geodesic curve-a) (:geodesic curve-b))
      (and (:geodesic curve-a) (:circle curve-b))
      (meet-geodesic-circle (:geodesic curve-a) (:circle curve-b))
      (and (:circle curve-a) (:geodesic curve-b))
      (mapv #(update % :at (fn [[a b]] [b a]))
            (meet-geodesic-circle (:geodesic curve-b) (:circle curve-a)))
      (and (:circle curve-a) (:circle curve-b))
      (meet-circles (:circle curve-a) (:circle curve-b))
      :else []))
  (curvature [_ _] -1)
  (embed [_ p] (models/from-klein model p)))

(defmethod ctx/make :hyperbolic
  [_ opts]
  (->HyperbolicPlane (or (:model opts) :klein)))
