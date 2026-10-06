(ns alexandria.ops.euclid
  "Euclid Book I operations that were absent from desargues.

   desargues.board.construction already supplies free points, points on lines,
   line-line meets, midpoints, maps and expression points; desargues.board.euclid
   adds reflection, foot, rotation, centroid, circumcenter and points around a
   circle.  Euclid I.1--I.3 also need postulate-level intersections that were
   not registered there: two circles meeting, a produced line meeting a circle,
   and a finite segment cut by a circle.  The methods below keep those
   constructions as Emmy expressions over the board environment so desargues can
   still lower them into raster kernels."
  (:require [desargues.board.construction :as c]
            [desargues.board.euclid]
            [desargues.board.linalg :as la]
            [emmy.env :as e]))

(defmethod c/draw-layer :circle [[_ center through opts] env]
  (merge {:layer :circle :center (c/at env center) :through (c/at env through)} opts))

(defn- square [x] (e/* x x))

(defn- radius [env [o p]]
  (c/dist (c/xy-of env o) (c/xy-of env p)))

(defn- perp-left [[x y]] [(e/- 0 y) x])

(defn circle-circle-point
  "Intersection of the xy circles (O1, r1) and (O2, r2) as an Emmy expression.
   side 0 is left of O1->O2, side 1 right."
  [O1 r1 O2 r2 side]
  (let [dvec (la/sub O2 O1)
        d (la/norm dvec)
        a (e/divide (e/+ (e/- (square r1) (square r2)) (square d)) (e/* 2 d))
        h (e/sqrt (e/- (square r1) (square a)))
        base (la/add O1 (la/scale (e/divide a d) dvec))
        left (la/scale (e/divide h d) (perp-left dvec))]
    (if (= 1 side)
      (la/sub base left)
      (la/add base left))))

(defn line-circle-roots
  "Parameter values t where A + t(B-A) meets the xy circle (O, r), as Emmy
   expressions. The +sqrt root is second."
  [A B O r]
  (let [d (la/sub B A)
        f (la/sub A O)
        aa (la/dot d d)
        bb (e/* 2 (la/dot f d))
        cc (e/- (la/dot f f) (square r))
        disc (e/sqrt (e/- (square bb) (e/* 4 aa cc)))
        denom (e/* 2 aa)]
    [(e/divide (e/- (e/- 0 bb) disc) denom)
     (e/divide (e/+ (e/- 0 bb) disc) denom)]))

(defn- env-circle [env [o _ :as circ]]
  [(c/xy-of env o) (radius env circ)])

(defmethod c/point :circles [{:keys [circles side] :or {side 0}} env]
  (let [[O1 r1] (env-circle env (first circles))
        [O2 r2] (env-circle env (second circles))]
    {:xy (circle-circle-point O1 r1 O2 r2 side)}))

(defmethod c/point :circle-circle [p env]
  (c/point (assoc p :op :circles) env))

(defn- env-line-circle-roots [env [a b] circ]
  (let [[O r] (env-circle env circ)]
    (line-circle-roots (c/xy-of env a) (c/xy-of env b) O r)))


(defn- root-parameter [[t1 t2] which]
  (case which
    (:first :near :cut-start :beyond-start) t1
    (:second :far :cut-end :beyond-end) t2
    t2))

(defn- line-point [env [a b] t]
  (la/lerp (c/xy-of env a) (c/xy-of env b) t))

(defmethod c/point :line-circle
  [{:keys [line circle beyond? which] :or {which :second}} env]
  (let [t (root-parameter (env-line-circle-roots env line circle)
                          (if beyond? :beyond-end which))]
    {:xy (line-point env line t)}))

(defmethod c/point :extend [p env]
  (c/point (assoc p :op :line-circle :beyond? true) env))

(defmethod c/point :cut
  [{:keys [segment circle which] :or {which :first}} env]
  (let [t (root-parameter (env-line-circle-roots env segment circle) which)]
    {:xy (line-point env segment t)}))

(defmethod c/point :segment-circle [p env]
  (c/point (assoc p :op :cut) env))

(defmethod c/check :equal-lengths [{segs :equal-lengths} env]
  (let [[[a b] & more] segs
        d0 (c/dist (c/coords-of env a) (c/coords-of env b))]
    (reduce e/+ (map (fn [[p q]]
                       (e/abs (e/- (c/dist (c/coords-of env p) (c/coords-of env q)) d0)))
                     more))))

(defmethod c/check :bisects [{spec :bisects} env]
  (let [{:keys [point segment]} spec
        [a b] segment]
    (e/+ (e/abs (c/check {:collinear [a point b]} env))
         (e/abs (e/- (c/dist (c/coords-of env a) (c/coords-of env point))
                     (c/dist (c/coords-of env point) (c/coords-of env b)))))))

(defmethod c/check :equal-angles [{angles :equal-angles} env]
  (let [[[a o b] & more] angles
        theta (c/check {:angle [a o b]} env)]
    (reduce e/+ (map (fn [[p q r]]
                       (e/abs (e/- (c/check {:angle [p q r]} env) theta)))
                     more))))

;; Flat-surface adapters: the kernels above, evaluated at xy values, with the
;; tangency guards and curve parameters a Surface `meet` reports.

(def ^:private eps 1.0e-9)

(defn- near-zero? [x] (<= (Math/abs (double x)) 1.0e-6))

(defn cross2
  "The signed 2D cross product of xy vectors a and b."
  [[x y] [u v]]
  (e/- (e/* x v) (e/* y u)))

(defn theta
  "The polar angle of p around center c, matching the parameter of circle-xy."
  [c p]
  (e/atan (e/- (nth p 1) (nth c 1))
          (e/- (nth p 0) (nth c 0))))

(defn circle-xy
  "Point at angle alpha on the xy circle centered at center with radius r."
  [center r alpha]
  [(e/+ (nth center 0) (e/* r (e/cos alpha)))
   (e/+ (nth center 1) (e/* r (e/sin alpha)))])

(defn line-line-xy
  "Intersection of the lines AB and RS as [{:point p :at [ta tb]}]; [] when parallel."
  [A B R S]
  (let [u (la/sub B A)
        v (la/sub S R)
        w (la/sub A R)
        d (cross2 u v)]
    (if (near-zero? d)
      []
      (let [ta (e/divide (cross2 v w) d)
            tb (e/divide (cross2 u w) d)]
        [{:point (la/lerp A B ta) :at [ta tb]}]))))

(defn line-circle-xy
  "Crossings of line AB with the circle (O, r) as {:point p :at [t theta]},
   in increasing t; [] when the line misses the circle."
  [A B O r]
  (let [d (la/sub B A)
        miss (- (/ (Math/abs (double (cross2 d (la/sub O A))))
                   (double (la/norm d)))
                (double r))]
    (if (or (near-zero? (la/norm d)) (> miss eps))
      []
      (mapv (fn [t]
              (let [t (double t)
                    p (la/lerp A B t)]
                {:point p :at [t (theta O p)]}))
            (line-circle-roots A B O r)))))

(defn circle-circle-results
  "Crossings of the circles (O1, r1) and (O2, r2) as {:point p :at [theta1 theta2]},
   the crossing left of O1->O2 first; [] when the circles do not meet."
  [O1 r1 O2 r2]
  (let [d (double (la/norm (la/sub O2 O1)))
        r1 (double r1)
        r2 (double r2)]
    (if (or (near-zero? d)
            (> d (+ r1 r2 eps))
            (< d (- (Math/abs (- r1 r2)) eps)))
      []
      (mapv (fn [side]
              (let [p (circle-circle-point O1 r1 O2 r2 side)]
                {:point p :at [(theta O1 p) (theta O2 p)]}))
            [0 1]))))

(defn square-corners
  "The two far corners of the square on AB (I.46), on the side `turn`
   names (:left or :right of A->B): [the corner next to B, the corner next
   to A]. desargues has :foot (I.12), :meet and :rotate; it has no square
   on a segment, so this one is registered here."
  [A B turn]
  (let [[dx dy] (la/sub B A)
        n (if (= :right turn) [dy (e/- 0 dx)] [(e/- 0 dy) dx])]
    [(la/add B n) (la/add A n)]))

(defmethod c/point :square-corner
  ;; {:op :square-corner :side [:A :B] :turn :right :corner 0}: corner 0 is
  ;; next to B, corner 1 next to A, of the square on AB to that side.
  [{:keys [side turn corner] :or {turn :left corner 0}} env]
  (let [[a b] side]
    {:xy (nth (square-corners (c/xy-of env a) (c/xy-of env b) turn) corner)}))

(defmethod c/point :translate
  ;; {:op :translate :of :L :by [:B :D]}: L carried along the segment B->D.
  [{:keys [of by]} env]
  (let [[a b] by]
    {:xy (la/add (c/xy-of env of) (la/sub (c/xy-of env b) (c/xy-of env a)))}))

(defmethod c/point :on-ray
  ;; {:op :on-ray :from :D :through :G :length [:A :B]}: the point on the ray
  ;; from D through G at distance AB from D (I.3 placed along a ray).
  [{:keys [from through length]} env]
  (let [[a b] length
        D (c/xy-of env from)
        d (la/sub (c/xy-of env through) D)]
    {:xy (la/add D (la/scale (e/divide (radius env [a b]) (la/norm d)) d))}))

(defmethod c/point :copy-angle
  ;; {:op :copy-angle :angle [:B :A :C] :at [:E :D] :length [:A :C]}: I.23
  ;; then I.3. The point F with angle EDF equal to angle BAC, turned the
  ;; same way, and DF equal to AC. desargues' :rotate takes an author
  ;; expression for the angle, which cannot name an angle of the figure.
  [{[b a cc] :angle [e d] :at [p q] :length} env]
  (let [A (c/xy-of env a)
        u (la/sub (c/xy-of env b) A)
        v (la/sub (c/xy-of env cc) A)
        uv (e/* (la/norm u) (la/norm v))
        cs (e/divide (la/dot u v) uv)
        sn (e/divide (cross2 u v) uv)
        D (c/xy-of env d)
        [wx wy] (la/sub (c/xy-of env e) D)
        k (e/divide (radius env [p q]) (la/norm [wx wy]))]
    {:xy (la/add D (la/scale k [(e/- (e/* cs wx) (e/* sn wy)) (e/+ (e/* sn wx) (e/* cs wy))]))}))
