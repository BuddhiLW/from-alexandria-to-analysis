(ns alexandria.apollonius.conics
  "Apollonius of Perga, Conics (c. 200 BC), after T. L. Heath, Apollonius
   of Perga: Treatise on Conic Sections (Cambridge 1896).

   One cone, one cutting plane, one equation.

     the cone     X^2 + Y^2 = k^2 Z^2, apex at the origin, axis Z; k is
                  the slope of a generator (tan of the half-angle)
     the plane    through P = (k h, 0, h) on the generator in the plane
                  Y = 0, tilted by theta from the horizontal; the
                  abscissa x runs from P along the diameter PM (Heath's PV),
                  the ordinate y across it (Heath's QV)
     symptoma     every section satisfies y^2 = p x + c x^2 (I.11-13),
                  p = 2 k h (k sin theta + cos theta) the parameter (latus
                  rectum), c = k^2 sin^2 theta - cos^2 theta = -p/d with d
                  the transverse diameter. The one sign decides the name:
                  c = 0 the square on the ordinate is applied exactly to p
                  (parabola), c > 0 it exceeds (hyperbola), c < 0 it falls
                  short (ellipse)
     focal        a central conic y^2 = (1 - e^2)(a^2 - x^2) with foci
                  S, S' = (+-ae, 0): AS.SA' = b^2 (III.45), SP = a - e x and
                  S'P = a + e x (III.51-52), equal angles with the tangent
                  (III.48)
     Dandelin     the two spheres in the cone tangent to the plane touch it
                  at the foci; tangents from a point are equal, so QS + QS'
                  is the generator between the circles of contact (1822)

   Every identity is graded by alexandria.grade (Emmy simplify to 0). The
   shelf's claims (resources/alexandria/apollonius/conics.edn) compose
   vocabulary operations only (length, area, ratio, square); this
   namespace builds the symbolic figures they are realized on, all from
   the one construction in space (section-point).

   Reuse searched (carto over desargues, Emmy, alexandria):
   desargues.board.construction checks :distance and :area realize the
   vocabulary lengths and areas; desargues.geometry.projective has conics as
   five-point projective objects (no cone, no symptoma, no foci); Emmy has no
   cone sections. So the cone model is written here; the algebra is Emmy's."
  (:require [alexandria.grade :as grade]
            [alexandria.library :as library]
            [alexandria.proofs :as proofs]
            [alexandria.vocab]
            [clojure.walk :as walk]
            [desargues.board.construction :as c]
            [emmy.env :as e]
            [hive-dsl.result :as r]
            [alexandria.raster :as raster]))

;; ---------------------------------------------------------------------------
;; The cone and the plane (Book I.11-13)

(defn parameter
  "p, the latus rectum of the section of the cone of slope k by the plane
   through P at height h tilted by theta."
  [k theta h]
  (e/* 2 k h (e/+ (e/* k (e/sin theta)) (e/cos theta))))

(defn excess
  "c = -p/d, the coefficient of x^2 in the symptoma: 0 for the parabola,
   positive for the hyperbola, negative for the ellipse."
  [k theta]
  (e/- (e/* k k (e/square (e/sin theta))) (e/square (e/cos theta))))

(defn transverse
  "d, the transverse diameter PP' (negative when P' lies on the other
   nappe: the hyperbola's opposite branch)."
  [k theta h]
  (e// (e/* 2 k h) (e/- (e/cos theta) (e/* k (e/sin theta)))))

(defn parabola-tilt
  "The tilt at which the plane runs parallel to the generator AC."
  [k]
  (e/atan 1 k))

(defn section-point
  "The point of the cutting plane at abscissa s along the diameter and
   ordinate y, in space: [X Y Z]."
  [k theta h]
  (fn [[s y]]
    [(e/- (e/* k h) (e/* s (e/cos theta))) y (e/+ h (e/* s (e/sin theta)))]))

(defn section-at
  "The point of the section on the generator at angle phi round the axis:
   where the line through the apex and (k cos phi, k sin phi, 1) meets the
   plane. Z < 0 is the opposite nappe."
  [k theta h]
  (fn [phi]
    (let [z (e// (e/* h (e/+ (e/* k (e/sin theta)) (e/cos theta)))
                 (e/+ (e/* k (e/sin theta) (e/cos phi)) (e/cos theta)))]
      [(e/* k z (e/cos phi)) (e/* k z (e/sin phi)) z])))

(defn kind
  "The name of a section, from the sign of its excess c (a number)."
  [c]
  (cond (< (Math/abs (double c)) 1e-9) :parabola
        (pos? c) :hyperbola
        :else :ellipse))

(def names
  "The three names, from the Pythagorean application of areas (Euclid
   I.44, VI.28-29)."
  {:parabola {:greek "παραβολή" :latin "parabole" :meaning "applied exactly"}
   :hyperbola {:greek "ὑπερβολή" :latin "hyperbole" :meaning "applied, exceeding"}
   :ellipse {:greek "ἔλλειψις" :latin "elleipsis" :meaning "applied, falling short"}})

;; ---------------------------------------------------------------------------
;; Projection for media: a point of space seen from (yaw, pitch)

(defn project
  "Orthographic view of [X Y Z]: turned by yaw about the axis, tipped by
   pitch toward the eye; the screen [x y]."
  [yaw pitch [X Y Z]]
  (let [x (e/- (e/* X (e/cos yaw)) (e/* Y (e/sin yaw)))
        d (e/+ (e/* X (e/sin yaw)) (e/* Y (e/cos yaw)))]
    [x (e/+ (e/* Z (e/cos pitch)) (e/* d (e/sin pitch)))]))

(defn cone-figure
  "The double cone's surface at angle phi and height z, projected."
  [k yaw pitch]
  (fn [[phi z]]
    (project yaw pitch [(e/* k z (e/cos phi)) (e/* k z (e/sin phi)) z])))

(defn section-figure
  "The section by the plane tilted theta through height h, at angle phi
   round the axis, projected."
  [k theta h yaw pitch]
  (fn [[phi]] (project yaw pitch ((section-at k theta h) phi))))

(defn plane-figure
  "A point of the cutting plane at abscissa s, ordinate y, projected."
  [k theta h yaw pitch]
  (fn [[s y]] (project yaw pitch ((section-point k theta h) [s y]))))

(defn ellipse-figure
  "The ellipse of semi-axes a, b at eccentric angle t."
  [a b]
  (fn [[t]] [(e/* a (e/cos t)) (e/* b (e/sin t))]))

(defn hyperbola-figure
  "The near branch of the hyperbola of semi-axes a, b at t in (-pi/2, pi/2):
   x = a / cos t, y = b tan t."
  [a b]
  (fn [[t]] [(e// a (e/cos t)) (e/* b (e/tan t))]))

(defn section-height
  "[Z c]: the height Z of the section's point on the generator at angle phi
   (Z < 0 on the opposite nappe; very large where the generator runs
   parallel to the plane), and the section's excess c."
  [k theta h]
  (fn [[phi]] [(nth ((section-at k theta h) phi) 2) (excess k theta)]))

(defn plane-height
  "[Z X]: height and distance from the axis plane of the cutting plane's
   point at abscissa s along the diameter."
  [k theta h]
  (fn [[s]] (let [[X _ Z] ((section-point k theta h) [s 0])] [Z X])))

(defn space-figure
  "A point [X Y Z] of space, projected from (yaw, pitch)."
  [yaw pitch]
  (fn [[X Y Z]] (project yaw pitch [X Y Z])))

(defn symptoma-figure
  "[y^2 y] at abscissa x of the symptoma y^2 = p x + c x^2."
  [p c]
  (fn [[x]] (let [y2 (e/+ (e/* p x) (e/* c x x))] [y2 (e/sqrt y2)])))

(defn application-figure
  "Apollonius' application of areas for the section of the cone of slope k
   by the plane tilted theta through height h, at the fraction t of a safe
   abscissa: [x p VR y c]. x = PV = t p / (1 - c), which keeps x inside the
   ellipse's transverse diameter d = -p/c, so VR stays positive; p = PL the
   upright side; VR = p + c x the side of the applied rectangle PV.VR
   (Heath's VR, I.12-13); y = QV with QV^2 = PV.VR; c the excess whose sign
   names the section. Every side the 2D panel draws, to scale."
  [k theta h]
  (fn [[t]]
    (let [p (parameter k theta h)
          c (excess k theta)
          x (e// (e/* t p) (e/- 1 c))
          vr (e/+ p (e/* c x))]
      [x p vr (e/sqrt (e/* x vr)) c])))

(defn trace-figure
  "Apollonius' trace of the cutting plane on the base (I.11: the plane cuts
   the base circle in DE at right angles to BC at M), the base at height zb,
   projected: [Mx My Dx Dy Ex Ey w]. w = (k zb)^2 - XM^2 is positive when
   the plane meets the base inside the cone; where it does not (an ellipse
   closing above the base) w < 0 and the scene draws no trace. D and E use
   sqrt |w|, so the kernel never takes the root of a negative number."
  [k theta h yaw pitch]
  (fn [[zb]]
    (let [s (e// (e/- zb h) (e/sin theta))
          mx (e/- (e/* k h) (e/* s (e/cos theta)))
          w (e/- (e/square (e/* k zb)) (e/square mx))
          half (e/sqrt (e/abs w))
          [a b] (project yaw pitch [mx 0 zb])
          [c d] (project yaw pitch [mx half zb])
          [f g] (project yaw pitch [mx (e/- 0 half) zb])]
      [a b c d f g w])))

(defn- dist [u v] (e/sqrt (reduce e/+ (map (fn [a b] (e/square (e/- a b))) u v))))

(defn ellipse-tangent
  "The point s along the unit tangent of the ellipse (a, b) at eccentric
   angle t: s = 0 the point of contact."
  [a b]
  (fn [[t s]]
    (let [dx (e/- 0 (e/* a (e/sin t))) dy (e/* b (e/cos t)) n (e/sqrt (e/+ (e/square dx) (e/square dy)))]
      [(e/+ (e/* a (e/cos t)) (e/* s (e// dx n))) (e/+ (e/* b (e/sin t)) (e/* s (e// dy n)))])))

(defn ellipse-focal
  "At eccentric angle t of the ellipse (a, b), state [t which]: which = 0
   gives [SP S'P], the focal distances (III.52); which = 1 gives the angles,
   in radians, that SP and S'P make with the tangent (III.48). Radians, not
   degrees: a factor 180/pi would multiply raster's acos error."
  [a b]
  (fn [[t which]]
    (let [c (e/sqrt (e/- (e/square a) (e/square b)))
          P [(e/* a (e/cos t)) (e/* b (e/sin t))]
          dir [(e/- 0 (e/* a (e/sin t))) (e/* b (e/cos t))]
          nd (dist dir [0 0])
          angle (fn [F] (let [u (mapv e/- F P)]
                          (e/acos (e// (e/abs (reduce e/+ (map e/* u dir))) (e/* nd (dist u [0 0]))))))
          S [c 0] S' [(e/- 0 c) 0]]
      [(e/+ (e/* (e/- 1 which) (dist P S)) (e/* which (angle S)))
       (e/+ (e/* (e/- 1 which) (dist P S')) (e/* which (angle S')))])))

(defn hyperbola-focal
  "[SP S'P] at t of the near branch x = a / cos t, y = b tan t, the foci at
   (+-sqrt(a^2 + b^2), 0)."
  [a b]
  (fn [[t]]
    (let [c (e/sqrt (e/+ (e/square a) (e/square b)))
          P ((hyperbola-figure a b) [t])]
      [(dist P [c 0]) (dist P [(e/- 0 c) 0])])))

(defn- dandelin-geometry
  "Dandelin's spheres for the cone of slope k and the plane tilted theta
   through height h, as Emmy expressions: :zs the centres' heights, :radii,
   :contact the heights of the circles of contact, :feet the feet of the
   centres on the plane (the foci)."
  [k theta h]
  (let [n (e/sqrt (e/+ 1 (e/square k))) sa (e// k n) ca (e// 1 n)
        co (e/cos theta) si (e/sin theta)
        top (e/* h (e/+ co (e/* k si)))
        zs [(e// top (e/+ co sa)) (e// top (e/- co sa))]
        nv [si 0 co] P [(e/* k h) 0 h]
        foot (fn [z] (let [t (reduce e/+ (map e/* nv (map e/- [0 0 z] P)))]
                       (mapv (fn [ci ni] (e/- ci (e/* t ni))) [0 0 z] nv)))]
    {:zs zs :radii (mapv #(e/* % sa) zs) :contact (mapv #(e/* % ca ca) zs) :feet (mapv foot zs)}))

(defn dandelin-figure
  "[QF1 QF2]: the distances from the section's point Q on the generator at
   angle phi to the two foci, the feet of Dandelin's spheres."
  [k theta h]
  (fn [[phi]]
    (let [Q ((section-at k theta h) phi)
          [F1 F2] (:feet (dandelin-geometry k theta h))]
      [(dist Q F1) (dist Q F2)])))

;; ---------------------------------------------------------------------------
;; In space, for a 3D viewer (MathBox): points [X Y Z], no projection; the
;; viewer's camera does the seeing.

(defn cone-3d
  "The double cone's point at angle phi round the axis and height z."
  [k]
  (fn [[phi z]] [(e/* k z (e/cos phi)) (e/* k z (e/sin phi)) z]))

(defn section-3d
  "The section's point on the generator at angle phi (section-at), in
   space: it lies on the cone and on the cutting plane at once."
  [k theta h]
  (fn [[phi]] ((section-at k theta h) phi)))

(defn plane-3d
  "The cutting plane's point at abscissa s along the diameter from P and
   ordinate y across it."
  [k theta h]
  (fn [[s y]] ((section-point k theta h) [s y])))

(defn ordinate-3d
  "I.11's construction at abscissa s = PV: [Vx Vz r Qy w]. V = (Vx, 0, Vz)
   is the foot of the ordinate on the diameter; the circle HK through V
   parallel to the base has radius r = k Vz about the axis (H, K = (-+r, 0,
   Vz)); Q = (Vx, Qy, Vz) is where that circle meets the cutting plane, with
   QV^2 = Qy^2 = HV.VK = r^2 - Vx^2 = w. Qy uses sqrt |w|; w < 0 means V lies
   outside the cone, and the viewer draws no ordinate there."
  [k theta h]
  (fn [[s]]
    (let [[vx _ vz] ((section-point k theta h) [s 0])
          r (e/* k vz)
          w (e/- (e/square r) (e/square vx))]
      [vx vz r (e/sqrt (e/abs w)) w])))

(def figures
  "Every moving figure, by name: {:f figure :params initial-params :state
   initial-state}, :opts for the compiler where simplifying does not help.
   Params: k the cone's slope, theta the tilt, h the height of P, yaw and
   pitch the view; a, b the semi-axes; p, c the symptoma's parameter and
   excess. Every number a scene draws or prints per frame comes out of one
   of these kernels."
  {:cone {:f cone-figure :params [0.6 0.5 0.32] :state [0 1]}
   :section {:f section-figure :params [0.6 0.4 0.7 0.5 0.32] :state [0]}
   :plane {:f plane-figure :params [0.6 0.4 0.7 0.5 0.32] :state [0 0]}
   :height {:f section-height :params [0.6 0.4 0.7] :state [0]}
   :plane-height {:f plane-height :params [0.6 0.4 0.7] :state [0]}
   :space {:f space-figure :params [0.5 0.32] :state [0 0 0]}
   :symptoma {:f symptoma-figure :params [1 0] :state [0.5]}
   :application {:f application-figure :params [0.6 0.4 0.7] :state [0.5] :opts {:simplify? false}}
   :trace {:f trace-figure :params [0.6 0.4 0.7 0.5 0.32] :state [2] :opts {:simplify? false}}
   :ellipse {:f ellipse-figure :params [1.5 1.2] :state [0]}
   :ellipse-tangent {:f ellipse-tangent :params [1.5 1.2] :state [0 0]}
   :ellipse-focal {:f ellipse-focal :params [1.5 1.2] :state [0.5 0] :opts {:simplify? false}}
   :hyperbola {:f hyperbola-figure :params [1 1.118] :state [0]}
   :hyperbola-focal {:f hyperbola-focal :params [1 1.118] :state [0] :opts {:simplify? false}}
   :dandelin {:f dandelin-figure :params [0.6 0.55 1] :state [0] :opts {:simplify? false}}
   ;; in space, for the MathBox view (no projection)
   :cone3 {:f cone-3d :params [0.6] :state [0 1]}
   :section3 {:f section-3d :params [0.6 0.4 0.7] :state [0]}
   :plane3 {:f plane-3d :params [0.6 0.4 0.7] :state [0 0]}
   :ordinate3 {:f ordinate-3d :params [0.6 0.4 0.7] :state [0.85] :opts {:simplify? false}}})

;; ---------------------------------------------------------------------------
;; Envs: the symbolic figures the shelf's vocabulary claims are realized on

(defn- pt [[px py]] {:x px :y py})

(def ^:private k 'k)
(def ^:private theta 'theta)
(def ^:private h 'h)
(def ^:private x 'x)

(defn circle-env
  "The circle through the foot of an ordinate parallel to the base: in its
   own plane, HK its diameter (in the axial triangle), V on HK, Q on the
   circle above V. Radius r, V at distance v from the centre."
  []
  (let [r 'r v 'v]
    {:points {:H (pt [(e/- r) 0]) :K (pt [r 0]) :V (pt [v 0])
              :Q (pt [v (e/sqrt (e/- (e/* r r) (e/* v v)))])}}))

(defn plane-env
  "The cone cut by the plane, as Apollonius reads it (I.11-13): the
   diameter PM in the cutting plane, P its vertex, X the foot of the
   ordinate at abscissa x (Heath's V), L the end of the latus rectum PL at
   right angles to PM; and the circle through that foot parallel to the
   base, its diameter HK in the axial triangle, V the same foot on HK.
   Both come from one construction in space (section-point): V sits at the
   distance of the foot from the axis, HK spans 2 k Z at its height Z."
  []
  (let [[X _ Z] ((section-point k theta h) [x 0])
        r (e/* k Z)]
    {:points {:P (pt [0 0]) :X (pt [x 0]) :L (pt [0 (parameter k theta h)])
              :H (pt [(e/- r) 0]) :K (pt [r 0]) :V (pt [X 0])}}))

(defn axial-env
  "The axial triangle ABC cut at height Z (A the apex, BC a diameter of the
   circle there) and F where AF, drawn parallel to the diameter PM of the
   section, meets BC (produced)."
  []
  (let [Z 'Z]
    {:points {:A (pt [0 0]) :B (pt [(e/- (e/* k Z)) Z]) :C (pt [(e/* k Z) Z])
              :F (pt [(e/- (e// (e/* Z (e/cos theta)) (e/sin theta))) Z])}}))

(def ^:private a 'a)
(def ^:private ecc 'e)

(defn focal-env
  "A central conic with semi-axis a and eccentricity e, written once for
   both: y^2 = (1 - e^2)(a^2 - x^2), e < 1 the ellipse, e > 1 the
   hyperbola. A, A' the vertices, S, S' the foci, P on the curve at
   abscissa x, T along the tangent at P."
  []
  (let [b2 (e/* (e/- 1 (e/square ecc)) (e/square a))
        y (e/sqrt (e/* b2 (e/- 1 (e// (e/square x) (e/square a)))))]
    {:points {:A (pt [a 0]) :A' (pt [(e/- a) 0])
              :S (pt [(e/* a ecc) 0]) :S' (pt [(e/- (e/* a ecc)) 0])
              :P (pt [x y])
              ;; the tangent at (x, y) of x^2/a^2 + y^2/b^2 = 1 runs along (-a^2 y, b^2 x)
              :T (pt [(e/- x (e/* (e/square a) y)) (e/+ y (e/* b2 x))])}}))

(def envs
  "The figure each shelf claim is realized on, by its :env key, and the
   names a claim uses for magnitudes the figure determines: p the
   parameter, d the transverse diameter, c the excess (coefficient of x^2).
   Claims compare squares where a length is a root of a sum: Emmy's
   simplifier halves sqrt((2u)^2) for a sum u, so squares keep the proof exact."
  (let [bind {'p (parameter k theta h) 'd (transverse k theta h) 'c (excess k theta)}]
    {:circle {:env circle-env :bind {}}
     :plane {:env plane-env :bind bind}
     :axial {:env axial-env :bind bind}
     :focal {:env focal-env :bind {}}}))

;; ---------------------------------------------------------------------------
;; Grading

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn grade-diff
  "The grade of one symbolic difference."
  [diff]
  (variant (grade/grade :symbolic [diff])))

(defn claim-diff
  "lhs - rhs of a vocabulary claim, realized on the figure named env-key."
  [env-key claim]
  (let [{:keys [env bind]} (get envs env-key)
        claim (walk/postwalk-replace (update-vals bind e/freeze) claim)]
    (c/check {:claim claim} (env))))

(defn grade-claim
  "The grade of a shelf claim {:env :claim}."
  [{:keys [env claim]}]
  (grade-diff (claim-diff env claim)))

;; ---------------------------------------------------------------------------
;; Proofs outside the vocabulary: the parabola's own ratio, and Dandelin

(defn parabola-case
  "[label diff] pairs: at the parabola's tilt the excess vanishes, and
   Apollonius' I.11 ratio PL : PA = BC^2 : BA.AC gives the parameter."
  []
  (let [n (e/sqrt (e/+ 1 (e/square k)))
        si (e// 1 n) co (e// k n)                ; theta with k sin = cos
        p (e/* 2 k h (e/+ (e/* k si) co))
        c (e/- (e/* k k si si) (e/* co co))
        Z 'Z
        pa (e/* h n)                             ; P = (k h, h) from the apex
        bc2 (e/square (e/* 2 k Z))
        ba-ac (e/* (e/square Z) (e/+ 1 (e/square k)))]
    [["parallel to the side AC, the excess vanishes: y^2 = p x" c]
     ["I.11: PL : PA = BC^2 : BA.AC" (e/- (e/* p ba-ac) (e/* bc2 pa))]]))

(defn dandelin-identities
  "[label diff] pairs for Dandelin's spheres, in space. The cone of
   half-angle alpha, the plane tilted theta through height h; the spheres'
   centres on the axis at heights z1, z2 (the two roots of 'distance to
   the plane = distance to the cone'). Q the point of the section at
   abscissa s; F1, F2 the feet of the centres on the plane; G1, G2 where
   the generator through Q meets the circles of contact."
  []
  (let [alpha 'alpha ca (e/cos alpha) sa (e/sin alpha) kk (e// sa ca)
        co (e/cos theta) si (e/sin theta) s 's
        P [(e/* kk h) 0 h] n [si 0 co]
        dot (fn [u v] (reduce e/+ (map e/* u v)))
        top (e/* h (e/+ co (e/* kk si)))
        z1 (e// top (e/+ co sa))
        z2 (e// top (e/- co sa))
        foot (fn [z] (let [C [0 0 z] t (dot n (map e/- C P))]
                       (mapv (fn [ci ni] (e/- ci (e/* t ni))) C n)))
        [qx _ qz] ((section-point kk theta h) [s 0])
        y2 (e/+ (e/* (parameter kk theta h) s) (e/* (excess kk theta) s s))
        dist2 (fn [[fx _ fz]] (e/+ (e/square (e/- qx fx)) y2 (e/square (e/- qz fz))))
        g1 (e/- (e// qz ca) (e/* z1 ca))
        g2 (e/- (e/* z2 ca) (e// qz ca))]
    [["the upper sphere touches the plane at F1"
      (e/- (e/square (dot n (map e/- [0 0 z1] P))) (e/square (e/* z1 sa)))]
     ["the lower sphere touches the plane at F2"
      (e/- (e/square (dot n (map e/- [0 0 z2] P))) (e/square (e/* z2 sa)))]
     ["QF1 = QG1, tangents from Q to the first sphere" (e/- (dist2 (foot z1)) (e/square g1))]
     ["QF2 = QG2, tangents from Q to the second sphere" (e/- (dist2 (foot z2)) (e/square g2))]
     ["QF1 + QF2 = G1G2, the same for every Q" (e/- (e/+ g1 g2) (e/* (e/- z2 z1) ca))]]))

(defn dandelin-spheres
  "Dandelin's spheres for the cone of slope k cut by the plane tilted theta
   (an ellipse: theta below the parabola's tilt) through height h, in
   numbers computed by one raster kernel of the Emmy construction
   (dandelin-geometry): {:centres [[0 0 z1] [0 0 z2]] :radii [r1 r2]
   :contact [c1 c2] (heights of the circles of contact) :feet [F1 F2] (the
   foci, in space) :source :raster}."
  [k theta h]
  (let [[z1 z2 r1 r2 c1 c2 & feet]
        (raster/value (fn [k theta h]
                        (let [{:keys [zs radii contact feet]} (dandelin-geometry k theta h)]
                          (vec (concat zs radii contact (apply concat feet)))))
                      k theta h)]
    {:centres [[0.0 0.0 z1] [0.0 0.0 z2]]
     :radii [r1 r2]
     :contact [c1 c2]
     :feet (mapv vec (partition 3 feet))
     :source raster/source}))

;; ---------------------------------------------------------------------------
;; The shelf, its proofs, and the grades

(def shelf-resource "alexandria/apollonius/conics.edn")
(def proofs-resource "alexandria/apollonius/conics.proofs.edn")

(defn proof
  "Result of the proofs told step by step."
  []
  (proofs/read-proofs proofs-resource))

(defn propositions
  "Result of {id proposition} for the Conics shelf, validated by
   alexandria.library."
  []
  (r/let-ok [m (library/shelves)]
    (r/ok (into (sorted-map) (filter (fn [[id _]] (= "apollonius" (namespace id)))) m))))

(def ^:private extra
  {:apollonius/I.11-13 parabola-case
   :apollonius/dandelin dandelin-identities})

(defn graded
  "{proposition-id [{:label :grade}]}: every shelf claim, then the
   parabola's own ratio and Dandelin's spheres."
  []
  (let [res (propositions)
        shelf (if (r/ok? res) (:ok res) {})
        claims (into {} (map (fn [[id p]] [id (mapv (fn [cl] {:label (:label cl) :grade (grade-claim cl)})
                                                   (:claims p))]))
                     shelf)]
    (reduce (fn [m [id f]] (update m id (fnil into []) (map (fn [[l d]] {:label l :grade (grade-diff d)}) (f))))
            claims extra)))
