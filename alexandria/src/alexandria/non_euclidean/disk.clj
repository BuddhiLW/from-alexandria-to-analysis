(ns alexandria.non-euclidean.disk
  "The hyperbolic plane (curvature K = -1) in Poincare's disk, and the same
   plane in Beltrami's (later Klein's) disk, for the non-euclidean notebook
   (Lobachevsky 1829/1840, Bolyai 1832, Beltrami 1868).

   Points are complex numbers z = [x y] with |z| < 1, in POINCARE
   coordinates. Everything is Emmy arithmetic on numbers or symbols and
   branch-free, so alexandria.raster and emmy-viewers' :raster backend
   compile it for the page.

   The one tool is the Moebius map that moves a point p to the centre,

     T_p(z) = (z - p) / (1 - conj(p) z),

   an isometry of the disk with inverse T_p^-1(w) = (w + p) / (1 + conj(p) w).
   Through the centre the geodesics are diameters, so

   - the geodesic from p towards q is T_p^-1(s T_p(q)), s in [0 1]; the
     whole line through p in the unit direction u is T_p^-1(s u), s in
     [-1 1] (an arc orthogonal to the boundary circle);
   - the angle at p between the geodesics to q and r is the Euclidean angle
     between T_p(q) and T_p(r) at 0 (T_p is conformal);
   - the distance is d(p, q) = acosh(1 + 2|p - q|^2 / ((1 - |p|^2)(1 - |q|^2))).

   The Beltrami-Klein picture of the same point is k = 2z / (1 + |z|^2): its
   geodesics are straight chords, its angles are not the true ones.

   A triangle's angles are computed twice, by the Moebius map and by the
   hyperbolic law of cosines from the three side lengths; their sum S and
   the area pi - S (Gauss-Bonnet with K = -1) are what the page shows.
   The distance from p to the diameter (the real axis) is
   asinh(2|y| / (1 - |p|^2)), and the angle at p between the two limiting
   parallels to it (towards the ideal points -1 and +1) is 2 Pi(d), with
   Lobachevsky's Pi(d) = 2 atan(e^-d)."
  (:require [alexandria.grade :as grade]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [hive-dsl.result :as r]
            [alexandria.structure.arith
 :as
 a
 :refer
 [at-least
  at-most
  clamp
  acosh*
  asinh*
  acos*
  angle2
  c+
  c-
  c*
  cconj
  cscale
  norm2
  c-div
  one]]
            [alexandria.structure.curvature :as kc]
            [alexandria.structure.groups :as gs]))

;; ---------------------------------------------------------------------------
;; Branch-free helpers (as alexandria.medium.tie; the core cannot require it)

;; ---------------------------------------------------------------------------
;; Complex numbers as pairs

(def H "Lobachevsky's plane, K = -1, in Poincare's chart." kc/lobachevsky)

(defn to-origin "T_p(z): the isometry taking p to 0." [p z] (kc/to-origin H p z))

(defn from-origin "T_p^-1(w)." [p w] (kc/from-origin H p w))

(defn geodesic-point
  "The point at parameter s in [0 1] on the geodesic segment from p to q."
  [p q s]
  (from-origin p (cscale s (to-origin p q))))

(defn line-point
  "The point at parameter s in [-1 1] on the whole geodesic through p in
   the direction of the vector u (unit or not: it is normalised)."
  [p u s]
  (let [n (e/sqrt (e/+ (norm2 u) 1e-30))]
    (from-origin p (cscale (e/divide s n) u))))

(defn klein "Beltrami-Klein picture of a Poincare point." [z]
  (cscale (e/divide 2 (e/+ 1 (norm2 z))) z))

(defn poincare "Poincare point of a Beltrami-Klein point." [k]
  (cscale (e/divide 1 (e/+ 1 (e/sqrt (at-least (e/- 1 (norm2 k)) 0)))) k))

(defn display
  "The point as drawn: model m = 0 Poincare, m = 1 Beltrami-Klein."
  [m z]
  (let [k (klein z)]
    [(e/+ (e/* (e/- 1 m) (first z)) (e/* m (first k)))
     (e/+ (e/* (e/- 1 m) (second z)) (e/* m (second k)))]))

(def r-max "Dragged points are kept inside this radius (Poincare)." 0.995)

(defn keep-inside
  "z pulled back to |z| <= rmax along its ray (branch-free)."
  ([z] (keep-inside z r-max))
  ([z rmax]
   (let [n (e/sqrt (e/+ (norm2 z) 1e-30))]
     (cscale (at-most 1 (e/divide rmax n)) z))))

(defn from-display
  "The Poincare point (inside r-max) under a drawn point d of model m."
  [m d]
  (let [p (poincare (keep-inside d 0.99999))
        z [(e/+ (e/* (e/- 1 m) (first d)) (e/* m (first p)))
           (e/+ (e/* (e/- 1 m) (second d)) (e/* m (second p)))]]
    (keep-inside z)))

;; ---------------------------------------------------------------------------
;; Metric quantities

(defn dist
  "Hyperbolic distance (K = -1), the disk's closed form
   acosh(1 + 2|p - q|^2 / ((1 - |p|^2)(1 - |q|^2))): equal to
   (kc/distance H p q) inside the disk (the curvature contract checks
   it), and finite outside it, where callers mask it (erlangen's :inside)."
  [p q]
  (acosh* (e/+ 1 (e/divide (e/* 2 (norm2 (c- p q)))
                           (e/* (e/- 1 (norm2 p)) (e/- 1 (norm2 q)))))))

(defn angle-at "The angle at p between the geodesics to q and to r." [p q r] (kc/angle H p q r))

(defn angle-cosines
  "The angle opposite c in a triangle of sides a, b, c, by the hyperbolic
   law of cosines cosh c = cosh a cosh b - sinh a sinh b cos C."
  [a b c]
  (kc/law-of-cosines-angle H a b c))

(defn triangle
  "Kernel for the triangle ABC (Poincare coordinates) drawn in model m:
   [A B C S area a b c A' B' C' dx-A dy-A dx-B dy-B dx-C dy-C min-side]
   with A B C the angles by the Moebius map, S their sum, area = pi - S,
   a = |BC| b = |CA| c = |AB|, A' B' C' by the law of cosines, then the
   drawn positions of the vertices, and the least side."
  [m ax ay bx by cx cy]
  (fn [_]
    (let [A [ax ay] B [bx by] C [cx cy]
          aa (angle-at A B C) bb (angle-at B C A) cc (angle-at C A B)
          s (e/+ aa bb cc)
          a (dist B C) b (dist C A) c (dist A B)
          [dax day] (display m A) [dbx dby] (display m B) [dcx dcy] (display m C)]
      [aa bb cc s (e/- Math/PI s) a b c
       (angle-cosines b c a) (angle-cosines c a b) (angle-cosines a b c)
       dax day dbx dby dcx dcy
       (at-most (at-most a b) c)])))

(def triangle-index
  {:A 0 :B 1 :C 2 :S 3 :area 4 :a 5 :b 6 :c 7 :A' 8 :B' 9 :C' 10
   :dax 11 :day 12 :dbx 13 :dby 14 :dcx 15 :dcy 16 :min-side 17})

(defn drawn-angle-sum
  "The Euclidean angle sum (radians) of the straight triangle on the DRAWN
   vertices, which in the Beltrami-Klein picture are the true vertices
   joined by chords: always pi, though the true sum is less (Klein's
   picture keeps lines straight and loses angles)."
  [m ax ay bx by cx cy]
  (fn [_]
    (let [[a b c] (map #(display m %) [[ax ay] [bx by] [cx cy]])
          ang (fn [p q r] (let [u (c- q p) v (c- r p)] (angle2 (e/abs (a/cross u v)) (a/dot u v))))]
      [(e/+ (ang a b c) (ang b c a) (ang c a b))])))

(defn side
  "The drawn geodesic side from p to q in model m: s -> [x y]."
  [m px py qx qy]
  (fn [s] (display m (geodesic-point [px py] [qx qy] s))))

;; ---------------------------------------------------------------------------
;; Parallels to the diameter (the real axis) through a point P

(defn- unit [v] (cscale (e/divide 1 (e/sqrt (e/+ (norm2 v) 1e-30))) v))

(def rotate "z turned by t about the centre (SO(2))." gs/rotate)

(defn frame
  "At P: the directions (in the frame where P is the centre) towards the
   ideal points -1 and +1 of the given line, and the bisector, the
   direction of the perpendicular from P to the line."
  [p]
  (let [u1 (unit (to-origin p [-1 0])) u2 (unit (to-origin p [1 0]))]
    {:u1 u1 :u2 u2 :delta (unit (c+ u1 u2))}))

(defn lobachevsky-pi "Pi(x) = 2 atan(e^-x), radians." [x] (kc/angle-of-parallelism H x))

(defn parallels
  "Kernel for P = (px, py) and a line through P at angle alpha (radians)
   from the perpendicular PD, all in model m:
   [d Pi-formula Pi-geometric residual alpha-minus-Pi
    dPx dPy dDx dDy]
   d the distance from P to the line (asinh(2|y|/(1 - |P|^2))), Pi by
   Lobachevsky's formula, Pi as half the angle at P between the two
   limiting parallels (Moebius), their difference, alpha - Pi (negative:
   the line meets; 0: limiting parallel; positive: does not meet), the
   drawn P and the drawn foot D of the perpendicular."
  [m px py alpha]
  (fn [_]
    (let [p [px py]
          {:keys [u1 u2 delta]} (frame p)
          d (asinh* (e/divide (e/* 2 (e/abs py)) (e/- 1 (norm2 p))))
          pi-f (lobachevsky-pi d)
          pi-g (e/* 0.5 (angle2 (e/abs (a/cross u1 u2)) (a/dot u1 u2)))
          ;; the foot D: where the perpendicular meets the real axis, the
          ;; point of the line through P in direction delta at distance d
          s-d (e/tanh (e/divide d 2))
          foot (from-origin p (cscale s-d delta))
          [dpx dpy] (display m p) [ddx ddy] (display m foot)]
      [d pi-f pi-g (e/- pi-f pi-g) (e/- alpha pi-f) dpx dpy ddx ddy])))

(def parallels-index {:d 0 :Pi 1 :Pi-g 2 :residual 3 :gap 4 :dpx 5 :dpy 6 :ddx 7 :ddy 8})

(defn line-through
  "The drawn whole geodesic through P at angle alpha from the perpendicular
   PD (model m): s in [-1 1] -> [x y]."
  [m px py alpha]
  (fn [s]
    (let [p [px py] {:keys [delta]} (frame p)]
      (display m (line-point p (rotate delta alpha) s)))))

(defn limiting
  "The drawn limiting parallel through P towards the ideal point sgn
   (-1 or +1): s in [-1 1] -> [x y]."
  [m px py sgn]
  (fn [s]
    (let [p [px py]]
      (display m (line-point p (to-origin p [sgn 0]) s)))))

(defn fan-line
  "The j-th of n non-meeting lines through P, between the two limiting
   parallels: angle Pi + (j/n)(pi - 2 Pi) from PD."
  [m px py j n]
  (fn [s]
    (let [p [px py]
          d (asinh* (e/divide (e/* 2 (e/abs py)) (e/- 1 (norm2 p))))
          pi-f (lobachevsky-pi d)
          alpha (e/+ pi-f (e/* (e/divide j n) (e/- Math/PI (e/* 2 pi-f))))
          {:keys [delta]} (frame p)]
      (display m (line-point p (rotate delta alpha) s)))))

(defn perpendicular
  "The drawn perpendicular from P to the diameter (P to its foot D), model
   m: s in [0 1] -> [x y]."
  [m px py]
  (fn [s]
    (let [p [px py]
          d (asinh* (e/divide (e/* 2 (e/abs py)) (e/- 1 (norm2 p))))
          {:keys [delta]} (frame p)]
      (display m (from-origin p (cscale (e/* s (e/tanh (e/divide d 2))) delta))))))

(defn drag-kernel
  "Kernel: the Poincare point under a point (x, y) dragged in the picture
   of model m, kept inside r-max: [px py]."
  [m x y]
  (fn [_] (from-display m [x y])))

;; ---------------------------------------------------------------------------
;; Grades

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(def samples
  "Triangles (Poincare coordinates) for the numeric grades: small near
   the centre, large, nearly ideal, and off-centre."
  [[0.05 0.0 -0.03 0.04 -0.02 -0.05]
   [0.6 0.1 -0.4 0.5 -0.3 -0.6]
   [0.98 0.0 -0.49 0.85 -0.49 -0.85]
   [0.2 0.7 0.5 0.6 0.3 0.3]])

(defn graded
  "[{:label :grade}] for the disk shelf."
  []
  (let [th 'theta p ['p_x 'p_y]
        w (to-origin p [(e/cos th) (e/sin th)])
        tri (fn [v] (raster/value (fn [_] ((apply triangle 0 v) nil)) 0))
        outs (map tri samples)
        g (fn [o k] (nth o (triangle-index k)))
        lc (for [o outs k [[:A :A'] [:B :B'] [:C :C']]] (- (g o (first k)) (g o (second k))))
        area-pos (for [o outs] (min 0.0 (g o :area)))
        par (for [px [-0.5 0.0 0.3] py [0.1 0.4 0.8]]
              (nth (raster/value (fn [_] ((parallels 0 px py 0.5) nil)) 0) 3))
        kp (for [[x y] [[0.1 0.2] [-0.5 0.3] [0.7 -0.6] [0.0 0.95]]]
             (let [[a b] (raster/value (fn [_] (poincare (klein [x y]))) 0)]
               (+ (Math/abs (- a x)) (Math/abs (- b y)))))]
    [{:label "T_p maps the boundary circle to itself: |T_p(e^{iθ})|² = 1 for every p"
      :grade (variant (grade/grade :symbolic [(e/- (norm2 w) 1)] 0))}
     {:label "T_p maps p to the centre, and T_p⁻¹ undoes it"
      :grade (variant (grade/grade :symbolic (concat (to-origin p p)
                                                     (c- (from-origin p (to-origin p ['z_x 'z_y])) ['z_x 'z_y])) 0))}
     {:label "the angles by the Moebius map equal those of the hyperbolic law of cosines (four triangles)"
      :grade (variant (grade/grade :numeric lc 1e-9))}
     {:label "angle sum < π, so the area π − S is positive (four triangles)"
      :grade (variant (grade/grade :numeric area-pos 1e-12))}
     {:label "half the angle between the limiting parallels is Lobachevsky's Π(d) = 2 atan e^{−d}, d = asinh(2|y|/(1 − |P|²))"
      :grade (variant (grade/grade :numeric par 1e-9))}
     {:label "Beltrami-Klein then back to Poincaré is the identity"
      :grade (variant (grade/grade :numeric kp 1e-12))}]))
