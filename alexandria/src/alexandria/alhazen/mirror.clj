(ns alexandria.alhazen.mirror
  "Ibn al-Haytham's mirrors (Kitab al-Manazir, Latin in Risner, Opticae
   thesaurus, Basel 1572; the parabolic burning mirror in Heiberg and
   Wiedemann, Bibl. math. 3. F. 10, 1910).

   Plane of the problem: the plane through the centre of the sphere, the
   eye and the visible point (Risner V.39: \"sumatur superficies, in qua
   sunt hae duae lineae\"). The mirror is the unit circle about the origin,
   the eye A = (a, 0), the visible point B = (b cos al, b sin al), the
   reflection point P = (cos ph, sin ph), whose normal is P itself.

     `g`          the equal-angles residual at ph: the signed sine of the
                  angle PA makes with the normal plus that of PB, divided
                  by their lengths. It is 0 exactly where the normal bisects
                  the angle APB, the law of IV.10 (angle of incidence equal
                  to the angle of reflection). Branch-free Emmy arithmetic.
     `source-for` the inverse: given the eye and the reflection point ph,
                  the distance b of the visible point on the ray at angle
                  al from the centre: the line PA reflected in the normal,
                  cut with that ray (closed form).
     `reflection-point` the root of g in (0, al), by bisection with one
                  raster call of g per step: the reflection point on the
                  convex mirror that faces both A and B.
     `roots`      every zero of g on the whole circle (scan plus bisection):
                  the points of the concave mirror as well, up to four.
   Kernels follow the raster calling convention (numbers in, vector out).
   `graded` gives the shelf's checks: proved by Emmy, numeric by raster."
  (:require [alexandria.grade :as grade]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

(defn- dot [[x1 y1] [x2 y2]] (e/+ (e/* x1 x2) (e/* y1 y2)))
(defn- cross [[x1 y1] [x2 y2]] (e/- (e/* x1 y2) (e/* y1 x2)))
(defn- sub [[x1 y1] [x2 y2]] [(e/- x1 x2) (e/- y1 y2)])
(defn- norm [v] (e/sqrt (dot v v)))

(defn g
  "Equal-angles residual [g] at reflection angle ph for eye (a, 0) and
   visible point (b, al) in polar form, mirror the unit circle."
  [a b al ph]
  (let [P [(e/cos ph) (e/sin ph)]
        u (sub [a 0] P)
        v (sub [(e/* b (e/cos al)) (e/* b (e/sin al))] P)]
    [(e/+ (e/divide (cross P u) (norm u)) (e/divide (cross P v) (norm v)))]))

(defn reflect
  "d reflected in the unit normal n: d - 2 (d.n) n."
  [d n]
  (let [k (e/* 2 (dot d n))] (sub d [(e/* k (first n)) (e/* k (second n))])))

(defn source-for
  "[b]: the distance from the centre of the visible point on the ray at
   angle al whose ray to P = (cos ph, sin ph) reflects into the eye (a, 0).
   The ray from P along w = reflect(A - P, P) meets t (cos al, sin al) at
   t = cross(P, w) / cross(e_al, w)."
  [a al ph]
  (let [P [(e/cos ph) (e/sin ph)]
        w (reflect (sub [a 0] P) P)
        dir [(e/cos al) (e/sin al)]]
    [(e/divide (cross P w) (cross dir w))]))

(defn- rv [f & xs] (binding [raster/*simplify?* false] (apply raster/value f xs)))
(defn- g-at [a b al ph] (first (rv g a b al ph)))

(defn bisect
  "Root of g in [lo, hi] (a sign change assumed), n raster calls of g."
  ([a b al lo hi] (bisect a b al lo hi 48))
  ([a b al lo hi n]
   (loop [lo lo hi hi glo (g-at a b al lo) i 0]
     (if (= i n)
       (/ (+ lo hi) 2)
       (let [m (/ (+ lo hi) 2) gm (g-at a b al m)]
         (if (pos? (* glo gm)) (recur m hi gm (inc i)) (recur lo m glo (inc i))))))))

(defn reflection-point
  "The reflection point ph on the convex mirror, eye and visible point
   outside the circle (a, b > 1, 0 < al < pi): the root of g on the arc
   both see from outside the tangent, al - acos(1/b) < ph < acos(1/a)
   (nil when that arc is empty: the mirror hides one from the other)."
  [a b al]
  (let [lo (max 0.0 (- al (Math/acos (/ 1.0 b)))) hi (min (double al) (Math/acos (/ 1.0 a)))]
    (when (< lo hi) (bisect a b al (+ lo 1e-12) (- hi 1e-12)))))

(defn roots
  "Every zero of g on [-pi, pi): a scan of `samples` raster calls, then
   bisection in each interval where g changes sign (poles of g, where P
   meets A or B, are skipped by requiring |g| small at the root)."
  ([a b al] (roots a b al 720))
  ([a b al samples]
   (let [xs (mapv #(+ (- Math/PI) (* 2 Math/PI (/ % samples))) (range (inc samples)))
         gs (mapv #(g-at a b al %) xs)]
     (vec (for [i (range samples)
                :let [g0 (gs i) g1 (gs (inc i))]
                :when (and (Double/isFinite g0) (Double/isFinite g1) (<= (* g0 g1) 0))
                :let [x (bisect a b al (xs i) (xs (inc i)))]
                :when (< (Math/abs (double (g-at a b al x))) 1e-6)]
            x)))))

(defn equal-distance-residual
  "Risner V.31: eye and visible point equally far (a) from the centre. In
   the frame whose x axis is the bisector of the angle at the centre, P =
   (1, 0) is the point on it, A = (a cos t, -a sin t), B = (a cos t,
   a sin t): the equal-angles residual there, exactly 0."
  []
  (let [P [1 0] u (sub [(e/* 'a (e/cos 't)) (e/- 0 (e/* 'a (e/sin 't)))] P)
        v (sub [(e/* 'a (e/cos 't)) (e/* 'a (e/sin 't))] P)]
    (e/+ (e/divide (cross P u) (norm u)) (e/divide (cross P v) (norm v)))))

(defn reflection-isometry-residual
  "Reflection in a unit normal keeps the length: |reflect(d, n)|^2 - |d|^2
   for n = (cos t, sin t)."
  []
  (let [d ['x 'y] w (reflect d [(e/cos 't) (e/sin 't)])]
    (e/- (dot w w) (dot d d))))

(defn equal-angles-residual
  "The law of IV.10 for the reflected direction w = reflect(d, n) of an
   incoming direction d: w and the reversed ray -d make equal angles with
   the normal (equal cosines) on opposite sides (opposite sines):
   [w.n - (-d).n, cross(n, w) + cross(n, -d)]."
  []
  (let [n [(e/cos 't) (e/sin 't)] d ['x 'y] -d [(e/- 0 'x) (e/- 0 'y)] w (reflect d n)]
    [(e/- (dot w n) (dot -d n)) (e/+ (cross n w) (cross n -d))]))

(defn parabola-hit
  "Parabolic burning mirror y^2 = 4 f x, focus (f, 0) (Heiberg-Wiedemann
   1910): [x y focus-gap]. The ray y = h parallel to the axis meets the
   mirror at (h^2 / 4f, h); reflected in the normal it crosses the axis at
   x*; the third number is x* - f."
  [f h]
  (let [x (e/divide (e/* h h) (e/* 4 f))
        n [(e/- 0 (e/* 2 f)) h]
        nn (norm n)
        w (reflect [-1 0] [(e/divide (first n) nn) (e/divide (second n) nn)])
        t (e/divide (e/- 0 h) (second w))]
    [x h (e/- (e/+ x (e/* t (first w))) f)]))

(defn focus-residual "The focus gap of `parabola-hit`, symbolic in f and h." []
  (nth (parabola-hit 'f 'h) 2))

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(def cases "Eye a, visible point b at angle al, convex mirror of radius 1; each sees the other in the mirror."
  [[2.0 3.0 1.5] [1.5 4.0 1.6] [3.0 1.2 0.7] [5.0 5.0 2.0] [1.3 8.0 1.9]])

(defn graded
  "[{:label :grade}] for the shelf."
  []
  (let [pts (for [[a b al] cases] [a b al (reflection-point a b al)])
        round (for [[a b al ph] pts] (- (first (rv source-for a al ph)) b))
        resid (for [[a b al ph] pts] (g-at a b al ph))
        focus (for [f [0.5 1 2] h [0.3 1 2.5 4]] (nth (rv parabola-hit f h) 2))]
    [{:label "reflection in a unit normal keeps lengths, |reflect(d, n)| = |d|"
      :grade (variant (grade/grade :symbolic [(reflection-isometry-residual)] 0))}
     {:label "the reflected ray makes the same angle with the normal as the incident ray, on the other side (IV.10)"
      :grade (variant (grade/grade :symbolic (equal-angles-residual) 0))}
     {:label "V.31: eye and visible point equally far from the centre, the point on the bisector reflects one into the other"
      :grade (variant (grade/grade :symbolic [(equal-distance-residual)] 0))}
     {:label (str "V.39: the reflection point found by bisection satisfies the law in " (count cases) " configurations (raster)")
      :grade (variant (grade/grade :numeric resid 1e-9))}
     {:label "the inverse: from eye and reflection point the visible point is recovered (raster round trip)"
      :grade (variant (grade/grade :numeric round 1e-8))}
     {:label "parabolic mirror: every ray parallel to the axis is reflected through the focus (Emmy, all f and h)"
      :grade (variant (grade/grade :symbolic [(focus-residual)] 0))}
     {:label "parabolic mirror, the same on raster for 12 rays"
      :grade (variant (grade/grade :numeric focus 1e-9))}]))
