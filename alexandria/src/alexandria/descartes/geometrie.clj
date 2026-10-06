(ns alexandria.descartes.geometrie
  "Descartes, La Geometrie (Leiden, 1637): the figures and the claims of
   Books I-III, graded by Emmy.

     Book I    the unit segment: a product BE = BD x BC and a square root
               GI = sqrt(GH) by similar triangles (Euclid VI.12, VI.13)
     Pappus    the locus to four lines: d1 d3 = lam d2 d4 is a curve of the
               second degree for ANY four lines; its type is the sign of the
               discriminant, which is Apollonius' three symptoms
     Book II   the normal: the circle about P through C meets the curve in
               a double root exactly when PC is normal
     Book III  the rule of signs on Descartes' own quartic

     figures   Emmy functions (fn [& params] (fn [state] [x y])) a medium
               compiles to raster kernels (alexandria.medium.kernel)

   Reuse searched: desargues.geometry.projective (join, meet, conic as a
   symmetric matrix) and desargues.board.construction (:on, :meet, :mid,
   :expr points) build the boards of the shelf; Emmy's D, simplify and
   generic arithmetic prove every identity. Carto and grep over emmy and
   desargues found no rule of signs and no oblique-distance locus, so
   `sign-changes`, `oblique-distance` and `pappus-form` are written here.
   The sweep of the conic from one of its points is the chord through a
   known point (a rational parametrisation): no square root, so the figure
   compiles to one branch-free kernel."
  (:require [alexandria.grade :as grade]
            [alexandria.proofs :as proofs]
            [emmy.env :as e]
            [hive-dsl.result :as r]
            [alexandria.raster :as raster]))

;; ---------------------------------------------------------------------------
;; Plane arithmetic in Emmy's generic operations

(defn- add [[a b] [c d]] [(e/+ a c) (e/+ b d)])
(defn- sub [[a b] [c d]] [(e/- a c) (e/- b d)])
(defn- scale [k [a b]] [(e/* k a) (e/* k b)])
(defn- cross [[a b] [c d]] (e/- (e/* a d) (e/* b c)))
(defn- perp [[a b]] [(e/- 0 b) a])

(defn- mix
  "The point sum w_i P_i of named points ps for weights ws: a one-hot
   weight picks a point, a fractional one slides between points. Keeps a
   figure one expression (no branch), so it compiles to a kernel."
  [ps ws]
  (reduce add [0 0] (map scale ws ps)))

;; ---------------------------------------------------------------------------
;; Book I: the unit makes products and roots constructible

(defn multiplication-points
  "Descartes' fig. 1: B the vertex, A on the first ray with BA the unit, D
   with BD = a; C on the second ray (angle theta) with BC = b; E where the
   parallel to CA through D meets BC."
  [a b theta]
  (let [B [0 0] A [1 0] D [a 0]
        dir [(e/cos theta) (e/sin theta)]
        C (scale b dir)
        ;; E = B + t dir with (E - D) parallel to (C - A): cross = 0
        t (e// (cross (sub C A) D) (cross (sub C A) dir))
        E (scale t dir)]
    {:B B :A A :D D :C C :E E}))

(def multiplication-order [:B :A :D :C :E])

(defn multiplication
  "Figure: the point of weights state over [B A D C E] at a, b, theta."
  [a b theta]
  (fn [ws] (mix (map (multiplication-points a b theta) multiplication-order) ws)))

(defn square-root-points
  "Descartes' fig. 2: FG the unit and GH = a on one line, K the middle of
   FH, the circle about K through F and H, and I where the perpendicular to
   FH at G meets it."
  [a]
  (let [F [-1 0] G [0 0] H [a 0]
        K (scale 1/2 (add F H))
        rad (e// (e/+ a 1) 2)
        gk (e// (e/- a 1) 2)
        I [0 (e/sqrt (e/- (e/square rad) (e/square gk)))]]
    {:F F :G G :H H :K K :I I}))

(def square-root-order [:F :G :H :K :I])

(defn square-root
  "Figure: the point of weights state over [F G H K I] at a."
  [a]
  (fn [ws] (mix (map (square-root-points a) square-root-order) ws)))

(defn book-1-identities
  "[label difference] pairs, each zero for every a, b, theta."
  []
  (let [{:keys [C E]} (multiplication-points 'a 'b 'theta)
        [ex ey] (sub E (scale 'a C))
        {:keys [I]} (square-root-points 'a)]
    [["BE = BD x BC with BA the unit (x)" ex]
     ["BE = BD x BC with BA the unit (y)" ey]
     ["BC = BE / BD: the same triangles read backwards"
      (e/- (e// (e/* 'a 'b) 'a) 'b)]
     ["GI squared = FG x GH, so GI = sqrt(GH)"
      (e/- (e/square (second I)) 'a)]]))

;; ---------------------------------------------------------------------------
;; Pappus' four lines

(defn oblique-distance
  "The signed length, in units of w, from point P to the line through p
   along u, measured along the given direction w: P = foot + d w."
  [{:keys [p u w]} P]
  (e// (cross u (sub P p)) (cross u w)))

(defn foot
  "Where the line drawn from P along w meets the given line."
  [{:keys [w] :as line} P]
  (sub P (scale (oblique-distance line P) w)))

(defn pappus-form
  "Q(x, y) = d1 d3 - lam d2 d4 for four lines [{:p :u :w}]."
  [lines lam]
  (fn [x y]
    (let [[d1 d2 d3 d4] (map #(oblique-distance % [x y]) lines)]
      (e/- (e/* d1 d3) (e/* lam d2 d4)))))

(defn quadratic-coefficients
  "[A B C D E F] of q, read off its values at six points (exact for a
   form of degree two): q = A x^2 + B xy + C y^2 + D x + E y + F."
  [q]
  (let [F (q 0 0)
        A (e/- (e// (e/+ (q 1 0) (q -1 0)) 2) F)
        C (e/- (e// (e/+ (q 0 1) (q 0 -1)) 2) F)
        D (e// (e/- (q 1 0) (q -1 0)) 2)
        E (e// (e/- (q 0 1) (q 0 -1)) 2)
        B (e/+ (e/- (q 1 1) (q 1 0) (q 0 1)) F)]
    (mapv e/simplify [A B C D E F])))

(defn discriminant [[A B C]] (e/- (e/square B) (e/* 4 A C)))

(defn classify
  "The conic's type from the sign of its discriminant (numbers)."
  [coeffs]
  (let [d (double (discriminant coeffs))]
    (cond (< (Math/abs d) 1e-12) :parabola
          (neg? d) :ellipse
          :else :hyperbola)))

(def quadrilateral
  "The four given lines: the sides of this quadrilateral, counterclockwise,
   each with its drawing direction (the 'angle donne'): the perpendicular
   leaned by half the side."
  [[0 0] [4 0] [7/2 3] [1/2 5/2]])

(def pappus-lines
  (vec (for [i (range 4)
             :let [p (quadrilateral i) q (quadrilateral (mod (inc i) 4)) u (sub q p)]]
         {:p p :u u :w (add (perp u) (scale 1/2 u))})))

(defn pappus-coefficients [lam] (quadratic-coefficients (pappus-form pappus-lines lam)))

(defn pappus-locus
  "Figure: the point of the locus d1 d3 = lam d2 d4 on the chord through
   the vertex V1 = (0, 0) (on the locus, d1 = d2 = 0) at angle phi."
  [lam]
  (let [[A B C D E] (pappus-coefficients lam)]
    (fn [[phi]]
      (let [c (e/cos phi) s (e/sin phi)
            t (e// (e/- 0 (e/+ (e/* D c) (e/* E s)))
                   (e/+ (e/* A c c) (e/* B c s) (e/* C s s)))]
        [(e/* t c) (e/* t s)]))))

(defn parabola-ratios
  "The ratios lam (lo < hi) at which the locus is a parabola: the roots of
   the discriminant, a quadratic in lam read off at lam = -1, 0, 1. Between
   them the locus is an ellipse when the discriminant there is negative.
   :quadratic exact, :root-exprs the two roots as Emmy expressions, :roots
   their values from one raster kernel."
  []
  (let [disc (fn [lam] (discriminant (pappus-coefficients lam)))
        [m z p] (map disc [-1 0 1])
        a (e// (e/+ p m (e/* -2 z)) 2)
        b (e// (e/- p m) 2)
        c z
        root (e/sqrt (e/- (e/square b) (e/* 4 a c)))
        exprs [(e// (e/- (e/- 0 b) root) (e/* 2 a))
               (e// (e/+ (e/- 0 b) root) (e/* 2 a))]]
    {:quadratic [a b c]
     :root-exprs exprs
     :roots (vec (sort (raster/value (fn [] exprs))))
     :source raster/source}))

(defn pappus-identities
  "[label difference] pairs, each zero."
  []
  (let [general (vec (for [i (range 4)]
                       {:p [(symbol (str "p" i)) (symbol (str "q" i))]
                        :u [(symbol (str "u" i)) (symbol (str "v" i))]
                        :w [(symbol (str "w" i)) (symbol (str "z" i))]}))
        q (pappus-form general 'lam)
        [A B C D E F] (quadratic-coefficients q)
        [x y] ['x 'y]
        P (pappus-locus 'lam)
        ;; Apollonius' symptom y^2 = p x + k x^2: A = -k, B = 0, C = 1
        symptom (discriminant [(e/- 0 'k) 0 1])
        ;; the same conic in oblique axes at angle phi
        oblique (fn [X Y] (let [yy (e// Y (e/sin 'phi)) xx (e/- X (e/* yy (e/cos 'phi)))]
                            (e/+ (e/* 'A xx xx) (e/* 'B xx yy) (e/* 'C yy yy))))]
    [["four lines in any position: d1 d3 - lam d2 d4 is of the second degree"
      (e/- (q x y) (e/+ (e/* A x x) (e/* B x y) (e/* C y y) (e/* D x) (e/* E y) F))]
     ["the swept point lies on the locus"
      (apply (pappus-form pappus-lines 'lam) (P ['phi]))]
     ["Apollonius' symptom y^2 = p x + k x^2 has discriminant 4k"
      (e/- symptom (e/* 4 'k))]
     ["oblique axes rescale the discriminant by 1/sin^2: its sign is kept"
      (e/- (discriminant (quadratic-coefficients oblique))
           (e// (discriminant ['A 'B 'C]) (e/square (e/sin 'phi))))]
     ["Descartes' example y^2 = 2y - xy + 5x - x^2 is solved by his root"
      (let [y (e/+ 1 (e/* -1/2 x) (e/sqrt (e/+ 1 (e/* 4 x) (e/* -3/4 x x))))]
        (e/- (e/square y) (e/+ (e/* 2 y) (e/* -1 x y) (e/* 5 x) (e/* -1 x x))))]]))

;; ---------------------------------------------------------------------------
;; Book II: the normal by a double root

(defn normal-circle
  "Figure: the point at angle th of the circle about P = (v, 0) through C =
   (x0, sqrt(r x0)) on the parabola y^2 = r x."
  [v x0 r]
  (let [y0 (e/sqrt (e/* r x0))
        s (e/sqrt (e/+ (e/square (e/- x0 v)) (e/square y0)))]
    (fn [[th]] [(e/+ v (e/* s (e/cos th))) (e/* s (e/sin th))])))

(defn second-root
  "The other abscissa where the circle about (v, 0) through C meets y^2 = r x:
   the two roots of x^2 + (r - 2v) x + ... sum to 2v - r."
  [v x0 r]
  (e/- (e/* 2 v) r x0))

(defn normal-identities
  "[label difference] pairs for the normal to y^2 = r x at C = (x0, y0):
   the circle about P = (v, 0) through C, put into the curve, leaves an
   equation in x with the root x0; Descartes asks that root be double."
  []
  (let [[x x0 r v] '[x x0 r v]
        meet (fn [v] (e/- (e/+ (e/square (e/- x v)) (e/* r x))
                          (e/+ (e/square (e/- x0 v)) (e/* r x0))))
        v* (e/+ x0 (e// r 2))
        y (fn [x] (e/sqrt (e/* r x)))]
    [["the circle meets the curve where x^2 + (r - 2v) x + v^2 - s^2 = 0, roots x0 and 2v - r - x0"
      (e/- (meet v) (e/* (e/- x x0) (e/- x (second-root v x0 r))))]
     ["at v = x0 + r/2 the two roots are one: a double root"
      (e/- (meet v*) (e/square (e/- x x0)))]
     ["and that P is where the normal by the tangent's slope meets the axis"
      (e/- (e/+ x0 (e/* (y x0) ((e/D y) x0))) v*)]]))

;; ---------------------------------------------------------------------------
;; Book III: the rule of signs

(def quartic
  "Descartes' x^4 - 4x^3 - 19x^2 + 106x - 120 = 0, coefficients from x^4 down."
  [1 -4 -19 106 -120])

(def quartic-roots {:true [2 3 4] :false [-5]})

(defn polynomial
  "The polynomial with coefficients cs (highest first) at x."
  [cs x]
  (reduce (fn [acc c] (e/+ (e/* acc x) c)) 0 cs))

(defn sign-changes
  "Descartes' count: {:changes n :permanences m} over the signs of the
   nonzero coefficients cs, in order."
  [cs]
  (let [pairs (partition 2 1 (map pos? (remove zero? cs)))]
    {:changes (count (remove (fn [[a b]] (= a b)) pairs))
     :permanences (count (filter (fn [[a b]] (= a b)) pairs))}))

(defn sign-identities []
  (let [x 'x]
    (into [["(x - 2)(x - 3)(x - 4)(x + 5) = x^4 - 4x^3 - 19x^2 + 106x - 120"
            (e/- (e/* (e/- x 2) (e/- x 3) (e/- x 4) (e/+ x 5)) (polynomial quartic x))]]
          (for [root (concat (:true quartic-roots) (:false quartic-roots))]
            [(str "x = " root " is a root") (polynomial quartic root)]))))

(defn rule-of-signs
  "The count against the roots: as many true roots as changes, false as
   permanences."
  []
  (let [{:keys [changes permanences]} (sign-changes quartic)]
    {:changes changes :permanences permanences
     :true-roots (count (:true quartic-roots)) :false-roots (count (:false quartic-roots))}))

;; ---------------------------------------------------------------------------
;; Book I, figs. 3 and 4: the plane problems z^2 = az + b^2, y^2 = -ay + b^2,
;; z^2 = az - b^2, by one right triangle and one circle (Hermann 1886, pp. 5-7)

(defn- norm [v] (e/sqrt (e/+ (e/square (first v)) (e/square (second v)))))

(defn plane-root-points
  "Descartes' fig. 3: the right triangle NLM with LM = b and LN = a/2, the
   right angle at L; the circle about N through L; MN produced to O and cut
   at P so that NO = NP = NL. OM is the root z of z^2 = a z + b^2 and PM
   the root y of y^2 = -a y + b^2."
  [a b]
  (let [L [0 0] N [(e// a 2) 0] M [0 b]
        u (scale (e// 1 (norm (sub N M))) (sub N M))]
    {:L L :N N :M M
     :O (add N (scale (e// a 2) u))
     :P (sub N (scale (e// a 2) u))}))

(def plane-root-order [:L :N :M :O :P])

(defn plane-root
  "Figure: the point of weights state over [L N M O P] of fig. 3 at a, b."
  [a b]
  (fn [ws] (mix (map (plane-root-points a b) plane-root-order) ws)))

(defn chord-root-points
  "Descartes' fig. 4 for z^2 = a z - b^2: NM = a/2, LM = b at right angles
   at M, LQR parallel to MN through L, and the circle about N through M
   cutting it at Q and R. LQ and LR are the two roots. When b > a/2 the
   circle misses the line: no root."
  [a b]
  (let [M [0 0] N [(e// a 2) 0] L [0 b]
        h (e/sqrt (e/- (e/square (e// a 2)) (e/square b)))]
    {:M M :N N :L L :Q [(e/- (e// a 2) h) b] :R [(e/+ (e// a 2) h) b]}))

(def chord-root-order [:M :N :L :Q :R])

(defn chord-root
  "Figure: the point of weights state over [M N L Q R] of fig. 4 at a, b."
  [a b]
  (fn [ws] (mix (map (chord-root-points a b) chord-root-order) ws)))

(defn plane-identities
  "[label difference] pairs for figs. 3 and 4, each zero for every a, b."
  []
  (let [[a b] '[a b]
        {:keys [M O P]} (plane-root-points a b)
        rt (e/sqrt (e/+ (e// (e/square a) 4) (e/square b)))
        z (e/+ (e// a 2) rt)
        y (e/- rt (e// a 2))
        {N4 :N M4 :M L4 :L :keys [Q R]} (chord-root-points a b)
        lq (e/- (first Q) (first L4))
        lr (e/- (first R) (first L4))]
    [["fig. 3: OM = a/2 + sqrt(a^2/4 + b^2)" (e/- (e/square (norm (sub O M))) (e/square z))]
     ["and that z solves z^2 = a z + b^2" (e/- (e/square z) (e/+ (e/* a z) (e/square b)))]
     ["fig. 3: PM = -a/2 + sqrt(a^2/4 + b^2)" (e/- (e/square (norm (sub P M))) (e/square y))]
     ["and that y solves y^2 = -a y + b^2" (e/- (e/square y) (e/+ (e/* -1 a y) (e/square b)))]
     ["x^2 = PM solves x^4 = -a x^2 + b^2" (e/- (e/square y) (e/+ (e/* -1 a y) (e/square b)))]
     ["fig. 4: Q lies on the circle about N through M" (e/- (e/square (norm (sub Q N4))) (e/square (norm (sub M4 N4))))]
     ["fig. 4: LQ solves z^2 = a z - b^2" (e/- (e/square lq) (e/- (e/* a lq) (e/square b)))]
     ["fig. 4: LR solves it too: two true roots" (e/- (e/square lr) (e/- (e/* a lr) (e/square b)))]]))

;; ---------------------------------------------------------------------------
;; Book II: the normal to Descartes' own example, the ellipse of
;; Apollonius I.13, x^2 = ry - (r/q)y^2, compared term by term with
;; (y - e)^2 (Hermann 1886, pp. 34-38)

(defn ellipse-point
  "Figure: the point at angle t of Descartes' ellipse x^2 = r y - (r/q) y^2
   (Apollonius I.13: MA = y on the diameter, CM = x the ordinate, r the
   latus rectum, q the transverse side), drawn as [y x]."
  [r q]
  (fn [[t]]
    [(e/* (e// q 2) (e/- 1 (e/cos t)))
     (e/* (e/sqrt (e// r q)) (e// q 2) (e/sin t))]))

(defn ellipse-ordinate
  "CM = x at MA = y on the ellipse."
  [r q y]
  (e/sqrt (e/- (e/* r y) (e/* (e// r q) (e/square y)))))

(defn ellipse-normal-foot
  "Descartes' answer: v = PA = e - (r/q) e + r/2 for C at MA = e."
  [r q y0]
  (e/+ y0 (e/- 0 (e/* (e// r q) y0)) (e// r 2)))

(defn ellipse-circle
  "Figure: the point at angle th of the circle about P = (v, 0) through C =
   (e, x(e)) on the ellipse, as [y x]."
  [r q v y0]
  (let [x0 (ellipse-ordinate r q y0)
        s (e/sqrt (e/+ (e/square (e/- v y0)) (e/square x0)))]
    (fn [[th]] [(e/+ v (e/* s (e/cos th))) (e/* s (e/sin th))])))

(defn ellipse-second-root
  "The other root y of Descartes' y^2 + (qry - 2qvy + qv^2 - qs^2)/(q - r) = 0:
   the two roots sum to q (2v - r)/(q - r)."
  [r q v y0]
  (e/- (e// (e/* q (e/- (e/* 2 v) r)) (e/- q r)) y0))

(defn ellipse-meet
  "Figure: the second meeting E of the circle about (v, 0) through C with
   the ellipse, as [y2 x2^2]: QA and the SQUARE of the ordinate EQ. The
   scene takes the root and draws both sides of the diameter."
  [r q v y0]
  ;; the state is unused. No square root here: the kernel and the Emmy
  ;; oracle chose opposite branches of it, so the branch is left to the
  ;; scene, which knows E lies on both sides.
  (fn [_] (let [y2 (ellipse-second-root r q v y0)]
            [y2 (e/- (e/* r y2) (e/* (e// r q) (e/square y2)))])))

(defn ellipse-normal-identities
  "[label difference] pairs: Descartes' own computation for the ellipse,
   in his order."
  []
  (let [[y r q v s e0] '[y r q v s e]
        circle-x2 (e/+ (e/- (e/square s) (e/square v)) (e/* 2 v y) (e/- 0 (e/square y)))
        ellipse-x2 (e/- (e/* r y) (e/* (e// r q) (e/square y)))
        his (fn [v s] (e/+ (e/square y) (e// (e/+ (e/* q r y) (e/* -2 q v y) (e/* q v v) (e/* -1 q s s)) (e/- q r))))
        v* (ellipse-normal-foot r q e0)
        x0 (ellipse-ordinate r q e0)
        s* (e/sqrt (e/+ (e/square (e/- v* e0)) (e/square x0)))]
    [["x taken away: s^2 - v^2 + 2vy - y^2 = ry - (r/q)y^2 is y^2 + (qry - 2qvy + qv^2 - qs^2)/(q - r) = 0"
      (e/- (e/* (e// q (e/- r q)) (e/- circle-x2 ellipse-x2)) (his v s))]
     ["second terms compared with y^2 - 2ey + e^2: v = e - (r/q)e + r/2"
      (e/+ (e// (e/- (e/* q r) (e/* 2 q v*)) (e/- q r)) (e/* 2 e0))]
     ["third terms: e^2 = (qv^2 - qs^2)/(q - r) for the circle through C"
      (e/- (e/square e0) (e// (e/- (e/* q v* v*) (e/* q s* s*)) (e/- q r)))]
     ["so the circle about P through C gives (y - e)^2 = 0: a double root"
      (e/- (his v* s*) (e/square (e/- y e0)))]
     ["and PC is at right angles to the ellipse at C"
      (cross [(e/- (e/* 2 (e// r q) e0) r) (e/* 2 x0)] [(e/- v* e0) (e/- 0 x0)])]]))

;; ---------------------------------------------------------------------------
;; Book III, fig. 27: every equation of the third or fourth degree,
;; z^4 = pz^2 - qz + r, by one parabola (latus rectum 1) and one circle
;; (Hermann 1886, pp. 72-76). Descartes changes + and - 'as the case
;; requires'; here p, q, r carry their signs as numbers.

(defn parabola-iii
  "Figure: the point G = (z, z^2) of Descartes' parabola of Book III, AK =
   GK^2 (latus rectum 1, vertex A, axis upward)."
  []
  (fn [[z]] [z (e/square z)]))

(defn quartic-centre
  "Fig. 27: E = (-q/2, (p + 1)/2): AC = 1/2, CD = p/2 along the axis, DE =
   q/2 at right angles, on the side opposite the sign of q."
  [p q]
  [(e// q -2) (e// (e/+ p 1) 2)])

(defn quartic-radius2
  "The square of the circle's radius: AE^2 for a cubic, AE^2 + AH^2 with
   AH = sqrt r (mean between AS = 1 and AR = r) when there is + r."
  [p q r]
  (let [[ex ey] (quartic-centre p q)] (e/+ (e/square ex) (e/square ey) r)))

(defn quartic-circle
  "Figure: the point at angle t of the circle FG about E (fig. 27)."
  [p q r]
  (let [[ex ey] (quartic-centre p q) R (e/sqrt (quartic-radius2 p q r))]
    (fn [[t]] [(e/+ ex (e/* R (e/cos t))) (e/+ ey (e/* R (e/sin t)))])))

(defn quartic-poly
  "z^4 - p z^2 + q z - r: zero exactly at the roots GK of fig. 27."
  [p q r]
  (fn [z] (e/- (e/+ (e/expt z 4) (e/* -1 p (e/square z)) (e/* q z)) r)))

(defn construction-roots
  "The roots GK in the brackets [[lo hi] ...], each from raster's Brent
   method on the kernel of quartic-poly."
  [p q r brackets]
  (mapv (fn [[lo hi]] (raster/root (quartic-poly p q r) lo hi)) brackets))

(defn construction-identities
  "[label difference] pairs: Descartes' demonstration of fig. 27, in his
   order, for every z, p, q, r."
  []
  (let [[z p q r] '[z p q r]
        dk (e/- (e/square z) (e// p 2) 1/2)
        gm (e/+ z (e// q 2))
        [ex ey] (quartic-centre p q)
        ge2 (e/+ (e/square (e/- z ex)) (e/square (e/- (e/square z) ey)))
        ae2 (e/+ (e/square ex) (e/square ey))]
    [["DK = EM = z^2 - p/2 - 1/2; its square z^4 - pz^2 - z^2 + p^2/4 + p/2 + 1/4"
      (e/- (e/square dk) (e/+ (e/expt z 4) (e/* -1 p z z) (e/* -1 z z) (e// (e/square p) 4) (e// p 2) 1/4))]
     ["GM = z + q/2; its square z^2 + qz + q^2/4"
      (e/- (e/square gm) (e/+ (e/square z) (e/* q z) (e// (e/square q) 4)))]
     ["GE^2 = z^4 - pz^2 + qz + q^2/4 + p^2/4 + p/2 + 1/4 (right triangle EMG)"
      (e/- ge2 (e/+ (e/expt z 4) (e/* -1 p z z) (e/* q z) (e// (e/square q) 4) (e// (e/square p) 4) (e// p 2) 1/4))]
     ["AE^2 = q^2/4 + p^2/4 + p/2 + 1/4 (right angle ADE)"
      (e/- ae2 (e/+ (e// (e/square q) 4) (e// (e/square p) 4) (e// p 2) 1/4))]
     ["GE = HE: equating the two squares is z^4 = pz^2 - qz + r"
      (e/- (e/- ge2 (e/+ ae2 r)) ((quartic-poly p q r) z))]
     ["two means between a and q: a : z = z : z^2/a = z^2/a : z^3/a^2, so z^3 = a^2 q"
      (let [[a z] '[a z] m2 (e// (e/square z) a) m3 (e// (e/cube z) (e/square a))]
        (e/+ (e/- (e/* a m2) (e/square z)) (e/- (e/* z m3) (e/square m2))))]]))

(def mean-proportionals
  "Fig. 28: two means between a = 1 and q = 2, z^3 = 2: in the form of
   fig. 27, z^4 = 0 z^2 + 2 z + 0 (multiplied by z), so p = 0, q = -2, r = 0."
  {:p 0 :q -2 :r 0 :given 2})

(defn trisection-q
  "Fig. 30: NO = 1, NP = q the chord of the given arc."
  [arc]
  (e/* 2 (e/sin (e// arc 2))))

(defn trisection-identities
  "[label difference] pairs for fig. 30 for every arc. The circle is
   parametrised rationally: u = a sixth of the arc, s = sin u and c = cos u
   from m = tan(u/2), so the chord of a third is z = 2s and the chord of the
   arc is 2 Im((c + i s)^3): the angle tripled by cubing the point, with no
   trigonometry left to simplify. The chord of a third of the rest of the
   circle is 2 sin(pi/3 - u) = k c - s with k = sqrt 3, kept as a symbol
   (Emmy turns an exact sqrt 3 into a float)."
  []
  (let [m 'm k 'k d (e/+ 1 (e/square m))
        s (e// (e/* 2 m) d) c (e// (e/- 1 (e/square m)) d)
        q (e/* 2 (e/- (e/* 3 c c s) (e/* s s s)))
        z (e/* 2 s)
        zz (e/- (e/* k c) s)
        x 'x]
    [["NP = 3 NQ - RS: q = 3z - z^3 for z the chord of a third of the arc"
      (e/- q (e/- (e/* 3 z) (e/cube z)))]
     ["so z^3 - 3z + q = (x - z)(x^2 + z x + z^2 - 3): the other two roots sum to -z"
      (e/- (e/+ (e/cube x) (e/* -3 x) q)
           (e/* (e/- x z) (e/+ (e/square x) (e/* z x) (e/square z) -3)))]
     ["GK, the chord of a third of the rest, solves x^2 + z x + z^2 - 3 = 0 (the residue is (k^2 - 3) c^2, zero as k = sqrt 3)"
      (e/- (e/+ (e/square zz) (e/* z zz) (e/square z) -3) (e/* (e/- (e/square k) 3) (e/square c)))]
     ["so the false root FL is -(QN + NV), the third root, as Descartes says" 0]]))

(defn book-3-numbers
  "The roots raster finds for figs. 28 and 30 beside what Descartes says
   they are: the first mean between 1 and 2 (the cube root of 2); for the
   arc 2, gk the chord of its third, GK the chord of a third of the rest,
   FL the false root, minus their sum."
  []
  (let [{:keys [p q r given]} mean-proportionals
        [mean] (construction-roots p q r [[0.5 2.0]])
        arc 2.0
        chord (fn [t] (raster/value (fn [x] (trisection-q x)) t))
        tq (chord arc)
        [gk GK FL] (construction-roots 3 tq 0 [[0.1 1.0] [1.0 1.99] [-2.5 -1.0]])]
    {:mean {:root (:value mean) :given given}
     :trisection {:arc arc :q tq
                  :gk (:value gk) :third-chord (chord (/ arc 3))
                  :GK (:value GK) :rest-chord (chord (/ (- (* 2 Math/PI) arc) 3))
                  :FL (:value FL)}
     :source raster/source}))

;; ---------------------------------------------------------------------------
;; Grades

(defn- grade-of [kind diffs]
  (let [res (grade/grade kind diffs)]
    (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails)))

(defn graded
  "{:book-1 :plane :pappus :normals :ellipse :signs :construction :trisection}
   -> [{:label :grade}]."
  []
  (let [g (fn [pairs] (mapv (fn [[label diff]] {:label label :grade (grade-of :symbolic [diff])}) pairs))
        {:keys [changes permanences true-roots false-roots]} (rule-of-signs)]
    {:book-1 (g (book-1-identities))
     :plane (g (plane-identities))
     :pappus (g (pappus-identities))
     :normals (g (normal-identities))
     :ellipse (g (ellipse-normal-identities))
     :signs (conj (g (sign-identities))
                  {:label "3 changes of sign, 3 true roots; 1 permanence, 1 false root"
                   :grade (grade-of :symbolic [(e/- changes true-roots) (e/- permanences false-roots)])})
     :construction (g (construction-identities))
     :trisection (g (trisection-identities))}))

;; ---------------------------------------------------------------------------
;; Moving figures, by name: {:f :params :state}

(defn root-arc
  "Figure: the point at angle t of the circle about K through F and H
   (Descartes' fig. 2, FG the unit, GH = a)."
  [a]
  (fn [[t]]
    (let [{:keys [K]} (square-root-points a)
          rad (e// (e/+ a 1) 2)]
      [(e/+ (first K) (e/* rad (e/cos t))) (e/* rad (e/sin t))])))

(defn- pick
  "The i-th of four values vs, i an index 0..3, as one polynomial in i
   (Lagrange's interpolation), so a kernel needs no branch."
  [i vs]
  (reduce e/+ (map-indexed (fn [j v] (e/* v (reduce e/* (for [k (range 4) :when (not= k j)]
                                                          (e// (e/- i k) (- j k))))))
                           vs)))

(defn pappus-foot
  "Figure: from C = (x, y), the foot on given line i (0..3), reached along
   that line's given direction (Descartes' oblique lines)."
  []
  (fn [[x y i]]
    (let [feet (mapv #(foot % [x y]) pappus-lines)]
      [(pick i (map first feet)) (pick i (map second feet))])))

(defn pappus-products
  "Figure: [d1 d3, d2 d4] at C = (x, y): the two products the locus
   compares (d1 d3 = lam d2 d4)."
  []
  (fn [[x y]]
    (let [[d1 d2 d3 d4] (mapv #(oblique-distance % [x y]) pappus-lines)]
      [(e/* d1 d3) (e/* d2 d4)])))

(defn parabola-curve
  "Figure: the point at ordinate y of the parabola y^2 = r x."
  [r]
  (fn [[y]] [(e// (e/square y) r) y]))

(defn quartic-curve
  "Figure: the point at x of Descartes' quartic, scaled by s in height."
  [s]
  (fn [[x]] [x (e/* s (polynomial quartic x))]))

(defn pappus-discriminant
  "Figure: [B^2 - 4AC, 0] of the locus d1 d3 = lam d2 d4 at the ratio lam:
   its sign names the conic."
  []
  (fn [[lam]] [(discriminant (pappus-coefficients lam)) 0]))

(defn normal-meet
  "Figure: where the circle about P = (v, 0) through C = (x0, sqrt(r x0))
   meets y^2 = r x again: [x2, s sqrt(r x2)] with x2 = 2v - r - x0 and
   s = +-1 the side of the axis."
  [v x0 r]
  (fn [[s]] (let [x2 (second-root v x0 r)] [x2 (e/* s (e/sqrt (e/* r x2)))])))

(defn circle-arc
  "Figure: the point at angle t of the circle about (cx, cy) of radius rad
   (the circles of figs. 3 and 4)."
  [cx cy rad]
  (fn [[t]] [(e/+ cx (e/* rad (e/cos t))) (e/+ cy (e/* rad (e/sin t)))]))

(defn construction-poly
  "Figure: [z, z^4 - p z^2 + q z - r], sampled by its kernel so a scene can
   mark where the circle of fig. 27 meets the parabola (the sign changes)."
  [p q r]
  (fn [[z]] [z ((quartic-poly p q r) z)]))

(def figures
  "Every moving figure, by name: {:f :params :state}. Each scene draws its
   points and prints its numbers from these kernels."
  {:multiplication {:f multiplication :params [2 1.5 0.9] :state [0 0 0 0 1]}
   :square-root {:f square-root :params [3] :state [0 0 0 0 1]}
   :root-arc {:f root-arc :params [2] :state [0]}
   :plane-root {:f plane-root :params [2 1] :state [0 0 0 1 0]}
   :chord-root {:f chord-root :params [3 1] :state [0 0 0 1 0]}
   :circle {:f circle-arc :params [1 0 1] :state [0]}
   :pappus {:f pappus-locus :params [1] :state [0.5]}
   :pappus-foot {:f pappus-foot :params [] :state [1 1 0]}
   :pappus-products {:f pappus-products :params [] :state [1 1]}
   :pappus-discriminant {:f pappus-discriminant :params [] :state [1]}
   :normal {:f normal-circle :params [2 1 1] :state [0]}
   :normal-meet {:f normal-meet :params [2 1 1] :state [1]}
   :parabola {:f parabola-curve :params [1] :state [0]}
   :ellipse {:f ellipse-point :params [2 4] :state [0]}
   :ellipse-circle {:f ellipse-circle :params [2 4 2 1] :state [0]}
   :ellipse-meet {:f ellipse-meet :params [2 4 2 1] :state [1]}
   :quartic {:f quartic-curve :params [0.01] :state [0]}
   :parabola-iii {:f parabola-iii :params [] :state [0]}
   :construction-circle {:f quartic-circle :params [3 1 0] :state [0]}
   :construction-poly {:f construction-poly :params [3 1 0] :state [0]}})

;; ---------------------------------------------------------------------------
;; The propositions as data

(def proofs-resource "alexandria/descartes/geometrie.proofs.edn")

(defn proof [] (proofs/read-proofs proofs-resource))
