(ns alexandria.delian.duplication
  "The doubling of the cube (the Delian problem) and its ancient solutions,
   as Eutocius collects them in his commentary on Archimedes, Sphere and
   Cylinder II.1 (Heiberg, Archimedis opera III, 1881, pp. 66-127), and its
   settlement by Wantzel (1837).

     hippocrates   the reduction: a : x = x : y = y : b gives x^3 = a^2 b,
                   so b = 2a doubles the cube (Emmy identities)
     menaechmus    the loci of the means, y^2 = A x, x y = A E, x^2 = E y,
                   all met at the means (Emmy identities)
     archytas      the cone, the cylinder and the torus (Heath's equations)
                   met where AC : AP = AP : AM = AM : AB (Emmy identities)
     mesolabe      Eratosthenes' three sliding frames: the line through A
                   and B passes the frames' edges in continued proportion
                   at every slide (Emmy); the slide that meets the given
                   end is a raster root
     conchoid      Nicomedes' curve: the chord beyond the ruler has the
                   interval's length on every line through the pole (Emmy)
     cissoid       Diocles' curve: its ordinate is the second mean (Emmy)
     wantzel       x^3 - 2 has no rational root, so it is irreducible over
                   the rationals, and its degree 3 is no power of 2
     numbers       every number by alexandria.raster (brent, kernels)

   Emmy is the symbolic layer only; every number comes from raster."
  (:require [alexandria.grade :as grade]
            [alexandria.proofs :as proofs]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

(def proofs-resource "alexandria/delian/duplication.proofs.edn")

(defn proof [] (proofs/read-proofs proofs-resource))

;; ---------------------------------------------------------------------------
;; Hippocrates: two means between a and b in continued proportion

(def hippocrates
  "[label difference] pairs, each zero for every a and t. The means between
   a and b = a t^3 are x = a t and y = a t^2."
  (let [a 'a t 't x (e/* a t) y (e/* a (e/square t)) b (e/* a (e/cube t))]
    [["a : x = x : y" (e/- (e// a x) (e// x y))]
     ["x : y = y : b" (e/- (e// x y) (e// y b))]
     ["then x^3 = a^2 b" (e/- (e/cube x) (e/* (e/square a) b))]
     ["the ratio a : b is the ratio a : x compounded three times" (e/- (e/cube (e// a x)) (e// a b))]]))

(def doubling
  "With b = 2a. s the side of the double cube, s^3 = 2 a^3, written by its
   consequence: a, s, s^2/a, 2a are in continued proportion."
  (let [a 'a s 's y (e// (e/square s) a)]
    [["a : s = s : s^2/a, for any s" (e/- (e// a s) (e// s y))]
     ["s : s^2/a = s^2/a : 2a exactly when s^3 = 2 a^3: the difference is (2a^3 - s^3) times a factor"
      (e/- (e/- (e/* s (e/* 2 a)) (e/square y))
           (e// (e/* s (e/- (e/* 2 (e/cube a)) (e/cube s))) (e/square a)))]]))

;; ---------------------------------------------------------------------------
;; Menaechmus (Eutocius' lettering: the given A, E; the means B, G; Theta
;; the meeting point, DZ = G, ZTheta = B)

(def menaechmus
  "The means B = a t, G = a t^2 between A = a and E = a t^3. Eutocius: since
   A, B, G are proportional, A . G = B^2, so Theta is on the parabola of
   parameter A; since B . G = A . E, Theta is on the hyperbola in the
   asymptotes KD, DZ; the second solution adds the parabola of parameter E."
  (let [a 'a t 't A a B (e/* a t) G (e/* a (e/square t)) E (e/* a (e/cube t))]
    [["A . G = B^2: (DZ, ZTheta) = (G, B) is on the parabola y^2 = A x" (e/- (e/* A G) (e/square B))]
     ["B . G = A . E: it is on the hyperbola x y = A E" (e/- (e/* G B) (e/* A E))]
     ["E . B = G^2: and on the parabola x^2 = E y" (e/- (e/* E B) (e/square G))]]))

(defn menaechmus-point
  "The meeting point of the parabolas x^2 = a y and y^2 = b x (vertex O,
   Heath's form of Menaechmus) for numbers a, b: x by raster's Brent root of
   x^3 - a^2 b on [min, max] of a and b, y = x^2/a by a raster kernel.
   {:x :y :root}."
  [a b]
  (let [{x :value :as root} (raster/root (fn [x] (e/- (e/cube x) (e/* (e/square a) b)))
                                         (min a b) (max a b))]
    {:x x :y (raster/value (fn [x] (e// (e/square x) a)) x) :root root}))

;; ---------------------------------------------------------------------------
;; Archytas (Heath 1896, p. xxii: AC = a, AB = b; P the meeting point, M
;; its foot on the circle ABC, AP = r, AM = m)

(defn archytas-surfaces
  "Heath's three surfaces as Emmy functions of [x y z], each zero on its
   surface: the cone x^2+y^2+z^2 = (a/b)^2 x^2, the cylinder x^2+y^2 = a x,
   the torus x^2+y^2+z^2 = a sqrt(x^2+y^2)."
  [a b]
  {:cone (fn [[x y z]] (e/- (e/+ (e/square x) (e/square y) (e/square z))
                            (e/* (e// (e/square a) (e/square b)) (e/square x))))
   :cylinder (fn [[x y _]] (e/- (e/+ (e/square x) (e/square y)) (e/* a x)))
   :torus (fn [[x y z]] (e/- (e/+ (e/square x) (e/square y) (e/square z))
                             (e/* a (e/sqrt (e/+ (e/square x) (e/square y))))))})

(def archytas
  "[label difference] pairs, with m = AM and b = AB free and AC = a =
   m^3/b^2 (so no root is needed): P = (m^2/a, y, z), y^2 = m^2 - x^2,
   AP = r = m^2/b, z^2 = r^2 - m^2."
  (let [m 'm b 'b a (e// (e/cube m) (e/square b))
        x (e// (e/square m) a) r (e// (e/square m) b)
        y2 (e/- (e/square m) (e/square x)) z2 (e/- (e/square r) (e/square m))]
    [["P is on the cylinder: x^2 + y^2 = AC . x" (e/- (e/+ (e/square x) y2) (e/* a x))]
     ["P is on the torus: AP^2 = AC . AM" (e/- (e/+ (e/square x) y2 z2) (e/* a m))]
     ["P is on the cone: AP^2 = (AC/AB)^2 x^2"
      (e/- (e/+ (e/square x) y2 z2) (e/* (e// (e/square a) (e/square b)) (e/square x)))]
     ["AC : AP = AP : AM" (e/- (e// a r) (e// r m))]
     ["AP : AM = AM : AB" (e/- (e// r m) (e// m b))]]))

(defn archytas-gap
  "The curve on the cylinder (cut by the torus: AP^2 = AC . AM) against the
   cone (AP = AM^2/AB), as one function of m = AM: m^4/b^2 - a m, zero at
   AM^3 = AC . AB^2."
  [a b]
  (fn [m] (e/- (e// (e/expt m 4) (e/square b)) (e/* a m))))

(defn archytas-numbers
  "AM by raster's Brent root of archytas-gap on [b, a], and P from it by a
   raster kernel: {:m :r :x :y :z :root}."
  [a b]
  (let [{m :value :as root} (raster/root (archytas-gap a b) b a)
        [x y z r] (raster/value (fn [m] (let [x (e// (e/square m) a) r (e// (e/square m) b)]
                                          [x (e/sqrt (e/- (e/square m) (e/square x)))
                                           (e/sqrt (e/- (e/square r) (e/square m))) r]))
                                m)]
    {:m m :r r :x x :y y :z z :root root}))

;; ---------------------------------------------------------------------------
;; Eratosthenes' mesolabe: three equal frames of height a and width w on a
;; base line; frame 1 fixed on [0, w], frame 2 slid onto [w s, w s + w],
;; frame 3 onto [w s (1 + s), w s (1 + s) + w]; each frame's diagonal runs
;; from its top left corner to its bottom right corner. s = 1 is the
;; frames side by side.

(defn mesolabe-points
  "The points at slide s, Emmy [x y]: A the top of frame 1's left edge (AE);
   B where frame 2's diagonal crosses frame 1's right edge (BZ); G where
   frame 3's diagonal crosses frame 2's right edge (GH); D the point of
   frame 3's right edge on the line AB (DTheta)."
  [a w s]
  {:A [0 a]
   :B [w (e/* a s)]
   :G [(e/* w (e/+ 1 s)) (e/* a (e/square s))]
   :D [(e/* w (e/+ 1 s (e/square s))) (e/* a (e/cube s))]})

(def mesolabe
  "[label difference] pairs: B and G are where the diagonals cross the
   edges; A, B, G, D are on one line for every slide; their heights are in
   continued proportion."
  (let [a 'a w 'w s 's {[ax ay] :A [bx by] :B [gx gy] :G [dx dy] :D} (mesolabe-points a w s)
        slope (fn [[x0 y0] [x1 y1]] (e// (e/- y1 y0) (e/- x1 x0)))]
    [["B is on frame 2's diagonal" (e/- by (e/* a (e/- 1 (e// (e/- w (e/* w s)) w))))]
     ["G is on frame 3's diagonal"
      (e/- gy (e/* a (e/- 1 (e// (e/- (e/+ w (e/* w s)) (e/* w s (e/+ 1 s))) w))))]
     ["A, B, G lie on one line" (e/- (slope [ax ay] [bx by]) (slope [bx by] [gx gy]))]
     ["B, G, D lie on one line" (e/- (slope [bx by] [gx gy]) (slope [gx gy] [dx dy]))]
     ["AE : BZ = BZ : GH" (e/- (e// ay by) (e// by gy))]
     ["BZ : GH = GH : DTheta" (e/- (e// by gy) (e// gy dy))]]))

(defn mesolabe-slide
  "The slide s that brings the line AB to the given height b on frame 3's
   right edge (a s^3 = b), by raster's Brent root on [0.05, 1]."
  [a b]
  (raster/root (fn [s] (e/- (e/* a (e/cube s)) b)) 0.05 1))

;; ---------------------------------------------------------------------------
;; Nicomedes' conchoid: the ruler is the line y = 0, the pole E = (0, -d),
;; the interval l: on every line through E the point l beyond the ruler.

(defn conchoid
  "The conchoid's point on the line through the pole at angle phi to the
   ruler: [x y]."
  [d l]
  (fn [phi]
    (let [t (e// d (e/sin phi))]
      [(e/* (e/+ t l) (e/cos phi)) (e/- (e/* (e/+ t l) (e/sin phi)) d)])))

(def conchoid-claims
  "The defining property, as squared lengths (exact)."
  (let [d 'd l 'l phi 'phi
        [x y] ((conchoid d l) phi)
        rx (e/* (e// d (e/sin phi)) (e/cos phi))]
    [["the segment from the ruler to the curve has the interval's length l"
      (e/- (e/+ (e/square (e/- x rx)) (e/square y)) (e/square l))]]))

;; ---------------------------------------------------------------------------
;; Diocles' cissoid: the circle of diameter 2r; u the abscissa from the
;; vertex, h the circle's ordinate there, y the cissoid's.

(defn cissoid
  "The cissoid point for abscissa u from the vertex: [u y], y^2 = u^3/(2r - u)."
  [r]
  (fn [u] [u (e/sqrt (e// (e/cube u) (e/- (e/* 2 r) u)))]))

(def cissoid-claims
  "Diocles' proportion: 2r - u, h, u, y are in continued proportion."
  (let [r 'r u 'u h (e/sqrt (e/* u (e/- (e/* 2 r) u))) y (e// (e/* u h) (e/- (e/* 2 r) u))]
    [["(2r - u) : h = h : u (the circle)" (e/- (e/* (e/- (e/* 2 r) u) u) (e/square h))]
     ["h : u = u : y" (e/- (e/* h y) (e/square u))]
     ["y^2 (2r - u) = u^3: the point is on the cissoid" (e/- (e/* (e/square y) (e/- (e/* 2 r) u)) (e/cube u))]]))

;; ---------------------------------------------------------------------------
;; Wantzel 1837

(defn rational-root-candidates
  "Every rational root of a monic integer polynomial is an integer dividing
   its constant term c0: the divisors, with both signs."
  [c0]
  (let [n (abs (long c0))]
    (for [d (range 1 (inc n)) :when (zero? (mod n d)) s [1 -1]] (* s d))))

(defn wantzel
  "Wantzel's facts for x^3 - 2, exact integers: the candidates, the values
   of x^3 - 2 at them, whether none is zero (then x^3 - 2 has no factor of
   degree 1 over the rationals, so as a cubic it is irreducible), and
   whether 3 is a power of 2."
  []
  (let [cs (vec (rational-root-candidates -2))
        vals (mapv (fn [x] (- (* x x x) 2)) cs)]
    {:candidates cs :values vals
     :irreducible (not-any? zero? vals)
     :degree 3
     :power-of-two? (boolean (some #{3} (map #(bit-shift-left 1 %) (range 0 8))))}))

(def wantzel-identities
  "The algebra of Wantzel's section III: a root p + q sqrt(k) of a rational
   equation brings its conjugate p - q sqrt(k) with it."
  (let [p 'p q 'q k 'k z (e/sqrt k)
        f (fn [x] (e/- (e/cube x) 2))]
    [["f(p + q sqrt k) + f(p - q sqrt k) has no sqrt k: 2 (p^3 + 3 p q^2 k - 2)"
      (e/- (e/+ (f (e/+ p (e/* q z))) (f (e/- p (e/* q z))))
           (e/* 2 (e/- (e/+ (e/cube p) (e/* 3 p (e/square q) k)) 2)))]
     ["f(p + q sqrt k) - f(p - q sqrt k) = 2 q sqrt k (3 p^2 + q^2 k)"
      (e/- (e/- (f (e/+ p (e/* q z))) (f (e/- p (e/* q z))))
           (e/* 2 q z (e/+ (e/* 3 (e/square p)) (e/* (e/square q) k))))]]))

;; ---------------------------------------------------------------------------
;; Grades

(defn- symbolic [label diff]
  (let [res (grade/grade :symbolic [diff])]
    {:label label :grade (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails)}))

(defn graded
  "{section [{:label :grade}]}, each identity graded by alexandria.grade."
  []
  (let [g (fn [pairs] (mapv (fn [[l d]] (symbolic l d)) pairs))
        w (wantzel)]
    {:hippocrates (g (concat hippocrates doubling))
     :menaechmus (g menaechmus)
     :archytas (g archytas)
     :mesolabe (g mesolabe)
     :conchoid (g conchoid-claims)
     :cissoid (g cissoid-claims)
     :wantzel (into (g wantzel-identities)
                    [{:label "x^3 - 2 is 1 - 2, -1 - 2, 8 - 2, -8 - 2 at the candidates 1, -1, 2, -2: never 0, so no rational root (exact)"
                      :grade (if (:irreducible w) :grade/proved :grade/fails)}
                     {:label "the degree 3 is none of 1, 2, 4, 8, ...: no power of 2 (exact)"
                      :grade (if (:power-of-two? w) :grade/fails :grade/proved)}])}))

(defn numeric
  "A raster number checked against its exact target within tol:
   {:label :value :source :grade}."
  [label value target tol]
  {:label label :value value :source raster/source
   :grade (if (< (abs (- (double value) (double target))) tol) :grade/numeric :grade/fails)})

;; ---------------------------------------------------------------------------
;; Figures: Emmy functions (fn [& params] (fn [state] [x y])) that
;; alexandria.medium.kernel compiles to raster kernels for the browser.

(defn- pick
  "Branch-free choice among vs by the index k = 0..n-1 (Lagrange weights)."
  [k vs]
  (let [n (count vs)]
    (reduce e/+ (for [i (range n)]
                  (e/* (nth vs i)
                       (reduce e/* 1 (for [j (range n) :when (not= i j)]
                                       (e// (e/- k j) (- i j)))))))))

(defn parabola-x
  "Menaechmus' parabola x^2 = a y (axis Oy, parameter a), by the abscissa u."
  [a] (fn [[u]] [u (e// (e/square u) a)]))

(defn parabola-y
  "The parabola y^2 = b x (axis Ox, parameter b), by the ordinate u."
  [b] (fn [[u]] [(e// (e/square u) b) u]))

(defn hyperbola
  "The hyperbola x y = a b in its asymptotes, by the abscissa u > 0."
  [a b] (fn [[u]] [u (e// (e/* a b) u)]))

(defn meeting
  "The meeting point for a and b = 2a: [a c, a c^2] with c the cube root of
   2 from raster's Brent root (passed in, so the kernel does the scaling)."
  [a c] (fn [[_]] [(e/* a c) (e/* a (e/square c))]))

(defn mesolabe-figure
  "Point k (0 A, 1 B, 2 G, 3 D) of the mesolabe at slide s."
  [a w s]
  (fn [[k]]
    (let [{:keys [A B G D]} (mesolabe-points a w s)]
      [(pick k (mapv first [A B G D])) (pick k (mapv second [A B G D]))])))

(defn frame-edge
  "The left edge of frame k (0, 1, 2) at slide s, on the base line: [x 0]."
  [w s]
  (fn [[k]] [(pick k [0 (e/* w s) (e/* w s (e/+ 1 s))]) 0]))

(defn conchoid-figure
  "Nicomedes' conchoid as a figure of the angle phi."
  [d l] (fn [[phi]] ((conchoid d l) phi)))

(defn cissoid-figure
  "Diocles' cissoid by the abscissa u from the vertex (the upper branch)."
  [r] (fn [[u]] ((cissoid r) u)))

(def figures
  "{name {:f :params :state}} for alexandria.medium.kernel."
  {:parabola-x {:f parabola-x :params [1] :state [0.5]}
   :parabola-y {:f parabola-y :params [2] :state [0.5]}
   :hyperbola {:f hyperbola :params [1 2] :state [1]}
   :meeting {:f meeting :params [1 1.26] :state [0]}
   :mesolabe {:f mesolabe-figure :params [2 1 1] :state [0]}
   :frame-edge {:f frame-edge :params [1 1] :state [0]}
   :conchoid {:f conchoid-figure :params [1 2] :state [1.0]}
   :cissoid {:f cissoid-figure, :params [1], :state [0.5]}})

(defn cube-root-2
  "The cube root of 2 by raster's Brent root of x^3 - 2 on [1, 2]."
  []
  (:value (raster/root (fn [x] (e/- (e/cube x) 2)) 1 2)))

(defn scene-data
  "The numbers the scenes show, every one from raster."
  []
  (let [c (cube-root-2)
        {m :m r :r} (archytas-numbers 2 1)
        s (:value (mesolabe-slide 2 1))]
    {:cube-root-2 c
     :archytas {:am m :ap r}
     :mesolabe-slide s
     :mesolabe-means (raster/value (fn [s] [(e/* 2 s) (e/* 2 (e/square s))]) s)}))
