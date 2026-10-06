(ns alexandria.newton.principia
  "Newton, Principia (1687), Book I, in Motte's translation (1729):

     Lemma I       first and last ratios: the limit, here the triangles' sum
                   tending to the area under the curve
     Prop. I       areas proportional to the times: the polygon of impulses,
                   an exact construction. A body moves from A to B in one
                   moment; unhindered it would reach c with Bc = AB; an
                   impulse toward S at B turns it to C, with cC parallel to
                   SB. Triangles SAB, SBc stand on equal bases AB, Bc with
                   the vertex S (Euclid I.38), and SBc, SBC on the base SB
                   between the parallels SB, cC (Euclid I.37): all equal.
     Prop. XI      an ellipse described about a focus requires a force
                   toward the focus reciprocally as the square of the
                   distance; proved here through the orbit equation (Binet's
                   form of the motion under a central force)

   Reuse searched: desargues.board.construction/check :area (Euclid's
   triangles, registered by desargues.board.euclid) through the vocabulary
   claim (area S A B); Emmy's D, simplify, literal functions for the orbit.
   The polygon is integrated by hand only in the sense Newton does it: one
   impulse per moment, each step an exact construction."
  (:require [alexandria.grade :as grade]
            [alexandria.proofs :as proofs]
            [desargues.board.construction :as c]
            [emmy.env :as e]
            [hive-dsl.result :as r]
            [alexandria.raster :as raster]))

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))
(defn- proved [diffs] (variant (grade/grade :symbolic diffs)))

;; ---------------------------------------------------------------------------
;; Proposition I: the polygon of impulses

(defn- v+ [[a b] [c d]] [(e/+ a c) (e/+ b d)])
(defn- v- [[a b] [c d]] [(e/- a c) (e/- b d)])
(defn- v* [k [a b]] [(e/* k a) (e/* k b)])

(defn impulse-step
  "Newton's step: from A to B in one moment; c = B + (B - A) where the body
   would go unhindered; the impulse at B toward S (at the origin) adds
   k (S - B), so C = c - k B, and cC is parallel to SB."
  [A B k]
  (let [c (v+ B (v- B A))]
    {:c c :C (v- c (v* k B))}))

(defn impulse-env
  "The board environment of one step, symbolic: S at the origin, A, B
   free, impulse k, for the vocabulary claims."
  []
  (let [A ['a1 'a2] B ['b1 'b2]
        {:keys [c C]} (impulse-step A B 'k)
        pt (fn [[x y]] {:x x :y y})]
    {:points {:S {:x 0 :y 0} :A (pt A) :B (pt B) :c (pt c) :C (pt C)}}))

(def prop-1-claims
  "Each step of Prop. I as a vocabulary claim over the impulse step."
  [["SAB = SBc: equal bases AB, Bc, the same vertex S (Euclid I.38)" '(= (area S A B) (area S B c))]
   ["SBc = SBC: one base SB, between the parallels SB and cC (Euclid I.37)" '(= (area S B c) (area S B C))]
   ["so SAB = SBC: equal areas in equal times" '(= (area S A B) (area S B C))]])

(defn impulse-move
  "One moment of the polygon of impulses as an Emmy function of the last two
   vertices (A, B) and the strength k = mu dt^2: the next vertex C of
   impulse-step with the impulse k / r^3 at B, r = |SB|."
  [ax ay bx by k]
  (let [r2 (e/+ (e/square bx) (e/square by))
        r3 (e/* r2 (e/sqrt r2))]
    (:C (impulse-step [ax ay] [bx by] (e// k r3)))))

(defn polygon
  "The vertices of the polygon of impulses: n moments of length dt, a force
   toward S of strength mu / r^2 acting at each vertex as one impulse
   (velocity changed by mu dt / r^2 toward S). Starts at A0 with velocity v0.
   Each moment is Newton's construction (impulse-move, Emmy) evaluated by a
   raster kernel (alexandria.raster/value)."
  [A0 v0 mu dt n]
  (let [A0 (mapv double A0)
        A1 (mapv double (v+ A0 (v* dt v0)))
        k (* (double mu) dt dt)]
    (loop [ps [A0 A1] i 1]
      (if (>= i n)
        ps
        (let [[ax ay] (ps (dec i)) [bx by] (ps i)]
          (recur (conj ps (raster/value impulse-move ax ay bx by k)) (inc i)))))))

(defn swept-areas
  "The areas SAB, SBC, ... of the polygon's triangles (S the origin), by the
   vocabulary's area."
  [ps]
  (mapv (fn [[a b]]
          (c/check {:area [:S :A :B]} {:points {:S {:x 0 :y 0} :A {:x (first a) :y (second a)}
                                               :B {:x (first b) :y (second b)}}}))
        (partition 2 1 ps)))

;; ---------------------------------------------------------------------------
;; Lemma I: first and last ratios

(defn inscribed-sum
  "Lemma II's inscribed parallelograms under y = 1 - x^2 on [0 1], n of
   them: the sum, exact."
  [n]
  (reduce + (map (fn [i] (* (/ 1 n) (- 1 (let [x (/ (inc i) n)] (* x x))))) (range n))))

(defn circumscribed-sum [n]
  (reduce + (map (fn [i] (* (/ 1 n) (- 1 (let [x (/ i n)] (* x x))))) (range n))))

;; ---------------------------------------------------------------------------
;; Proposition XI: the ellipse about a focus, the inverse square

(defn orbit-r
  "The ellipse with focus S at the origin: r(theta) = p / (1 + e cos theta),
   p the semi latus rectum (Newton's L/2)."
  [p ecc]
  (fn [theta] (e// p (e/+ 1 (e/* ecc (e/cos theta))))))

(def prop-11-identities
  "[label difference]: the orbit equation of the ellipse, and the
   acceleration of a body on it that sweeps equal areas (r^2 dtheta/dt = h,
   Prop. I), computed by Emmy from the position."
  (let [p 'p ecc 'e h 'h
        r (orbit-r p ecc)
        u (fn [t] (e// 1 (r t)))
        ;; position as a function of theta; time derivatives through
        ;; d/dt = (h / r^2) d/dtheta (equal areas)
        pos (fn [t] [(e/* (r t) (e/cos t)) (e/* (r t) (e/sin t))])
        ddt (fn [f] (fn [t] (e/* (e// h (e/square (r t))) ((e/D f) t))))
        coord (fn [i] (fn [t] (nth (pos t) i)))
        acc (fn [i] ((ddt (ddt (coord i))) 'theta))
        rr (r 'theta)
        [x y] (pos 'theta)]
    [["the conic about its focus: u = 1/r satisfies u'' + u = 1/p"
      (e/- (e/+ (((e/expt e/D 2) u) 'theta) (u 'theta)) (e// 1 p))]
     ["the acceleration points to S: a_x y - a_y x = 0"
      (e/- (e/* (acc 0) y) (e/* (acc 1) x))]
     ["its size is h^2 / (p r^2): a_x = -(h^2/p) x / r^3"
      (e/+ (acc 0) (e/* (e// (e/square h) p) (e// x (e/expt rr 3))))]
     ["and a_y = -(h^2/p) y / r^3"
      (e/+ (acc 1) (e/* (e// (e/square h) p) (e// y (e/expt rr 3))))]]))

(defn force-at
  "The size of the force at theta on the ellipse (p, e) for areal constant h:
   h^2 / (p r^2)."
  [p ecc h theta]
  (let [r ((orbit-r p ecc) theta)] (/ (* h h) (* p r r))))

(defn orbit-figure
  "The body on the ellipse (p, e) about S at the origin with its force
   arrow: state [theta end], end 0 the body P, end 1 the arrow's head,
   pointed at S with length scale * (r_min / r)^2, so the arrow scales as
   the inverse square of SP."
  [p ecc scale]
  (fn [[theta end]]
    (let [r ((orbit-r p ecc) theta)
          rmin (e// p (e/+ 1 ecc))
          len (e/* scale (e/square (e// rmin r)))
          k (e/- 1 (e/* end (e// len r)))]
      [(e/* k r (e/cos theta)) (e/* k r (e/sin theta))])))

(defn focal-figure
  "The two focal distances of the body at theta on the ellipse (p, e), S at
   the origin and the empty focus H at (-2 a e, 0), a = p / (1 - e^2):
   [SP, PH], both measured as distances (sqrt of a sum of squares)."
  [p ecc]
  (fn [[theta]]
    (let [r ((orbit-r p ecc) theta)
          x (e/* r (e/cos theta)) y (e/* r (e/sin theta))
          hx (e/- 0 (e/* 2 ecc (e// p (e/- 1 (e/square ecc)))))]
      [(e/sqrt (e/+ (e/square x) (e/square y)))
       (e/sqrt (e/+ (e/square (e/- x hx)) (e/square y)))])))

(def figures
  "Every moving figure, by name: {:f :params :state}.
   :orbit  the body and its force arrow, params [p e scale], state [theta end]
   :focal  [SP PH] at theta, params [p e], state [theta]; compiled
           unsimplified, since Emmy's simplifier roots the squares out of
           the square roots under a sign assumption that does not hold"
  {:orbit {:f orbit-figure :params [1.0 0.6 0.5] :state [0 0]}
   :focal {:f focal-figure :params [1.0 0.6] :state [0] :opts {:simplify? false}}})

(defn true-anomaly
  "The angle theta at the focus reached after fraction t of a period on the
   ellipse of eccentricity ecc, areas swept equally: Kepler's equation
   E - e sin E = 2 pi t solved by raster's Brent, then
   theta = 2 atan(sqrt(1+e) sin(E/2), sqrt(1-e) cos(E/2)) by a raster kernel."
  [ecc t]
  (let [ecc (double ecc) M (* 2 Math/PI (double t))
        E (if (zero? ecc) M
              (:value (raster/root (fn [E] (e/- E (e/* ecc (e/sin E)) M)) (- M ecc) (+ M ecc))))]
    (raster/value (fn [E] (e/* 2 (e/atan (e/* (e/sqrt (+ 1 ecc)) (e/sin (e// E 2)))
                                         (e/* (e/sqrt (- 1 ecc)) (e/cos (e// E 2))))))
                  E)))

(defn prop-11-data
  "The numbers the Prop. XI scenes show, all from raster: the angle at
   equal fractions of the period (:thetas, n + 1 of them over one period,
   the scene interpolates between neighbours) and the empty focus H."
  ([] (prop-11-data 240))
  ([n]
   (let [[p ecc] (:params (:orbit figures))
         a (/ p (- 1 (* ecc ecc)))]
     {:thetas (mapv #(true-anomaly ecc (/ % n)) (range (inc n)))
      :H [(- (* 2 a ecc)) 0.0]
      :params (:params (:orbit figures))})))

(defn polygon-data
  "The polygons of impulses a scene shows, coarse to fine, each with the
   same total time: {:polygons [[x y] ...] per impulse count, :counts}."
  []
  (let [counts [6 12 24 96]
        total 3.6]
    {:counts counts
     :polygons (mapv (fn [n] (mapv (fn [p] (mapv double p))
                                   (polygon [1.0 0.0] [0.0 1.25] 1.0 (/ total n) n)))
                     counts)}))

(defn graded
  "[{:label :grade}] of Prop. I, Lemma I and Prop. XI; the numeric grade
   (the polygon, whose vertices raster computes) carries :engine :raster.
   Lemma II's sums are exact rationals: proved, no engine."
  []
  (let [env (impulse-env)
        ps (polygon [1.0 0.0] [0.0 1.2] 1.0 0.05 200)
        areas (swept-areas ps)]
    (concat
     (map (fn [[label claim]] {:label label :grade (proved [(c/check {:claim claim} env)])}) prop-1-claims)
     [{:label "a 200-impulse polygon under the inverse square, its vertices computed by raster, sweeps equal triangles"
       :grade (variant (grade/grade :numeric (map #(e/- % (first areas)) areas) 1e-12))
       :engine :raster}
      {:label "Lemma II: circumscribed - inscribed = 1/n exactly, n = 1..50"
       :grade (proved (map (fn [n] (- (circumscribed-sum n) (inscribed-sum n) (/ 1 n))) (range 1 51)))}]
     (map (fn [[label d]] {:label label :grade (proved [d])}) prop-11-identities))))

(def proofs-resource "alexandria/newton/principia.proofs.edn")

(defn proof [] (proofs/read-proofs proofs-resource))
