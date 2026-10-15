(ns alexandria.riemann-geometry.metric
  "Riemann, Ueber die Hypothesen, welche der Geometrie zu Grunde liegen
   (Habilitation lecture, Goettingen, 10 June 1854; printed 1868), II §1-§4:
   the line element as the square root of a positive quadratic form in the
   dx whose coefficients vary continuously with the point, and the normal
   form of a manifold of constant curvature alpha,

     ds = sqrt(sum dx^2) / (1 + alpha/4 sum x^2).

   The playground's metric, in two dimensions, is that normal form written
   in skew coordinates (the reader's constant quadratic form A = [g11 g12;
   g12 g22]) with a hill of height h on top:

     g(x) = lambda(x) A,   lambda = (1 + h e^(-Q)) / (1 + alpha/4 Q)^2,
     Q = A(x, x) = g11 x^2 + 2 g12 x y + g22 y^2.

   At h = 0 it is Riemann's normal form after a linear change of
   coordinates, so K = alpha at every point whatever A is (proved by
   Emmy); the hill makes K vary from point to point. K comes from Gauss'
   formula on the metric alone (alexandria.gauss.egregium/brioschi), never
   from a picture of a surface.

   The tie: d = det A = g11 g22 - g12^2 is a control too. Moving d
   re-solves g22 = (d + g12^2) / g11 (g11 guarded away from 0); moving g11,
   g12 or g22 recomputes d. Both round trips are graded :proved.

   Every function takes numbers or Emmy symbols: Emmy derives, raster
   compiles the same functions for the page."
  (:require [alexandria.gauss.egregium :as egregium]
            [alexandria.grade :as grade]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; The metric field

(defn quad
  "A(x, x) for A = [g11 g12 g22]."
  [g11 g12 g22 x y]
  (e/+ (e/* g11 x x) (e/* 2 g12 x y) (e/* g22 y y)))

(defn conformal
  "lambda(x, y) = (1 + h e^(-Q)) / (1 + alpha Q / 4)^2."
  [g11 g12 g22 alpha h x y]
  (let [Q (quad g11 g12 g22 x y)]
    (e/divide (e/+ 1 (e/* h (e/exp (e/- 0 Q))))
              (e/square (e/+ 1 (e/* (e/divide alpha 4) Q))))))

(defn metric
  "The metric field (x, y) -> [E F G] = lambda A."
  [g11 g12 g22 alpha h]
  (fn [x y]
    (let [l (conformal g11 g12 g22 alpha h x y)]
      [(e/* l g11) (e/* l g12) (e/* l g22)])))

(defn normal-form
  "Riemann's normal form in two dimensions (A = identity, h = 0):
   (x, y) -> [E F G], E = G = 1 / (1 + alpha/4 (x^2 + y^2))^2, F = 0."
  [alpha]
  (metric 1 0 1 alpha 0))

(defn det [g11 g12 g22] (e/- (e/* g11 g22) (e/square g12)))

(def eps "Origin guard of the tie and of the indicatrix." 1e-6)

(defn away-from-zero
  "x pushed out of (-eps, eps) keeping its sign (branch-free; 0 goes to +eps)."
  [x]
  (e/+ x (e/* eps (e/- 1 (e/divide (e/abs x) (e/+ (e/abs x) eps))))))

;; ---------------------------------------------------------------------------
;; The tie g11 g12 g22 <-> d

(defn g22-of
  "The inverse of d = g11 g22 - g12^2 for g22."
  [g11 g12 d]
  (e/divide (e/+ d (e/square g12)) g11))

(defn from-g
  "g11, g12 or g22 moved: d recomputed. [g11 g12 g22 d]."
  [g11 g12 g22 _d]
  [g11 g12 g22 (det g11 g12 g22)])

(defn from-d
  "d moved (g11, g12 held): g22 = (d + g12^2) / g11. [g11 g12 g22 d]."
  [g11 g12 _g22 d]
  [g11 g12 (g22-of (away-from-zero g11) g12 d) d])

(defn tie
  "The tie kernel: from-g then from-d at a state, 8 numbers."
  [g11 g12 g22 d]
  (fn [_] (vec (concat (from-g g11 g12 g22 d) (from-d g11 g12 g22 d)))))

(def tied "The tied keys, in the order of tie-blocks." [:g11 :g12 :g22 :d])

(def ranges {:g11 [0.1 3 0.01] :g12 [-2 2 0.01] :g22 [-1 3 0.01] :d [-3 6 0.01]})

(defn tie-blocks
  "The tie kernel in alexandria.medium.tie's shape: one solved state per
   source key (g11, g12, g22: from-g; d: from-d), 16 numbers."
  [g11 g12 g22 d]
  (fn [_] (vec (concat (from-g g11 g12 g22 d) (from-g g11 g12 g22 d)
                       (from-g g11 g12 g22 d) (from-d g11 g12 g22 d)))))

;; ---------------------------------------------------------------------------
;; The probe: what the page prints about the point P

(def probe-index {:E 0 :F 1 :G 2 :det 3 :K 4 :lambda 5 :Q 6 :K-alpha 7})

(defn probe
  "Kernel (its state ignored): at P = (x, y), [E F G det(g) K lambda Q K-alpha],
   K from the metric alone (Brioschi)."
  [g11 g12 g22 alpha h x y]
  (fn [_]
    (let [m (metric g11 g12 g22 alpha h)
          [E F G] (m x y)
          K ((egregium/brioschi m) x y)]
      [E F G (e/- (e/* E G) (e/square F)) K
       (conformal g11 g12 g22 alpha h x y) (quad g11 g12 g22 x y) (e/- K alpha)])))

(defn probe-at
  "probe evaluated on the JVM at doubles, Emmy's forward-mode derivatives
   doing Brioschi's second derivatives: a vector of 8 numbers. (A raster
   JVM kernel of the unsimplified probe exceeds the JVM's method size, and
   simplifying it symbolically exhausts the heap; the page compiles the
   same probe to wasm, which has neither limit.)"
  [g11 g12 g22 alpha h x y]
  (mapv double ((probe (double g11) (double g12) (double g22) (double alpha) (double h) (double x) (double y)) 0)))

(defn curvature-field
  "Kernel of K alone at (x, y), for the colour raster of the page."
  [g11 g12 g22 alpha h]
  (fn [[x y]] ((egregium/brioschi (metric g11 g12 g22 alpha h)) x y)))

(defn indicatrix
  "The curve E dx^2 + 2F dx dy + G dy^2 = r^2 about (x0, y0): the points at
   metric distance r, to first order, drawn in the coordinate plane. phi ->
   [x y]; guarded (|q| + eps) so a degenerate form draws a long thin curve
   instead of NaN."
  [g11 g12 g22 alpha h x0 y0 r]
  (fn [phi]
    (let [[E F G] ((metric g11 g12 g22 alpha h) x0 y0)
          c (e/cos phi) s (e/sin phi)
          q (e/+ (e/* E c c) (e/* 2 F c s) (e/* G s s))
          rho (e/divide r (e/sqrt (e/+ (e/abs q) eps)))]
      [(e/+ x0 (e/* rho c)) (e/+ y0 (e/* rho s))])))

;; ---------------------------------------------------------------------------
;; Emmy's derivations, for the page and the grades

(defn normal-form-K
  "Emmy's K of Riemann's normal form, simplified (it is alpha)."
  []
  (e/simplify ((egregium/brioschi (normal-form 'alpha)) 'x 'y)))

(defn skew-normal-form-K
  "Emmy's K of the normal form in skew coordinates (h = 0) for an exact
   form A = [g11 g12 g22], alpha symbolic, simplified. (A fully symbolic A
   overflows the simplifier; exact rationals keep it polynomial.)"
  ([] (skew-normal-form-K 2 1/2 1))
  ([g11 g12 g22]
   (e/simplify ((egregium/brioschi (metric g11 g12 g22 'alpha 0)) 'x 'y))))

(defn- variant [kind diffs tol]
  (let [res (grade/grade kind diffs tol)]
    (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails)))

(defn graded
  "[{:label :grade}] of the line-element section."
  []
  (let [round-d (e/simplify (e/- (det 'a 'b (g22-of 'a 'b 'd)) 'd))
        round-g (e/simplify (e/- (g22-of 'a 'b (det 'a 'b 'c)) 'c))
        skew (for [[g11 g12 g22] [[1 0 1] [2 0.5 1] [0.7 -0.3 1.6]]
                   al [-1.0 0.0 0.8]
                   [x y] [[0.2 -0.1] [0.6 0.4] [-0.5 0.3]]]
               (nth (probe-at g11 g12 g22 al 0 x y) (probe-index :K-alpha)))]
    [{:label "Riemann's normal form ds = √(Σdx²)/(1 + α/4·Σx²) has K = α at every point (Emmy, Gauss' formula)"
      :grade (variant :symbolic [(e/- (normal-form-K) 'alpha)] 0)}
     {:label "the same in skew coordinates, g = A/(1 + α/4·A(x,x))², A = [2 ½; ½ 1] and [1 −½; −½ 3]: K = α (Emmy)"
      :grade (variant :symbolic [(e/- (skew-normal-form-K) 'alpha) (e/- (skew-normal-form-K 1 -1/2 3) 'alpha)] 0)}
     {:label "the tie: g₂₂ from d then d from g₂₂ gives d back, and g₂₂ back the other way (Emmy, g₁₁ ≠ 0)"
      :grade (variant :symbolic [round-d round-g] 0)}
     {:label "raster's Brioschi on the page's kernel: K = α at h = 0 (27 points, 3 forms A, 3 values of α)"
      :grade (variant :numeric skew 1e-8)}]))
