(ns alexandria.riemann-geometry.geodesic
  "Riemann, Ueber die Hypothesen (1854), II §2-§3: the shortest lines
   (geodesics) of a manifold with line element ds^2 = sum g_ij dx^i dx^j,
   and the curvature as the measure of how geodesics issuing from one
   point spread apart.

   Four surfaces, each a patch r(u, v) in space (only for drawing): its
   metric [E F G] comes from alexandria.gauss.egregium/first-form, and from
   the metric alone Emmy derives the Christoffel symbols

     Gamma^k_ij = 1/2 g^kl (d_i g_jl + d_j g_il - d_l g_ij)

   and the geodesic equation x''^k = -Gamma^k_ij x'^i x'^j, a first order
   flow on the state [u v u' v']. The flow is a raster kernel; the browser
   integrates it with RK4 (alexandria.raster.step/rk4 on the JVM for the
   grades). K at the start comes from Brioschi's formula (egregium).

     :sphere  r = (cos v cos u, cos v sin u, sin v)        K = 1
     :saddle  r = (u, v, (u^2 - v^2)/2)                    K = -1/(1+u^2+v^2)^2
     :torus   r = ((R + a cos v) cos u, (R + a cos v) sin u, a sin v),
              R = 2, a = 0.8                               K = cos v/(a (R + a cos v))
     :flat    r = (u, v, 0)                                K = 0

   Every function takes numbers or Emmy symbols."
  (:require [alexandria.gauss.egregium :as egregium]
            [alexandria.grade :as grade]
            [alexandria.raster :as raster]
            [alexandria.raster.step :as step]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; The surfaces

(def torus-R 2)
(def torus-a "Exact, so Emmy's simplifier stays rational." 4/5)

(def patches
  {:sphere (fn [u v] [(e/* (e/cos v) (e/cos u)) (e/* (e/cos v) (e/sin u)) (e/sin v)])
   :saddle (fn [u v] [u v (e/* 1/2 (e/- (e/square u) (e/square v)))])
   :torus (fn [u v] (let [w (e/+ torus-R (e/* torus-a (e/cos v)))]
                      [(e/* w (e/cos u)) (e/* w (e/sin u)) (e/* torus-a (e/sin v))]))
   :flat (fn [u v] [u v 0])})

(def kinds [:sphere :saddle :torus :flat])

(def domains
  "{kind [[u0 u1] [v0 v1]]}: the drawn parameter domain."
  {:sphere [[(- Math/PI) Math/PI] [-1.45 1.45]]
   :saddle [[-1.6 1.6] [-1.6 1.6]]
   :torus [[(- Math/PI) Math/PI] [(- Math/PI) Math/PI]]
   :flat [[-1.6 1.6] [-1.6 1.6]]})

(defn metric
  "(u, v) -> [E F G] of kind's patch."
  [kind]
  (egregium/first-form (patches kind)))

;; ---------------------------------------------------------------------------
;; Christoffel symbols and the geodesic flow, from the metric alone

(defn- d-u [f] (fn [u v] ((e/D (fn [x] (f x v))) u)))
(defn- d-v [f] (fn [u v] ((e/D (fn [y] (f u y))) v)))

(defn christoffel
  "(u, v) -> {[k i j] Gamma^k_ij} for a metric (u, v) -> [E F G], k i j in
   #{0 1} (0 = u, 1 = v)."
  [metric]
  (let [g (fn [i j] (fn [u v] (nth (metric u v) (case (+ i j) 0 0 1 1 2 2))))
        dg (fn [l i j] (if (zero? l) (d-u (g i j)) (d-v (g i j))))]
    (fn [u v]
      (let [[E F G] (metric u v)
            det (e/- (e/* E G) (e/square F))
            inv [[(e/divide G det) (e/divide (e/- 0 F) det)]
                 [(e/divide (e/- 0 F) det) (e/divide E det)]]
            first-kind (fn [l i j] (e/* 1/2 (e/- (e/+ ((dg i j l) u v) ((dg j i l) u v)) ((dg l i j) u v))))]
        (into {} (for [k [0 1] i [0 1] j [0 1]]
                   [[k i j] (e/+ (e/* (get-in inv [k 0]) (first-kind 0 i j))
                                 (e/* (get-in inv [k 1]) (first-kind 1 i j)))]))))))

(defn- expr->fn
  "An Emmy function of (u v) from expr in those symbols (frozen, evaluated
   with emmy.env's generic ops: numbers give numbers, symbols expressions)."
  [expr]
  (let [form (e/freeze expr)]
    (binding [*ns* (the-ns 'emmy.env)]
      (eval (list 'fn '[u v] form)))))

(def symbolic
  "{kind {:gamma {[k i j] expr} :K expr :metric [E F G]}}: Emmy's
   simplified Christoffel symbols and curvature in u v (memoized)."
  (memoize
   (fn [kind]
     (let [m (metric kind)]
       {:metric (mapv e/simplify (m 'u 'v))
        :gamma (update-vals ((christoffel m) 'u 'v) e/simplify)
        :K (e/simplify ((egregium/brioschi m) 'u 'v))}))))

(def compact
  "{kind {:gamma {[k i j] f} :K f :metric [f f f]}}, f of (u v)."
  (memoize
   (fn [kind]
     (let [{:keys [gamma K] :as s} (symbolic kind)]
       {:gamma (update-vals gamma expr->fn)
        :K (expr->fn K)
        :metric (mapv expr->fn (:metric s))}))))

(defn flow
  "The geodesic equation of kind as a first order flow:
   [u v p q] -> [p q p' q'], p' = -Gamma^0_ij x'^i x'^j, q' likewise."
  [kind]
  (let [gam (:gamma (compact kind))]
    (fn [[u v p q]]
      (let [vel [p q]
            acc (fn [k] (e/- 0 (reduce e/+ (for [i [0 1] j [0 1]]
                                             (e/* ((gam [k i j]) u v) (nth vel i) (nth vel j))))))]
        [p q (acc 0) (acc 1)]))))

(defn deriv-kernel
  "Kernel (state ignored) of the flow of kind at [u v p q], for raster."
  [kind]
  (fn [u v p q] (fn [_] ((flow kind) [u v p q]))))

(defn unit-velocity
  "[p q] of g-length 1 making the angle theta, measured by the metric, with
   the u line at (u, v): cos theta e1 + sin theta e2 in the orthonormal
   frame e1 = d_u / sqrt E, e2 = (E d_v - F d_u) / sqrt(E (EG - F^2))."
  [kind u v theta]
  (let [[E F G] (mapv #(% u v) (:metric (compact kind)))
        c (e/cos theta) s (e/sin theta)
        rE (e/sqrt E)
        w (e/sqrt (e/* E (e/- (e/* E G) (e/square F))))]
    [(e/- (e/divide c rE) (e/divide (e/* s F) w))
     (e/divide (e/* s E) w)]))

(defn probe
  "Kernel (state ignored): at (u, v) with heading theta,
   [x y z K E F G p q] (p q the unit initial velocity)."
  [kind]
  (fn [u v theta]
    (fn [_]
      (let [{:keys [K metric]} (compact kind)]
        (vec (concat ((patches kind) u v) [(K u v)] (mapv #(% u v) metric)
                     (unit-velocity kind u v theta)))))))

(defn xyz-kernel
  "Kernel (state ignored) of the patch of kind at (u, v)."
  [kind]
  (fn [u v] (fn [_] ((patches kind) u v))))

(def zoom
  "How much larger each surface is drawn (a similarity, so angles and the
   sign of K are kept): the four fit one MathBox range."
  {:sphere 1.6 :saddle 1.2 :torus 0.62 :flat 1.2})

(defn drawn-xyz
  "The drawn point of kind at (u, v): the patch times zoom."
  [kind u v]
  (mapv #(e/* (zoom kind) %) ((patches kind) u v)))

(defn- weight
  "1 when the integer k is i, 0 at the other kinds' indices (Lagrange
   basis on 0..3, exact for integers)."
  [k i]
  (reduce e/* 1 (for [j (range 4) :when (not= j i)] (e/divide (e/- k j) (- i j)))))

(defn- unit->domain
  "(U, V) in [-1 1]^2 to kind's parameter domain."
  [kind U V]
  (let [[[u0 u1] [v0 v1]] (domains kind)]
    [(e/+ (/ (+ u0 u1) 2) (e/* (/ (- u1 u0) 2) U))
     (e/+ (/ (+ v0 v1) 2) (e/* (/ (- v1 v0) 2) V))]))

(defn surface-of-k
  "The drawn surface of the kind with index k (0 sphere, 1 saddle, 2 torus,
   3 flat) over the unit square (U, V), branch-free for the browser."
  [k U V]
  (apply mapv e/+ (for [[i kind] (map-indexed vector kinds)]
                    (let [[u v] (unit->domain kind U V)]
                      (mapv #(e/* (weight k i) %) (drawn-xyz kind u v))))))

;; ---------------------------------------------------------------------------
;; JVM integration (grades, tests)

(def ^:private compiled
  "{kind {:flow ftms :start ftms :xyz ftms}}: raster kernels compiled once
   (unsimplified: the expressions are Emmy's simplified ones already)."
  (memoize
   (fn [kind]
     (binding [raster/*simplify?* false]
       {:flow (raster/kernels (fn [a b c d] ((flow kind) [a b c d])) 4)
        :start (raster/kernels (fn [a b t] (unit-velocity kind a b t)) 3)
        :xyz (raster/kernels (patches kind) 2)}))))

(defn- call-all [ks & xs]
  (mapv (fn [k] (case (count xs)
                  2 (k (double (nth xs 0)) (double (nth xs 1)))
                  3 (k (double (nth xs 0)) (double (nth xs 1)) (double (nth xs 2)))
                  4 (k (double (nth xs 0)) (double (nth xs 1)) (double (nth xs 2)) (double (nth xs 3)))))
        ks))

(defn xyz-at
  "The point of kind's patch at (u, v), by raster."
  [kind u v]
  (call-all (:xyz (compiled kind)) u v))

(defn run
  "RK4 path of kind from (u, v) with heading theta (unit speed), n steps of
   dt: [[u v p q] ...], the flow evaluated by its raster kernels."
  [kind u v theta n dt]
  (let [{:keys [flow start]} (compiled kind)
        [p q] (call-all start u v theta)
        deriv (fn [[a b c d]] (call-all flow a b c d))]
    (step/run deriv dt [u v p q] n)))

(defn speed2
  "g(x', x') at a state [u v p q] of kind."
  [kind [u v p q]]
  (let [[E F G] (mapv #(% u v) (:metric (compact kind)))]
    (+ (* E p p) (* 2 F p q) (* G q q))))

(defn spread
  "The 3D distance between the ends of two unit-speed geodesics of kind
   from (u, v) with headings theta +- delta, after length L (n steps), and
   the distance the plane would give, 2 L sin delta: [d d-flat]."
  [kind u v theta delta L n]
  (let [dt (/ L n)
        end (fn [th] (let [[a b] (last (run kind u v th n dt))] ((patches kind) a b)))
        a (end (+ theta delta)) b (end (- theta delta))]
    [(Math/sqrt (reduce + (map #(let [d (- (double %1) (double %2))] (* d d)) a b)))
     (* 2 L (Math/sin delta))]))

(defn case-name
  "The case of two geodesics from one point, by d / d-flat."
  [d d-flat]
  (let [ratio (/ d d-flat)]
    (cond (< ratio 0.98) :converge (> ratio 1.02) :diverge :else :parallel)))

;; ---------------------------------------------------------------------------
;; Grades

(defn- variant [kind diffs tol]
  (let [res (grade/grade kind diffs tol)]
    (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails)))

(defn graded
  "[{:label :grade}] of the geodesic section."
  []
  (let [{sg :gamma sK :K} (symbolic :sphere)
        saddle-K (:K (symbolic :saddle))
        flat-gam (vals (:gamma (symbolic :flat)))
        path (run :sphere 0.0 0.0 0.7 400 (/ (* 2 Math/PI) 400))
        speeds (for [kind kinds s (run kind 0.3 0.2 0.9 200 0.01)] (- (speed2 kind s) 1.0))
        [d-s _] (spread :sphere 0.0 0.0 0.3 0.4 Math/PI 400)
        [d-t df-t] (spread :saddle 0.0 0.0 0.3 0.2 1.2 200)]
    [{:label "Christoffel symbols of the sphere from its metric alone: Γᵘᵤᵥ = −tan v, Γᵛᵤᵤ = sin v cos v, the rest 0"
      :grade (variant :symbolic [(e/simplify (e/+ (sg [0 0 1]) (e/tan 'v)))
                                 (e/simplify (e/- (sg [1 0 0]) (e/* (e/sin 'v) (e/cos 'v))))
                                 (sg [0 0 0]) (sg [0 1 1]) (sg [1 1 1]) (sg [1 0 1])] 0)}
     {:label "the plane: every Γ is 0, geodesics are straight lines"
      :grade (variant :symbolic flat-gam 0)}
     {:label "Brioschi from the metric: K = 1 on the sphere, K = −1/(1 + u² + v²)² on the saddle"
      :grade (variant :symbolic [(e/simplify (e/- sK 1))
                                 (e/simplify (e/+ saddle-K (e/divide 1 (e/square (e/+ 1 (e/square 'u) (e/square 'v))))))] 0)}
     {:label "raster RK4 keeps the speed g(ẋ, ẋ) = 1 along geodesics of all four surfaces"
      :grade (variant :numeric speeds 1e-6)}
     {:label "a sphere geodesic (a great circle) closes after length 2π"
      :grade (variant :numeric (map - (take 2 (last path)) [(* 2 Math/PI) 0.0]) 1e-6)}
     {:label "two sphere geodesics from one point meet again at length π (K > 0 converges)"
      :grade (variant :numeric [d-s] 1e-6)}
     {:label "on the saddle they end farther apart than in the plane (K < 0 diverges)"
      :grade (variant :numeric [(max 0.0 (- df-t d-t))] 0)}]))
