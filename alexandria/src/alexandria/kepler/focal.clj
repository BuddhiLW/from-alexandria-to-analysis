(ns alexandria.kepler.focal
  "Kepler's orbit as Apollonius' conic about its focus, and the laws that
   tie its numbers: the derivation behind the Kepler widget.

   Frame: the Sun at the origin, the orbit of semi-major axis a and
   eccentricity e, the far vertex (aphelion) on +x, so the planet at
   eccentric anomaly E is

     x = a e - a cos E,  y = b sin E,  b = a sqrt(1 - e^2)

   and its distance from the Sun is rho = a (1 - e cos E). Emmy derives:

     focus-directrix  rho - e x is the same at every E (its E-derivative
                      simplifies to 0); at E = 0 it is l = a (1 - e^2), the
                      semi-latus rectum (Apollonius' half parameter)
     focal polar      rho - e rho cos(theta) = l is linear in rho; Emmy's
                      solve-linear gives rho = l / (1 - e cos theta), the
                      conics playground's form, here with the Sun at F
     third law        Newton's period T = 2 pi sqrt(a^3 / mu) with mu = 4 pi^2
                      (AU, years, the Sun's mass) is T^2 = a^3; solved
                      forward T = a sqrt a and back a = T^(2/3)
     second law       the sweep rate (x vy - y vx)/2 has zero time
                      derivative under any force toward the Sun (Principia
                      I.1), so equal times sweep equal areas
     vis viva         the speed at perihelion sqrt(mu (1 + e) / (a (1 - e)))
                      gives an orbit of energy -mu/(2a), so the RK4 flow
                      started there runs on the orbit of axis a

   The tie of the widget: a, T, e, l, every one a control.

     a moves   e fixed: T = a sqrt a, l = a (1 - e^2)
     T moves   e fixed: a = T^(2/3), l = a (1 - e^2)
     e moves   a fixed: l = a (1 - e^2) (T stays)
     l moves   e fixed: a = l / (1 - e^2) (Emmy's solve-linear), T = a sqrt a

   forward/inverse compose to the identity, :grade/proved. The equal areas
   themselves are raster's RK4 (alexandria.raster/ode) on the inverse-square
   flow: one period returns the planet to perihelion and n equal times sweep
   n equal areas, each pi a b / n (:grade/numeric, :engine :raster).

   Reuse: alexandria.apollonius.projection/solve-linear-in (Emmy's D and
   solve-linear), alexandria.kepler.orbit (the ellipse), alexandria.raster
   (ode, value)."
  (:require [alexandria.apollonius.projection :as pj]
            [alexandria.grade :as grade]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; The orbit about the Sun

(defn minor [a ecc] (e/* a (e/sqrt (e/- 1 (e/square ecc)))))

(defn planet
  "The planet at eccentric anomaly E, the Sun at the origin and the
   aphelion on +x: [x y]."
  [a ecc E]
  [(e/- (e/* a ecc) (e/* a (e/cos E))) (e/* (minor a ecc) (e/sin E))])

(defn distance
  "Sun-planet distance through E: a (1 - e cos E)."
  [a ecc E]
  (e/* a (e/- 1 (e/* ecc (e/cos E)))))

(defn semi-latus
  "l = a (1 - e^2)."
  [a ecc]
  (e/* a (e/- 1 (e/square ecc))))

(defn focal-rho
  "rho = l / (1 - e cos theta)."
  [l ecc theta]
  (e// l (e/- 1 (e/* ecc (e/cos theta)))))

(def mu
  "The Sun's GM in AU^3 / year^2: 4 pi^2, so that the Earth (a = 1) has
   T = 1."
  (e/* 4 (e/square 'pi)))

(defn period-newton
  "T = 2 pi sqrt(a^3 / mu) (Principia I.15)."
  [a mu]
  (e/* 2 'pi (e/sqrt (e// (e/cube a) mu))))

;; ---------------------------------------------------------------------------
;; The tie (numbers or symbols; raster compiles them)

(defn period
  "Forward third law: T = a sqrt a (T^2 = a^3)."
  [a]
  (e/* a (e/sqrt a)))

(defn axis-of-period
  "Inverse third law: a = T^(2/3), written exp((2/3) log T)."
  [T]
  (e/exp (e/* 2/3 (e/log T))))

(defn axis-of-latus
  "a from l and e, by Emmy's solve-linear on a (1 - e^2) = l."
  [l ecc]
  (:root (pj/solve-linear-in (fn [a] (e/- (semi-latus a ecc) l)))))

;; ---------------------------------------------------------------------------
;; The flow (Newton's inverse square, the sweep rate as a fifth coordinate)

(defn accel
  "-mu (x, y) / r^3."
  [mu x y]
  (let [r2 (e/+ (e/square x) (e/square y))
        r3 (e/* r2 (e/sqrt r2))]
    [(e/- 0 (e// (e/* mu x) r3)) (e/- 0 (e// (e/* mu y) r3))]))

(defn sweep-rate
  "dA/dt = (x vy - y vx) / 2: half the cross product of position and
   velocity."
  [x y vx vy]
  (e/* 1/2 (e/- (e/* x vy) (e/* y vx))))

(defn flow
  "The state derivative of [x y vx vy A] (A the area swept)."
  [mu [x y vx vy _A]]
  (let [[ax ay] (accel mu x y)]
    [vx vy ax ay (sweep-rate x y vx vy)]))

(defn perihelion-state
  "[x y vx vy A] at perihelion (-q, 0), q = a (1 - e), moving
   counter-clockwise at the vis-viva speed sqrt(mu l) / q, l = a (1 - e^2).
   With `l` given as its own argument (the 4-arity) the kernel keeps the
   square root of the positive mu l: written through a and e alone, Emmy's
   simplifier cancels (1 - e^2)/(1 - e) and splits the root into
   sqrt(-..)/sqrt(-..), NaN in a kernel."
  ([mu a ecc] (perihelion-state mu (e/* a (e/- 1 ecc)) (semi-latus a ecc) :q-l))
  ([mu q l _]
   [(e/- 0 q) 0 0 (e/- 0 (e// (e/sqrt (e/* mu l)) q)) 0]))

(defn rk4-step
  "One RK4 step of `flow` over dt: the browser's raster kernel."
  [mu dt s]
  (let [ad (fn [s k h] (mapv (fn [a b] (e/+ a (e/* h b))) s k))
        k1 (flow mu s)
        k2 (flow mu (ad s k1 (e/* dt 1/2)))
        k3 (flow mu (ad s k2 (e/* dt 1/2)))
        k4 (flow mu (ad s k3 dt))]
    (mapv (fn [s a b c d] (e/+ s (e/* (e// dt 6) (e/+ a (e/* 2 b) (e/* 2 c) d)))) s k1 k2 k3 k4)))

;; ---------------------------------------------------------------------------
;; The derivation, with symbols

(def derivation
  (delay
    (let [[x y] (planet 'a 'e 'E)
          rho (distance 'a 'e 'E)
          invariant (e/- rho (e/* 'e x))
          lin (pj/solve-linear-in (fn [rh] (e/- (e/- rh (e/* 'e rh (e/cos 'theta))) 'ell)))
          a-back (pj/solve-linear-in (fn [a] (e/- (semi-latus a 'e) 'ell)))]
      {:planet [x y]
       :rho rho
       :invariant (e/simplify invariant)
       :d-invariant (e/simplify ((e/D (fn [E] (let [[x _] (planet 'a 'e E)] (e/- (distance 'a 'e E) (e/* 'e x))))) 'E))
       :ell (e/simplify (e/- (distance 'a 'e 0) (e/* 'e (first (planet 'a 'e 0)))))
       :linear (e/- (e/- 'rho (e/* 'e 'rho (e/cos 'theta))) 'ell)
       :focal (e/simplify (:root lin))
       :second (e/simplify (:second lin))
       :newton (period-newton 'a 'mu)
       :T (period 'a)
       :a-of-T (axis-of-period 'T)
       :a-of-ell (e/simplify (:root a-back))})))

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn- proved [label diff]
  {:label label :grade (variant (grade/grade :symbolic [diff]))})

(defn grades
  "[{:label :grade}]: the derivation and the tie, each a difference Emmy
   simplifies to exact 0."
  []
  (let [d @derivation
        [x y] (:planet d)
        rho (:rho d)
        s ['x 'y 'vx 'vy 'A]
        rate (fn [[x y vx vy _]] (sweep-rate x y vx vy))
        [x0 _ _ vy0 _] (perihelion-state 'mu 'a 'e)]
    [(proved "the Sun-planet distance is a (1 − e cos E)"
             (e/- (e/+ (e/square x) (e/square y)) (e/square rho)))
     (proved "ρ − e x does not change along the orbit: d/dE = 0" (:d-invariant d))
     (proved "ρ − e x = ℓ = a (1 − e²): the focus-directrix property" (e/- (:invariant d) (semi-latus 'a 'e)))
     (proved "ρ − eρ cos θ = ℓ is linear in ρ: second derivative 0" (:second d))
     (proved "solve-linear: ρ = ℓ / (1 − e cos θ)" (e/- (:focal d) (focal-rho 'ell 'e 'theta)))
     (proved "on the orbit, ℓ / (1 − e x/ρ) = a (1 − e cos E)"
             (e/- (e// (semi-latus 'a 'e) (e/- 1 (e/* 'e (e// x rho)))) rho))
     (proved "Newton's period with μ = 4π²: T² = a³" (e/- (e/square (period-newton 'a mu)) (e/cube 'a)))
     (proved "forward T = a√a: T² = a³" (e/- (e/square (period 'a)) (e/cube 'a)))
     (proved "inverse then forward: T(a(T)) = T" (e/- (period (axis-of-period 'T)) 'T))
     (proved "forward then inverse: a(T(a)) = a" (e/- (axis-of-period (period 'a)) 'a))
     (proved "solve-linear: a = ℓ / (1 − e²)" (e/- (e/* (:a-of-ell d) (e/- 1 (e/square 'e))) 'ell))
     (proved "forward then inverse: a(ℓ(a)) = a" (e/- (axis-of-latus (semi-latus 'a 'e) 'e) 'a))
     (proved "inverse then forward: ℓ(a(ℓ)) = ℓ" (e/- (semi-latus (axis-of-latus 'ell 'e) 'e) 'ell))
     ;; the rate's derivative along the flow: d/dt at t = 0 of rate(s + t flow(s)),
     ;; by Emmy's D (exact: the rate is quadratic, the directional derivative linear)
     (proved "a force toward the Sun keeps the sweep rate: d/dt (x v_y − y v_x)/2 = 0 (Principia I.1)"
             ((e/D (fn [t] (rate (mapv (fn [si fi] (e/+ si (e/* t fi))) s (flow 'mu s))))) 0))
     (proved "vis viva: the perihelion speed gives energy −μ/(2a), the orbit of axis a"
             (e/+ (e/- (e/* 1/2 (e/square vy0)) (e// 'mu (e/- 0 x0))) (e// 'mu (e/* 2 'a))))]))

;; ---------------------------------------------------------------------------
;; Equal areas, by raster's RK4

(defn equal-areas
  "Integrate the flow from perihelion over one period T = a sqrt a in n
   equal times (steps-per substeps each), by raster's RK4:
   {:areas [area per time] :expected pi a b / n :closure |end - start|}."
  [a ecc n steps-per]
  (let [mu (* 4 Math/PI Math/PI)
        T (raster/value period (double a))
        [q l] (raster/value (fn [a e] [(e/* a (e/- 1 e)) (semi-latus a e)]) (double a) (double ecc))
        s0 (raster/value (fn [q l] (perihelion-state mu q l :q-l)) q l)
        {:keys [us]} (raster/ode (fn [_t x y vx vy A] (flow mu [x y vx vy A])) s0 T (/ T (* n steps-per)))
        at (mapv #(nth us (* % steps-per)) (range (inc n)))
        As (map #(nth % 4) at)
        end (last us)]
    {:areas (mapv - (rest As) As)
     :expected (raster/value (fn [a e] (e// (e/* Math/PI a (minor a e)) n)) (double a) (double ecc))
     :closure (Math/hypot (- (end 0) (s0 0)) (- (end 1) (s0 1)))
     :T T}))

(defn numeric-grades
  "Raster's RK4 on Mars and on an orbit of e = 0.6: equal areas in equal
   times, and the planet back at perihelion after T = a sqrt a."
  []
  (vec
   (for [[nm a ecc] [["Mars" 1.5235 0.09264] ["an orbit of e = 0.6" 2.0 0.6]]
         :let [{:keys [areas expected closure]} (equal-areas a ecc 12 200)]
         g [{:label (str nm ": 12 equal times sweep 12 equal areas π a b / 12 (RK4)")
             :grade (variant (grade/grade :numeric (map #(- % expected) areas) 1e-6))
             :engine :raster}
            {:label (str nm ": after T = a√a the planet is back at perihelion (RK4)")
             :grade (variant (grade/grade :numeric [closure] 1e-4))
             :engine :raster}]]
     g)))

;; ---------------------------------------------------------------------------
;; The derivation on the page

(defn- eq [lhs rhs] (e/literal-number (list '= (e/freeze lhs) (e/freeze rhs))))

(defn equations
  "[{:label :eqs}] for emmy.clerk/->TeX."
  []
  (let [d @derivation
        [x y] (:planet d)]
    [{:label "the planet at eccentric anomaly E, the Sun at the origin"
      :eqs [(eq 'x x) (eq 'y y) (eq 'rho (:rho d))]}
     {:label "ρ − e x, simplified by Emmy: the same at every E, so the distance to the Sun is e times the distance to a line (the directrix)"
      :eqs [(eq (list '- 'rho (list '* 'e 'x)) (:invariant d)) (eq 'ell (:ell d))]}
     {:label "with x = ρ cos θ the focal equation is linear in ρ; Emmy's solve-linear"
      :eqs [(eq (:linear d) 0) (eq 'rho (:focal d))]}
     {:label "the third law from Newton's period (μ = 4π² in AU and years)"
      :eqs [(eq 'T (:newton d)) (eq 'T (:T d)) (eq 'a (:a-of-T d))]}
     {:label "solved back for a, linear in a"
      :eqs [(eq 'a (:a-of-ell d))]}]))
