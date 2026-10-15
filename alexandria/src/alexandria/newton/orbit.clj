(ns alexandria.newton.orbit
  "The orbit under the inverse square, as a tie of live quantities
   (widget brief, section 5), for Principia Book I, Props. I, XI-XIII.

   The body starts at distance r0 from the centre S, moving at right
   angles to SP with speed v0 (an apsis); the force toward S is mu / r^2
   (mu = 1 in the widget). Emmy derives, from the conic about the focus
   r = p / (1 + e cos theta) and equal areas r^2 theta' = h (Prop. I):

     areal constant    h = r0 v0 (the start is an apsis)
     the conic         the acceleration of a body on the conic is
                       h^2 / (p r^2) toward S (Emmy's D twice, as in
                       alexandria.newton.principia); equal to mu / r^2
                       exactly when p = h^2 / mu
     the start         r(0) = r0 gives 1 + e = p / r0, so
                       e = r0 v0^2 / mu - 1, p = r0^2 v0^2 / mu
     energy            E = v0^2/2 - mu/r0 and e^2 = 1 + 2 E h^2 / mu^2:
                       e < 1 bound, e = 1 parabola, e > 1 hyperbola
     solved back       v0 = sqrt(mu (1 + e) / r0) (from e, linear in v0^2,
                       Emmy's solve-linear), v0 = sqrt(mu p) / r0 (from p)
     the tie           v0 -> e -> v0 and v0 -> p -> v0 compose to the
                       identity, :grade/proved

   e is signed: e < 0 means the start is the far apsis (the conic's
   r = p / (1 + e cos theta) is the same curve turned half a turn); its
   name goes by |e|. The polygon of impulses of Prop. I is the same
   start integrated by Newton's construction (alexandria.newton.principia/
   impulse-move), one raster kernel step per moment."
  (:require [alexandria.apollonius.projection :as pj]
            [alexandria.grade :as grade]
            [alexandria.newton.principia :as principia]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

(defn ecc
  "e of the start (r0, v0) under mu / r^2."
  [mu r0 v0]
  (e/- (e// (e/* r0 (e/square v0)) mu) 1))

(defn semi-latus
  "p = h^2 / mu with h = r0 v0."
  [mu r0 v0]
  (e// (e/square (e/* r0 v0)) mu))

(defn v0-of-e
  "v0 from e (r0, mu held): v0^2 is the root of r0 w / mu - 1 - e = 0
   (Emmy's solve-linear), v0 its positive square root."
  [mu r0 ecc]
  (e/sqrt (:root (pj/solve-linear-in (fn [w] (e/- (e/- (e// (e/* r0 w) mu) 1) ecc))))))

(defn v0-of-p
  "v0 from p (r0, mu held): v0 = sqrt(mu p) / r0."
  [mu r0 p]
  (e// (e/sqrt (e/* mu p)) r0))

(defn energy [mu r0 v0] (e/- (e// (e/square v0) 2) (e// mu r0)))

(defn conic-r
  "r = p / (1 + e cos theta)."
  [p ecc theta]
  (e// p (e/+ 1 (e/* ecc (e/cos theta)))))

(def derivation
  (delay
    (let [p 'p h 'h
          r (fn [t] (conic-r p 'e t))
          pos (fn [t] [(e/* (r t) (e/cos t)) (e/* (r t) (e/sin t))])
          ddt (fn [f] (fn [t] (e/* (e// h (e/square (r t))) ((e/D f) t))))
          acc (fn [i] ((ddt (ddt (fn [t] (nth (pos t) i)))) 'theta))
          [x y] (pos 'theta)
          rr (r 'theta)
          ;; the size of the acceleration toward S, a = -(ax x + ay y)/r
          a-size (e/simplify (e// (e/- 0 (e/+ (e/* (acc 0) x) (e/* (acc 1) y))) rr))
          ;; the start: r(0) = r0, solved for e (linear)
          e-start (:root (pj/solve-linear-in (fn [ee] (e/- (e/* 'r_0 (e/+ 1 ee)) p))))
          p-start (:root (pj/solve-linear-in (fn [pp] (e/- (e/* pp 'mu) (e/square h)))))]
      {:conic (r 'theta)
       :a-size a-size
       :p-start (e/simplify p-start)
       :e-start (e/simplify e-start)
       :ecc (e/simplify (ecc 'mu 'r_0 'v_0))
       :p (e/simplify (semi-latus 'mu 'r_0 'v_0))
       :v0-of-e (e/simplify (v0-of-e 'mu 'r_0 'e))
       :v0-of-p (e/simplify (v0-of-p 'mu 'r_0 'p))
       :energy (energy 'mu 'r_0 'v_0)})))

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn- proved [label & diffs]
  {:label label :grade (variant (grade/grade :symbolic (vec diffs)))})

(defn grades
  "[{:label :grade}]: the derivation of the tie and the tie itself."
  []
  (let [d @derivation
        h (e/* 'r_0 'v_0)
        ee (ecc 'mu 'r_0 'v_0)
        pp (semi-latus 'mu 'r_0 'v_0)]
    [(proved "a body on r = p/(1 + e cos θ) sweeping equal areas (r²θ′ = h) is pulled toward S by h²/(p r²)"
             (e/- (:a-size d) (e// (e/square 'h) (e/* 'p (e/square (:conic d))))))
     (proved "that pull is μ/r² exactly when p = h²/μ (solve-linear)"
             (e/- (:p-start d) (e// (e/square 'h) 'mu)))
     (proved "starting at the apsis r(0) = r₀: e = p/r₀ − 1 (solve-linear)"
             (e/- (:e-start d) (e/- (e// 'p 'r_0) 1)))
     (proved "with h = r₀v₀: e = r₀v₀²/μ − 1 and p = r₀²v₀²/μ"
             (e/- (e/- (e// pp 'r_0) 1) ee) (e/- pp (e// (e/square h) 'mu)))
     (proved "the energy names the conic: e² = 1 + 2Eh²/μ²"
             (e/- (e/square ee) (e/+ 1 (e// (e/* 2 (energy 'mu 'r_0 'v_0) (e/square h)) (e/square 'mu)))))
     (proved "e = 1 exactly at the speed of escape v₀² = 2μ/r₀ (E = 0)"
             (e/- (ecc 'mu 'r_0 (e/sqrt (e// (e/* 2 'mu) 'r_0))) 1))
     (proved "solved back: v₀ = √(μ(1 + e)/r₀) and v₀ = √(μp)/r₀"
             (e/- (e/square (:v0-of-e d)) (e// (e/* 'mu (e/+ 1 'e)) 'r_0))
             (e/- (e/square (:v0-of-p d)) (e// (e/* 'mu 'p) (e/square 'r_0))))
     (proved "inverse then forward: e of v₀(e) is e" (e/- (ecc 'mu 'r_0 (v0-of-e 'mu 'r_0 'e)) 'e))
     (proved "inverse then forward: p of v₀(p) is p" (e/- (semi-latus 'mu 'r_0 (v0-of-p 'mu 'r_0 'p)) 'p))
     (proved "forward then inverse: v₀ from e(v₀) (v₀ > 0)" (e/- (v0-of-e 'mu 'r_0 ee) 'v_0))
     (proved "forward then inverse: v₀ from p(v₀) (v₀ > 0)" (e/- (v0-of-p 'mu 'r_0 pp) 'v_0))]))

(defn- eq [lhs rhs]
  (e/literal-number (list '= (e/freeze lhs) (e/freeze rhs))))

(defn equations
  "[{:label :eqs}] for emmy.clerk/->TeX: the derivation as Emmy left it."
  []
  (let [d @derivation]
    [{:label "the conic about the focus S (Apollonius, through Pappus' focus and directrix)"
      :eqs [(eq 'r (:conic d))]}
     {:label "equal areas, r²θ′ = h (Prop. I): Emmy differentiates the position twice in time; the pull toward S is"
      :eqs [(eq 'a (:a-size d))]}
     {:label "equal to μ/r² (Prop. XI) when"
      :eqs [(eq 'p (:p-start d))]}
     {:label "the start at the apsis, r(0) = r₀, h = r₀v₀"
      :eqs [(eq 'e (:ecc d)) (eq 'p (:p d))]}
     {:label "solved back for the speed"
      :eqs [(eq 'v_0 (:v0-of-e d)) (eq 'v_0 (:v0-of-p d))]}]))

(defn polygon-of-impulses
  "Newton's polygon for the start (r0, v0): n moments of length dt, one
   impulse mu dt / r^2 toward S at each vertex (principia/polygon, every
   vertex a raster kernel of impulse-move)."
  [mu r0 v0 dt n]
  (principia/polygon [r0 0.0] [0.0 v0] mu dt n))
