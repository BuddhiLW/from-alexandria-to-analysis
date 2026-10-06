(ns alexandria.apollonius.projection
  "Polar coordinates as a projection: the section of a right cone by a
   plane, dropped straight down onto the plane through the apex at right
   angles to the axis, is rho = l / (1 - e cos theta) about the foot of the
   axis. Emmy derives it; nothing below is typed in as a result.

   The figure (Heath 1896, The Cone): the apex at the origin, the axis z
   up, the cone of half-angle alpha, z = rho / tan(alpha) in cylindrical
   coordinates (rho, theta, z); negative rho is the opposite cone, as in
   I.14. The cutting plane z = h + x tan(tau) meets the axis at height h
   and is tilted by tau (the playground's tilt) about a line at right
   angles to the plane of x and z; x = rho cos(theta). Write c for
   cos(theta).

     eliminate z     F(rho) = rho / tan(alpha) - (h + rho c tan(tau)) = 0
     linear in rho   F'' = 0 (Emmy: D twice), so F = a rho + b with
                     a = F'(0), b = F(0), and rho = solve-linear(a, -b)
                     (Emmy's emmy.generic/solve-linear, a x = y)
     read off        a is linear in c: a = a0 + a1 c. Dividing by a0,
                     rho = l / (1 - e c) with l = -b / a0, e = -a1 / a0;
                     Emmy simplifies them to h tan(alpha), tan(tau) tan(alpha)
     solve back      l = h tan(alpha) is linear in h, e = T tan(alpha) in
                     T = tan(tau): h and T by solve-linear, tau = atan(T)
     the tie         forward (h, tau) -> (l, e) and inverse (l, e) -> (h, tau)
                     compose to the identity both ways, :grade/proved

   `forward` and `inverse` are Emmy functions of numbers or symbols:
   alexandria.raster compiles them to raster kernels (the playground's
   numbers), and `grades` checks them symbolically."
  (:require [alexandria.grade :as grade]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; The two surfaces, in cylindrical coordinates

(defn cone-z
  "The height of the cone of half-angle alpha over the radius rho."
  [alpha rho]
  (e// rho (e/tan alpha)))

(defn plane-z
  "The height of the cutting plane over the point at radius rho in the
   direction whose cosine is c: z = h + x tan(tau), x = rho c."
  [tau h rho c]
  (e/+ h (e/* rho c (e/tan tau))))

(defn elimination
  "F(rho) = cone - plane, the equation for the section's radius in the
   direction c = cos(theta), z eliminated."
  [alpha tau h c]
  (fn [rho] (e/- (cone-z alpha rho) (plane-z tau h rho c))))

;; ---------------------------------------------------------------------------
;; Solving a linear equation with Emmy

(defn solve-linear-in
  "The root of f(u) = 0 for f linear in u: f = a u + b with a = f'(0) and
   b = f(0) (Emmy's D), the root by emmy.generic/solve-linear (a x = -b),
   and :second = f''(u), which Emmy must simplify to 0 for f to be linear."
  [f]
  (let [a ((e/D f) 0)
        b (f 0)]
    {:a a :b b
     :root (e/solve-linear a (e/- b))
     :second ((e/D (e/D f)) 'u)}))

;; ---------------------------------------------------------------------------
;; The derivation, with symbols

(defn- rho-of
  "The solved radius in the direction c (symbolic alpha, tau, h)."
  [c]
  (:root (solve-linear-in (elimination 'alpha 'tau 'h c))))

(defn- coefficient-of-rho
  "a(c) = F'(0): the coefficient of rho, as a function of c."
  [c]
  (:a (solve-linear-in (elimination 'alpha 'tau 'h c))))

(def derivation
  "Every step, as Emmy expressions (simplified where Emmy derived them)."
  (delay
    (let [{:keys [a b root second]} (solve-linear-in (elimination 'alpha 'tau 'h 'c))
          a0 (coefficient-of-rho 0)
          a1 ((e/D coefficient-of-rho) 0)
          ell (e/simplify (e// (e/- b) a0))
          ecc (e/simplify (e// (e/- a1) a0))
          h-back (solve-linear-in (fn [h] (e/- (e/* h (e/tan 'alpha)) 'ell)))
          t-back (solve-linear-in (fn [t] (e/- (e/* t (e/tan 'alpha)) 'e)))]
      {:cone (cone-z 'alpha 'rho)
       :plane (plane-z 'tau 'h 'rho (e/cos 'theta))
       :elimination ((elimination 'alpha 'tau 'h (e/cos 'theta)) 'rho)
       :a (e/simplify a) :b (e/simplify b) :second (e/simplify second)
       :rho (e/simplify (rho-of (e/cos 'theta)))
       :rho-c (e/simplify root)
       :a0 (e/simplify a0) :a1 (e/simplify a1)
       :ell ell :e ecc
       :focal (e// 'ell (e/- 1 (e/* 'e (e/cos 'theta))))
       :h (e/simplify (:root h-back)) :h-second (e/simplify (:second h-back))
       :tan-tau (e/simplify (:root t-back)) :t-second (e/simplify (:second t-back))
       :tau (e/atan (e/simplify (:root t-back)))})))

;; ---------------------------------------------------------------------------
;; The forward and inverse maps (numbers or symbols; raster compiles them)

(defn forward
  "[l e] of the plane (tau, h) on the cone alpha: the expressions Emmy read
   off, l = h tan(alpha), e = tan(tau) tan(alpha)."
  [alpha tau h]
  [(e/* h (e/tan alpha)) (e/* (e/tan tau) (e/tan alpha))])

(defn inverse
  "[h tau] of the conic (l, e) on the cone alpha: h = l / tan(alpha),
   tau = atan(e / tan(alpha))."
  [alpha ell ecc]
  [(e// ell (e/tan alpha)) (e/atan (e// ecc (e/tan alpha)))])

;; ---------------------------------------------------------------------------
;; Grades

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn- proved [label diff]
  {:label label :grade (variant (grade/grade :symbolic [diff]))})

(defn grades
  "[{:label :grade}]: each step of the derivation and the tie, graded by
   alexandria.grade (Emmy simplify to exact 0)."
  []
  (let [d @derivation
        F (elimination 'alpha 'tau 'h 'c)
        [l1 e1] (forward 'alpha 'tau 'h)
        [h2 t2] (inverse 'alpha 'ell 'e)
        [h3 t3] (inverse 'alpha l1 e1)
        [l4 e4] (forward 'alpha t2 h2)]
    [(proved "F(ρ) = cone − plane is linear in ρ: F″ = 0" (:second d))
     (proved "ρ from solve-linear satisfies F(ρ) = 0" (F (:rho-c d)))
     (proved "ρ = ℓ / (1 − e cos θ) with the ℓ and e read off"
             (e/- (:rho d) (e// (:ell d) (e/- 1 (e/* (:e d) (e/cos 'theta))))))
     (proved "ℓ = h tan α" (e/- (:ell d) l1))
     (proved "e = tan τ tan α" (e/- (:e d) e1))
     (proved "solved back: h = ℓ / tan α" (e/- (:h d) h2))
     (proved "solved back: tan τ = e / tan α" (e/- (:tan-tau d) (e/tan t2)))
     (proved "forward then inverse: h" (e/- h3 'h))
     (proved "forward then inverse: τ" (e/- t3 'tau))
     (proved "inverse then forward: ℓ" (e/- l4 'ell))
     (proved "inverse then forward: e" (e/- e4 'e))]))

;; ---------------------------------------------------------------------------
;; TeX (strings; a Clerk page renders them with emmy.clerk/->TeX's viewer)

(defn- tex [x] (e/->TeX x))

(defn steps
  "[{:label :tex}]: the derivation on the page, every right-hand side an
   Emmy expression rendered by emmy.env/->TeX."
  []
  (let [d @derivation]
    [{:label "the cone" :tex (str "z = " (tex (:cone d)))}
     {:label "the plane" :tex (str "z = " (tex (:plane d)))}
     {:label "eliminate z" :tex (str "F(\\rho) = " (tex (:elimination d)) " = 0")}
     {:label "linear in ρ (c = cos θ)" :tex (str "F''(\\rho) = " (tex (:second d))
                                     ",\\quad a = F'(0) = " (tex (:a d))
                                     ",\\quad b = F(0) = " (tex (:b d)))}
     {:label "solve-linear" :tex (str "\\rho = " (tex (:rho d)))}
     {:label "a = a₀ + a₁ c" :tex (str "a_0 = " (tex (:a0 d)) ",\\qquad a_1 = " (tex (:a1 d)))}
     {:label "read off" :tex (str "\\rho = " (tex (:focal d))
                                  ",\\qquad \\ell = -\\frac{b}{a_0} = " (tex (:ell d))
                                  ",\\qquad e = -\\frac{a_1}{a_0} = " (tex (:e d)))}
     {:label "solve back" :tex (str "h = " (tex (:h d))
                                    ",\\qquad \\tau = " (tex (:tau d)))}]))

(defn- eq
  "The equation lhs = rhs as one Emmy literal expression (rendered whole by
   ->TeX)."
  [lhs rhs]
  (e/literal-number (list '= (e/freeze lhs) (e/freeze rhs))))

(defn equations
  "[{:label :eqs}]: the derivation as Emmy equations (each right-hand side
   as Emmy derived and simplified it), for emmy.clerk/->TeX on the page."
  []
  (let [d @derivation]
    [{:label "the cone (Heath: the apex at the origin, the axis z up)"
      :eqs [(eq 'z (:cone d))]}
     {:label "the cutting plane, tilted by τ, meeting the axis at height h"
      :eqs [(eq 'z (:plane d))]}
     {:label "eliminate z: the radius of the section in the direction θ is the root of F"
      :eqs [(eq (list 'F 'rho) (:elimination d))]}
     {:label "F is linear in ρ (c = cos θ): Emmy differentiates twice, then reads off F = aρ + b"
      :eqs [(eq (list (list 'expt 'D 2) (list 'F 'rho)) (:second d))
            (eq 'a (:a d)) (eq 'b (:b d))]}
     {:label "solve-linear: aρ = −b"
      :eqs [(eq 'rho (:rho d))]}
     {:label "a is linear in c: a = a₀ + a₁c"
      :eqs [(eq 'a_0 (:a0 d)) (eq 'a_1 (:a1 d))]}
     {:label "divide by a₀: the focal form, and ℓ, e read off"
      :eqs [(eq 'rho (:focal d)) (eq 'ell (:ell d)) (eq 'e (:e d))]}
     {:label "solve back (linear in h and in tan τ)"
      :eqs [(eq 'h (:h d)) (eq 'tau (:tau d))]}]))
