(ns alexandria.gauss.egregium
  "Gauss, Disquisitiones generales circa superficies curvas (1828), art. 11
   and 12: the measure of curvature from the first fundamental form alone,
   and the Theorema egregium.

   A surface is a patch r(u, v) in space. Gauss writes its line element as
   ds^2 = E du^2 + 2F du dv + G dv^2 with E = r_u.r_u, F = r_u.r_v,
   G = r_v.r_v (art. 11, his a a + b b + c c = E ...). His formula of art. 11
   gives the curvature from E, F, G and their first and second derivatives;
   we use it in Brioschi's arrangement (1852):

                 | -E_vv/2 + F_uv - G_uu/2   E_u/2   F_u - E_v/2 |   | 0      E_v/2  G_u/2 |
     K (EG-F^2)^2 = | F_v - G_u/2              E       F           | - | E_v/2  E      F     |
                 | G_v/2                    F       G           |   | G_u/2  F      G     |

   The same K from the embedding, (LN - M^2)/(EG - F^2) with the second
   fundamental form L = r_uu.n etc., is the check: the two agree, and the
   first never looks at the space the surface sits in.

   Two families of surfaces, each with a bend t and a stretch s:

     :catenoid  r = (cos t sinh v sin u + sin t cosh v cos u,
                     -cos t sinh v cos u + sin t cosh v sin u,
                     s (u cos t + v sin t))
                t = 0 the helicoid, t = pi/2 the catenoid; at s = 1 every
                member has E = G = cosh^2 v, F = 0, so K = -1/cosh^4 v for
                every t (an isometric bending); s != 1 stretches the axis
                and changes E, F, G and K.
     :sheet     a flat sheet u in [-1 1], v in [-1 1] rolled about the v axis
                with curvature k = t pi (t in [0 1]: flat to a half
                cylinder): r = (sin(k u)/k, v, (1 - cos(k u))/k), plus a
                bulge (s - 1)(1 - u^2)(1 - v^2)/2 along the normal of the
                flat sheet, which is not a bending. At s = 1, E = G = 1,
                F = 0, K = 0 for every t.

   Every function takes numbers or Emmy symbols: Emmy derives E, F, G and K
   (graded :grade/proved), alexandria.raster compiles them for the numbers."
  (:require [alexandria.grade :as grade]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; Branch-free helpers (raster)

(defn at-least [x lo] (e/* 0.5 (e/+ x lo (e/abs (e/- x lo)))))

(def k-min "The least curvature of the rolled sheet (k = 0 is the flat limit)." 1e-3)

;; ---------------------------------------------------------------------------
;; The surfaces

(defn catenoid
  "The helicoid-catenoid family, bend t (radians), stretch s: (u, v) -> xyz."
  [t s]
  (fn [u v]
    (let [ct (e/cos t) st (e/sin t)]
      [(e/+ (e/* ct (e/sinh v) (e/sin u)) (e/* st (e/cosh v) (e/cos u)))
       (e/+ (e/* -1 ct (e/sinh v) (e/cos u)) (e/* st (e/cosh v) (e/sin u)))
       (e/* s (e/+ (e/* u ct) (e/* v st)))])))

(defn sheet
  "The rolled sheet: bend t in [0 1] rolls the strip u in [-1 1] onto an arc
   of curvature k = (t + k-min) pi (flat at t = 0, a half cylinder at
   t = 1); stretch s lifts it by (s - 1) v^2 / 2 along v, which is no
   bending: (u, v) -> xyz. E = 1, F = (s - 1) v sin(k u),
   G = 1 + (s - 1)^2 v^2."
  [t s]
  (fn [u v]
    (let [k (e/* (e/+ t k-min) Math/PI)]
      [(e/divide (e/sin (e/* k u)) k)
       v
       (e/+ (e/divide (e/- 1 (e/cos (e/* k u))) k) (e/* 0.5 (e/- s 1) (e/square v)))])))

(def surfaces {:catenoid catenoid :sheet sheet})

;; ---------------------------------------------------------------------------
;; The first fundamental form, and K from it alone

(defn- dot [a b] (reduce e/+ (map e/* a b)))

(defn- du [f] (fn [u v] ((e/D (fn [x] (f x v))) u)))
(defn- dv [f] (fn [u v] ((e/D (fn [y] (f u y))) v)))

(defn- comp* [f i] (fn [u v] (nth (f u v) i)))
(defn- vec-d [d f] (fn [u v] (mapv (fn [i] ((d (comp* f i)) u v)) (range 3))))

(defn first-form
  "(u, v) -> [E F G] of the patch r."
  [r]
  (let [ru (vec-d du r) rv (vec-d dv r)]
    (fn [u v] (let [a (ru u v) b (rv u v)] [(dot a a) (dot a b) (dot b b)]))))

(defn- det3 [[[a b c] [d f g] [h i j]]]
  (e/+ (e/* a (e/- (e/* f j) (e/* g i)))
       (e/- 0 (e/* b (e/- (e/* d j) (e/* g h))))
       (e/* c (e/- (e/* d i) (e/* f h)))))

(defn brioschi
  "K at (u, v) from a metric (u, v) -> [E F G] alone: Brioschi's
   arrangement of Gauss' formula of art. 11. Nothing but E, F, G and their
   derivatives in u and v enters."
  [metric]
  (let [E (comp* metric 0) F (comp* metric 1) G (comp* metric 2)]
    (fn [u v]
      (let [e (E u v) f (F u v) g (G u v)
            Eu ((du E) u v) Ev ((dv E) u v) Fu ((du F) u v) Fv ((dv F) u v)
            Gu ((du G) u v) Gv ((dv G) u v)
            Evv ((dv (dv E)) u v) Guu ((du (du G)) u v) Fuv ((du (dv F)) u v)
            half #(e/divide % 2)
            m1 [[(e/+ (e/- 0 (half Evv)) Fuv (e/- 0 (half Guu))) (half Eu) (e/- Fu (half Ev))]
                [(e/- Fv (half Gu)) e f]
                [(half Gv) f g]]
            m2 [[0 (half Ev) (half Gu)] [(half Ev) e f] [(half Gu) f g]]]
        (e/divide (e/- (det3 m1) (det3 m2)) (e/square (e/- (e/* e g) (e/square f))))))))

(defn- cross [[a b c] [d f g]]
  [(e/- (e/* b g) (e/* c f)) (e/- (e/* c d) (e/* a g)) (e/- (e/* a f) (e/* b d))])

(defn shape-K
  "K at (u, v) from the embedding: (LN - M^2)/(EG - F^2), L = r_uu.N etc.
   with N = r_u x r_v unnormalised, so K = (LN - M^2)/(EG - F^2)^2 there."
  [r]
  (let [ru (vec-d du r) rv (vec-d dv r)
        ruu (vec-d du ru) ruv (vec-d dv ru) rvv (vec-d dv rv)]
    (fn [u v]
      (let [a (ru u v) b (rv u v) n (cross a b)
            E (dot a a) F (dot a b) G (dot b b)
            L (dot (ruu u v) n) M (dot (ruv u v) n) N (dot (rvv u v) n)]
        (e/divide (e/- (e/* L N) (e/square M)) (e/square (e/- (e/* E G) (e/square F))))))))

(defn- expr->fn
  "An Emmy function of (t s u v) from the simplified expression expr in
   those symbols: the frozen form evaluated with emmy.env's generic ops, so
   numbers give numbers and symbols give expressions (raster and
   emmy-viewers trace it)."
  [expr]
  (let [form (e/freeze expr)]
    (binding [*ns* (the-ns 'emmy.env)]
      (eval (list 'fn '[t s u v] form)))))

(def symbolic
  "{kind {:xyz [..] :E :F :G :K :K-shape}}: Emmy's expressions in the
   symbols t s u v, each simplified once (memoized)."
  (memoize
   (fn [kind]
     (let [r ((surfaces kind) 't 's)
           [E F G] ((first-form r) 'u 'v)]
       {:xyz (mapv e/simplify (r 'u 'v))
        :E (e/simplify E) :F (e/simplify F) :G (e/simplify G)
        :K (e/simplify ((brioschi (first-form r)) 'u 'v))
        :K-shape (e/simplify ((shape-K r) 'u 'v))}))))

(def compact
  "{kind {key (fn [t s u v])}}: each simplified expression as a function."
  (memoize (fn [kind] (into {} (for [[k v] (symbolic kind)]
                                 [k (if (vector? v) (mapv expr->fn v) (expr->fn v))])))))

(defn probe
  "The numbers at a point, as one vector-valued Emmy function of
   (t s u v) for surface kind: [x y z E F G K-brioschi K-shape]."
  [kind]
  (let [{:keys [xyz E F G K K-shape]} (compact kind)]
    (fn [t s u v]
      (conj (mapv #(% t s u v) xyz) (E t s u v) (F t s u v) (G t s u v) (K t s u v) (K-shape t s u v)))))

;; ---------------------------------------------------------------------------
;; Emmy's derivations, for the page and the grades

(defn catenoid-metric
  "Emmy's E, F, G of the catenoid family at symbolic t, s, simplified."
  []
  (mapv e/simplify ((first-form (catenoid 't 's)) 'u 'v)))

(defn catenoid-K
  "Emmy's K of the family at s = 1 from the metric alone, simplified."
  []
  (e/simplify ((brioschi (first-form (catenoid 't 1))) 'u 'v)))

(defn- variant [kind diffs tol]
  (let [res (grade/grade kind diffs tol)]
    (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails)))

(defn sample-points
  "A grid of (u, v) inside each surface's drawn domain."
  [kind]
  (for [u (if (= kind :sheet) [-0.7 0.1 0.6] [-2.0 0.5 2.5])
        v (if (= kind :sheet) [-0.6 0.0 0.4] [-1.2 0.3 1.0])]
    [u v]))

(defn- cosh-exp [v] (e/divide (e/+ (e/exp v) (e/exp (e/- 0 v))) 2))
(defn- sinh-exp [v] (e/divide (e/- (e/exp v) (e/exp (e/- 0 v))) 2))

(defn catenoid-exp
  "The catenoid family with cosh and sinh written by exp, so that Emmy's
   simplifier sees cosh^2 - sinh^2 = 1 (rational in e^v)."
  [t s]
  (fn [u v]
    (let [ct (e/cos t) st (e/sin t)]
      [(e/+ (e/* ct (sinh-exp v) (e/sin u)) (e/* st (cosh-exp v) (e/cos u)))
       (e/+ (e/* -1 ct (sinh-exp v) (e/cos u)) (e/* st (cosh-exp v) (e/sin u)))
       (e/* s (e/+ (e/* u ct) (e/* v st)))])))

(defn graded
  "[{:label :grade}]: the bending keeps E, F, G (proved by Emmy for the
   catenoid family, cosh and sinh by exp), K from the metric = -1/cosh^4 v
   for every t (proved), Brioschi = the shape-operator K (raster, both
   families, a stretch included), and the sheet's K = 0 for every roll
   (raster)."
  []
  (let [r (catenoid-exp 't 1)
        [E F G] ((first-form r) 'u 'v)
        c2 (e/square (cosh-exp 'v))
        pc (probe :catenoid) ps (probe :sheet)
        diffs-cat (for [[u v] (sample-points :catenoid) t [0.0 0.7 1.5] s [1.0 1.6]
                        :let [[_ _ _ _ _ _ kb ks] (raster/value pc t s u v)]]
                    (- kb ks))
        diffs-sheet (for [[u v] (sample-points :sheet) t [0.0 0.5 1.0] s [1.0 1.8]
                          :let [[_ _ _ _ _ _ kb ks] (raster/value ps t s u v)]]
                      (- kb ks))
        sheet-zero (for [[u v] (sample-points :sheet) t [0.0 0.5 1.0]]
                     (nth (raster/value ps t 1.0 u v) 6))]
    [{:label "bending the helicoid into the catenoid keeps E = G = cosh² v, F = 0"
      :grade (variant :symbolic [(e/- E c2) F (e/- G c2)] 0)}
     {:label "K from E, F, G alone is −1/cosh⁴ v, the same for every bend t"
      :grade (variant :symbolic [(e/+ ((brioschi (first-form r)) 'u 'v) (e/divide 1 (e/square c2)))] 0)}
     {:label "Brioschi's K (metric only) = (LN − M²)/(EG − F²)² (embedding), catenoid family, stretched or not"
      :grade (variant :numeric diffs-cat 1e-9)}
     {:label "the same agreement on the rolled sheet, lifted or not"
      :grade (variant :numeric diffs-sheet 1e-9)}
     {:label "the rolled sheet keeps K = 0 at every roll"
      :grade (variant :numeric sheet-zero 1e-9)}]))
