(ns alexandria.apollonius.symptom
  "The symptoma of Apollonius' sections (Conics I.11-13, Heath 1896), the
   one equation every section of the cone satisfies, in the letters of the
   notation passage (alexandria.apollonius.notation-steps): x = PV the
   abscissa, y = QV the ordinate, p = PL the parameter, d = PP' the
   transverse diameter.

     I.11  parabola   y^2 = p x                   applied exactly
     I.12  hyperbola  y^2 = p x + (p/d) x^2       exceeding
     I.13  ellipse    y^2 = p x - (p/d) x^2       falling short

   One form: y^2 = p x + c x^2 with c = -p/d, 0, +p/d (d taken positive;
   alexandria.apollonius.conics signs d, and there c = -p/d throughout).

   Here: `symptom-figure`, the Emmy function the widget compiles to a
   raster kernel (every number the widget shows: both sides of the
   symptoma, the inverse map y -> x on both branches, the sides of the
   application of areas); `identities`, each graded :grade/proved by Emmy
   from the cone and the plane (conics/section-point); `tex`, the forms as
   TeX from Emmy expressions; `heath`, the short quotations."
  (:require [alexandria.apollonius.conics :as conics]
            [alexandria.raster :as raster]
            [emmy.env :as e]))

;; ---------------------------------------------------------------------------
;; The kernel: one Emmy function, every number the widget shows

(def outputs
  "The outputs of `symptom-figure`, in order.
     theta    the tilt in radians
     p c d    parameter, excess (c = -p/d), signed transverse diameter
     x y      the abscissa PV (the state) and the ordinate QV
     y2       QV^2 = HV.VK from the cone: (k Z_V)^2 - X_V^2, the circle
              through V parallel to the base
     rhs      p x + c x^2, the symptoma's right side
     px cx2   its two terms
     gap      y2 - rhs, evaluated in the kernel (0 up to raster's libm)
     disc     p^2 + 4 c yin^2, the discriminant of c x^2 + p x - yin^2
     xnear    the root through P: 2 yin^2 / (p + sqrt disc)
     xfar     the ellipse's other root, past the centre: (p + sqrt disc) / (-2 c)
     xbase    the abscissa where PM meets the base (the drawn reach)
     b        the ellipse's greatest ordinate p / (2 sqrt|c|), at x = d/2
     vr       VR = p + c x, the side of the applied rectangle PV.VR
     lo hi    the strip c x^2 along PL runs from min(p, VR) to max(p, VR)
     w        its width |c x|
     half     d / 2, the centre (where the ellipse's halves meet)
     ny xy    -y and x + y: the square on QV in SVG coordinates (y down)
     mx mp my mq  label points: x/2, p/2, -y/2, (2x + y)/2"
  [:theta :p :c :d :x :y :y2 :rhs :px :cx2 :gap :disc :xnear :xfar :xbase :b :vr :lo :hi :w :half :ny :xy
   :mx :mp :my :mq])

(defn symptom-figure
  "The symptoma of the section of the cone of slope k by the plane through
   P at height h tilted by deg degrees, the base at height zb, at the
   abscissa x (the state); yin an ordinate to solve back for x. The vector
   of `outputs`."
  [k h zb deg yin]
  (fn [[x]]
    (let [theta (e/* deg (/ Math/PI 180))
          p (conics/parameter k theta h)
          c (conics/excess k theta)
          d (conics/transverse k theta h)
          [vx _ vz] ((conics/section-point k theta h) [x 0])
          y2 (e/- (e/square (e/* k vz)) (e/square vx))
          y (e/sqrt (e/abs y2))
          px (e/* p x)
          cx2 (e/* c x x)
          rhs (e/+ px cx2)
          disc (e/+ (e/square p) (e/* 4 c (e/square yin)))
          r (e/sqrt (e/abs disc))
          vr (e/+ p (e/* c x))
          spread (e/abs (e/- p vr))]
      [theta p c d x y y2 rhs px cx2 (e/- y2 rhs) disc
       (e// (e/* 2 (e/square yin)) (e/+ p r))
       (e// (e/+ p r) (e/* -2 c))
       (e// (e/- zb h) (e/sin theta))
       (e// p (e/* 2 (e/sqrt (e/abs c))))
       vr
       (e// (e/- (e/+ p vr) spread) 2)
       (e// (e/+ (e/+ p vr) spread) 2)
       (e/abs (e/* c x))
       (e// d 2)
       (e/negate y)
       (e/+ x y)
       (e// x 2) (e// p 2) (e// y -2) (e// (e/+ (e/* 2 x) y) 2)])))

(defn curve-figure
  "The section in its own plane, Heath's frame drawn in SVG coordinates (PM
   to the right, ordinates up): at the fraction t of the abscissa xmax,
   [x -y y] with y = QV from the cone (the circle through V)."
  [k h deg xmax]
  (fn [[t]]
    (let [theta (e/* deg (/ Math/PI 180))
          x (e/* t xmax)
          [vx _ vz] ((conics/section-point k theta h) [x 0])
          y (e/sqrt (e/abs (e/- (e/square (e/* k vz)) (e/square vx))))]
      [x (e/negate y) y])))

(def figures
  "The widget's own kernels, for alexandria.medium.kernel."
  {:symptom {:f symptom-figure :params [0.6 0.7 2.2 35 0.5] :state [0.85] :opts {:simplify? false}}
   :curve {:f curve-figure :params [0.6 0.7 35 1.7] :state [0.5] :opts {:simplify? false}}})

(defn numbers
  "{output value} of `symptom-figure` at {:k :h :zb :deg :x :yin}, from a
   raster kernel (alexandria.raster/value)."
  [{:keys [k h zb deg x yin] :or {yin 0}}]
  (zipmap outputs (raster/value (fn [k h zb deg yin x] ((symptom-figure k h zb deg yin) [x]))
                                k h zb deg yin x)))

;; ---------------------------------------------------------------------------
;; The identities, proved by Emmy from the cone and the plane

(defn identities
  "[label diff] pairs, each diff simplifying to 0. The first three are the
   symptoma itself, from the cone and the plane in space."
  []
  (let [k 'k theta 'theta h 'h x 'x y 'y P 'p C 'c
        [vx _ vz] ((conics/section-point k theta h) [x 0])
        hv-vk (e/- (e/square (e/* k vz)) (e/square vx))
        p (conics/parameter k theta h)
        c (conics/excess k theta)
        d (conics/transverse k theta h)
        root (e/sqrt (e/+ (e/square P) (e/* 4 C (e/square y))))
        near (e// (e/* 2 (e/square y)) (e/+ P root))
        far (e// (e/+ P root) (e/* -2 C))
        sym (fn [u] (e/+ (e/* P u) (e/* C (e/square u))))]
    [["QV² = HV·VK on the cone equals p x + c x² (I.11-13)" (e/- hv-vk (e/+ (e/* p x) (e/* c x x)))]
     ["c = −p/d" (e/+ c (e// p d))]
     ["QV² = p x − (p/d) x², with the transverse side d = PP′" (e/- hv-vk (e/- (e/* p x) (e/* (e// p d) x x)))]
     ["PM parallel to AC (I.11): c = 0, y² = p x" (second (first (conics/parabola-case)))]
     ["x = 2y² / (p + √(p² + 4cy²)) solves y² = p x + c x²" (e/- (sym near) (e/square y))]
     ["the ellipse's far root (p + √(p² + 4cy²)) / (−2c) solves it too" (e/- (sym far) (e/square y))]
     ["the two roots sum to −p/c = d" (e/+ near far (e// P C))]]))

(defn graded
  "[{:label :grade}] of `identities`."
  []
  (mapv (fn [[label diff]] {:label label :grade (conics/grade-diff diff)}) (identities)))

;; ---------------------------------------------------------------------------
;; TeX, from Emmy expressions

(defn- eq [lhs rhs] (str (e/->TeX lhs) " = " (e/->TeX rhs)))

(defn tex
  "The forms of the symptoma as TeX strings (emmy.env/->TeX, the renderer
   emmy.clerk/->TeX uses): :parabola :hyperbola :ellipse :general, and :c
   the three values of c."
  []
  (let [x 'x y 'y p 'p d 'd c 'c
        y2 (e/square y)]
    {:parabola (eq y2 (e/* p x))
     :hyperbola (eq y2 (e/+ (e/* p x) (e/* (e// p d) (e/square x))))
     :ellipse (eq y2 (e/- (e/* p x) (e/* (e// p d) (e/square x))))
     :general (eq y2 (e/+ (e/* p x) (e/* c (e/square x))))
     :c (str (e/->TeX c) " = " (e/->TeX (e/negate (e// p d))) ",\\; 0,\\; " (e/->TeX (e// p d)))}))

(def heath
  "Heath 1896, the conclusions of I.11, I.12 and I.13 (pp. 9, 11, 13),
   shortened."
  {:parabola "the square on any ordinate to the fixed diameter PM is equal to a rectangle applied to the fixed straight line PL drawn at right angles to PM with altitude equal to the corresponding abscissa PV. Hence the section is called a Parabola."
   :hyperbola "[its] base lies along the fixed straight line PL but overlaps it by a length equal to the difference between VR and PL. Hence the section is called a Hyperbola."
   :ellipse "[its] base lies along the fixed straight line PL but falls short of it by a length equal to the difference between VR and PL. The section is therefore called an Ellipse."})
