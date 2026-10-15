(ns alexandria.archimedes.widgets
  "The Archimedes notebook's tied-controls widgets (alexandria.medium.tie),
   each on the equations of alexandria.archimedes.ties, every curve and
   number a raster kernel:

     circle      Measurement of a Circle, Prop. 3: n, d, p, P, eps
     parabola    Quadrature of the Parabola, Props 21-24: a, T, k, S, R
     sphere      Sphere and Cylinder I.34 / The Method, Prop. 2: r, V, C, K
                 in MathBox, orbitable

   Policies (which quantity holds still) are named in `policies`."
  (:require [alexandria.archimedes.ties :as t]
            [alexandria.medium.tied :as tie]
            [emmy.env :as e]
            [emmy.mafs :as mafs]
            [emmy.mathbox.plot :as plot]
            [emmy.viewer :as ev]))

(def ^:private PI Math/PI)

(defn- katex [tex-form]
  [:div {:data-formula tex-form} [:f> 'nextjournal.clerk.render/render-katex tex-form {:inline? false}]])

(defn polygon-curve
  "The regular n-gon about the origin with apothem a, as a polar curve:
   rho = a / cos(s), s the angle from the nearest apothem, folded into
   [0, pi/n] by acos(c cos(n t)) / n. The factor c = 1 - 1e-10 keeps Emmy's
   simplifier from cancelling acos(cos) (as it cancels asin(sin)), and
   keeps acos inside its domain."
  [n a]
  (fn [t]
    (let [s (e// (e/acos (e/* 0.9999999999 (e/cos (e/* n t)))) n)
          rho (e// a (e/cos s))]
      [(e/* rho (e/cos t)) (e/* rho (e/sin t))])))

;; ---------------------------------------------------------------------------
;; Measurement of a Circle, Prop. 3

(def circle-keys [:n :d :p :P :eps])

(def circle-ranges
  {:n [3 400 1] :d [0.5 2 0.001] :p [1 7 0.0001] :P [1.5 11 0.0001] :eps [0.00002 2.7 0.00001]})

(defn- circle-fwd [n d] [n d (t/inscribed-perimeter n d) (t/circumscribed-perimeter n d) (t/gap n)])

(def circle-sources
  "n moves: d held, p, P, eps forward. d moves: n held. p moves: n held, d
   and P by the exact inverses. P moves: n held, p and d back. eps moves: d
   held, n by Newton on Emmy's D (n need not be whole: the bound between
   two polygons), then p and P forward."
  {:n (fn [n d _ _ _] (circle-fwd n d))
   :d (fn [n d _ _ _] (circle-fwd n d))
   :p (fn [n _ p _ _] (circle-fwd n (t/d-of-p n p)))
   :P (fn [n _ _ P _] (circle-fwd n (t/d-of-p n (t/p-of-P n P))))
   :eps (fn [_ d _ _ eps] (circle-fwd (t/n-of-gap eps) d))})

(defn circle-readout
  "[p/d P/d eps n d p P sin(pi/n) tan(pi/n) p96/d P96/d]."
  [n d p P eps]
  (fn [_]
    [(e// p d) (e// P d) eps n d p P (e/sin (e// PI n)) (e/tan (e// PI n))
     (t/inscribed-perimeter 96 1) (t/circumscribed-perimeter 96 1)]))

(def circle-start (zipmap circle-keys [96 1 (* 96 (Math/sin (/ PI 96))) (* 96 (Math/tan (/ PI 96))) (- (* 96 (Math/tan (/ PI 96))) (* 96 (Math/sin (/ PI 96))))]))

(defn- circle-body [!s r]
  (let [pf (fn [ks f] (ev/with-params {:atom !s :params ks} f))
        n (fn [i] (tie/fmt (list 'aget r i) 6))
        in? (list 'and (list '< (/ 223.0 71) (list 'aget r 0)) (list '< (list 'aget r 1) (/ 22.0 7)))]
    [:div
     (mafs/mafs
      {:view-box {:x [-1.3 1.3] :y [-1.1 1.1]} :height 360}
      (mafs/cartesian)
      (mafs/parametric {:xy (pf [:d] (fn [d] (fn [t] [(e/* (e// d 2) (e/cos t)) (e/* (e// d 2) (e/sin t))])))
                        :t [0 (* 2 PI)] :color :blue})
      (mafs/parametric {:xy (pf [:n :d] (fn [n d] (polygon-curve n (e/* (e// d 2) (e/cos (e// PI n))))))
                        :t [0 (* 2 PI)] :color :green})
      (mafs/parametric {:xy (pf [:n :d] (fn [n d] (polygon-curve n (e// d 2))))
                        :t [0 (* 2 PI)] :color :red}))
     (katex (list 'str "p = n\\,d\\sin\\frac{\\pi}{n} = " (n 3) "\\cdot" (n 4) "\\cdot" (n 7) " = " (n 5)
                  ",\\quad P = n\\,d\\tan\\frac{\\pi}{n} = " (n 3) "\\cdot" (n 4) "\\cdot" (n 8) " = " (n 6)))
     (katex (list 'str (n 0) " = \\frac{p}{d} < \\pi < \\frac{P}{d} = " (n 1)
                  ",\\qquad \\varepsilon = \\frac{P-p}{d} = " (n 2)))
     [:p {:style {:margin "0.3em 0"}}
      [:strong {:data-bracket (list 'if in? "inside" "outside")}
       (list 'if in? "inside Archimedes' bracket" "wider than Archimedes' bracket")]
      ": 3 10/71 = 3.140845 < π < 3 1/7 = 3.142857. "
      "A regular polygon needs a whole number of sides: the bound ε is met from n = "
      [:span {:data-n-whole (list 'str (list 'js/Math.ceil (list '- (list 'aget r 3) 1e-9)))}
       (list 'js/Math.ceil (list '- (list 'aget r 3) 1e-9))] "."]
     [:p {:style {:font-size "0.9em" :opacity 0.85}}
      "Archimedes' 96-gon, by raster: " (n 9) " < π < " (n 10)
      "; his whole-number ratios round both outward to 3 10/71 and 3 1/7. "
      "Green: inscribed n-gon, red: circumscribed, blue: the circle of diameter d."]]))

(defn circle
  "Prop. 3 as a tied widget."
  []
  (tie/widget {:id :ngon :keys circle-keys :start circle-start :ranges circle-ranges
               :labels {:n "n (sides)" :d "d (diameter)" :p "p (inscribed)" :P "P (circumscribed)" :eps "ε = (P − p)/d"}
               :sources circle-sources :readout circle-readout :body circle-body}))

;; ---------------------------------------------------------------------------
;; Quadrature of the Parabola, Props 21-24, on y = a^2 - x^2

(def parabola-keys [:a :T :k :S :R])

(def parabola-ranges {:a [0.3 1.5 0.001] :T [0.027 3.375 0.0001] :k [0 8 1] :S [0.02 4.5 0.0001] :R [0.000001 1.2 0.000001]})

(defn- parabola-fwd [a k]
  (let [T (t/first-triangle a)] [a T k (t/polygon T k) (t/remainder T k)]))

(def parabola-sources
  "a or k moves: the other held, T, S, R forward. T moves: k held, a back
   (a = T^(1/3)). R moves: T held, k back (exact: 4^(k+1) = 4T/3R). S
   moves: T held, R = (4/3) T - S, then k back."
  {:a (fn [a _ k _ _] (parabola-fwd a k))
   :T (fn [_ T k _ _] (parabola-fwd (t/half-chord T) k))
   :k (fn [a _ k _ _] (parabola-fwd a k))
   :S (fn [a T _ S _] (parabola-fwd a (t/k-of-remainder T (e/- (e/* 4/3 T) S))))
   :R (fn [a T _ _ R] (parabola-fwd a (t/k-of-remainder T R)))})

(defn parabola-readout
  "[a T k S R S/T (4/3)T R/T]."
  [a T k S R]
  (fn [_] [a T k S R (e// S T) (e/* 4/3 T) (e// R T)]))

(def parabola-start (zipmap parabola-keys [1 1 3 (* 4/3 (- 1 (Math/pow 4 -4))) (* 4/3 (Math/pow 4 -4))]))

(defn- inscribed-polyline
  "The polygon through generation k on the segment of y = a^2 - x^2: the
   chords between the 2^(k+1) + 1 equally spaced points, as one curve. In
   each gap of width h the parabola exceeds its chord by v (h - v), v the
   distance to the nearest point (a triangle wave by acos(cos))."
  [a k]
  (fn [x]
    (let [h (e// (e/* 2 a) (e/exp (e/* (e/+ k 1) (Math/log 2))))
          v (e/* (e// h (* 2 PI)) (e/acos (e/* 0.9999999999 (e/cos (e// (e/* 2 PI (e/+ x a)) h)))))]
      [x (e/- (e/- (e/square a) (e/square x)) (e/* v (e/- h v)))])))

(defn- parabola-body [!s r]
  (let [pf (fn [ks f] (ev/with-params {:atom !s :params ks} f))
        n (fn [i] (tie/fmt (list 'aget r i) 5))]
    [:div
     (mafs/mafs
      {:view-box {:x [-1.6 1.6] :y [-0.2 2.4]} :height 340}
      (mafs/cartesian)
      (mafs/parametric {:xy (pf [:a] (fn [a] (fn [x] [x (e/- (e/square a) (e/square x))]))) :t [-1.6 1.6] :color :blue})
      (mafs/parametric {:xy (pf [:a] (fn [a] (fn [x] [x 0]))) :t [-1.5 1.5] :color :gray})
      (mafs/parametric {:xy (pf [:a :k] (fn [a k] (fn [x] ((inscribed-polyline a k) (e/* a x))))) :t [-1 1] :color :red}))
     (katex (list 'str "S_k = T\\left(1 + \\tfrac14 + \\cdots + \\tfrac1{4^{k}}\\right) = " (n 3)
                  ",\\quad \\frac{S_k}{T} = " (n 5) " \\;\\to\\; \\frac43"))
     (katex (list 'str "R_k = \\tfrac43 T - S_k = \\tfrac13\\cdot\\frac{T}{4^{k}}\\cdot\\tfrac14\\cdot 4 = " (n 4)
                  ",\\quad \\tfrac43 T = " (n 6) ",\\quad T = a^3 = " (n 1)))
     [:p {:style {:font-size "0.9em" :opacity 0.85}}
      "Red: the inscribed polygon through generation k (" [:span {:data-k (list 'str (list 'aget r 2))} (list 'aget r 2)]
      "), 2" [:sup "k+1"] " chords; blue: the parabola y = a² − x² over the chord of half-width a. "
      "Each generation adds a quarter of the one before (Props 21-22); "
      "the remainder R is one third of the last generation (Prop. 23), so S + R = (4/3) T for every k (Prop. 24)."]]))

(defn parabola
  "Props 21-24 as a tied widget."
  []
  (tie/widget {:id :quadrature :keys parabola-keys :start parabola-start :ranges parabola-ranges
               :labels {:a "a (half chord)" :T "T = a³ (first triangle)" :k "k (generations)" :S "S (polygon)" :R "R (left over)"}
               :sources parabola-sources :readout parabola-readout :body parabola-body}))

;; ---------------------------------------------------------------------------
;; On the Sphere and Cylinder I.34, The Method Prop. 2: MathBox on raster

(def sphere-keys [:r :V :C :K])
(def sphere-ranges {:r [0.3 1.5 0.001] :V [0.1 14.2 0.0001] :C [0.16 21.3 0.0001] :K [0.028 3.6 0.0001]})

(defn- sphere-fwd [r] [r (t/sphere r) (t/cylinder r) (t/cone r)])

(def sphere-sources
  "Any one moves: r by its exact inverse, then the other three forward."
  {:r (fn [r _ _ _] (sphere-fwd r))
   :V (fn [_ V _ _] (sphere-fwd (t/radius-of-sphere V)))
   :C (fn [_ _ C _] (sphere-fwd (t/radius-of-cylinder C)))
   :K (fn [_ _ _ K] (sphere-fwd (t/radius-of-cone K)))})

(defn sphere-readout "[r V C K V/C V/K]." [r V C K] (fn [_] [r V C K (e// V C) (e// V K)]))

(def sphere-start (zipmap sphere-keys [1 (* 4/3 PI) (* 2 PI) (/ PI 3)]))

(defn- sphere-body [!s r]
  (let [pf (fn [f] (ev/with-params {:atom !s :params [:r]} f))
        n (fn [i] (tie/fmt (list 'aget r i) 4))]
    [:div
     (plot/scene
      {:range [[-1.6 1.6] [-1.6 1.6] [-1.6 1.6]] :scale [1 1 1] :camera [2.2 -2.6 1.4]
       :axes [] :grids [] :container {:style {:height "440px" :width "100%"}}}
      (plot/parametric-surface {:f (pf (fn [rr] (fn [[u v]] [(e/* rr (e/sin u) (e/cos v)) (e/* rr (e/sin u) (e/sin v)) (e/* rr (e/cos u))])))
                                :u [0 PI] :v [0 (* 2 PI)] :u-samples 32 :v-samples 48
                                :color "#C0392B" :opacity 0.55 :grid-u 8 :grid-v 12 :simplify? false})
      (plot/parametric-surface {:f (pf (fn [rr] (fn [[u v]] [(e/* rr (e/cos v)) (e/* rr (e/sin v)) (e/* rr u)])))
                                :u [-1 1] :v [0 (* 2 PI)] :u-samples 8 :v-samples 48
                                :color "#5AA0D0" :opacity 0.18 :grid-u 4 :grid-v 12 :simplify? false})
      (plot/parametric-surface {:f (pf (fn [rr] (fn [[u v]] [(e/* rr u (e/cos v)) (e/* rr u (e/sin v)) (e/* rr (e/- 1 u))])))
                                :u [0 1] :v [0 (* 2 PI)] :u-samples 8 :v-samples 48
                                :color "#E67E22" :opacity 0.45 :grid-u 4 :grid-v 12 :simplify? false}))
     (katex (list 'str "V = \\tfrac43\\pi r^3 = " (n 1) ",\\quad C = 2\\pi r^3 = " (n 2) ",\\quad K = \\tfrac13\\pi r^3 = " (n 3)))
     (katex (list 'str "\\frac{V}{C} = " (n 4) " = \\frac23,\\qquad \\frac{V}{K} = " (n 5) " = 4"))
     [:p {:style {:font-size "0.9em" :opacity 0.85}}
      "Red: the sphere of radius r; blue: the cylinder about it (base a great circle, height the diameter); "
      "orange: the cone of I.34 (base the great circle, height the radius). "
      "Every volume is a control: move one and r follows, and the others with it; the ratios stay 2 : 3 and 4 : 1. "
      "drag: rotate · scroll: zoom · right-drag: pan"]]))

(defn sphere
  "I.34 and The Method Prop. 2 as a tied widget in MathBox."
  []
  (tie/widget {:id :sphere-cylinder :keys sphere-keys :start sphere-start :ranges sphere-ranges
               :labels {:r "r (radius)" :V "V (sphere)" :C "C (cylinder)" :K "K (cone)"}
               :sources sphere-sources :readout sphere-readout :body sphere-body}))
