(ns alexandria.apollonius.areas-widget
  "Apollonius' application of areas (Conics I.11-13, Heath 1896) as a Mafs
   widget on emmy-viewers' raster backend, driven by a Leva panel.

   Heath's frame: P the vertex, PM the diameter (here the x axis), QV an
   ordinate (vertical), PV = x the abscissa, PL = p the parameter drawn at
   right angles to PM, VR parallel to PL meeting P'L produced at R, PP' = d
   the transverse. The symptoma y^2 = p x + c x^2 with c = -p/d (I.12-13)
   is drawn to scale:

     the square on QV          y^2, standing on QV
     the rectangle VL          PV.PL = p x, under PM
     the rectangle LR          c x^2, by which PV.VR exceeds VL (c > 0,
                               hyperbola) or falls short of it (c < 0,
                               ellipse); none when c = 0 (parabola)

   I.11: \"the square on any ordinate to the fixed diameter PM is equal to a
   rectangle applied to the fixed straight line PL drawn at right angles to
   PM with altitude equal to the corresponding abscissa PV. Hence the
   section is called a Parabola.\"

   The panel reads the {:l :e} atom of the conics playground (r = l / (1 + e
   cos t) about a focus). In Apollonius' vertex frame that curve is y^2 =
   2 l x - (1 - e^2) x^2, so p = 2 l and c = e^2 - 1: the sign of c is the
   sign of e - 1. A movable point V on PM sets the abscissa x.

   Every number on the figure is an output of one Emmy function (`figure`)
   compiled by raster: in the browser through emmy.viewer.compile/*backend*
   :raster (a wasm kernel, Emmy's :js only without WebAssembly), on the JVM
   through alexandria.raster (`numbers`). The browser only indexes the
   kernel's outputs and compares them; it computes nothing."
  (:require [alexandria.raster :as raster]
            [emmy.clerk]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]))

;; ---------------------------------------------------------------------------
;; The kernel: one Emmy function, every number the panel draws

(def outputs
  "The kernel's outputs, in order. P' = (P'x, 0) is the other end of the
   transverse (-p/c: behind P for the hyperbola, beyond V for the ellipse);
   x-max is 0.98 d, the farthest V may go on the ellipse."
  [:x :y :x+y :-p :-vr :y2 :px :cx2 :c :p :vr :px+cx2 :P'x :x-max])

(def ^:private index (zipmap outputs (range)))

(defn figure
  "Apollonius' sides and areas for the section of focal parameter l and
   eccentricity e at the abscissa x = PV, as a vector in the order of
   `outputs`. QV = sqrt(PV.VR) (I.12-13), VR = p + c x."
  [l e x]
  (let [p (e/* 2 l)
        c (e/- (e/square e) 1)
        vr (e/+ p (e/* c x))
        y2 (e/* x vr)
        y (e/sqrt y2)
        px (e/* p x)
        cx2 (e/* c x x)
        p' (e// (e/negate p) c)]
    [x y (e/+ x y) (e/negate p) (e/negate vr) y2 px cx2 c p vr (e/+ px cx2) p' (e/* 0.98 p')]))

(defn numbers
  "{output value} of `figure` at {:l :e :x}, every value from a raster
   kernel (alexandria.raster/value)."
  [{:keys [l e x]}]
  (zipmap outputs (raster/value figure l e x)))

(def tolerance
  "|c| below which the panel names the section a parabola (Leva's e step is
   0.01, so e = 1 gives c = 0 up to rounding)."
  0.005)

(defn kind
  "The section's name from the sign of c: :parabola (applied exactly),
   :hyperbola (exceeding), :ellipse (falling short)."
  [c]
  (cond (< (Math/abs (double c)) tolerance) :parabola
        (pos? c) :hyperbola
        :else :ellipse))

(def labels
  "What the name says of the areas, after the sign of c."
  {:parabola "the rectangle on PL is applied exactly (παραβολή)"
   :hyperbola "PV.VR exceeds VL by LR (ὑπερβολή)"
   :ellipse "PV.VR falls short of VL by LR (ἔλλειψις)"})

;; ---------------------------------------------------------------------------
;; The section itself, from its vertex P

(defn section
  "The upper half of the section y^2 = p x + c x^2 (p = 2 l, c = e^2 - 1)
   from its vertex P, at the parameter u >= 0: x = p u^2 / (1 + (|c| - c)
   u^2), y = p u sqrt(1 + |c| u^2) / (1 + (|c| - c) u^2). The denominator
   never vanishes, so the path has no pole: the parabola and the hyperbola's
   near branch run out to infinity, the ellipse rises from P to the end of
   its conjugate diameter (x = d/2)."
  [l e]
  (let [p (e/* 2 l)
        c (e/- (e/square e) 1)
        k (e/abs c)]
    (fn [u]
      (let [den (e/+ 1 (e/* (e/- k c) u u))]
        [(e// (e/* p u u) den)
         (e// (e/* p u (e/sqrt (e/+ 1 (e/* k u u)))) den)]))))

(defn reflected
  "`section` turned about the centre, x -> d - x with d = PP' = -p/c: the
   ellipse's half from the conjugate diameter to P', or the hyperbola's
   opposite section (I.14) through P'. For the parabola d is infinite and
   Mafs drops the non-finite points."
  [l e]
  (let [f (section l e)
        d (e// (e/* -2 l) (e/- (e/square e) 1))]
    (fn [u] (let [[x y] (f u)] [(e/- d x) y]))))

;; ---------------------------------------------------------------------------
;; The widget

(def defaults
  "The playground's initial state plus the abscissa."
  {:l 1.5 :e 0.6 :x 1.5})

(def schema
  {:l {:min 0.25 :max 2.5 :step 0.01 :label "l (p = 2l)"}
   :e {:min 0 :max 1.6 :step 0.01 :label "e (c = e² − 1)"}})

(defn- kernel-binding
  "[binding call-form]: the kernel of `figure` compiled by the current
   backend into a client-side binding, and the form that evaluates it at the
   abscissa form `x` with the atom's l and e."
  [!p]
  (let [pf (ev/with-params {:atom !p :params [:l :e]}
             (fn [l e] (fn [x] (figure l e x))))
        [binding {g :f}] (vc/compile-1d {:f pf} :f)]
    [binding g]))

(defn- at [v k] (list 'nth v (index k)))

(defn- fixed [form] (list '.toFixed form 2))

(defn- body
  "The Mafs scene and the caption for the kernel call g, inside the scope
   where the atom !p and the abscissa atom !v are bound."
  [!p !v g]
  (let [v0 (gensym "v0") xe (gensym "x") v (gensym "v")
        c (at v :c)
        kind-form (list 'cond
                        (list '< (list 'js/Math.abs c) tolerance) :parabola
                        (list 'pos? c) :hyperbola
                        :else :ellipse)
        k (gensym "kind")
        pt (fn [a b] [(at v a) (if (keyword? b) (at v b) b)])
        x (at v :x)]
    (list 'let [v0 (list g (list 'first (list 'deref !v)))
                ;; on the ellipse V stays inside the transverse PP'
                xe (list 'let ['x (list 'first (list 'deref !v))]
                         (list 'if (list 'and (list 'neg? (at v0 :c)) (list '> 'x (at v0 :x-max)))
                               (at v0 :x-max) 'x))
                v (list g xe)
                k kind-form]
          [:div
           (mafs/mafs
            {:view-box {:x [-2 7] :y [-6 4.5]} :height 520}
            (mafs/cartesian {:subdivisions false})
            ;; the section itself, upper half, from P (and from P')
            (mafs/parametric
             {:xy (ev/with-params {:atom !p :params [:l :e]} section)
              :t [0 12] :min-sampling-depth 10
              :color :foreground :opacity 0.6})
            ;; at c = 0 (up to rounding) P' is at infinity: no second piece
            (list 'when (list 'not= k :parabola)
                  (mafs/parametric
                   {:xy (ev/with-params {:atom !p :params [:l :e]} reflected)
                    :t [0 12] :min-sampling-depth 10
                    :color :foreground :opacity 0.6}))
            ;; rectangle VL = PV.PL
            (mafs/polygon {:points [[0 0] (pt :x 0) (pt :x :-p) [0 (at v :-p)]]
                           :color :blue :fill-opacity 0.25})
            ;; rectangle LR = c x^2, the excess or the deficiency
            (mafs/polygon {:points [[0 (at v :-p)] (pt :x :-p) (pt :x :-vr) [0 (at v :-vr)]]
                           :color (list 'if (list '= k :ellipse) "#d9534f" "#3aa655")
                           :fill-opacity 0.35
                           :stroke-style (list 'if (list '= k :ellipse) "dashed" "solid")})
            ;; the square on QV
            (mafs/polygon {:points [(pt :x 0) (pt :x+y 0) (pt :x+y :y) (pt :x :y)]
                           :color :yellow :fill-opacity 0.35})
            ;; P'L produced to R
            (list 'when (list 'not= k :parabola)
                  (mafs/through-points {:point1 [0 (at v :-p)] :point2 (pt :x :-vr)
                                        :color :violet :style "dashed"}))
            (list 'when (list '< (list 'js/Math.abs (at v :P'x)) 50)
                  (mafs/point {:x (at v :P'x) :y 0 :color :violet}))
            (list 'when (list '< (list 'js/Math.abs (at v :P'x)) 50)
                  (mafs/text "P′" {:x (at v :P'x) :y 0 :attach "ne" :color :violet}))
            (mafs/segment {:point1 (pt :x 0) :point2 (pt :x :y) :color :yellow :weight 3})
            (mafs/text "P" {:x 0 :y 0 :attach "nw"})
            (mafs/text "L" {:x 0 :y (at v :-p) :attach "w"})
            (mafs/text "Q" {:x x :y (at v :y) :attach "n"})
            (mafs/text "R" {:x x :y (at v :-vr) :attach "e"})
            (mafs/text "V" {:x x :y 0 :attach "se"})
            (mafs/movable-point {:atom !v :color :red
                                 :constrain (list 'fn '[[x _]]
                                                  (list 'let ['w (list g 'x)]
                                                        (list 'cond
                                                              '(< x 0.05) [0.05 0]
                                                              (list 'and (list 'neg? (at 'w :c)) (list '> 'x (at 'w :x-max)))
                                                              [(at 'w :x-max) 0]
                                                              :else ['x 0])))}))
           [:p {:style {:font-size "1.05em"}}
            "PV = " (fixed x) ", PL = " (fixed (at v :p)) ", c = " (fixed c) ". "
            "QV² = " [:span {:data-areas "square"} (fixed (at v :y2))]
            " = PV.PL ± LR = " (fixed (at v :px)) " + (" (fixed (at v :cx2)) ") = "
            (fixed (at v :px+cx2)) ". "
            [:strong {:data-conic-name "true"} (list 'name k)] ": " (list 'get labels k)]])))

(defn panel
  "The application-of-areas panel for the client-side atom !p ({:l :e}),
   which a caller shares with the conics playground. Call it inside an
   ev/with-let that binds !p. Compiles on the raster backend."
  ([!p] (panel !p (:x defaults)))
  ([!p x0]
   (binding [vc/*backend* :raster]
     (let [[binding g] (kernel-binding !p)]
       (ev/with-let [!v [x0 0]]
         [:div
          (leva/sub-panel {:fill true :titleBar {:drag false}}
                          (leva/controls {:folder {:name "Application of areas"}
                                          :atom !p :schema schema}))
          (vc/wrap [binding] (body !p !v g))])))))

(defn areas
  "The application-of-areas widget with its own atom, from `init` ({:l :e},
   optionally :x, the playground's shape); defaults to `defaults`."
  ([] (areas defaults))
  ([init]
   (let [{:keys [x] :as init} (merge defaults init)]
     (ev/with-let [!p (select-keys init [:l :e])]
       (panel !p x)))))
