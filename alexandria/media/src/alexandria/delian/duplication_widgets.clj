(ns alexandria.delian.duplication-widgets
  "The doubling of the cube as emmy-viewers widgets on the raster backend,
   for a Clerk notebook that calls (history-of-math.widgets/install!).

   Two widgets, each a figure stepped sentence by sentence (the step list
   beside the drawing, the current sentence highlighted, each step adding
   exactly what its sentence introduces) with a Leva panel for the given
   line:

   - `archytas`: Archytas' half-cylinder, ring and cone in MathBox (orbit
     camera: drag rotates, scroll zooms, right-drag pans), AD = 2 fixed and
     the chord AB on a slider. The letters are HTML
     (alexandria.medium.html-labels), never MathBox text.
   - `menaechmus`: Menaechmus' two parabolas and hyperbola in Mafs, the
     given line a on a slider, b = 2a, and their meeting point.

   Every surface and curve is compiled by emmy-viewers with
   emmy.viewer.compile/*backend* bound to :raster here (not left to the
   caller), and every lettered point and every number on the page comes
   from one raster kernel per widget (`archytas-letters`,
   `menaechmus-letters`) that the page calls in the browser."
  (:require [clojure.walk :as walk]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.mathbox.plot :as plot]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]))

;; ---------------------------------------------------------------------------
;; Shared page forms: the step list and the kernel binding

(def colours
  {:circle "#3C4650" :cylinder "#4a7fb0" :torus "#c8913a" :cone "#2f8a3e" :meet "#c0392b"
   :mean "#7B5EA7" :parabola-x "#1F6FB4" :parabola-y "#B8860B" :hyperbola "#C0392B"})

(def ^:private button-style
  {:font-family "ui-monospace, monospace" :font-size "0.95em" :padding "0.15em 0.7em"
   :margin-right "0.35em" :border "1px solid currentColor" :border-radius "0.35em"
   :background "transparent" :color "inherit" :cursor "pointer" :opacity 0.8})

(defn- step-form
  "Browser code for the step of atom !c."
  [!c]
  (ev/get !c :step))

(defn- shown
  "Browser code: `frag` once the step reaches i (0-based)."
  [!c i frag]
  (list 'when (list '>= (step-form !c) i) frag))

(defn- step-list
  "Browser code: prev / next and the sentences, the current one
   highlighted; a click on a sentence goes to its step."
  [!c sentences figure-id]
  (let [n (count sentences)
        go (fn [form] (list 'fn [] (list 'swap! !c 'assoc :step form)))]
    [:div {:data-steps figure-id}
     [:div {:style {:margin-bottom "0.4em"}}
      [:button {:style button-style :data-act "prev"
                :on-click (go (list 'max 0 (list 'dec (step-form !c))))} "◀"]
      [:button {:style button-style :data-act "next"
                :on-click (go (list 'min (dec n) (list 'inc (step-form !c))))} "▶▶"]
      [:span {:style {:opacity 0.6 :font-size "0.85em"}}
       "step " (list 'inc (step-form !c)) " of " n]]
     (into [:ol {:style {:margin 0 :padding-left "2.2em" :font-size "0.95em" :line-height 1.45}}]
           (for [[i text] (map-indexed vector sentences)]
             (let [current? (list '= (step-form !c) i)]
               [:li {:data-step (inc i) :data-current (list 'str current?)
                     :on-click (go i)
                     :style {:cursor "pointer" :margin-bottom "0.45em" :padding "0.2em 0.5em"
                             :border-left (list 'if current? "3px solid #c0392b" "3px solid transparent")
                             :background (list 'if current? "rgba(192,57,43,0.08)" "transparent")
                             :opacity (list 'if (list '<= i (step-form !c)) 1 0.45)}}
                text])))]))

(defn- with-kernel
  "Browser code: bind the compiled kernel kform once, poll until its module
   is ready, then evaluate body with v = (kernel (array 0) params-form)."
  [kform params-form body]
  (let [ksym (gensym "letters")]
    (list 'reagent.core/with-let
          [ksym kform
           'box '(atom nil)
           'ready '(reagent.core/atom false)
           '_ (list 'let ['poll (list 'fn 'poll []
                                      (list 'if (list '.ready ksym)
                                            '(reset! ready true)
                                            '(js/setTimeout poll 20)))]
                    '(poll))]
          (list 'let ['v (list 'when '@ready (list ksym '(array 0) params-form))]
                body))))

(defn- fixed3 [form] (list '.toFixed form 4))

(defn- vget [i] (list 'aget 'v i))

;; ---------------------------------------------------------------------------
;; Archytas: AD = a on the x axis, A at the origin, the circle ABD in the
;; plane z = 0, z up

(def archytas-a "AD, the greater line." 2)

(def archytas-defaults "The slider's start: AB = 1, so AI^3 = 2." {:b 1.0 :step 0})

(def archytas-sentences
  "Archytas' construction, one object per sentence (Eutocius, after Eudemus;
   Heath 1896, pp. xxii-xxiii)."
  ["Let AD, the greater of the two given lines, be the diameter of a circle ABD, and AB, the lesser, a chord in it."
   "On the semicircle ABD stand a half-cylinder at right angles to the plane of the circle."
   "Turn a semicircle on AD, standing upright, about A: it sweeps a ring, a torus with no hole, and cuts the half-cylinder in a curve."
   "Turn the line AB about AD: it sweeps a cone with its apex at A."
   "The cone, the half-cylinder and the ring meet in one point K."
   "Drop KI at right angles to the plane of the circle: AB, AI, AK, AD are in continued proportion, so AI and AK are the two mean proportionals between AB and AD."])

(defn archytas-cylinder
  "The half-cylinder on the semicircle ABD, height 2: u in [0 pi], v in [0 2]."
  [a]
  (fn [[u v]] [(e/* (e// a 2) (e/+ 1 (e/cos u))) (e/* (e// a 2) (e/sin u)) v]))

(defn archytas-torus
  "The ring swept by the upright semicircle on AD turning about A:
   rho = a cos^2 w, z = a cos w sin w, at azimuth u; u, w in [0 pi/2]."
  [a]
  (fn [[u w]]
    (let [rho (e/* a (e/square (e/cos w)))]
      [(e/* rho (e/cos u)) (e/* rho (e/sin u)) (e/* a (e/cos w) (e/sin w))])))

(defn- meeting
  "[m x y z r]: AI = m = (a b^2)^(1/3), K = (x, y, z), AK = r = m^2 / b."
  [a b]
  (let [m (e/expt (e/* a (e/square b)) (e// 1 3))
        x (e// (e/square m) a)
        r (e// (e/square m) b)]
    [m x (e/sqrt (e/- (e/square m) (e/square x))) (e/sqrt (e/- (e/square r) (e/square m))) r]))

(defn archytas-cone
  "The cone swept by AB about AD (half-angle with cos = AB/AD), cut a
   little beyond K: u in [0 pi] (the upper half), v in [0 1]."
  [a b]
  (let [tan (e// (e/sqrt (e/- (e/square a) (e/square b))) b)
        [_ x] (meeting a b)
        reach (e/* 1.2 x)]
    (fn [[u v]]
      (let [s (e/* v reach)]
        [s (e/* tan s (e/cos u)) (e/* tan s (e/sin u))]))))

(defn archytas-curve
  "The curve where the ring cuts the half-cylinder: the point of the
   cylinder over the circle at azimuth u in [0 pi/2] (AI = a cos u) at
   height sqrt(AK^2 - AI^2) with AK^2 = a AI. The radicand a AI - AI^2 is
   clamped at 0 without branching ((q + |q|) / 2): raster's cos 0 lies a
   few ulps above 1."
  [a]
  (fn [u]
    (let [c (e/cos u)
          m (e/* a c)
          q (e/- (e/* a m) (e/square m))]
      [(e/* m c) (e/* m (e/sin u)) (e/sqrt (e// (e/+ q (e/abs q)) 2))])))

(defn archytas-generator
  "AB produced, the cone's generator in the plane of the circle, t in [0 1]
   to 1.2 times K's abscissa."
  [a b]
  (let [[_ x] (meeting a b)
        bx (e// (e/square b) a)
        by (e// (e/* b (e/sqrt (e/- (e/square a) (e/square b)))) a)
        s (e// (e/* 1.2 x) bx)]
    (fn [t] [(e/* t s bx) (e/* t s by) 0])))

(def archytas-index
  "Where each letter's [x y z] and each number starts in the output of
   `archytas-letters`."
  {"B" 0 "I" 3 "K" 6 :ab 9 :ai 10 :ak 11 :ad 12 :ai3 13 :ad-ab2 14})

(defn archytas-letters
  "The live letters B, I, K and the numbers AB, AI, AK, AD, AI^3, AD.AB^2,
   for the slider [a b], as one flat vector (`archytas-index`). The state is
   a dummy [0]."
  [a b]
  (fn [_]
    (let [[m x y z r] (meeting a b)
          bx (e// (e/square b) a)
          by (e// (e/* b (e/sqrt (e/- (e/square a) (e/square b)))) a)]
      [bx by 0 x y 0 x y z
       b m r a (e/cube m) (e/* a (e/square b))])))

(def archytas-range [[-0.2 2.2] [-1.6 1.6] [0 2]])

(def archytas-scale [1 1.33 0.85])

(defn- xyz [i] [(vget i) (vget (+ i 1)) (vget (+ i 2))])

(defn- archytas-labels
  "Browser code: the HTML labels, each shown from its sentence on."
  [!c]
  (let [style {:colour "#1d1f21" :halo "rgba(255,255,255,0.95)" :size 16 :offset [10 12]}
        lab (fn [i text at & [extra]]
              (list 'when (list '>= (step-form !c) i) (merge style {:text text :at at} extra)))]
    (list 'filterv 'some?
          [(lab 0 "A" [0 0 0]) (lab 0 "D" [archytas-a 0 0])
           (list 'when 'v (lab 0 "B" (xyz (archytas-index "B"))))
           (list 'when 'v (lab 4 "K" (xyz (archytas-index "K")) {:colour (colours :meet)}))
           (list 'when 'v (lab 5 "I" (xyz (archytas-index "I")) {:colour (colours :mean)}))])))

(defn- archytas-readout []
  (let [n #(fixed3 (vget (archytas-index %)))]
    (list 'when 'v
          [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.85em" :line-height 1.6
                         :margin-top "0.6em"}
                 :data-ai (list 'str (vget (archytas-index :ai)))}
           [:div "AB = " (n :ab) ", AD = " (n :ad)]
           [:div "AI = " (n :ai) ", AK = " (n :ak)]
           [:div "AB : AI = " (fixed3 (list '/ (vget (archytas-index :ab)) (vget (archytas-index :ai))))
            " = AI : AK = " (fixed3 (list '/ (vget (archytas-index :ai)) (vget (archytas-index :ak))))
            " = AK : AD = " (fixed3 (list '/ (vget (archytas-index :ak)) (vget (archytas-index :ad))))]
           [:div "AI³ = " (n :ai3) " = AD·AB² = " (n :ad-ab2)]
           [:div {:style {:opacity 0.6 :margin-top "0.4em"}}
            "drag: rotate · scroll: zoom · right-drag: pan"]])))

(defn- archytas-scene [!c]
  (let [a archytas-a
        with-b (fn [f] (ev/with-params {:atom !c :params [:b]} (fn [b] (f a b))))]
    (plot/scene
     {:range archytas-range :scale archytas-scale
      :ref '(fn [b] (reset! box b))
      :camera [1.4 -2.3 1.3]
      :axes [] :grids []
      :container {:style {:height "480px" :width "100%"}}}
     ;; S1: the circle ABD, AD, AB
     (plot/parametric-curve {:f (fn [u] [(e/* (/ a 2) (e/+ 1 (e/cos u))) (e/* (/ a 2) (e/sin u)) 0])
                             :t [0 (* 2 Math/PI)] :samples 97 :color (colours :circle) :width 2})
     (plot/line {:coords [[0 0 0] [a 0 0]] :color (colours :circle) :width 3})
     (plot/point {:coords [0 0 0] :size 8 :color (colours :circle)})
     (plot/point {:coords [a 0 0] :size 8 :color (colours :circle)})
     (list 'when 'v (plot/line {:coords [[0 0 0] (xyz (archytas-index "B"))] :color (colours :circle) :width 3}))
     (list 'when 'v (plot/point {:coords (xyz (archytas-index "B")) :size 8 :color (colours :circle)}))
     ;; S2: the half-cylinder
     (shown !c 1 (plot/parametric-surface {:f (archytas-cylinder a) :u [0 Math/PI] :v [0 2]
                                           :color (colours :cylinder) :opacity 0.3
                                           :u-samples 48 :v-samples 12 :grid-u 8 :grid-v 4}))
     ;; S3: the ring and its curve on the cylinder
     (shown !c 2 (plot/parametric-surface {:f (archytas-torus a) :u [0 (/ Math/PI 2)] :v [0 (/ Math/PI 2)]
                                           :color (colours :torus) :opacity 0.4
                                           :u-samples 40 :v-samples 40 :grid-u 6 :grid-v 6}))
     (shown !c 2 (plot/parametric-curve {:f (archytas-curve a) :t [0 (/ Math/PI 2)] :samples 97
                                         :color (colours :torus) :width 4}))
     ;; S4: the cone and its generator AB produced
     (shown !c 3 (plot/parametric-surface {:f (with-b archytas-cone) :u [0 Math/PI] :v [0 1]
                                           :color (colours :cone) :opacity 0.3 :simplify? false
                                           :u-samples 48 :v-samples 16 :grid-u 8 :grid-v 4}))
     (shown !c 3 (plot/parametric-curve {:f (with-b archytas-generator) :t [0 1] :samples 2
                                         :color (colours :cone) :width 3 :simplify? false}))
     ;; S5: K
     (shown !c 4 (list 'when 'v (plot/point {:coords (xyz (archytas-index "K")) :size 14 :color (colours :meet)})))
     ;; S6: KI, AI, AK
     (shown !c 5 (list 'when 'v
                       [:<>
                        (plot/line {:coords [(xyz (archytas-index "K")) (xyz (archytas-index "I"))]
                                    :color (colours :mean) :width 3})
                        (plot/line {:coords [[0 0 0] (xyz (archytas-index "I"))] :color (colours :mean) :width 5})
                        (plot/line {:coords [[0 0 0] (xyz (archytas-index "K"))] :color (colours :meet) :width 5})
                        (plot/point {:coords (xyz (archytas-index "I")) :size 9 :color (colours :mean)})])))))

(defn archytas
  "Archytas' solution as a stepped MathBox widget with an AB slider (AD = 2).
   Compiled on the raster backend whatever the caller's binding."
  []
  (binding [vc/*backend* :raster]
    (ev/with-let [!c archytas-defaults]
      (with-kernel
        (vc/compiled-fn archytas-letters [archytas-a (:b archytas-defaults)] [0] {:simplify? false})
        (list 'array archytas-a (ev/get !c :b))
        [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}
               :data-figure "delian-archytas" :data-step (list 'inc (step-form !c))}
         [:div {:style {:flex "1 1 460px" :min-width "300px" :position "relative"}}
          ['alexandria.medium.html-labels/overlay
           {:box 'box :range archytas-range :scale archytas-scale :labels (archytas-labels !c)}]
          (archytas-scene !c)]
         [:div {:style {:flex "1 1 280px" :min-width "240px"}}
          (leva/sub-panel {:fill true :flat true :title-bar false}
                          (leva/controls {:atom !c :schema {:b {:min 0.7 :max 1.6 :step 0.01 :label "AB"}}}))
          (step-list !c archytas-sentences "archytas")
          (archytas-readout)]]))))

;; ---------------------------------------------------------------------------
;; Menaechmus: x^2 = a y, y^2 = b x, x y = a b with b = 2a

(def menaechmus-defaults {:a 1.4 :step 0})

(def menaechmus-sentences
  "Menaechmus' loci, one curve per sentence (Eutocius; Heath 1896, p. xx)."
  ["Let a and b = 2a be the given lines, and x, y the two means sought: a : x = x : y = y : b."
   "From a : x = x : y, the square on x equals the rectangle a·y: the point (x, y) lies on the parabola x² = a y."
   "From x : y = y : b, the square on y equals the rectangle b·x: the point lies on the parabola y² = b x."
   "From a : x = y : b, the rectangle x·y equals the rectangle a·b: the point lies on the hyperbola x y = a b."
   "The curves meet in one point Θ. Its abscissa x is the first mean: x³ = a² b = 2a³, so x = a ∛2."])

(def menaechmus-index {"Θ" 0 :a 2 :x 3 :ratio 4 :x3 5 :two-a3 6})

(defn menaechmus-letters
  "Θ = (a 2^(1/3), a 2^(2/3)) and the numbers a, x, x/a, x^3, 2a^3."
  [a]
  (fn [_]
    (let [c (e/expt 2 (e// 1 3))
          x (e/* a c)]
      [x (e// (e/square x) a) a x (e// x a) (e/cube x) (e/* 2 (e/cube a))])))

(defn menaechmus-parabola-x "x^2 = a y, as y of x." [a] (fn [x] (e// (e/square x) a)))

(defn menaechmus-parabola-y "y^2 = 2a x, as (x, y) of y = t." [a] (fn [t] [(e// (e/square t) (e/* 2 a)) t]))

(defn menaechmus-hyperbola "x y = 2a^2, as (x, y) of x = t." [a] (fn [t] [t (e// (e/* 2 (e/square a)) t)]))

(defn- menaechmus-readout []
  (let [n #(fixed3 (vget (menaechmus-index %)))]
    (list 'when 'v
          [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.85em" :line-height 1.6
                         :margin-top "0.6em"}
                 :data-theta-x (list 'str (vget (menaechmus-index :x)))}
           [:div "a = " (n :a) ", b = 2a"]
           [:div "Θ: x = " (n :x) ", x / a = " (n :ratio) " (∛2)"]
           [:div "x³ = " (n :x3) " = 2a³ = " (n :two-a3)]])))

(defn menaechmus
  "Menaechmus' two parabolas and hyperbola in Mafs with an a slider, stepped
   one curve per sentence. Compiled on the raster backend whatever the
   caller's binding."
  []
  (binding [vc/*backend* :raster]
    (ev/with-let [!c menaechmus-defaults]
      (let [with-a (fn [f] (ev/with-params {:atom !c :params [:a]} f))
            theta (xyz 0)]
        (with-kernel
          (vc/compiled-fn menaechmus-letters [(:a menaechmus-defaults)] [0] {:simplify? false})
          (list 'array (ev/get !c :a))
          [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}
                 :data-figure "delian-menaechmus" :data-step (list 'inc (step-form !c))}
           [:div {:style {:flex "1 1 460px" :min-width "300px"}}
            (mafs/mafs {:height 440 :view-box {:x [-0.3 4.6] :y [-0.3 4.4]} :zoom true}
                       (mafs/cartesian)
                       (shown !c 1 (mafs/of-x {:y (with-a menaechmus-parabola-x) :color (colours :parabola-x)}))
                       (shown !c 2 (mafs/parametric {:xy (with-a menaechmus-parabola-y) :t [-4.4 4.4]
                                                     :color (colours :parabola-y)}))
                       (shown !c 3 (mafs/parametric {:xy (with-a menaechmus-hyperbola) :t [0.25 4.6]
                                                     :color (colours :hyperbola) :style "dashed"}))
                       (shown !c 4 (list 'when 'v
                                         [:<>
                                          (mafs/segment {:point1 [(first theta) 0] :point2 [(first theta) (second theta)]
                                                         :color (colours :mean) :style "dashed"})
                                          (mafs/point {:x (first theta) :y (second theta) :color (colours :meet)})
                                          (mafs/text "Θ" {:x (first theta) :y (second theta) :attach "ne"
                                                          :color (colours :meet) :size 20})
                                          (mafs/text "x" {:x (list '/ (first theta) 2) :y 0 :attach "s"
                                                          :color (colours :mean)})])))]
           [:div {:style {:flex "1 1 280px" :min-width "240px"}}
            (leva/sub-panel {:fill true :flat true :title-bar false}
                            (leva/controls {:atom !c :schema {:a {:min 0.6 :max 2.2 :step 0.01 :label "a"}}}))
            (step-list !c menaechmus-sentences "menaechmus")
            (menaechmus-readout)]])))))

(defn kernel-forms
  "Every (js/Function. \"fb\" glue) form in a built fragment: the compiled
   kernels it ships."
  [form]
  (let [acc (volatile! [])]
    (walk/postwalk (fn [x]
                     (when (and (seq? x) (= 'js/Function. (first x)) (= "fb" (second x)))
                       (vswap! acc conj (nth x 2)))
                     x)
                   form)
    @acc))
