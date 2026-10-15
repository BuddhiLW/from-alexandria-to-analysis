(ns alexandria.delian.archytas-playground
  "Archytas' solution as a playground (MathBox on the raster backend,
   orbitable, plus a plan view in Mafs on raster). Free: the diameter AD,
   the chord AB (drag B round the circle ABD in the plan, or the slider:
   any ratio, also AB past AD, where the chord no longer exists), and a
   point P on the curve where the ring cuts the half-cylinder (drag its
   foot round the circle). Drawn: the circle ABD, the half-cylinder, the
   ring and its curve, the cone swept by AB, K where the three meet, KI,
   AI, AK, and P with AP.

   Live: AI^3 = AD.AB^2 and the three ratios at K, substituted; at P the
   ratios AB : AI_P, AI_P : AK_P, AK_P : AD, equal only when P is K; the
   angle PAD against the cone's half-angle, naming P inside / on / outside
   the cone; the case of AD : AB (Delian, a ratio of cubes, a = b, a solid
   problem, no chord). Every number is a row of the raster kernel of
   alexandria.delian.playgrounds/archytas-figure."
  (:require [alexandria.delian.duplication-widgets :as w]
            [alexandria.delian.playgrounds :as pg]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.mathbox.plot :as plot]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]))

(def index "Where each scalar sits in the kernel row." (zipmap pg/archytas-scalars (range)))

(defn- v [k] (list 'aget 'v (index k)))
(defn- fx [form] (list 'let ['q form] '(if (js/isFinite q) (.replace (.toFixed q 3) "-" "−") "—")))
(defn- p3 [kx ky kz] [(v kx) (v ky) (v kz)])

(def range3 [[-0.2 2.6] [-1.4 1.4] [0 2.4]])
(def scale3 [1 1.1 0.85])

;; ---------------------------------------------------------------------------
;; Surfaces (Emmy, compiled by emmy-viewers on :raster)

(defn- at-most [x hi] (e/* 0.5 (e/- (e/+ x hi) (e/abs (e/- x hi)))))

(defn circle "The circle ABD on the diameter AD." [ad]
  (fn [t] [(e/* (e// ad 2) (e/+ 1 (e/cos t))) (e/* (e// ad 2) (e/sin t)) 0]))

(defn cylinder "The half-cylinder on the semicircle ABD, height AD." [ad]
  (fn [[u z]] [(e/* (e// ad 2) (e/+ 1 (e/cos u))) (e/* (e// ad 2) (e/sin u)) (e/* z ad)]))

(defn cone
  "The cone swept by AB about AD, AB held just short of AD (the chord
   must exist), cut at slant length 1.1 AD (K lies within: AK <= AD)."
  [ad ab]
  (let [b (at-most ab (e/* 0.999 ad))
        c (e// b ad)
        s (e// (e/sqrt (e/- (e/square ad) (e/square b))) ad)]
    (fn [[u t]]
      (let [l (e/* t 1.1 ad)]
        [(e/* l c) (e/* l s (e/cos u)) (e/* l s (e/sin u))]))))

(defn generator
  "AB produced in the plane of the circle, to slant length 1.1 AD."
  [ad ab]
  (let [b (at-most ab (e/* 0.999 ad))
        c (e// b ad)
        s (e// (e/sqrt (e/- (e/square ad) (e/square b))) ad)]
    (fn [t] [(e/* t 1.1 ad c) (e/* t 1.1 ad s) 0])))

;; ---------------------------------------------------------------------------

(def names
  {:delian "AD = 2 AB: the Delian case, AI is the side of the doubled cube"
   :cube "AD : AB is a ratio of cubes of numbers: the means are found by ruler alone"
   :equal "AD = AB: the cone shuts onto AD, AI = AK = AD, nothing to find"
   :solid "any other ratio: a solid problem, the means need the three surfaces"
   :no-chord "AB longer than the diameter AD: AB is no chord, there is no cone and no K"})

(def p-names
  {:on "P is on the cone too: P is K, and AB : AI = AI : AK = AK : AD"
   :inside "P lies inside the cone (∠PAD smaller than the cone's angle): slide it on"
   :outside "P lies outside the cone (∠PAD larger): slide it back"})

(defn- button [!s k label m]
  [:button {:data-preset (name k) :style {:margin "0.2em"} :on-click (list 'fn [] (list 'reset! !s m))} label])

(defn- labels [ad]
  (let [st {:colour "#ECEEE4" :halo "rgba(0,0,0,0.75)" :size 16 :offset [8 10]}
        l (fn [t at & [x]] (merge st {:text t :at at} x))]
    (list 'if 'v
          [(l "A" [0 0 0]) (l "D" [ad 0 0]) (l "B" [(v :bx) (v :by) 0])
           (l "K" (p3 :kx :ky :kz) {:colour "#E8735A"}) (l "I" [(v :ix) (v :iy) 0] {:colour "#B9A3E3"})
           (l "P" (p3 :px :py :pz) {:colour "#f0c960"})]
          [])))

(defn- plan [!s ad]
  (mafs/mafs {:height 260 :view-box {:x [-0.2 2.6] :y [-0.1 1.3]}}
             (mafs/cartesian {:subdivisions false})
             (mafs/circle {:center [(list '/ ad 2) 0] :radius (list '/ ad 2) :color "#9aa7b4"})
             (mafs/segment {:point1 [0 0] :point2 [ad 0] :color "#ECEEE4"})
             (mafs/text "A" {:x 0 :y 0 :attach "sw"})
             (mafs/text "D" {:x ad :y 0 :attach "se"})
             (list 'when 'v
                   [:<>
                    (mafs/segment {:point1 [0 0] :point2 [(v :bx) (v :by)] :color "#2f8a3e"})
                    (list 'when '(not= kind :no-chord)
                          [:<>
                           (mafs/point {:x (v :ix) :y (v :iy) :color "#B9A3E3"})
                           (mafs/text "I" {:x (v :ix) :y (v :iy) :attach "s" :color "#B9A3E3"})])
                    (mafs/movable-point
                     {:point [(v :bx) (v :by)] :color "#2f8a3e"
                      :constrain (list 'fn '[[x y]]
                                       (list 'let ['c (list '/ ad 2)
                                                   'n '(max 1e-9 (js/Math.hypot (- x c) y))]
                                             '[(+ c (* c (/ (- x c) n))) (max 0 (* c (/ y n)))]))
                      :on-move (list 'fn '[[x y]]
                                     (list 'swap! !s 'assoc :ab '(max 0.1 (min 2.6 (js/Math.hypot x y)))))})
                    (mafs/text "B" {:x (v :bx) :y (v :by) :attach "ne" :color "#2f8a3e"})
                    (mafs/movable-point
                     {:point [(v :pix) (v :piy)] :color "#f0c960"
                      :constrain (list 'fn '[[x y]]
                                       (list 'let ['t '(max 0.02 (min 1.55 (js/Math.atan2 (max 0 y) x)))
                                                   'm (list '* ad '(js/Math.cos t))]
                                             '[(* m (js/Math.cos t)) (* m (js/Math.sin t))]))
                      :on-move (list 'fn '[[x y]]
                                     (list 'swap! !s 'assoc :u '(max 0.02 (min 1.55 (js/Math.atan2 (max 0 y) x)))))})
                    (mafs/text "P" {:x (v :pix) :y (v :piy) :attach "nw" :color "#f0c960"})])))

(defn- scene [!s ad]
  (let [p-ad (fn [f] (ev/with-params {:atom !s :params [:ad]} f))
        p-ad-ab (fn [f] (ev/with-params {:atom !s :params [:ad :ab]} f))]
    (plot/scene
     {:range range3 :scale scale3 :ref '(fn [b] (reset! box b)) :camera [1.5 -2.4 1.4]
      :axes [] :grids [] :container {:style {:height "440px" :width "100%"}}}
     (plot/parametric-curve {:f (p-ad circle) :t [0 (* 2 Math/PI)] :samples 97 :color "#9aa7b4" :width 2})
     (plot/parametric-surface {:f (p-ad cylinder) :u [0 Math/PI] :v [0 1] :color (w/colours :cylinder)
                               :opacity 0.25 :u-samples 48 :v-samples 10 :grid-u 8 :grid-v 4})
     (plot/parametric-surface {:f (p-ad w/archytas-torus) :u [0 (/ Math/PI 2)] :v [0 (/ Math/PI 2)]
                               :color (w/colours :torus) :opacity 0.35 :u-samples 36 :v-samples 36
                               :grid-u 6 :grid-v 6})
     (plot/parametric-curve {:f (p-ad w/archytas-curve) :t [0 (/ Math/PI 2)] :samples 97
                             :color (w/colours :torus) :width 4})
     (plot/parametric-surface {:f (p-ad-ab cone) :u [0 Math/PI] :v [0 1] :color (w/colours :cone)
                               :opacity 0.25 :simplify? false :u-samples 48 :v-samples 12 :grid-u 8 :grid-v 4})
     (plot/parametric-curve {:f (p-ad-ab generator) :t [0 1] :samples 2 :color (w/colours :cone)
                             :width 3 :simplify? false})
     (list 'when 'v
           [:<>
            (plot/line {:coords [[0 0 0] [ad 0 0]] :color "#ECEEE4" :width 3})
            (plot/line {:coords [[0 0 0] [(v :bx) (v :by) 0]] :color (w/colours :cone) :width 4})
            (list 'when '(not= kind :no-chord)
                  [:<>
                   (plot/point {:coords (p3 :kx :ky :kz) :size 14 :color "#E8735A"})
                   (plot/line {:coords [(p3 :kx :ky :kz) [(v :ix) (v :iy) 0]] :color "#B9A3E3" :width 3})
                   (plot/line {:coords [[0 0 0] [(v :ix) (v :iy) 0]] :color "#B9A3E3" :width 5})
                   (plot/line {:coords [[0 0 0] (p3 :kx :ky :kz)] :color "#E8735A" :width 4})])
            (plot/point {:coords (p3 :px :py :pz) :size 12 :color "#f0c960"})
            (plot/line {:coords [[0 0 0] (p3 :px :py :pz)] :color "#f0c960" :width 2})
            (plot/line {:coords [(p3 :px :py :pz) [(v :pix) (v :piy) 0]] :color "#f0c960" :width 1})]))))

(defn- panel [!s ad ab u]
  [:div {:style {:flex "1 1 300px" :min-width "260px"}}
   (leva/sub-panel {:fill true :flat true :title-bar false}
                   (leva/controls {:atom !s :schema {:ad {:min 0.8 :max 2.4 :step 0.01 :label "AD (greater)"}
                                                     :ab {:min 0.1 :max 2.6 :step 0.01 :label "AB (lesser)"}
                                                     :u {:min 0.02 :max 1.55 :step 0.001 :label "P: azimuth"}}}))
   (list 'when 'v
         [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.9em" :line-height 1.6}}
          [:div {:data-live-case '(name kind) :style {:font-size "1.12em" :font-weight 600}}
           "AD : AB = " (fx 'ratio) " — " '(get names kind)]
          [:div {:data-readout "k"} "AI³ = " (fx (v :ai3)) " = AD·AB² = " (fx ad) "·" (fx ab) "² = " (fx (v :ad-ab2))]
          [:div {:data-readout "ratios"} "at K: AB : AI = " (fx (v :r1)) ",  AI : AK = " (fx (v :r2)) ",  AK : AD = " (fx (v :r3))]
          [:div {:data-readout "p"} "at P: AB : AIₚ = " (fx (v :p1)) ",  AIₚ : AKₚ = " (fx (v :p2)) ",  AKₚ : AD = " (fx (v :p3))]
          [:div {:data-readout "angle"} "∠PAD = " (fx (v :p-angle)) "°,  cone's angle = " (fx (v :cone-angle))
           "°;  azimuth of K = " (fx (list '* (/ 180 Math/PI) (v :uk))) "°, of P = " (fx (list '* (/ 180 Math/PI) u)) "°"]
          [:div {:data-live-p '(name pk) :style {:font-family "Georgia, serif" :margin-top "0.3em"}} '(get p-names pk)]
          [:div {:data-badge '(if (= kind :no-chord) "broken" "ok")
                 :style {:margin "0.5em 0" :padding "0.3em 0.6em" :border-radius "4px" :color "#fff"
                         :background '(if (= kind :no-chord) "#7a2b22" "#2f6b3a")}}
           '(if (= kind :no-chord)
              "Archytas needs AB to be a chord of the circle on AD: make AB shorter than AD."
              "The three surfaces meet in K: AB, AI, AK, AD are in continued proportion.")]])
   [:div "Try this: "
    (button !s :on-k "Slide P onto K" (:on-k pg/archytas-presets))
    (button !s :eight "AD = 8 AB" (:eight pg/archytas-presets))
    (button !s :equal "AB = AD" (:equal pg/archytas-presets))
    (button !s :no-chord "AB longer than AD" (:no-chord pg/archytas-presets))
    (button !s :delian "Reset to Archytas" (:delian pg/archytas-presets))]
   [:div {:style {:font-size "0.85em" :opacity 0.75 :margin-top "0.4em"}}
    "P runs along the curve the ring cuts on the half-cylinder, so two of the three conditions already hold; "
    "only on the cone does the third, and that one point is K. Change AB: the cone opens or closes and K moves."]])

(defn playground
  "The Archytas playground. Compiled on the raster backend."
  []
  (binding [vc/*backend* :raster]
    (ev/with-let [!s pg/archytas-start]
      (let [ksym (gensym "archytas")
            kform (vc/compiled-fn pg/archytas-figure (mapv pg/archytas-start [:ad :ab :u]) [0] {:simplify? false})
            ad (ev/get !s :ad) ab (ev/get !s :ab) u (ev/get !s :u)]
        (list 'reagent.core/with-let
              [ksym kform 'box '(atom nil) 'ready '(reagent.core/atom false)
               '_ (list 'let ['poll (list 'fn 'poll [] (list 'if (list '.ready ksym) '(reset! ready true) '(js/setTimeout poll 20)))]
                        '(poll))]
              (list 'let ['v (list 'when '@ready (list ksym '(array 0) (list 'array ad ab u)))
                          'ratio (list '/ ad ab)
                          'kind (list 'let ['w? '(fn [q] (some #(< (js/Math.abs (- q (* % % %))) 0.01) [2 3 4 5]))]
                                      (list 'cond (list '> (list '- ab ad) 0.01) :no-chord
                                            '(< (js/Math.abs (- ratio 1)) 0.01) :equal
                                            '(< (js/Math.abs (- ratio 2)) 0.01) :delian
                                            '(w? ratio) :cube :else :solid))
                          'gap (list 'if 'v (v :p-gap) 0)
                          'pk '(cond (= kind :no-chord) :outside (< (js/Math.abs gap) 0.25) :on (neg? gap) :inside :else :outside)
                          'names names 'p-names p-names]
                    [:div {:data-playground "delian-archytas" :data-case '(name kind) :data-p '(name pk)
                           :data-state (list 'str ad " " ab " " u)
                           :style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                     [:div {:style {:flex "1 1 480px" :min-width "300px" :position "relative"}}
                      ['alexandria.medium.html-labels/overlay {:box 'box :range range3 :scale scale3 :labels (labels ad)}]
                      (scene !s ad)
                      [:div {:style {:font-size "0.8em" :opacity 0.7}}
                       "3D: drag rotates, scroll zooms. Plan below: drag B round the circle (the chord AB), and P's foot (P on the curve)."]
                      (plan !s ad)]
                     (panel !s ad ab u)]))))))
