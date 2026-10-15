(ns alexandria.gauss.egregium-widget
  "The Theorema egregium playground of the Gauss notebook (Disquisitiones
   generales circa superficies curvas, 1828, art. 11-12), an emmy-viewers
   fragment on the raster backend.

   Two surfaces from alexandria.gauss.egregium, picked by kind k (0 or 1):

     k = 0  the helicoid-catenoid family, bend t in [0 1] (angle t pi/2:
            the helicoid at 0, the catenoid at 1), u = pi U, v = V
     k = 1  the sheet rolled about the v axis, t in [0 1] (flat to a half
            cylinder), u = U, v = V

   and a stretch s that is not a bending (the catenoid's axis stretched,
   the sheet lifted along v). The reader moves t, s and the point P = (U, V)
   (Leva, or drag P in the parameter square); the surface bends in MathBox
   (orbitable), P and the two parameter lines through it ride on it.

   Every number comes from one raster kernel, `probe`, run at the state
   and at the author's configuration (t = 0, s = 1): E, F, G and K from the
   metric alone (Brioschi) and K from the embedding. The parameter square
   draws the first fundamental form at P as its indicatrix
   E dU^2 + 2F dU dV + G dV^2 = const, beside the one of the unbent
   surface: a bending leaves it alone, a stretch deforms it, and K follows
   the indicatrix, not the shape in space. The case is named live."
  (:require [alexandria.gauss.egregium :as g]
            [emmy.clerk :as ec]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.mathbox.plot :as plot]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [nextjournal.clerk :as clerk]))

;; ---------------------------------------------------------------------------
;; The maps, Emmy functions of numbers or symbols

(def half-pi (/ Math/PI 2))

(defn- blend
  "(1 - k) a + k b, componentwise: k = 0 the catenoid family, k = 1 the
   sheet (k is 0 or 1 exactly, so nothing of the other leaks in)."
  [k a b]
  (mapv (fn [x y] (e/+ (e/* (e/- 1 k) x) (e/* k y))) a b))

(defn- u-scale [k] (e/+ (e/* (e/- 1 k) Math/PI) k))

(def sheet-zoom "The rolled sheet is drawn this much larger (a similarity)." 1.8)

(defn- cat-probe [t s U V] ((g/probe :catenoid) (e/* t half-pi) s (e/* Math/PI U) V))
(defn- sheet-probe [t s U V] ((g/probe :sheet) t s U V))

(defn point-values
  "[x y z E F G K K-shape] at the state (k t s U V), x y z as drawn (the
   sheet scaled by sheet-zoom; its E, F, G and K in its own units)."
  [k t s U V]
  (let [c (cat-probe t s U V)
        h (sheet-probe t s U V)
        h' (vec (concat (mapv #(e/* sheet-zoom %) (take 3 h)) (drop 3 h)))]
    (blend k c h')))

(def index {:x 0 :y 1 :z 2 :E 3 :F 4 :G 5 :K 6 :Ks 7})

(defn probe
  "The kernel: point-values at the state and at the author's configuration
   (t = 0, s = 1) at the same P: 16 numbers."
  [k t s U V]
  (fn [_] (vec (concat (point-values k t s U V) (point-values k 0 1 U V)))))

(defn point-values-xyz
  "Only the drawn position (cheap: no curvature), for the surface and the
   parameter lines."
  [k t s U V]
  (let [cr ((g/catenoid (e/* t half-pi) s) (e/* Math/PI U) V)
        hr ((g/sheet t s) U V)
        [x y z] (blend k cr (mapv #(e/* sheet-zoom %) hr))]
    [x y z]))

(defn surface [k t s] (fn [[U V]] (point-values-xyz k t s U V)))
(defn u-line [k t s V] (fn [U] (point-values-xyz k t s U V)))
(defn v-line [k t s U] (fn [V] (point-values-xyz k t s U V)))

(def indicatrix-radius 0.16)

(defn- metric-at
  "[E' F' G'] in the coordinates (U, V) of the square."
  [k t s U V]
  (let [pv (point-values k t s U V)
        su (u-scale k)]
    [(e/* (nth pv 3) su su) (e/* (nth pv 4) su) (nth pv 5)]))

(defn indicatrix
  "The curve E' dU^2 + 2F' dU dV + G' dV^2 = r^2 about P in the square."
  [k t s U V]
  (fn [phi]
    (let [[E F G] (metric-at k t s U V)
          c (e/cos phi) sn (e/sin phi)
          rho (e/divide indicatrix-radius (e/sqrt (e/+ (e/* E c c) (e/* 2 F c sn) (e/* G sn sn))))]
      [(e/+ U (e/* rho c)) (e/+ V (e/* rho sn))])))

(defn indicatrix-0
  "The same at the author's configuration (t = 0, s = 1)."
  [k U V]
  (indicatrix k 0 1 U V))

;; ---------------------------------------------------------------------------
;; The page

(def init {:k 0 :t 0 :s 1 :U 0.3 :V 0.4 :play false})

(def schema
  {:t {:min 0 :max 1 :step 0.01 :label "bend t"}
   :s {:min 0.5 :max 2 :step 0.01 :label "stretch s"}
   :U {:min -1 :max 1 :step 0.01 :label "P: U"}
   :V {:min -1 :max 1 :step 0.01 :label "P: V"}
   :play {:label "bend to and fro"}})

(def colours {:surface "#5AA0D0" :uline "#C0392B" :vline "#2E8B57" :point "#111111"})

(defn- fmt
  ([x] (fmt x 4))
  ([x n] (list 'let ['v x] (list 'if '(js/isFinite v) (list '.replace (list '.toFixed 'v n) "-" "−") "∞"))))

(defn- button [attrs title on-click]
  [:button (merge {:type "button"
                   :style {:margin "0.25em" :padding "0.3em 0.6em" :cursor "pointer" :border-radius "0.3em"
                           :border "1px solid currentColor" :background "transparent" :color "inherit"}
                   :on-click on-click}
                  attrs)
   title])

(defn- case-form
  "Browser form: the case named live, from the state and the kernel output q."
  []
  '(let [k (:k st) t (:t st) s (:s st)
         stretched? (> (js/Math.abs (- s 1)) 0.005)
         shape (if (zero? k)
                 (cond (< t 0.005) "the helicoid" (> t 0.995) "the catenoid" :else "a surface between helicoid and catenoid")
                 (cond (< t 0.005) "the flat sheet" (> t 0.995) "a half cylinder" :else "a partly rolled sheet"))]
     (if stretched?
       (str "stretched (s = " (.toFixed s 2) "): not a bending; E, F, G change and so does K")
       (if (< t 0.005)
         (str shape ": Gauss' starting surface")
         (str "a bending of " (if (zero? k) "the helicoid" "the flat sheet") " into " shape ": E, F, G unchanged, so K unchanged (Theorema egregium)")))))

(defn- runner
  "Browser form: while :play is on, t = (1 - cos(2 pi phase))/2 with the
   phase advancing 1 per 8 s."
  [!s]
  (list 'let ['phase '(atom 0)
              'step (list 'fn 'step ['now]
                          (list 'when (list 'and '@t-last (list :play (list 'deref !s)))
                                '(swap! phase (fn [p] (mod (+ p (/ (- now @t-last) 8000)) 1)))
                                (list 'swap! !s 'assoc :t '(/ (- 1 (js/Math.cos (* 2 js/Math.PI @phase))) 2)))
                          '(reset! t-last now)
                          '(reset! raf (js/requestAnimationFrame step)))]
        '(reset! raf (js/requestAnimationFrame step))))

(defn playground
  "The Theorema egregium playground."
  []
  (binding [vc/*backend* :raster]
    (ev/with-let [!s init]
      (let [ksym (gensym "probe")
            kform (vc/compiled-fn probe (mapv init [:k :t :s :U :V]) [0] {:simplify? false})
            pf (fn [params f] (ev/with-params {:atom !s :params params} f))
            q-form (list ksym '(array 0) (list 'array (ev/get !s :k) (ev/get !s :t) (ev/get !s :s) (ev/get !s :U) (ev/get !s :V)))
            q (fn [k] (list 'aget 'q (index k)))
            q0 (fn [k] (list 'aget 'q (+ 8 (index k))))]
        (list 'reagent.core/with-let [ksym kform 'raf '(atom nil) 't-last '(atom nil) '_ (runner !s)]
              (list 'let ['st (list 'deref !s)
                          'q q-form
                          'dK (list '- (q :K) (q0 :K))
                          'same? '(< (js/Math.abs dK) 1e-9)
                          'case-name (case-form)]
                    [:div {:data-figure "gauss-egregium" :data-case 'case-name
                           :data-k (list 'str (q :K)) :data-k0 (list 'str (q0 :K)) :data-ks (list 'str (q :Ks))
                           :data-efg (list 'str (q :E) " " (q :F) " " (q :G))
                           :data-invariant '(str same?)}
                     [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                      [:div {:style {:flex "1 1 440px" :min-width "300px"}}
                       (plot/scene
                        {:range [[-2.2 2.2] [-2.2 2.2] [-3.6 3.6]]
                         :scale [1 1 1.64]
                         :camera [2.2 -2.6 1.4]
                         :axes [] :grids []
                         :container {:style {:height "500px" :width "100%"}}}
                        (plot/parametric-surface {:f (pf [:k :t :s] surface) :u [-1 1] :v [-1 1]
                                                  :u-samples 64 :v-samples 24
                                                  :color (colours :surface) :opacity 0.55
                                                  :grid-u 16 :grid-v 8 :grid-opacity 0.45 :simplify? false})
                        (plot/parametric-curve {:f (pf [:k :t :s :V] u-line) :t [-1 1] :samples 128
                                                :width 6 :color (colours :uline) :simplify? false})
                        (plot/parametric-curve {:f (pf [:k :t :s :U] v-line) :t [-1 1] :samples 64
                                                :width 6 :color (colours :vline) :simplify? false})
                        (plot/point {:coords [(q :x) (q :y) (q :z)] :size 16 :color (colours :point)}))
                       [:div {:style {:font-size "0.85em" :opacity 0.75}}
                        "drag: rotate · scroll: zoom · right-drag: pan. Red: the line V = const through P; green: U = const."]]
                      [:div {:style {:flex "1 1 320px" :min-width "280px" :font-size "0.93em" :line-height 1.55}}
                       (leva/sub-panel {:fill true :flat true :title-bar false}
                                       (leva/controls {:atom !s :schema schema}))
                       [:div
                        (button {:data-kind "0"} "helicoid ↔ catenoid" (list 'fn [] (list 'swap! !s 'assoc :k 0)))
                        (button {:data-kind "1"} "flat sheet ↔ cylinder" (list 'fn [] (list 'swap! !s 'assoc :k 1)))]
                       [:div {:data-case true :style {:font-weight 600 :font-size "1.08em" :margin "0.4em 0"}} 'case-name]
                       [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.9em"}}
                        [:div "at P: E = " (fmt (q :E)) ", F = " (fmt (q :F)) ", G = " (fmt (q :G))]
                        [:div {:style {:opacity 0.7}} "unbent: E₀ = " (fmt (q0 :E)) ", F₀ = " (fmt (q0 :F)) ", G₀ = " (fmt (q0 :G))]
                        [:div "EG − F² = " (fmt (list '- (list '* (q :E) (q :G)) (list '* (q :F) (q :F))))]
                        [:div "K from E, F, G alone (Brioschi) = " [:span {:data-kb true} (fmt (q :K) 6)]]
                        [:div "K from the embedding (LN − M²)/(EG − F²)² = " (fmt (q :Ks) 6)]
                        [:div "K of the unbent surface at P = " (fmt (q0 :K) 6)]
                        (list 'when '(zero? (:k st))
                              [:div {:style {:opacity 0.8}} "Emmy: K = −s²/(s² + sinh² v)², at v = " (fmt (ev/get !s :V) 2)
                               " and s = " (fmt (ev/get !s :s) 2)])]
                       [:div {:data-hypothesis "egregium"
                              :style {:margin "0.4em 0" :padding "0.3em 0.6em" :border-radius "0.3em" :color "white"
                                      :background '(if same? "#2E8B57" "#C0392B")}}
                        '(if same?
                           "K = K₀: the measure of curvature survived the bending"
                           (str "K − K₀ = " (.toFixed dK 4) ": the theorem needs a bending (lengths kept), and the stretch broke it"))]]]
                     [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start" :margin-top "0.6em"}}
                      [:div {:style {:flex "1 1 340px" :min-width "280px"}}
                       (mafs/mafs {:height 340 :view-box {:x [-1.15 1.15] :y [-1.15 1.15]}}
                                  (mafs/cartesian {:x-axis {:lines 0.25} :y-axis {:lines 0.25}})
                                  (mafs/polygon {:points [[-1 -1] [1 -1] [1 1] [-1 1]] :color "#5AA0D0" :fill-opacity 0.05})
                                  (mafs/segment {:point1 [-1 (ev/get !s :V)] :point2 [1 (ev/get !s :V)] :color (colours :uline)})
                                  (mafs/segment {:point1 [(ev/get !s :U) -1] :point2 [(ev/get !s :U) 1] :color (colours :vline)})
                                  (mafs/parametric {:xy (pf [:k :U :V] indicatrix-0) :t [0 (* 2 Math/PI)] :color "#8A9BB0" :style "dashed"})
                                  (mafs/parametric {:xy (pf [:k :t :s :U :V] indicatrix) :t [0 (* 2 Math/PI)] :color "#7B5EA7"})
                                  (mafs/movable-point {:point [(ev/get !s :U) (ev/get !s :V)] :color "#111111"
                                                       :constrain '(fn [[x y]] [(max -1 (min 1 x)) (max -1 (min 1 y))])
                                                       :on-move (list 'fn '[[x y]] (list 'swap! !s 'assoc :U 'x :V 'y))})
                                  (mafs/text "P" {:x (ev/get !s :U) :y (ev/get !s :V) :attach "ne"}))]
                      [:div {:style {:flex "1 1 300px" :min-width "260px" :font-size "0.9em"}}
                       [:p "The parameter square (U, V), as Gauss' p, q: drag P here. The violet curve is the first fundamental form at P, the points at one small distance from P measured on the surface, E dU² + 2F dU dV + G dV² = r²; dashed, the same for the unbent surface. Bend: the two coincide. Stretch: they part, and K moves."]
                       [:p {:style {:margin "0.6em 0 0.2em"}} [:strong "Try this:"]]
                       (button {:data-preset "catenoid"} "Bend the helicoid all the way into the catenoid"
                               (list 'fn [] (list 'reset! !s (assoc init :t 1))))
                       (button {:data-preset "play"} "Let it bend to and fro, watch K"
                               (list 'fn [] (list 'reset! !s (assoc init :t 0.5 :play true))))
                       (button {:data-preset "stretch"} "Stretch the catenoid along its axis (s = 1.6)"
                               (list 'fn [] (list 'reset! !s (assoc init :t 1 :s 1.6))))
                       (button {:data-preset "cylinder"} "Roll the flat sheet into a half cylinder"
                               (list 'fn [] (list 'reset! !s (assoc init :k 1 :t 1))))
                       (button {:data-preset "lift"} "Lift the sheet along V (s = 1.6): no longer developable"
                               (list 'fn [] (list 'reset! !s (assoc init :k 1 :t 0.5 :s 1.6))))
                       (button {:data-preset "reset"} "Reset: the helicoid, unbent"
                               (list 'fn [] (list 'reset! !s init)))]]])
              '(finally (some-> @raf js/cancelAnimationFrame)))))))

;; ---------------------------------------------------------------------------
;; The derivation, for the page

(defn derivation
  "Emmy's E, F, G and K of the catenoid family (t the bend, s the stretch)."
  []
  (let [[E F G] (g/catenoid-metric)
        K (:K (g/symbolic :catenoid))]
    (clerk/col
     (clerk/html [:div {:style {:font-size "0.85em" :opacity 0.7}} "E, F, G of the family, by Emmy's derivatives of r(u, v):"])
     (ec/->TeX E :simplify? false) (ec/->TeX F :simplify? false) (ec/->TeX G :simplify? false)
     (clerk/html [:div {:style {:font-size "0.85em" :opacity 0.7}} "K from E, F, G alone (Brioschi), simplified; t has dropped out:"])
     (ec/->TeX K :simplify? false))))
