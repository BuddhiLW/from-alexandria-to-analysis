(ns alexandria.riemann-geometry.geodesic-widget
  "The geodesic playground of the Riemann notebook (Ueber die Hypothesen,
   1854, II §2-§3): shortest lines on a curved surface, drawn in MathBox on
   the raster backend (orbitable).

   Four surfaces from alexandria.riemann-geometry.geodesic (sphere, saddle,
   torus, plane). The reader drags the start point P and the heading handle
   in the parameter chart (Mafs) or moves the Leva controls: the heading
   theta (measured by the metric from the u line), the opening 2 delta
   between two geodesics leaving P, and their length L. Each geodesic is
   the geodesic equation x''^k = -Gamma^k_ij x'^i x'^j, the Christoffel
   symbols derived by Emmy from the metric alone, integrated in the page by
   RK4 over a raster wasm kernel of the flow (alexandria.medium.flow).

   Readouts: K at P (Brioschi), E F G at P, the distance between the two
   ends in space against 2 L sin delta (the plane's value), and their
   ratio; the case is named live: converge (K > 0 on the way), diverge
   (K < 0), stay parallel (flat). The speed g(x', x') at the end of the
   path is printed, the check that RK4 kept it 1."
  (:require [alexandria.medium.flow :as flow]
            [alexandria.riemann-geometry.geodesic :as g]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.mathbox.plot :as plot]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]))

(def n-steps 160)
(def samples (inc n-steps))

(def init {:k 0 :u 0 :v 0 :theta 0.4 :delta 0.25 :L 2.6})

(def presets
  [["sphere: two great circles from P meet again at length π" {:k 0 :u 0 :v 0 :theta 0.4 :delta 0.25 :L 3.1416}]
   ["saddle: the same opening, the geodesics fly apart" {:k 1 :u 0 :v 0 :theta 0.4 :delta 0.25 :L 1.5}]
   ["torus, outside (K > 0): they converge" {:k 2 :u 0 :v 0 :theta 1.2 :delta 0.25 :L 1.2}]
   ["torus, inside (K < 0): they diverge" {:k 2 :u 0 :v 3.1 :theta 1.2 :delta 0.25 :L 1.2}]
   ["plane: Euclid's straight lines, the gap grows as 2L sin δ" {:k 3 :u 0 :v 0 :theta 0.4 :delta 0.25 :L 1.5}]])

(def schema
  {:theta {:min -3.14 :max 3.14 :step 0.01 :label "heading θ"}
   :delta {:min 0.02 :max 0.8 :step 0.01 :label "half opening δ"}
   :L {:min 0.2 :max 6.3 :step 0.01 :label "length L"}})

(defn- chart-of [kind]
  (let [[[u0 u1] [v0 v1]] (g/domains kind)] {:x [u0 u1] :y [v0 v1]}))

(defn- dynamic-kernel
  "[syms forms]: f (a fn of kind returning a kernel) compiled for each kind,
   indexed by k; called in the page as (sym state params)."
  ([label f init-params] (dynamic-kernel label f init-params [0]))
  ([label f init-params init-state]
   (let [pairs (mapv (fn [kind] (flow/kernel (str label (name kind)) (f kind) init-params init-state)) g/kinds)]
     [(mapv first pairs) (mapv second pairs)])))

(defn- button [attrs title on-click]
  [:button (merge {:type "button" :on-click on-click
                   :style {:margin "0.2em" :padding "0.25em 0.55em" :cursor "pointer" :border-radius "0.3em"
                           :border "1px solid currentColor" :background "transparent" :color "inherit"}}
                  attrs)
   title])

(defn- line-array [src colour width]
  [:<>
   ['mathbox.primitives/Array {:width samples :channels 3 :items 1 :live true
                                :expr (list 'fn '[emit i _t]
                                            (list 'let ['p (list 'nth (list 'deref src) 'i '[js/NaN js/NaN js/NaN])]
                                                  '(emit (nth p 0) (nth p 1) (nth p 2))))}]
   ['mathbox.primitives/Line {:width width :color colour :zBias 6}]])

(defn playground
  "The geodesic playground."
  []
  (binding [vc/*backend* :raster]
    (ev/with-let [!s init]
      (let [[fsyms fforms] (dynamic-kernel "flow" (fn [kind] (fn [_] (fn [[u v p q]] ((g/flow kind) [u v p q])))) [0] [0 0 1 0])
            [psyms pforms] (dynamic-kernel "probe" g/probe [0 0 0.4])
            [xsyms xforms] (dynamic-kernel "xyz" (fn [kind] (fn [u v] (fn [_] (g/drawn-xyz kind u v)))) [0 0])
            charts (mapv chart-of g/kinds)
            pf (fn [params f] (ev/with-params {:atom !s :params params} f))]
        (list 'reagent.core/with-let
              (vec (concat (interleave fsyms fforms) (interleave psyms pforms) (interleave xsyms xforms)
                           ['path-a '(atom []) 'path-b '(atom [])
                            'rk4 (list 'fn '[fk s h]
                                       '(let [d (fn [x] (fk x (array 0)))
                                              ad (fn [x k c] (let [o (array 0 0 0 0)] (dotimes [i 4] (aset o i (+ (aget x i) (* c (aget k i))))) o))
                                              k1 (d s) k2 (d (ad s k1 (/ h 2))) k3 (d (ad s k2 (/ h 2))) k4 (d (ad s k3 h))
                                              o (array 0 0 0 0)]
                                          (dotimes [i 4]
                                            (aset o i (+ (aget s i) (* (/ h 6) (+ (aget k1 i) (* 2 (aget k2 i)) (* 2 (aget k3 i)) (aget k4 i))))))
                                          o))]))
              (list 'let ['st (list 'deref !s)
                          'k '(int (:k st)) 'u '(:u st) 'v '(:v st) 'L '(:L st) 'delta '(:delta st) 'theta '(:theta st)
                          'fk (list 'nth fsyms 'k) 'pk (list 'nth psyms 'k) 'xk (list 'nth xsyms 'k)
                          'probe '(fn [th] (pk (array 0) (array u v th)))
                          'r '(probe theta)
                          'K '(aget r 3)
                          'h (list '/ 'L n-steps)
                          'run (list 'fn '[th]
                                     (list 'let '[pr (probe th)]
                                           (list 'loop ['i 0 's '(array u v (aget pr 7) (aget pr 8)) 'acc '(transient [])]
                                                 (list 'if (list '> 'i n-steps)
                                                       '[(persistent! acc) s]
                                                       '(recur (inc i) (rk4 fk s h) (conj! acc s))))))
                          '[sa enda] '(run (+ theta delta))
                          '[sb endb] '(run (- theta delta))
                          'xyz '(fn [s] (vec (xk (array 0) (array (aget s 0) (aget s 1)))))
                          '_ '(do (reset! path-a (mapv xyz sa)) (reset! path-b (mapv xyz sb)))
                          'pa '(xyz enda) 'pb '(xyz endb)
                          'zoom (list 'nth (mapv g/zoom g/kinds) 'k)
                          'gap '(/ (js/Math.hypot (- (pa 0) (pb 0)) (- (pa 1) (pb 1)) (- (pa 2) (pb 2))) zoom)
                          'flat '(* 2 L (js/Math.sin delta))
                          'ratio '(/ gap flat)
                          'pe '(probe 0)
                          'speed '(let [o (pk (array 0) (array (aget enda 0) (aget enda 1) 0))
                                        e (aget o 4) f (aget o 5) gg (aget o 6) p (aget enda 2) q (aget enda 3)]
                                    (+ (* e p p) (* 2 f p q) (* gg q q)))
                          'case-name '(cond (not (js/isFinite ratio)) "the path left the chart"
                                            (< ratio 0.98) (str "converge: the ends are closer than in the plane (×" (.toFixed ratio 3) "), positive curvature on the way")
                                            (> ratio 1.02) (str "diverge: the ends are farther apart than in the plane (×" (.toFixed ratio 3) "), negative curvature on the way")
                                            :else "parallel: the gap is the plane's 2L sin δ, no curvature felt")
                          'chart (list 'nth charts 'k)
                          'handle '[(+ u (* 0.5 (js/Math.cos theta))) (+ v (* 0.5 (js/Math.sin theta)))]]
                    [:div {:data-figure "riemann-geodesics" :data-case 'case-name :data-ratio '(str ratio)
                           :data-k '(str K) :data-speed '(str speed) :data-kind '(str k)}
                     [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                      [:div {:style {:flex "1 1 460px" :min-width "300px"}}
                       (plot/scene
                        {:range [[-2.2 2.2] [-2.2 2.2] [-2.2 2.2]]
                         :scale [1 1 1]
                         :camera [2.4 -2.8 1.8]
                         :axes [] :grids []
                         :container {:style {:height "480px" :width "100%"}}}
                        (plot/parametric-surface {:f (pf [:k] (fn [kk] (fn [[U V]] (g/surface-of-k kk U V))))
                                                  :u [-1 1] :v [-1 1] :u-samples 56 :v-samples 40
                                                  :color "#5AA0D0" :opacity 0.5 :grid-u 16 :grid-v 12 :grid-opacity 0.4
                                                  :simplify? false})
                        (line-array 'path-a "#C0392B" 7)
                        (line-array 'path-b "#2E8B57" 7)
                        (plot/point {:coords '(xyz (array u v)) :size 16 :color "#111111"}))
                       [:div {:style {:font-size "0.85em" :opacity 0.75}}
                        "drag: rotate · scroll: zoom · right-drag: pan. Red and green: two geodesics from P at headings θ ± δ."]]
                      [:div {:style {:flex "1 1 320px" :min-width "280px" :font-size "0.93em" :line-height 1.5}}
                       (into [:div]
                             (for [[i label] (map-indexed vector ["sphere" "saddle" "torus" "plane"])]
                               (button {:data-kind (str i)} label
                                       (list 'fn [] (list 'swap! !s 'assoc :k i :u 0 :v 0)))))
                       (leva/sub-panel {:fill true :flat true :title-bar false}
                                       (leva/controls {:atom !s :schema schema}))
                       [:div {:data-case true :style {:font-weight 600 :font-size "1.05em" :margin "0.4em 0"}} 'case-name]
                       [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.9em"}}
                        [:div "at P: E = " (flow/fmt '(aget r 4) 4) ", F = " (flow/fmt '(aget r 5) 4) ", G = " (flow/fmt '(aget r 6) 4)]
                        [:div "K at P (from E, F, G alone) = " [:span {:data-kp true} (flow/fmt 'K 4)]]
                        [:div "gap between the ends = " [:span {:data-gap true} (flow/fmt 'gap 4)]]
                        [:div "in the plane, 2L sin δ = " (flow/fmt 'flat 4)]
                        [:div "ratio = " (flow/fmt 'ratio 4)]
                        [:div "speed g(ẋ, ẋ) at the end (RK4, should be 1) = " [:span {:data-end-speed true} (flow/fmt 'speed 8)]]]
                       (mafs/mafs {:height 260 :view-box 'chart :preserve-aspect-ratio false}
                                  (mafs/cartesian)
                                  ['mafs.core/Polyline {:points '(mapv (fn [s] [(aget s 0) (aget s 1)]) sa) :color "#C0392B" :weight 2.5}]
                                  ['mafs.core/Polyline {:points '(mapv (fn [s] [(aget s 0) (aget s 1)]) sb) :color "#2E8B57" :weight 2.5}]
                                  (mafs/segment {:point1 '[u v] :point2 'handle :color "#7B5EA7"})
                                  (mafs/movable-point {:point '[u v] :color "#111111"
                                                       :on-move (list 'fn '[[x y]] (list 'swap! !s 'assoc :u 'x :v 'y))})
                                  (mafs/movable-point {:point 'handle :color "#7B5EA7"
                                                       :on-move (list 'fn '[[x y]] (list 'swap! !s 'assoc :theta '(js/Math.atan2 (- y v) (- x u))))})
                                  (mafs/text "P" {:x 'u :y 'v :attach "sw"}))
                       [:div {:style {:font-size "0.85em" :opacity 0.75}}
                        "The parameter chart (u, v): drag P, and the violet handle to aim (the heading the page uses is θ measured by the metric)."]]]
                     [:div {:style {:margin-top "0.4em"}}
                      [:strong "Try this: "]
                      (into [:<>]
                            (for [[label st] presets]
                              (button {:data-preset label} label (list 'fn [] (list 'reset! !s st)))))
                      (button {:data-preset "reset"} "Reset" (list 'fn [] (list 'reset! !s init)))]]))))))

(defn derivation
  "Emmy's metric, Christoffel symbols and K of the torus, for the page."
  []
  (let [{:keys [metric gamma K]} (g/symbolic :torus)
        tex (fn [x] ((requiring-resolve 'emmy.clerk/->TeX) x :simplify? false))
        html (requiring-resolve 'nextjournal.clerk/html)
        col (requiring-resolve 'nextjournal.clerk/col)
        note (fn [s] (html [:div {:style {:font-size "0.85em" :opacity 0.7}} s]))]
    (col
     (note "The torus r(u, v) = ((2 + ⁴⁄₅ cos v) cos u, (2 + ⁴⁄₅ cos v) sin u, ⁴⁄₅ sin v): E, F, G by Emmy's derivatives,")
     (tex (nth metric 0)) (tex (nth metric 1)) (tex (nth metric 2))
     (note "the nonzero Christoffel symbols Γᵘᵤᵥ = Γᵘᵥᵤ and Γᵛᵤᵤ, from E, F, G alone,")
     (tex (gamma [0 0 1])) (tex (gamma [1 0 0]))
     (note "and K by Gauss–Brioschi: positive outside (cos v > 0), negative in the hole.")
     (tex K))))
