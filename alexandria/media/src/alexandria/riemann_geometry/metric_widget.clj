(ns alexandria.riemann-geometry.metric-widget
  "The line-element playground of the Riemann notebook (Ueber die
   Hypothesen, 1854, II §1-§4), an emmy-viewers fragment on the raster
   backend.

   The reader edits the quadratic form A = [g11 g12; g12 g22] (tied to its
   determinant d = g11 g22 - g12^2 through alexandria.medium.tie: move d and
   g22 is re-solved), Riemann's curvature alpha of the normal form and a
   hill h, and drags the point P. Over the coordinate square the page draws

   - a colour raster of the curvature K, one raster call of
     alexandria.riemann-geometry.metric/probe per cell (Brioschi, from the
     metric alone): blue K > 0, red K < 0, white K = 0;
   - the indicatrix field: at a 5 x 5 grid of points the curve
     E dx^2 + 2F dx dy + G dy^2 = r^2 (the points one small metric step
     away), from the kernel's E, F, G;
   - P with its indicatrix and readouts E, F, G, det g, K, the conformal
     factor and K - alpha, every number from the probe kernel.

   The case is named live: flat, positive or negative curvature, the
   constant-curvature check of the normal form (green while h = 0, red when
   the hill breaks it), degenerate (det g <= 0: no line element), and the
   edge of the hyperbolic disc (1 + alpha/4 Q <= 0)."
  (:require [alexandria.medium.tie :as tie]
            [alexandria.riemann-geometry.metric :as m]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]))

(def init-g {:g11 1 :g12 0 :g22 1 :d 1})
(def init-q {:alpha 0 :h 0 :x 0.4 :y 0.3})

(def labels {:g11 "g₁₁" :g12 "g₁₂" :g22 "g₂₂" :d "det A = g₁₁g₂₂ − g₁₂²"})

(def q-schema
  {:alpha {:min -2 :max 2 :step 0.01 :label "Riemann's α"}
   :h {:min -1 :max 2 :step 0.01 :label "hill h"}
   :x {:min -1.6 :max 1.6 :step 0.01 :label "P: x₁"}
   :y {:min -1.6 :max 1.6 :step 0.01 :label "P: x₂"}})

(def half 1.6)
(def cells 12)
(def grid 5)

(def presets
  [["Euclid's plane: ds² = dx₁² + dx₂²" {:g11 1 :g12 0 :g22 1 :d 1} {:alpha 0 :h 0}]
   ["the sphere's normal form, α = 1" {:g11 1 :g12 0 :g22 1 :d 1} {:alpha 1 :h 0}]
   ["α = −1: curvature negative, a disc with an edge" {:g11 1 :g12 0 :g22 1 :d 1} {:alpha -1 :h 0}]
   ["skew coordinates A = [2 ½; ½ 1]: K is still α" {:g11 2 :g12 0.5 :g22 1 :d 1.75} {:alpha 0.8 :h 0}]
   ["raise a hill h = 1.5: K varies from point to point" {:g11 1 :g12 0 :g22 1 :d 1} {:alpha 0 :h 1.5}]
   ["make det g ≤ 0: no line element" {:g11 1 :g12 1.2 :g22 1 :d -0.44} {:alpha 0 :h 0}]])

(defn- katex [tex-form attr]
  (list 'let ['tex tex-form] [:div {attr 'tex} [:f> 'nextjournal.clerk.render/render-katex 'tex {:inline? false}]]))

(defn- button [attrs title on-click]
  [:button (merge {:type "button" :on-click on-click
                   :style {:margin "0.2em" :padding "0.25em 0.55em" :cursor "pointer" :border-radius "0.3em"
                           :border "1px solid currentColor" :background "transparent" :color "inherit"}}
                  attrs)
   title])

(defn playground
  "The line-element playground."
  []
  (binding [vc/*backend* :raster]
    (ev/with-let [!p init-g !s init-g !q init-q]
      (let [[tsym tform] (tie/kernel-binding "tie" m/tie-blocks (mapv init-g m/tied))
            [psym pform] (tie/kernel-binding "probe" m/probe [1 0 1 0 0 0.4 0.3])
            opts {:tie-sym tsym :p !p :s !s :keys m/tied :ranges m/ranges :counter "__riemannSolves"}
            n (fn [form d] (tie/fmt form d))
            ix (fn [k] (m/probe-index k))
            step (/ (* 2 half) cells)
            centres (vec (for [i (range grid) j (range grid)]
                           [(+ (- half) (* (+ i 0.5) (/ (* 2 half) grid))) (+ (- half) (* (+ j 0.5) (/ (* 2 half) grid)))]))
            cell-lls (vec (for [i (range cells) j (range cells)]
                            [(+ (- half) (* i step)) (+ (- half) (* j step))]))]
        (list 'reagent.core/with-let (into [tsym tform psym pform] (tie/locals opts))
              (list 'let ['g (list 'deref !s) 'q (list 'deref !q) 'step step
                          'args (list 'fn '[x y] '(array (:g11 g) (:g12 g) (:g22 g) (:alpha q) (:h q) x y))
                          'pr (list 'fn '[x y] (list psym '(array 0) '(args x y)))
                          'r '(pr (:x q) (:y q))
                          'E (list 'aget 'r (ix :E)) 'F (list 'aget 'r (ix :F)) 'G (list 'aget 'r (ix :G))
                          'detg (list 'aget 'r (ix :det)) 'K (list 'aget 'r (ix :K))
                          'lam (list 'aget 'r (ix :lambda)) 'Q (list 'aget 'r (ix :Q)) 'dK (list 'aget 'r (ix :K-alpha))
                          'edge? '(<= (+ 1 (* (/ (:alpha q) 4) Q)) 0)
                          'degenerate? '(<= (:d g) 0)
                          'case-name '(cond degenerate? "degenerate: det g ≤ 0, ds² is not positive, so no line element"
                                            edge? "beyond the edge 1 + α/4·Q = 0: the metric is not defined here"
                                            (not (js/isFinite K)) "K is not finite here"
                                            (< (js/Math.abs K) 1e-6) "flat at P: K = 0, Euclid's geometry in the small"
                                            (> K 0) "positive curvature at P: geometry of the sphere in the small"
                                            :else "negative curvature at P: geometry of the saddle in the small")
                          'constant? '(and (not degenerate?) (< (js/Math.abs (:h q)) 0.005) (< (js/Math.abs dK) 1e-6))
                          'colour '(fn [k] (cond (not (js/isFinite k)) "#999999" (> k 0) "#1F6FB4" :else "#C0392B"))
                          'alpha-of '(fn [k] (if (js/isFinite k) (min 0.85 (* 0.5 (js/Math.abs k))) 0.5))
                          'ellipse '(fn [x0 y0 rr]
                                      (let [o (pr x0 y0) e (aget o 0) f (aget o 1) gg (aget o 2)]
                                        (vec (for [i (range 49)]
                                               (let [t (* 2 js/Math.PI (/ i 48)) c (js/Math.cos t) s (js/Math.sin t)
                                                     qq (js/Math.abs (+ (* e c c) (* 2 f c s) (* gg s s)))
                                                     rho (/ rr (js/Math.sqrt (+ qq 1e-6)))
                                                     rho (min rho 0.6)]
                                                 [(+ x0 (* rho c)) (+ y0 (* rho s))])))))]
                    [:div {:data-figure "riemann-metric" :data-case 'case-name :data-k '(str K) :data-det '(str detg)
                           :data-constant '(str constant?)}
                     [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                      [:div {:style {:flex "1 1 420px" :min-width "300px"}}
                       (mafs/mafs {:height 440 :view-box {:x [(- half) half] :y [(- half) half]}}
                                  (list 'into [:<>]
                                        (list 'for ['[x y] cell-lls]
                                              (list 'let ['k (list 'aget '(pr (+ x (/ step 2)) (+ y (/ step 2))) (ix :K))]
                                                    ['mafs.core/Polygon {:points '[[x y] [(+ x step) y] [(+ x step) (+ y step)] [x (+ y step)]]
                                                                         :color '(colour k) :weight 0
                                                                         :fill-opacity '(alpha-of k)}])))
                                  (mafs/cartesian {:x-axis {:lines 0.4} :y-axis {:lines 0.4}})
                                  (list 'into [:<>]
                                        (list 'for ['[x y] centres]
                                              ['mafs.core/Polyline {:points '(ellipse x y 0.13) :color "#555555" :weight 1.5}]))
                                  ['mafs.core/Polyline {:points '(ellipse (:x q) (:y q) 0.22) :color "#7B5EA7" :weight 3}]
                                  (mafs/text "P" {:x (ev/get !q :x) :y (ev/get !q :y) :attach "ne"})
                                  (mafs/movable-point {:point [(ev/get !q :x) (ev/get !q :y)] :color "#111111"
                                                       :constrain (list 'fn '[[x y]] [(list 'max (- half) (list 'min half 'x))
                                                                                      (list 'max (- half) (list 'min half 'y))])
                                                       :on-move (list 'fn '[[x y]] (list 'swap! !q 'assoc :x 'x :y 'y))}))
                       [:div {:style {:font-size "0.85em" :opacity 0.75}}
                        "Coordinates x₁, x₂. Colour: K at each cell (blue > 0, red < 0, white 0), from the metric alone. Grey curves: the points one small step ds away. Violet: the same at P; drag P."]]
                      [:div {:style {:flex "1 1 320px" :min-width "280px" :font-size "0.93em" :line-height 1.5}}
                       (leva/sub-panel {:fill true :flat true :title-bar false}
                                       (leva/controls {:atom !p :schema (tie/schema m/tied m/ranges labels)})
                                       (leva/controls {:atom !q :schema q-schema}))
                       [:div {:data-case true :style {:font-weight 600 :font-size "1.08em" :margin "0.4em 0"}} 'case-name]
                       (katex (list 'str "ds^2 = " (n 'E 3) "\\,dx_1^2 + 2\\cdot" (n 'F 3) "\\,dx_1dx_2 + " (n 'G 3) "\\,dx_2^2") :data-ds2)
                       (katex (list 'str "\\lambda = \\frac{1 + h\\,e^{-Q}}{(1 + \\tfrac{\\alpha}{4}Q)^2} = \\frac{1 + " (n '(:h q) 2) "\\,e^{-" (n 'Q 3) "}}{(1 + " (n '(/ (:alpha q) 4) 3) "\\cdot" (n 'Q 3) ")^2} = " (n 'lam 4)) :data-lambda)
                       [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.9em"}}
                        [:div "det A = g₁₁g₂₂ − g₁₂² = " [:span {:data-d true} (n '(:d g) 3)]]
                        [:div "det g = EG − F² = " (n 'detg 4)]
                        [:div "K at P (Gauss–Brioschi, from E, F, G alone) = " [:span {:data-kp true} (n 'K 5)]]
                        [:div "K − α = " (list '.toExponential 'dK 2)]]
                       [:div {:data-hypothesis "constant-curvature"
                              :style {:margin "0.4em 0" :padding "0.3em 0.6em" :border-radius "0.3em" :color "white"
                                      :background '(if constant? "#2E8B57" "#C0392B")}}
                        '(if constant?
                           "h = 0: Riemann's normal form, K = α at every point"
                           (if degenerate?
                             "det g ≤ 0: Riemann's hypothesis of a positive quadratic form is broken"
                             "h ≠ 0: the hill breaks Riemann's normal form, K varies"))]]]
                     [:div {:style {:margin-top "0.4em"}}
                      [:strong "Try this: "]
                      (into [:<>]
                            (for [[label gv qv] presets]
                              (button {:data-preset label} label
                                      (list 'fn [] (list 'reset! 'pushing true) (list 'reset! !p gv) (list 'reset! !s gv)
                                            (list 'reset! 'pushing false) (list 'swap! !q 'merge qv)))))
                      (button {:data-preset "reset"} "Reset: Euclid's plane"
                              (list 'fn [] (list 'reset! 'pushing true) (list 'reset! !p init-g) (list 'reset! !s init-g)
                                    (list 'reset! 'pushing false) (list 'reset! !q init-q)))]])
              (list 'finally (tie/unwatch-form !p)))))))
