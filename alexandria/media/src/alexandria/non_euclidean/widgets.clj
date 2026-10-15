(ns alexandria.non-euclidean.widgets
  "The widgets of the non-euclidean notebook (Lobachevsky, Bolyai,
   Beltrami), emmy-viewers fragments on the raster backend.

   `parallel-tie`  Lobachevsky's angle of parallelism: x, Pi(x) and
                   t = tan(Pi/2) = e^-x are three tied Leva controls
                   (alexandria.non-euclidean.parallels/tie, the guards of
                   alexandria.medium.tie). His figure is drawn in Poincare's
                   disk: the line BC a diameter, A at distance x above D,
                   the two limiting parallels, Euclid's single parallel AE
                   dashed. The Euclidean limit is named live.
   `triangle-playground`  three draggable vertices in the Poincare disk (or
                   Beltrami-Klein, one toggle); sides as geodesics, the
                   angles, their sum S and the area pi - S (Gauss-Bonnet,
                   K = -1) from one raster kernel; the case named live.
   `parallels-playground` a draggable point P above a line; a line through P
                   at a chosen angle (meets / limiting / does not meet,
                   named live), the two limiting parallels highlighted and a
                   fan of the infinitely many non-meeting lines between them.
   `pseudosphere`  Beltrami's surface of constant negative curvature in
                   MathBox (orbitable), K from E, F, G by the Gauss shelf,
                   beside the sphere (K = +1/R^2), one toggle.

   Curves: mafs/parametric compiles through emmy-viewers' param-1d, which
   always runs Emmy's simplifier, and on the Moebius expressions of the disk
   that exhausts the heap. So every curve family here is one UNSIMPLIFIED
   raster kernel (params..., s) -> [x y] (`curve-kernel`), sampled in the
   browser by `pts!` into a mafs/polyline.

   Every number on the page comes from a raster kernel of
   alexandria.non-euclidean.*; the browser only formats and samples."
  (:require [alexandria.medium.flow :as flow]
            [alexandria.medium.tie :as tie]
            [alexandria.non-euclidean.disk :as d]
            [alexandria.non-euclidean.parallels :as p]
            [alexandria.non-euclidean.pseudosphere :as ps]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.mathbox.plot :as plot]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [nextjournal.clerk :as clerk]))

(def colours {:line "#1F6FB4" :limit "#C0392B" :euclid "#8A9BB0" :fan "#E39B2D" :perp "#2E8B57"
              :tri "#7B5EA7" :point "#111111" :ok "#2E8B57" :bad "#C0392B"})

(defn- katex [tex-form attr]
  (list 'let ['tex tex-form] [:div {attr 'tex} [:f> 'nextjournal.clerk.render/render-katex 'tex {:inline? false}]]))

(defn- button [attrs title on-click]
  [:button (merge {:type "button" :on-click on-click
                   :style {:margin "0.2em" :padding "0.3em 0.6em" :cursor "pointer" :border-radius "0.3em"
                           :border "1px solid currentColor" :background "transparent" :color "inherit"}}
                  attrs)
   title])

(def boundary
  "The ideal boundary: the unit circle."
  (mafs/circle {:center [0 0] :radius 1 :color "#555555" :fill-opacity 0.03}))

;; ---------------------------------------------------------------------------
;; Curves as sampled raster kernels

(defn curve-kernel
  "[sym form]: the raster kernel of ((curve params...) s), s the last
   parameter, unsimplified."
  [label curve init]
  (flow/kernel label (fn [& args] (fn [_] ((apply curve (butlast args)) (last args))))
               (conj (vec init) 0) [0]))

(def pts-form
  "Quoted (fn [k ps s0 s1 n] points): kernel k sampled at n + 1 values of s
   in [s0 s1], parameters ps."
  '(fn [k ps s0 s1 n]
     (vec (for [i (range (inc n))]
            (let [s (+ s0 (* (- s1 s0) (/ i n)))
                  o (k (array 0) (into-array (conj (vec ps) s)))]
              [(aget o 0) (aget o 1)])))))

(defn curve
  "A polyline of kernel symbol k at params ps (browser forms), s in [s0 s1]."
  [k ps s0 s1 n opts]
  (mafs/polyline (assoc opts :points (list 'pts! k (vec ps) s0 s1 n))))

;; ---------------------------------------------------------------------------
;; 1. The angle of parallelism, tied

(defn tie-init
  "The starting tied state at x = 0.8, by raster on the JVM."
  []
  (let [out (raster/value (fn [_] ((p/tie 0.8 45 0.5) nil)) 0)]
    (zipmap p/tied (subvec (vec out) 0 3))))

(defn parallel-tie
  "Lobachevsky's Pi(x), x and tan(Pi/2) tied, his figure in the disk."
  ([] (parallel-tie (tie-init)))
  ([start]
   (binding [vc/*backend* :raster]
     (ev/with-let [!p start !s start]
       (let [s (mapv start p/tied)
             [tsym tform] (tie/kernel-binding "tie" p/tie s)
             [rsym rform] (tie/kernel-binding "readout" p/readout [(first s)])
             [lsym lform] (curve-kernel "limiting" d/limiting [0 0 0.3 1])
             [wsym wform] (curve-kernel "line" d/line-through [0 0 0.3 1.0])
             opts {:tie-sym tsym :p !p :s !s :keys p/tied :ranges p/ranges :counter "__lobachevskySolves"}
             n (fn [k dd] (tie/fmt (list 'aget 'r (p/readout-index k)) dd))]
         (flow/template
          {'?r rsym '?s !s}
          (list 'reagent.core/with-let (into [tsym tform rsym rform lsym lform wsym wform 'pts! pts-form] (tie/locals opts))
                (list 'let '[r (?r (array 0) (array (get @?s :x)))
                             x (aget r 0) P (aget r 1)
                             kind (cond (< x 0.02) "euclid" (< P 10) "far" :else "between")]
                      [:div {:data-figure "lobachevsky-tie" :data-case 'kind :data-pi '(str P) :data-x '(str x)
                             :data-t '(str (aget r 2))}
                       [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                        [:div {:style {:flex "1 1 380px" :min-width "300px"}}
                         (mafs/mafs {:view-box {:x [-1.1 1.1] :y [-1.1 1.1]} :height 400}
                                    boundary
                                    (mafs/segment {:point1 [-1 0] :point2 [1 0] :color (colours :line) :weight 3})
                                    (curve wsym [0 0 '(aget r 5) (/ Math/PI 2)] -1 1 48 {:color (colours :euclid) :style "dashed"})
                                    (curve lsym [0 0 '(aget r 5) 1] -1 1 64 {:color (colours :limit) :weight 3})
                                    (curve lsym [0 0 '(aget r 5) -1] -1 1 64 {:color (colours :limit) :weight 3})
                                    (mafs/segment {:point1 [0 0] :point2 [0 '(aget r 5)] :color (colours :perp)})
                                    (mafs/point {:x 0 :y '(aget r 5) :color (colours :point)})
                                    (mafs/text "A" {:x 0 :y '(aget r 5) :attach "n"})
                                    (mafs/text "D" {:x 0 :y 0 :attach "s"})
                                    (mafs/text "C" {:x 0.92 :y 0 :attach "s"})
                                    (mafs/text "B" {:x -0.92 :y 0 :attach "s"}))
                         [:div {:style {:font-size "0.85em" :opacity 0.75}}
                          "Poincaré's disk. Blue: the line BC; green: the perpendicular AD = x; red: the two parallels through A, which meet BC only at the rim (at infinity); dashed: AE, at right angles to AD, Euclid's one parallel."]]
                        [:div {:style {:flex "0 1 330px" :min-width "260px"}}
                         (leva/sub-panel {:fill true :flat true :title-bar false}
                                         (leva/controls {:atom !p :schema (tie/schema p/tied p/ranges p/labels)}))
                         (katex (list 'str "\\tan\\tfrac{\\Pi(x)}{2} = \\tan\\tfrac{" (n :Pi 2) "^\\circ}{2} = " (n :t 4)
                                      " = e^{-" (n :x 3) "}") :data-formula)
                         [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.9em"}}
                          [:div "Π(x) = 2 atan e^−x = " (n :Pi 3) "°"]
                          [:div "departure from Euclid, 90° − Π(x) = " [:span {:data-defect true} (n :defect 3)] "°"]
                          [:div "first order near x = 0: 90° − (180/π) x = " (n :linear 3) "°"]
                          [:div "A drawn at radius tanh(x/2) = " (n :r 4)]]
                         [:div {:data-case-name 'kind :style {:font-weight 600 :font-size "1.08em" :margin "0.4em 0"}}
                          '(case kind
                             "euclid" "x → 0: Π → 90°, the two parallels close onto AE: Euclid's case, one parallel"
                             "far" "A far from the line: Π → 0, almost every line through A misses BC"
                             "between" "Lobachevsky's plane: two parallels at Π(x) < 90°, every line between them misses BC")]
                         [:p [:strong "Try this: "]]
                         (button {:data-preset "near"} "Bring A down to the line (x = 0.01)"
                                 (list 'fn [] (list 'swap! !p 'assoc :x 0.01)))
                         (button {:data-preset "pi60"} "Set Π = 60°"
                                 (list 'fn [] (list 'swap! !p 'assoc :Pi 60)))
                         (button {:data-preset "far"} "Send A far away (x = 4)"
                                 (list 'fn [] (list 'swap! !p 'assoc :x 4)))
                         (button {:data-preset "reset"} "Reset (x = 0.8)"
                                 (list 'fn [] (list 'swap! !p 'assoc :x 0.8)))]]])
                (list 'finally (tie/unwatch-form !p)))))))))

;; ---------------------------------------------------------------------------
;; 2a. The triangle playground

(def triangle-start
  "A moderate triangle about the centre (Poincare coordinates)."
  {:m 0 :ax 0.55 :ay -0.15 :bx -0.35 :by 0.5 :cx -0.3 :cy -0.5})

(def triangle-presets
  [["near the centre: almost Euclidean" {:ax 0.08 :ay -0.02 :bx -0.05 :by 0.07 :cx -0.04 :cy -0.06}]
   ["a large triangle" {:ax 0.85 :ay -0.1 :bx -0.4 :by 0.75 :cx -0.45 :cy -0.72}]
   ["vertices to the rim: an ideal triangle" {:ax 0.995 :ay 0 :bx -0.4975 :by 0.8617 :cx -0.4975 :cy -0.8617}]
   ["off to one side" {:ax 0.8 :ay 0.4 :bx 0.55 :by 0.75 :cx 0.35 :cy 0.35}]])

(defn triangle-playground
  "Gauss-Bonnet on the hyperbolic triangle, vertices free."
  ([] (triangle-playground triangle-start))
  ([start]
   (binding [vc/*backend* :raster]
     (ev/with-let [!s start]
       (let [ks [:m :ax :ay :bx :by :cx :cy]
             [qsym qform] (flow/kernel "tri" d/triangle (mapv start ks) [0])
             [gsym gform] (flow/kernel "drawn" d/drawn-angle-sum (mapv start ks) [0])
             [dsym dform] (flow/kernel "drag" d/drag-kernel [0 0.1 0.1] [0])
             [ssym sform] (curve-kernel "side" d/side [0 0 0 0.1 0.1])
             g (fn [k] (list 'aget 'o (d/triangle-index k)))
             dg (fn [k] (flow/fmt (list '* (g k) (/ 180 Math/PI)) 2))
             f4 (fn [k] (flow/fmt (g k) 4))
             side (fn [[ax ay] [bx by]]
                    (curve ssym ['(:m s) (list ax 's) (list ay 's) (list bx 's) (list by 's)]
                           0 1 48 {:color (colours :tri) :weight 3}))
             mover (fn [kx ky dx dy label]
                     [:<>
                      (mafs/movable-point {:point [(g dx) (g dy)] :color (colours :point)
                                           :on-move (list 'fn '[[x y]] (list 'drag! kx ky 'x 'y))})
                      (mafs/text label {:x (g dx) :y (g dy) :attach "ne" :attach-distance 22})])]
         (flow/template
          {'?s !s '?q qsym '?g gsym '?d dsym}
          (list 'reagent.core/with-let
                [qsym qform gsym gform dsym dform ssym sform 'pts! pts-form
                 'drag! '(fn [kx ky x y]
                           (let [o (?d (array 0) (array (:m @?s) x y))]
                             (swap! ?s assoc kx (aget o 0) ky (aget o 1))))]
                (list 'let '[s @?s
                             o (?q (array 0) (array (:m s) (:ax s) (:ay s) (:bx s) (:by s) (:cx s) (:cy s)))
                             gs (aget (?g (array 0) (array (:m s) (:ax s) (:ay s) (:bx s) (:by s) (:cx s) (:cy s))) 0)
                             S (aget o 3) area (aget o 4)
                             kind (cond (< (aget o 17) 1e-3) "degenerate"
                                        (< area 0.05) "euclidean"
                                        (< S 0.05) "ideal"
                                        :else "hyperbolic")
                             agree (< (js/Math.abs (- (+ (aget o 8) (aget o 9) (aget o 10)) S)) 1e-3)
                             klein (= 1 (:m s))]
                      [:div {:data-playground "hyperbolic-triangle" :data-case 'kind :data-sum '(str S)
                             :data-area '(str area) :data-model '(str (:m s))}
                       [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                        [:div {:style {:flex "1 1 420px" :min-width "300px"}}
                         (mafs/mafs {:view-box {:x [-1.1 1.1] :y [-1.1 1.1]} :height 440}
                                    boundary
                                    (side [:ax :ay] [:bx :by]) (side [:bx :by] [:cx :cy]) (side [:cx :cy] [:ax :ay])
                                    (mover :ax :ay :dax :day "A")
                                    (mover :bx :by :dbx :dby "B")
                                    (mover :cx :cy :dcx :dcy "C"))]
                        [:div {:style {:flex "0 1 340px" :min-width "260px"}}
                         [:div
                          (button {:data-model "0"} "Poincaré disk (angles true)" (list 'fn [] (list 'swap! !s 'assoc :m 0)))
                          (button {:data-model "1"} "Beltrami-Klein disk (lines straight)" (list 'fn [] (list 'swap! !s 'assoc :m 1)))]
                         [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.9em" :margin-top "0.4em"}}
                          [:div "A = " (dg :A) "°,  B = " (dg :B) "°,  C = " (dg :C) "°"]
                          [:div "S = A + B + C = " [:span {:data-angle-sum true} (dg :S)] "°"]
                          [:div "sides a = " (f4 :a) ", b = " (f4 :b) ", c = " (f4 :c)]]
                         (katex (list 'str "\\text{area} = \\pi - (A+B+C) = \\pi - " (f4 :S) " = " (f4 :area)) :data-gauss-bonnet)
                         [:div {:style {:font-size "0.85em" :opacity 0.8}} "Gauss-Bonnet with K = −1: the area is the angle defect."]
                         [:div {:data-case-name 'kind :style {:font-weight 600 :font-size "1.08em" :margin "0.4em 0"}}
                          '(case kind
                             "euclidean" "near the centre: S is almost π, the triangle almost Euclidean (area ≈ 0)"
                             "ideal" "vertices at the rim: an ideal triangle, S → 0 and the area → π, the largest a triangle can have"
                             "hyperbolic" "a hyperbolic triangle: S < π, and the missing angle is its area"
                             "degenerate" "two vertices together: no triangle")]
                         [:div {:data-invariant '(if agree "pass" "fail")
                                :style {:display "inline-block" :padding "0.15em 0.6em" :border-radius "0.6em" :color "white" :margin "0.2em 0"
                                        :background '(if agree "#2E8B57" "#C0392B")}}
                          '(if agree "✓ angles by the Moebius map = angles by the law of cosines" "✗ the two angle computations disagree")]
                         [:div {:data-klein-badge '(if klein "drawn" "true")
                                :style {:display "block" :padding "0.15em 0.6em" :border-radius "0.6em" :margin "0.2em 0"
                                        :border '(str "1px solid " (if klein "#C0392B" "#2E8B57"))}}
                          '(if klein
                             (str "Klein's picture: the straight triangle you see has Euclidean angle sum "
                                  (.toFixed (* gs (/ 180 js/Math.PI)) 2) "°, but its true sum is "
                                  (.toFixed (* S (/ 180 js/Math.PI)) 2) "°: lines straight, angles false")
                             "Poincaré's picture: the angles you see are the true angles (the map is conformal)")]]]
                       [:p {:style {:font-size "0.9em" :opacity 0.85}}
                        "Drag A, B or C. Each side is the geodesic between its ends: an arc of a circle meeting the rim at right angles (Poincaré), or a straight chord (Beltrami-Klein). Angles, sides, S and the area come from one raster kernel; the same angles are recomputed from the sides by the hyperbolic law of cosines."]
                       [:p [:strong "Try this:"]]
                       (into [:<>] (for [[label v] triangle-presets]
                                     (button {:data-preset label} label (list 'fn [] (list 'swap! !s 'merge v)))))
                       (button {:data-preset "reset"} "Reset" (list 'fn [] (list 'reset! !s start)))]))))))))

;; ---------------------------------------------------------------------------
;; 2b. The parallels playground

(def parallels-start {:m 0 :px 0.0 :py 0.35 :alpha 1.2 :n 6})

(defn parallels-playground
  "A point P above the line BC (the diameter); the lines through P."
  ([] (parallels-playground parallels-start))
  ([start]
   (binding [vc/*backend* :raster]
     (ev/with-let [!s start]
       (let [ks [:m :px :py :alpha]
             [qsym qform] (flow/kernel "par" d/parallels (mapv start ks) [0])
             [dsym dform] (flow/kernel "drag" d/drag-kernel [0 0.1 0.1] [0])
             [lsym lform] (curve-kernel "limiting" d/limiting [0 0 0.3 1])
             [wsym wform] (curve-kernel "line" d/line-through [0 0 0.3 1.0])
             [fsym fform] (curve-kernel "fan" d/fan-line [0 0 0.3 1 7])
             [psym pform] (curve-kernel "perp" d/perpendicular [0 0 0.3])
             g (fn [k] (list 'aget 'o (d/parallels-index k)))
             mp ['(:m s) '(:px s) '(:py s)]
             fan (fn [j] (curve fsym (conj mp j '(+ 1 (:n s))) -1 1 48 {:color (colours :fan)}))]
         (flow/template
          {'?s !s '?q qsym '?d dsym}
          (list 'reagent.core/with-let
                [qsym qform dsym dform lsym lform wsym wform fsym fform psym pform 'pts! pts-form]
                (list 'let '[s @?s
                             o (?q (array 0) (array (:m s) (:px s) (:py s) (:alpha s)))
                             P (aget o 1) gap (aget o 4)
                             kind (cond (< (js/Math.abs gap) 0.01) "limiting"
                                        (neg? gap) "meets"
                                        :else "misses")
                             near (< (aget o 0) 0.05)]
                      [:div {:data-playground "hyperbolic-parallels" :data-case 'kind :data-pi '(str P)
                             :data-gap '(str gap) :data-d '(str (aget o 0))}
                       [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                        [:div {:style {:flex "1 1 420px" :min-width "300px"}}
                         (mafs/mafs {:view-box {:x [-1.1 1.1] :y [-1.1 1.1]} :height 440}
                                    boundary
                                    (mafs/segment {:point1 [-1 0] :point2 [1 0] :color (colours :line) :weight 3})
                                    (into [:<>] (for [j (range 1 13)]
                                                  (list 'when (list '< j '(+ 1 (:n s))) (fan j))))
                                    (curve lsym (conj mp 1) -1 1 64 {:color (colours :limit) :weight 4})
                                    (curve lsym (conj mp -1) -1 1 64 {:color (colours :limit) :weight 4})
                                    (curve psym mp 0 1 16 {:color (colours :perp)})
                                    (curve wsym (conj mp '(:alpha s)) -1 1 64 {:color "#111111" :weight 2.5})
                                    (mafs/movable-point {:point [(g :dpx) (g :dpy)] :color (colours :point)
                                                         :on-move '(fn [[x y]]
                                                                     (let [r (?d (array 0) (array (:m @?s) x y))]
                                                                       (swap! ?s assoc :px (aget r 0) :py (aget r 1))))})
                                    (mafs/text "P" {:x (g :dpx) :y (g :dpy) :attach "ne" :attach-distance 22})
                                    (mafs/text "D" {:x (g :ddx) :y (g :ddy) :attach "s"}))]
                        [:div {:style {:flex "0 1 340px" :min-width "260px"}}
                         [:div
                          (button {:data-model "0"} "Poincaré" (list 'fn [] (list 'swap! !s 'assoc :m 0)))
                          (button {:data-model "1"} "Beltrami-Klein" (list 'fn [] (list 'swap! !s 'assoc :m 1)))]
                         (leva/sub-panel {:fill true :flat true :title-bar false}
                                         (leva/controls {:atom !s :schema {:alpha {:min 0.02 :max 3.12 :step 0.01 :label "angle of the black line from PD (rad)"}
                                                                           :n {:min 0 :max 12 :step 1 :label "non-meeting lines drawn"}}}))
                         [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.9em"}}
                          [:div "distance PD = d = " (flow/fmt (g :d) 4)]
                          [:div "Π(d) = 2 atan e^−d = " [:span {:data-pi-text true} (flow/fmt (list '* (g :Pi) (/ 180 Math/PI)) 2)] "°"]
                          [:div "half the angle between the red parallels = " (flow/fmt (list '* (g :Pi-g) (/ 180 Math/PI)) 2) "°"]
                          [:div "black line: " (flow/fmt (list '* '(:alpha s) (/ 180 Math/PI)) 2) "° from PD, minus Π = "
                           (flow/fmt (list '* (g :gap) (/ 180 Math/PI)) 2) "°"]]
                         [:div {:data-case-name 'kind :style {:font-weight 600 :font-size "1.08em" :margin "0.4em 0"}}
                          '(case kind
                             "meets" "the black line cuts BC: its angle with PD is less than Π(d)"
                             "limiting" "the black line is a limiting parallel: it meets BC only at infinity (angle = Π(d))"
                             "misses" "the black line does not meet BC: one of the infinitely many lines between the two parallels")]
                         [:div {:style {:font-size "0.9em" :opacity 0.85}}
                          '(if near "P almost on the line: Π(d) → 90°, the two parallels close onto one, as in Euclid"
                               (str "Euclid allows one line through P that misses BC; here every line within "
                                    (.toFixed (- 180 (* 2 (aget o 1) (/ 180 js/Math.PI))) 1)
                                    "° of the two red ones misses it."))]]]
                       [:p {:style {:font-size "0.9em" :opacity 0.85}}
                        "Drag P. Red: the two limiting parallels through P, towards the two ends of BC on the rim; orange: some of the non-meeting lines between them; green: the perpendicular PD; black: the line at the angle you set."]
                       [:p [:strong "Try this:"]]
                       (button {:data-preset "limiting"} "Turn the black line onto the parallel (angle = Π)"
                               '(fn [] (swap! ?s (fn [st] (assoc st :alpha (aget (?q (array 0) (array (:m st) (:px st) (:py st) (:alpha st))) 1))))))
                       (button {:data-preset "near"} "Bring P down to the line"
                               (list 'fn [] (list 'swap! !s 'assoc :px 0 :py 0.02)))
                       (button {:data-preset "far"} "Push P towards the rim"
                               (list 'fn [] (list 'swap! !s 'assoc :px 0.2 :py 0.93)))
                       (button {:data-preset "reset"} "Reset" (list 'fn [] (list 'reset! !s start)))]))))))))

;; ---------------------------------------------------------------------------
;; 3. Beltrami's pseudosphere

(def pseudo-start {:k 0 :R 1 :u 0.8 :v 1.0})

(defn pseudosphere
  "The pseudosphere in MathBox (orbitable), K from E, F, G alone."
  ([] (pseudosphere pseudo-start))
  ([start]
   (binding [vc/*backend* :raster]
     (ev/with-let [!s start]
       (let [[qsym qform] (flow/kernel "pseudo" ps/probe-both (mapv start [:k :R :u :v]) [0])
             pf (fn [params f] (ev/with-params {:atom !s :params params} f))
             q (fn [k] (list 'aget 'o (ps/probe-index k)))
             f6 (fn [k] (flow/fmt (q k) 6))]
         (flow/template
          {'?s !s '?q qsym}
          (list 'reagent.core/with-let [qsym qform]
                (list 'let '[s @?s
                             o (?q (array 0) (array (:k s) (:R s) (:u s) (:v s)))
                             holds (< (js/Math.abs (- (aget o 6) (aget o 8))) (* 1e-4 (js/Math.abs (aget o 8))))
                             kind (if (zero? (:k s)) "pseudosphere" "sphere")]
                      [:div {:data-figure "beltrami-pseudosphere" :data-case 'kind :data-k '(str (aget o 6))}
                       [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                        [:div {:style {:flex "1 1 440px" :min-width "300px"}}
                         (plot/scene
                          {:range [[-2.2 2.2] [-2.2 2.2] [-2.2 2.2]] :scale [1 1 1]
                           :camera [2.4 -2.4 1.4] :axes [] :grids []
                           :container {:style {:height "460px" :width "100%"}}}
                          (plot/parametric-surface {:f (pf [:k :R] (fn [k R] (fn [[u v]] (ps/surface-of k R u v))))
                                                    :u [0 (* 2 Math/PI)] :v [0.05 3]
                                                    :u-samples 64 :v-samples 48 :color "#5AA0D0" :opacity 0.6
                                                    :grid-u 16 :grid-v 12 :grid-opacity 0.45 :simplify? false})
                          (plot/parametric-curve {:f (pf [:k :R :u] (fn [k R u] (fn [v] (ps/surface-of k R u v))))
                                                  :t [0.05 3] :samples 96 :width 6 :color "#C0392B" :simplify? false})
                          (plot/parametric-curve {:f (pf [:k :R :v] (fn [k R v] (fn [u] (ps/surface-of k R u v))))
                                                  :t [0 (* 2 Math/PI)] :samples 96 :width 5 :color "#2E8B57" :simplify? false})
                          (plot/point {:coords [(q :x) (q :y) (q :z)] :size 16 :color "#111111"}))
                         [:div {:style {:font-size "0.85em" :opacity 0.75}}
                          "drag: rotate · scroll: zoom · right-drag: pan. Red: the meridian through P (a tractrix, a geodesic); green: the circle v = const through P."]]
                        [:div {:style {:flex "0 1 330px" :min-width "260px"}}
                         [:div
                          (button {:data-kind "0"} "Beltrami's pseudosphere" (list 'fn [] (list 'swap! !s 'assoc :k 0)))
                          (button {:data-kind "1"} "the sphere, for comparison" (list 'fn [] (list 'swap! !s 'assoc :k 1)))]
                         (leva/sub-panel {:fill true :flat true :title-bar false}
                                         (leva/controls {:atom !s :schema {:R {:min 0.5 :max 1.6 :step 0.01 :label "radius R"}
                                                                           :u {:min 0 :max 6.28 :step 0.01 :label "P: u (around)"}
                                                                           :v {:min 0.05 :max 3 :step 0.01 :label "P: v (along)"}}}))
                         [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.9em"}}
                          [:div "at P: E = " (f6 :E) ", F = " (f6 :F) ", G = " (f6 :G)]
                          [:div "K from E, F, G alone (Brioschi) = " [:span {:data-kb true} (f6 :K)]]
                          [:div "K from the embedding = " (f6 :Ks)]
                          [:div '(if (zero? (:k s)) "−1/R² = " "+1/R² = ") (f6 :K-expected)]]
                         [:div {:data-case-name 'kind :style {:font-weight 600 :font-size "1.08em" :margin "0.4em 0"}}
                          '(if (zero? (:k s))
                             (if (< (js/Math.abs (- (:R s) 1)) 0.005)
                               "the pseudosphere, R = 1: K = −1 at every point, the curvature of Lobachevsky's plane"
                               "the pseudosphere: K = −1/R² everywhere, a constant negative curvature")
                             "the sphere: K = +1/R², triangles have angle sum above π")]
                         [:div {:data-invariant '(if holds "pass" "fail")
                                :style {:display "inline-block" :padding "0.15em 0.6em" :border-radius "0.6em" :color "white"
                                        :background '(if holds "#2E8B57" "#C0392B")}}
                          '(if holds "✓ K is the same constant at P as everywhere" "✗ K differs from the constant")]
                         [:p [:strong "Try this:"]]
                         (button {:data-preset "rim"} "Move P to the rim (v = 0.05)" (list 'fn [] (list 'swap! !s 'assoc :v 0.05)))
                         (button {:data-preset "spike"} "Move P up the spike (v = 2.8)" (list 'fn [] (list 'swap! !s 'assoc :v 2.8)))
                         (button {:data-preset "R2"} "Shrink R to 0.5: K = −4" (list 'fn [] (list 'swap! !s 'assoc :R 0.5)))
                         (button {:data-preset "sphere"} "Compare the sphere" (list 'fn [] (list 'swap! !s 'assoc :k 1)))
                         (button {:data-preset "reset"} "Reset" (list 'fn [] (list 'reset! !s start)))]]]))))))))

;; ---------------------------------------------------------------------------
;; Derivations for the page

(defn derivation-parallels []
  (clerk/col
   (clerk/tex (:pi p/tex))
   (clerk/tex (:tie p/tex))))

(defn derivation-pseudosphere
  "Emmy's E, F, G of the unit pseudosphere."
  []
  (let [[E F G] (ps/metric)]
    (clerk/col
     (clerk/html [:div {:style {:font-size "0.85em" :opacity 0.7}} "E, F, G of r(u, v) = (sech v cos u, sech v sin u, v − tanh v), by Emmy (cosh and tanh written with exp):"])
     (clerk/tex (str "E = " (e/->TeX E)))
     (clerk/tex (str "F = " (e/->TeX F)))
     (clerk/tex (str "G = " (e/->TeX G))))))
