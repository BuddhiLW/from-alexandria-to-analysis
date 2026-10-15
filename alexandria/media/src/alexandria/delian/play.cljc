(ns alexandria.delian.play
  "Browser playgrounds for the doubling of the cube (render-fns). The math
   and the kernels are alexandria.delian.playgrounds (core); every curve
   point and every number on screen is a raster kernel row.

     means     the given lines a, b (drag their ends), the three loci of
               Menaechmus, the meeting point Theta = (x, y), the two means;
               a free point P with its residual on each locus
     conchoid  the pole E, the ruler, the interval l, the turning line
               through E (drag R), the given line x = c of the neusis
     cissoid   the circle of diameter 2r, the abscissa u (drag), the
               ordinates h and y, Diocles' four lines in proportion"
  (:require #?(:cljs [alexandria.medium.figure :as figure])
            #?(:cljs [leva.core :as leva])
            #?(:cljs [reagent.core :as r])
            #?(:cljs [clojure.string :as str])))

#?(:cljs
   (do
     (defn- fx [x d] (if (js/isFinite x) (str/replace (.toFixed (js/Number x) d) "-" "−") "—"))
     (defn- at [svg ev]
       (let [p (.createSVGPoint svg)]
         (set! (.-x p) (.-clientX ev)) (set! (.-y p) (.-clientY ev))
         (let [q (.matrixTransform p (.inverse (.getScreenCTM svg)))]
           [(.-x q) (- (.-y q))])))
     (defn- path [ps colour & [{:keys [width dashed?] :or {width 0.035}}]]
       [:polyline {:points (str/join " " (keep (fn [[x y]] (when (and (js/isFinite x) (js/isFinite y) (< (js/Math.abs x) 50) (< (js/Math.abs y) 50)) (str x "," (- y)))) ps))
                   :fill "none" :stroke colour :stroke-width width :stroke-dasharray (when dashed? "0.12 0.08")}])
     (defn- seg [[x1 y1] [x2 y2] colour & [{:keys [width dashed?] :or {width 0.035}}]]
       [:line {:x1 x1 :y1 (- y1) :x2 x2 :y2 (- y2) :stroke colour :stroke-width width
               :stroke-dasharray (when dashed? "0.12 0.08")}])
     (defn- letter [[x y] s & [colour size]]
       [:text {:x x :y (- y) :fill (or colour "#ECEEE4") :font-size (or size 0.28) :text-anchor "middle"
               :dominant-baseline "middle" :font-family "Georgia, serif" :font-style "italic"} s])
     (defn- handle [[x y] nm drag & [colour]]
       [:circle {:cx x :cy (- y) :r 0.13 :fill (or colour "#E8735A") :stroke "#fff" :stroke-width 0.03
                 :data-drag nm :style {:cursor "grab"}
                 :on-pointer-down (fn [ev] (.preventDefault ev) (reset! drag nm))}])
     (defn- badge [ok? text]
       [:div {:data-badge (if ok? "ok" "broken")
              :style {:margin "0.5em 0" :padding "0.3em 0.6em" :border-radius "4px"
                      :background (if ok? "#2f6b3a" "#7a2b22") :color "#fff"}} text])
     (defn- presets-row [presets labels prm]
       [:div "Try this: "
        (for [[k label] labels]
          ^{:key k}
          [:button {:data-preset (name k) :style {:margin "0.2em"} :on-click #(reset! prm (get presets k))} label])])
     (defn- svg-box [view svg drag on-drag & children]
       (into [:svg {:viewBox view :ref #(reset! svg %)
                    :style {:width "100%" :background "#1d1f21" :border-radius "6px" :touch-action "none" :user-select "none"}
                    :on-pointer-move (fn [ev] (when (and @drag @svg) (on-drag @drag (at @svg ev))))
                    :on-pointer-up #(reset! drag nil) :on-pointer-leave #(reset! drag nil)}]
             children))
     (defn- controls [prm order ranges labels]
       [leva/SubPanel {:fill true :flat true :titleBar false}
        [leva/Controls {:atom prm
                        :schema (into {} (for [k order :let [[lo hi s] (get ranges k)]]
                                           [k {:min lo :max hi :step s :label (get labels k)}]))}]])
     (defn- clamp [ranges k v] (let [[lo hi] (get ranges k)] (max lo (min hi v))))
     (def ^:private mono {:font-family "ui-monospace, monospace" :font-size "0.92em" :line-height 1.6})
     (def ^:private layout {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"})

     ;; -----------------------------------------------------------------
     ;; Hippocrates' two means on Menaechmus' loci

     (def ^:private means-names
       {:equal "a = b: the means are a itself; nothing to find"
        :delian "b = 2a: the Delian case; the cube on x is double the cube on a"
        :cube "b : a is a ratio of cubes of numbers: x is a whole multiple, found by ruler alone"
        :solid "b : a any other ratio: a solid problem, the means need the conics"})

     (defn means
       "Clerk render-fn. value: {:kernels :scalars :start :presets :ranges}."
       [{:keys [kernels scalars start presets ranges]}]
       (r/with-let [cf (figure/kernel-figure (:curves kernels))
                    ff (figure/kernel-figure (:fig kernels))
                    prm (r/atom start) drag (atom nil) svg (atom nil)
                    ts (mapv (fn [i] [(+ 0.02 (* i 0.025))]) (range 200))]
         (let [{:keys [a b px py]} @prm
               F (zipmap scalars (first (figure/points ff [a b px py] [[0]])))
               {:keys [x y ratio ax xy yb r1 r2 r3 cube-x cube-a]} F
               rows (figure/points cf [a b] ts)
               kind (let [q ratio w? (fn [v] (some #(< (js/Math.abs (- v (* % % %))) 0.01) [2 3 4 5]))]
                      (cond (< (js/Math.abs (- q 1)) 0.01) :equal (< (js/Math.abs (- q 2)) 0.01) :delian
                            (or (w? q) (w? (/ 1 q))) :cube :else :solid))
               on (fn [rv] (< (js/Math.abs rv) 0.02))
               on-all? (and (on r1) (on r2) (on r3))]
           [:div {:data-playground "delian-means" :data-case (name kind) :data-on (str (on r1) " " (on r2) " " (on r3))
                  :data-state (str/join " " [a b px py]) :style layout}
            [:div {:style {:flex "1 1 460px" :min-width "300px"}}
             (svg-box "-0.6 -4.6 5.4 6.0" svg drag
                      (fn [d [mx my]]
                        (swap! prm (fn [m] (case d
                                             "P" (assoc m :px (clamp ranges :px mx) :py (clamp ranges :py my))
                                             "a" (assoc m :a (clamp ranges :a mx))
                                             "b" (assoc m :b (clamp ranges :b mx))
                                             m))))
                      [:line {:x1 0 :y1 0.2 :x2 0 :y2 -4.5 :stroke "#555" :stroke-width 0.02}]
                      [:line {:x1 -0.2 :y1 0 :x2 4.7 :y2 0 :stroke "#555" :stroke-width 0.02}]
                      (path (mapv (fn [[x1 y1]] [x1 y1]) rows) "#8FC7E8")
                      (path (mapv (fn [[_ _ x2 y2]] [x2 y2]) rows) "#F2D16B")
                      (path (mapv (fn [[_ _ _ _ x3 y3]] [x3 y3]) rows) "#B9A3E3")
                      (letter [3.9 4.3] "x² = a y" "#8FC7E8")
                      (letter [4.2 (min 4.3 (+ 0.3 (js/Math.sqrt (* b 4.2))))] "y² = b x" "#F2D16B")
                      (letter [4.3 (+ 0.25 (/ (* a b) 4.3))] "x y = a b" "#B9A3E3")
                      ;; Theta and its coordinates, the two means
                      (seg [x 0] [x y] "#ECEEE4" {:dashed? true}) (seg [0 y] [x y] "#ECEEE4" {:dashed? true})
                      [:circle {:cx x :cy (- y) :r 0.08 :fill "#ECEEE4" :data-theta true}]
                      (letter [(+ x 0.25) (+ y 0.2)] "Θ")
                      (letter [x -0.22] "x") (letter [-0.22 y] "y")
                      ;; the given lines and the first mean, below the axis
                      (seg [0 -0.55] [a -0.55] "#7FD18B" {:width 0.07}) (letter [-0.3 -0.55] "a" "#7FD18B")
                      (seg [0 -0.95] [b -0.95] "#E8735A" {:width 0.07}) (letter [-0.3 -0.95] "b" "#E8735A")
                      (seg [0 -1.35] [x -1.35] "#ECEEE4" {:width 0.07}) (letter [-0.3 -1.35] "x")
                      (handle [a -0.55] "a" drag "#7FD18B") (handle [b -0.95] "b" drag "#E8735A")
                      (letter [(+ px 0.25) (+ py 0.22)] "P" "#f0c960")
                      (handle [px py] "P" drag "#f0c960"))
             [:div {:style {:font-size "0.8em" :opacity 0.7}} "drag the ends of a and b, and the point P"]]
            [:div {:style {:flex "1 1 300px" :min-width "260px"}}
             (controls prm [:a :b :px :py] ranges {:a "a (given)" :b "b (given)" :px "P: x" :py "P: y"})
             [:div {:style mono}
              [:div {:data-live-case (name kind) :style {:font-size "1.15em" :font-weight 600}}
               (str "b : a = " (fx ratio 3) " — " (means-names kind))]
              [:div {:data-readout "means"} "x = ∛(a²b) = " (fx x 4) ",  y = x²/a = " (fx y 4)]
              [:div {:data-readout "ratios"} "a : x = " (fx ax 4) ",  x : y = " (fx xy 4) ",  y : b = " (fx yb 4)]
              [:div {:data-readout "cubes"} "x³ = " (fx cube-x 4) " = (b/a)·a³ = " (fx ratio 3) "·" (fx cube-a 4)]
              [:div {:data-readout "P"} "P on x² = a y: " (fx r1 3) (if (on r1) " ✓" "") ";  y² = b x: " (fx r2 3) (if (on r2) " ✓" "")
               ";  x y = a b: " (fx r3 3) (if (on r3) " ✓" "")]
              (badge on-all? (if on-all? "P is on all three loci: P is Θ, its coordinates are the two means."
                                 "P is not Θ: off any one locus, a : x = x : y = y : b breaks. Drag P onto the crossing."))
              (presets-row presets [[:delian "Delian: b = 2a"] [:eight "b = 8a: ruler alone"] [:half "b = a/2: halve the cube"]
                                    [:equal "a = b"] [:theta "Put P on Θ"]] prm)
              [:div {:style {:font-size "0.85em" :opacity 0.75 :margin-top "0.4em"}}
               "Each locus says one equal ratio. Two of them already fix Θ; the third passes through it, as Eutocius says."]]]])))

     ;; -----------------------------------------------------------------
     ;; Nicomedes' conchoid

     (def ^:private conchoid-names
       {:loop "l > d: the inner branch makes a loop through the pole"
        :cusp "l = d: the loop has shrunk to a cusp at the pole"
        :smooth "l < d: the inner branch never reaches the pole"})

     (defn conchoid
       "Clerk render-fn. value: {:kernels :scalars :start :presets :ranges}."
       [{:keys [kernels scalars start presets ranges]}]
       (r/with-let [cf (figure/kernel-figure (:conchoid kernels))
                    ff (figure/kernel-figure (:cfig kernels))
                    prm (r/atom start) drag (atom nil) svg (atom nil)
                    ts (mapv (fn [i] [(+ 0.05 (* i (/ (- js/Math.PI 0.1) 239)))]) (range 240))]
         (let [{:keys [d l phi c]} @prm
               F (zipmap scalars (first (figure/points ff [d l phi c] [[0]])))
               {:keys [rx qx qy q'x q'y RQ RQ' gap ey]} F
               rows (figure/points cf [d l] ts)
               kind (let [q (- l d)] (cond (< (js/Math.abs q) 0.01) :cusp (pos? q) :loop :else :smooth))
               neusis? (< (js/Math.abs gap) 0.02)]
           [:div {:data-playground "delian-conchoid" :data-case (name kind) :data-neusis (str neusis?)
                  :data-state (str/join " " [d l phi c]) :style layout}
            [:div {:style {:flex "1 1 460px" :min-width "300px"}}
             (svg-box "-4 -3.6 8 6.4" svg drag
                      (fn [dr [mx my]]
                        (swap! prm (fn [m]
                                     (case dr
                                       "E" (assoc m :d (clamp ranges :d (- my)))
                                       "R" (assoc m :phi (clamp ranges :phi (* (/ 180 js/Math.PI) (js/Math.atan2 (:d m) mx))))
                                       "Q" (assoc m :l (clamp ranges :l (js/Math.hypot (- mx rx) my)))
                                       "c" (assoc m :c (clamp ranges :c mx))
                                       m))))
                      (seg [-4 0] [4 0] "#9aa7b4" {:width 0.03}) (letter [-3.5 0.2] "ruler" "#9aa7b4" 0.22)
                      (seg [c -3] [c 2.8] "#7FD18B" {:dashed? true}) (letter [(+ c 0.3) 2.5] "given" "#7FD18B" 0.2)
                      (path (mapv (fn [[x y]] [x y]) rows) "#F2D16B" {:width 0.04})
                      (path (mapv (fn [[_ _ x y]] [x y]) rows) "#F2D16B" {:width 0.04 :dashed? true})
                      (seg [0 ey] [qx qy] "#ECEEE4") (seg [0 ey] [q'x q'y] "#ECEEE4" {:dashed? true})
                      (seg [rx 0] [qx qy] "#E8735A" {:width 0.07})
                      (letter [-0.3 ey] "E") (letter [(- rx 0.15) -0.25] "R") (letter [(+ qx 0.25) (+ qy 0.15)] "Q")
                      (handle [0 ey] "E" drag) (handle [rx 0] "R" drag) (handle [qx qy] "Q" drag "#f0c960")
                      (handle [c -2.8] "c" drag "#7FD18B"))
             [:div {:style {:font-size "0.8em" :opacity 0.7}} "drag E (the pole), R (turns the line), Q (the interval), the given line"]]
            [:div {:style {:flex "1 1 300px" :min-width "260px"}}
             (controls prm [:d :l :phi :c] ranges {:d "d (pole below ruler)" :l "l (interval)" :phi "line angle °" :c "given line x = c"})
             [:div {:style mono}
              [:div {:data-live-case (name kind) :style {:font-size "1.15em" :font-weight 600}} (conchoid-names kind)]
              [:div {:data-readout "RQ"} "RQ = " (fx RQ 4) ",  RQ′ = " (fx RQ' 4) "  (both l = " (fx l 3) ": the instrument's interval)"]
              [:div {:data-readout "gap"} "Q from the given line: " (fx gap 4)]
              (badge neusis? (if neusis? "Neusis done: between the ruler and the given line lies a segment of length l pointing at E."
                                 "Not yet a neusis: turn the line until Q falls on the given line."))
              (presets-row presets [[:nicomedes "Nicomedes' curve"] [:cusp "l = d: the cusp"] [:no-loop "l < d"] [:neusis "Do the neusis"]] prm)]]])))

     ;; -----------------------------------------------------------------
     ;; Diocles' cissoid

     (def ^:private cissoid-names
       {:centre "u = r: all four lines are equal, the means are trivial"
        :delian "y = 2 (2r − u): h and u are the two means between a line and its double"
        :near "u < r: the four lines fall, 2r − u > h > u > y"
        :far "u > r: the four lines rise, 2r − u < h < u < y"})

     (defn cissoid
       "Clerk render-fn. value: {:kernels :scalars :start :presets :ranges}."
       [{:keys [kernels scalars start presets ranges]}]
       (r/with-let [cf (figure/kernel-figure (:cissoid kernels))
                    ff (figure/kernel-figure (:sfig kernels))
                    prm (r/atom start) drag (atom nil) svg (atom nil)]
         (let [{:keys [r u]} @prm
               ts (mapv (fn [i] [(* 2 r (/ (+ 1 i) 202))]) (range 196))
               F (zipmap scalars (first (figure/points ff [r u] [[0]])))
               {:keys [w h y r1 r2 r3 ratio mean]} F
               rows (figure/points cf [r] ts)
               kind (cond (< (js/Math.abs (- u r)) 0.01) :centre (< (js/Math.abs (- ratio 2)) 0.01) :delian
                          (< u r) :near :else :far)
               prop? (and (< (js/Math.abs (- r1 r2)) 1e-6) (< (js/Math.abs (- r2 r3)) 1e-6))]
           [:div {:data-playground "delian-cissoid" :data-case (name kind) :data-state (str r " " u) :style layout}
            [:div {:style {:flex "1 1 460px" :min-width "300px"}}
             (svg-box "-0.4 -3.2 3.8 5.4" svg drag
                      (fn [dr [mx _]]
                        (swap! prm (fn [m] (case dr
                                             "u" (assoc m :u (max 0.02 (min (- (* 2 (:r m)) 0.05) mx)))
                                             "r" (let [r' (clamp ranges :r (/ mx 2))] (assoc m :r r' :u (min (:u m) (- (* 2 r') 0.05))))
                                             m))))
                      [:circle {:cx r :cy 0 :r r :fill "none" :stroke "#8FC7E8" :stroke-width 0.03}]
                      (seg [-0.2 0] [3.3 0] "#555" {:width 0.02})
                      (path (mapv (fn [[_ _ x yy]] [x yy]) rows) "#F2D16B" {:width 0.04})
                      (path (mapv (fn [[_ _ x yy]] [x (- yy)]) rows) "#F2D16B" {:width 0.04})
                      (seg [u 0] [u y] "#E8735A" {:width 0.05}) (seg [u 0] [u h] "#8FC7E8" {:width 0.06})
                      (seg [0 0] [u y] "#ECEEE4" {:dashed? true})
                      (letter [-0.15 -0.2] "O" "#ECEEE4" 0.22) (letter [(* 2 r) -0.2] "2r" "#8FC7E8" 0.22)
                      (letter [(+ u 0.12) (+ h 0.1)] "h" "#8FC7E8" 0.22) (letter [(+ u 0.15) (min 2.6 y)] "y" "#E8735A" 0.22)
                      ;; Diocles' four lines, side by side under the figure
                      (for [[i [len col nm]] (map-indexed vector [[w "#9aa7b4" "2r−u"] [h "#8FC7E8" "h"] [u "#ECEEE4" "u"] [y "#E8735A" "y"]])]
                        ^{:key nm}
                        [:g (seg [(+ 0.2 (* i 0.8)) -2.9] [(+ 0.2 (* i 0.8)) (+ -2.9 (min 2.4 (* 0.5 len)))] col {:width 0.12})
                         (letter [(+ 0.2 (* i 0.8)) -3.1] nm col 0.2)])
                      (handle [u 0] "u" drag) (handle [(* 2 r) 0] "r" drag "#8FC7E8"))
             [:div {:style {:font-size "0.8em" :opacity 0.7}} "drag u along the diameter, and the end 2r"]]
            [:div {:style {:flex "1 1 300px" :min-width "260px"}}
             (controls prm [:r :u] ranges {:r "r (radius)" :u "u (abscissa)"})
             [:div {:style mono}
              [:div {:data-live-case (name kind) :style {:font-size "1.15em" :font-weight 600}} (cissoid-names kind)]
              [:div {:data-readout "lines"} "2r − u = " (fx w 4) ",  h = " (fx h 4) ",  u = " (fx u 4) ",  y = " (fx y 4)]
              [:div {:data-readout "ratios"} "(2r−u) : h = " (fx r1 4) ",  h : u = " (fx r2 4) ",  u : y = " (fx r3 4)]
              [:div {:data-readout "extremes"} "y : (2r − u) = " (fx ratio 4) ",  its cube root = u : h = " (fx mean 4)]
              (badge prop? (if prop? "Continued proportion holds: the point is on Diocles' curve, h and u are the two means."
                               "Off the curve, the proportion fails."))
              (presets-row presets [[:diocles "Diocles' figure"] [:centre "u = r"] [:delian "Extremes in ratio 2"] [:edge "u near 2r"]] prm)]]])))))
