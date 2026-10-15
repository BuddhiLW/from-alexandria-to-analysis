(ns alexandria.ptolemy.theorem-widget
  "Ptolemy's theorem on the page (widget brief section 5, playground brief).

   `tied`: the quadrilateral ABCD on the unit circle with A, B, C fixed and
   D moving; the arc d = CD and the three chords that depend on it (CD, BD,
   AD) are four tied Leva controls (alexandria.ptolemy.theorem/tie, origin
   and epsilon guards from alexandria.medium.tie). Both sides of
   AC.BD = AB.CD + AD.BC are printed with the current numbers, the residual
   beside them, every number from the readout kernel.

   `playground`: the four points are free. Drag any of them on the figure
   (Mafs movable points); with \"keep on the circle\" on, a dragged point is
   put back on the circle through the other three. The six lengths, the two
   sides of Ptolemy's relation and the residual come from one raster kernel
   (theorem/quad). The case is named live: cyclic (the theorem), D inside
   or outside the circle (Ptolemy's inequality, strict), the order crossed
   (the points are concyclic but ABCD is not a convex quadrilateral), three
   points on a line, or two points together. Presets set the state."
  (:require [alexandria.medium.flow :as flow]
            [alexandria.medium.tie :as tie]
            [alexandria.ptolemy.theorem :as th]
            [alexandria.raster :as raster]
            [emmy.clerk :as ec]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [nextjournal.clerk :as clerk]))

(def colours {:circle "#1F6FB4" :quad "#C0392B" :diag "#2E8B57" :point "#111111" :ok "#2E8B57" :bad "#C0392B"})

(defn- katex [tex-form attr]
  (list 'let ['tex tex-form] [:div {attr 'tex} [:f> 'nextjournal.clerk.render/render-katex 'tex {:inline? false}]]))

;; ---------------------------------------------------------------------------
;; The tied widget

(defn init
  "The starting tied state at d = 40 degrees, by raster on the JVM."
  []
  (zipmap th/tied (raster/value (fn [d] (th/chords-of d)) 40)))

(defn tied
  "The tied quadrilateral on the unit circle."
  ([] (tied (init)))
  ([start]
   (binding [vc/*backend* :raster]
     (ev/with-let [!p start !s start]
       (let [s (mapv start th/tied)
             [tsym tform] (tie/kernel-binding "tie" th/tie s)
             [rsym rform] (tie/kernel-binding "readout" th/tie-readout [(first s)])
             opts {:tie-sym tsym :p !p :s !s :keys th/tied :ranges th/ranges :counter "__ptolemySolves"}
             n (fn [k d] (tie/fmt (list 'aget 'r (th/tie-index k)) d))
             pt (fn [deg] [(Math/cos (Math/toRadians deg)) (Math/sin (Math/toRadians deg))])
             [A B C] (map (comp pt th/angles) [:A :B :C])
             D '[(aget r 11) (aget r 12)]]
         (flow/template
          {'?r rsym '?s !s}
          (list 'reagent.core/with-let (into [tsym tform rsym rform] (tie/locals opts))
                (list 'let '[r (?r (array 0) (array (get @?s :d)))]
                      [:div {:data-figure "ptolemy-tied" :data-residual '(str (aget r 10)) :data-d '(str (get @?s :d))}
                       [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                        [:div {:style {:flex "1 1 380px" :min-width "300px"}}
                         (mafs/mafs {:view-box {:x [-1.3 1.3] :y [-1.3 1.3]} :height 380}
                                    (mafs/circle {:center [0 0] :radius 1 :color (colours :circle) :fill-opacity 0})
                                    (mafs/polygon {:points [A B C D] :color (colours :quad) :fill-opacity 0.08})
                                    (mafs/segment {:point1 A :point2 C :color (colours :diag)})
                                    (mafs/segment {:point1 B :point2 D :color (colours :diag)})
                                    (mafs/point {:x (first A) :y (second A) :color (colours :point)})
                                    (mafs/point {:x (first B) :y (second B) :color (colours :point)})
                                    (mafs/point {:x (first C) :y (second C) :color (colours :point)})
                                    (mafs/point {:x (first D) :y (second D) :color (colours :quad)})
                                    (mafs/text "A" {:x (first A) :y (second A) :attach "sw"})
                                    (mafs/text "B" {:x (first B) :y (second B) :attach "n"})
                                    (mafs/text "C" {:x (first C) :y (second C) :attach "ne"})
                                    (mafs/text "D" {:x (first D) :y (second D) :attach "se"}))]
                        [:div {:style {:flex "0 1 320px" :min-width "260px"}}
                         (leva/sub-panel {:fill true :flat true :title-bar false}
                                         (leva/controls {:atom !p :schema (tie/schema th/tied th/ranges th/labels)}))
                         (katex (list 'str "AC\\cdot BD = " (n :AC 4) "\\times" (n :BD 4) " = " (n :lhs 4)) :data-lhs)
                         (katex (list 'str "AB\\cdot CD + AD\\cdot BC = " (n :AB 4) "\\times" (n :CD 4) " + "
                                      (n :DA 4) "\\times" (n :BC 4) " = " (n :rhs 4)) :data-rhs)
                         [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.9em"}}
                          [:div "CD = 2 sin(d/2) = " (n :CD 4) ";  BD = 2 sin((60° + d)/2) = " (n :BD 4)]
                          [:div "AD = 2 cos(d/2) = " (n :DA 4)]
                          [:div "residual AC·BD − (AB·CD + AD·BC) = " [:span {:data-tie-residual true} (list '.toExponential '(aget r 10) 2)]]]
                         [:div {:style {:font-weight 600 :margin-top "0.3em"}}
                          "D on the circle, ABCD in order: the theorem holds"]]]
                       [:p {:style {:font-size "0.9em" :opacity 0.85}}
                        "A, B and C stay at 200°, 80° and 20° on a circle of diameter 2. Move the arc d = CD, or any chord that depends on it: the others are solved back through d (CD = 2 sin(d/2), BD = 2 sin((60° + d)/2), AD = 2 cos(d/2)), and both sides of Ptolemy's relation are recomputed by raster."]])
                (list 'finally (tie/unwatch-form !p)))))))))

;; ---------------------------------------------------------------------------
;; The playground

(def author-state
  "Ptolemy's own configuration for the lemma: four points in order on the
   circle (the tied widget's at d = 40)."
  (let [p (fn [deg] [(Math/cos (Math/toRadians deg)) (Math/sin (Math/toRadians deg))])
        [ax ay] (p 200) [bx by] (p 80) [cx cy] (p 20) [dx dy] (p -20)]
    {:ax ax :ay ay :bx bx :by by :cx cx :cy cy :dx dx :dy dy :snap true}))

(defn- preset [state label values]
  [:button {:type "button" :data-preset label :style {:margin "0.2em"}
            :on-click (list 'fn [] (list 'swap! state 'merge values))} label])

(def presets
  "The \"try this\" states; each sets all four points, so a preset never
   inherits a point left by the one before."
  (let [p (fn [deg] [(Math/cos (Math/toRadians deg)) (Math/sin (Math/toRadians deg))])
        st (fn [a b c d snap] (let [[ax ay] a [bx by] b [cx cy] c [dx dy] d]
                                {:ax ax :ay ay :bx bx :by by :cx cx :cy cy :dx dx :dy dy :snap snap}))
        [A B C] (map p [200 80 20])]
    [["The rectangle: Pythagoras" (st (p 210) (p 150) (p 30) (p -30) true)]
     ["Pull D inside the circle" (st A B C [0.35 -0.45] false)]
     ["Push D outside" (st A B C [1.25 -0.95] false)]
     ["Cross the order: swap B and C" (st A C B (p -20) true)]]))

(defn playground
  "Ptolemy's lemma with the four points free."
  ([] (playground author-state))
  ([start]
   (binding [vc/*backend* :raster]
     (ev/with-let [!q start]
       (let [ks [:ax :ay :bx :by :cx :cy :dx :dy]
             [qsym qform] (flow/kernel "quad" th/quad (mapv start ks) [0])
             g (fn [k] (list 'aget 'o (th/quad-index k)))
             f4 (fn [k] (flow/fmt (g k) 4))
             ;; the letter is drawn BEFORE its point and lets the pointer through
             ;; (the style at the top of the playground): drawn after it, the
             ;; label covered the hitbox, took the mousedown, and the Mafs pane
             ;; panned the whole view instead of moving the point
             mover (fn [px py label colour]
                     [:<>
                      (mafs/text label {:x (list 'get '@?q px) :y (list 'get '@?q py) :attach "ne"})
                      (mafs/movable-point
                       {:point [(list 'get '@?q px) (list 'get '@?q py)] :color colour
                        :on-move (list 'fn '[[x y]] (list 'drag! px py 'x 'y))})])]
         (flow/template
          {'?q !q '?k qsym}
          (list 'reagent.core/with-let
                [qsym qform
                 'drag! '(fn [kx ky x y]
                           ;; with snap on, the dragged point goes back on the circle
                           ;; through the other three, along the ray from its centre
                           (let [s @?q
                                 o (?k (array 0) (array (:ax s) (:ay s) (:bx s) (:by s) (:cx s) (:cy s) (:dx s) (:dy s)))
                                 others (case kx :ax [:bx :cx :dx] :bx [:ax :cx :dx] :cx [:ax :bx :dx] [:ax :bx :cx])
                                 ;; the circle through the other three, by the same kernel with
                                 ;; those three as A, B, C
                                 [p1 p2 p3] (map (fn [k] [(get s k) (get s (keyword (str (subs (name k) 0 1) "y")))]) others)
                                 o2 (?k (array 0) (array (first p1) (second p1) (first p2) (second p2) (first p3) (second p3) x y))
                                 ox (aget o2 9) oy (aget o2 10) rr (aget o2 11)
                                 vx (- x ox) vy (- y oy) len (js/Math.hypot vx vy)
                                 ok (and (:snap s) (js/isFinite rr) (< rr 50) (> len 1e-9))]
                             (swap! ?q assoc kx (if ok (+ ox (* rr (/ vx len))) x) ky (if ok (+ oy (* rr (/ vy len))) y))))]
                (list 'let '[s @?q
                             o (?k (array 0) (array (:ax s) (:ay s) (:bx s) (:by s) (:cx s) (:cy s) (:dx s) (:dy s)))
                             convex (and (neg? (* (aget o 13) (aget o 14))) (neg? (* (aget o 15) (aget o 16))))
                             kind (cond (< (aget o 18) 1e-3) "degenerate"
                                        (< (js/Math.abs (aget o 17)) 1e-6) "collinear"
                                        (< (js/Math.abs (aget o 12)) 2e-3) (if convex "cyclic" "cyclic-crossed")
                                        (neg? (aget o 12)) "inside" :else "outside")
                             holds (< (js/Math.abs (aget o 8)) 1e-6)]
                      [:div {:data-playground "ptolemy-quad" :data-case 'kind :data-residual '(str (aget o 8))}
                       [:style "[data-playground=\"ptolemy-quad\"] .MafsView text { pointer-events: none; }"]
                       [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                        [:div {:style {:flex "1 1 420px" :min-width "300px"}}
                         (mafs/mafs {:view-box {:x [-1.6 1.6] :y [-1.6 1.6]} :height 440}
                                    (mafs/cartesian {:subdivisions 1})
                                    '(when (and (js/isFinite (aget o 11)) (< (aget o 11) 50))
                                       [mafs.core/Circle {:center [(aget o 9) (aget o 10)] :radius (aget o 11)
                                                          :color "#1F6FB4" :fillOpacity 0 :strokeStyle "dashed"}])
                                    '[mafs.core/Polygon {:points [[(:ax s) (:ay s)] [(:bx s) (:by s)] [(:cx s) (:cy s)] [(:dx s) (:dy s)]]
                                                         :color "#C0392B" :fillOpacity 0.08}]
                                    '[mafs.line/Segment {:point1 [(:ax s) (:ay s)] :point2 [(:cx s) (:cy s)] :color "#2E8B57"}]
                                    '[mafs.line/Segment {:point1 [(:bx s) (:by s)] :point2 [(:dx s) (:dy s)] :color "#2E8B57"}]
                                    (mover :ax :ay "A" "#111111")
                                    (mover :bx :by "B" "#111111")
                                    (mover :cx :cy "C" "#111111")
                                    (mover :dx :dy "D" "#C0392B"))]
                        [:div {:style {:flex "0 1 320px" :min-width "260px"}}
                         [:label {:style {:display "block" :margin "0.3em 0"}}
                          [:input {:type "checkbox" :data-control "snap" :checked '(boolean (:snap s))
                                   :on-change '(fn [ev] (swap! ?q assoc :snap (.. ev -target -checked)))}]
                          " keep a dragged point on the circle through the other three"]
                         [:div {:style {:font-size "1.15em" :font-weight 600 :margin "0.3em 0"} :data-case-name 'kind}
                          '(case kind
                             "cyclic" "cyclic quadrilateral: AC·BD = AB·CD + AD·BC (Ptolemy's theorem)"
                             "cyclic-crossed" "on one circle, but out of order: ABCD crosses itself, the diagonals are now sides"
                             "inside" "D inside the circle through A, B, C: AC·BD < AB·CD + AD·BC (Ptolemy's inequality)"
                             "outside" "D outside the circle through A, B, C: AC·BD < AB·CD + AD·BC (Ptolemy's inequality)"
                             "collinear" "A, B, C on one line: no circle through them"
                             "degenerate" "two points coincide: a side or diagonal is 0")]
                         [:div {:data-invariant '(if holds "pass" "fail")
                                :style {:display "inline-block" :padding "0.15em 0.6em" :border-radius "0.6em" :color "white"
                                        :background '(if holds "#2E8B57" "#C0392B")}}
                          '(if holds "✓ AC·BD = AB·CD + AD·BC" "✗ equality fails: the theorem needs the four points on one circle, in order")]
                         (katex (list 'str "AC\\cdot BD = " (f4 :AC) "\\times" (f4 :BD) " = " (f4 :lhs)) :data-lhs)
                         (katex (list 'str "AB\\cdot CD + AD\\cdot BC = " (f4 :AB) "\\times" (f4 :CD) " + "
                                      (f4 :DA) "\\times" (f4 :BC) " = " (f4 :rhs)) :data-rhs)
                         [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.88em"}}
                          [:div "residual = " [:span {:data-residual-text true} '(.toExponential (aget o 8) 3)]]
                          [:div "D from the circle through A, B, C: |OD| − r = " (flow/fmt (g :off) 4)]]]]
                       [:p {:style {:font-size "0.9em" :opacity 0.85}}
                        "Drag A, B, C or D. The dashed circle passes through A, B and C; the green lines are the diagonals AC and BD. Every length and both sides of the relation come from one raster kernel."]
                       [:p "Try this:"]
                       (into [:<>] (for [[label v] presets] (preset !q label v)))
                       (preset !q "Reset to Ptolemy's figure" author-state)]))))))))

;; ---------------------------------------------------------------------------
;; The derivation

(defn derivation
  "Emmy's identities behind the widgets, with their grades."
  []
  (clerk/col
   (clerk/tex (:theorem th/tex))
   (clerk/tex (:tie th/tex))
   (clerk/html
    (into [:ul {:style {:font-size "0.9em"}}]
          (for [{:keys [label grade]} (th/graded)]
            [:li {:data-grade (name grade)} [:code (name grade)] " " label])))))
