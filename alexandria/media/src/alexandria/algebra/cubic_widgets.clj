(ns alexandria.algebra.cubic-widgets
  "The widgets of the algebra_cubic notebook, emmy-viewers fragments on the
   raster backend (alexandria.medium.tied), every number from a raster
   kernel of alexandria.algebra.cubic.

     square-widget   al-Khwarizmi's second figure, stepped in Rosen's
                     words (1831), on a tied state {:b :c :x}: b roots and
                     the number c (forward: x), or the root x (inverse: c)
     cardano-widget  x^3 + p x = q on a tied state {:p :q :x}: p or q
                     (forward, Cardano's rule) or x (inverse, q = x^3 + px).
                     :view :cube   Cardano's dissection of the cube AC = u
                                   into AB^3 = x^3, BC^3 = v^3 and the solids
                                   (MathBox, orbitable), the rule with the
                                   current values, the root checked back
                     :view :plane  the two cube roots of q/2 +- sqrt(D) on
                                   the complex plane, conjugate when D < 0,
                                   their sum on the real axis (Bombelli), the
                                   cubic's graph, the case named live
     viete-widget    the three roots dragged on the line, the coefficients
                     following as the symmetric functions"
  (:require [alexandria.algebra.cubic :as c]
            [alexandria.medium.tied :as tied]
            [emmy.mafs :as mafs]
            [emmy.mathbox.plot :as plot]
            [emmy.viewer :as ev]))

(def colours
  {:square "#C0392B" :rect "#1F6FB4" :corner "#E67E22" :great "#2E8B57" :u "#3C4650"
   :x "#C0392B" :v "#1F6FB4" :solid "#8A9BB0" :t1 "#7B5EA7" :t2 "#16A085" :sum "#111111"})

(def fmt tied/fmt)

(defn- katex [tex-form attrs]
  (list 'let ['tex tex-form]
        [:div (merge attrs {:data-formula 'tex})
         '[:f> nextjournal.clerk.render/render-katex tex {:inline? false}]]))

(defn- n [i k] (fmt (list 'aget 'r i) k))

;; ---------------------------------------------------------------------------
;; 1. al-Khwarizmi

(def rosen-steps
  "One sentence of Rosen's translation (1831, pp. 15-16) per step, with
   the pieces of the figure that step draws."
  [{:show #{:square}
    :text "\"Roots and Squares are equal to Numbers; for instance, one square, and ten roots of the same, amount to thirty-nine dirhems.\""
    :now "x² + b·x = c"}
   {:show #{:square :rects}
    :text "\"We halve for this purpose the ten, so that it becomes five, and construct two quadrangles on two sides of the quadrate A B, namely, G and D, the length of each of them being five, as the moiety of the ten roots, whilst the breadth of each is equal to a side of the quadrate A B.\""
    :now "two rectangles of x by b/2"}
   {:show #{:square :rects :corner-dashed}
    :text "\"Then a quadrate remains opposite the corner of the quadrate A B. This is equal to five multiplied by five: this five being half of the number of the roots which we have added to each of the two sides of the first quadrate.\""
    :now "the missing corner, (b/2)²"}
   {:show #{:square :rects :corner :great}
    :text "\"This we add to thirty-nine, in order to complete the great square S H. The sum is sixty-four.\""
    :now "c + (b/2)²"}
   {:show #{:square :rects :corner :great :side}
    :text "\"We extract its root, eight, which is one of the sides of the great quadrangle. By subtracting from this the same quantity which we have before added, namely five, we obtain three as the remainder.\""
    :now "x = √(c + (b/2)²) − b/2"}])

(def ^:private si c/square-index)

(defn- square-figure
  "The Mafs figure on the readout r, pieces by the step `st`."
  [!step]
  (let [x '(aget r 4) h '(aget r 0) X (list '+ x h)
        show (list 'fn ['k] (list 'contains? (list :show (list 'nth rosen-steps (list 'deref !step))) 'k))
        lab (fn [t px py] (mafs/text t {:x px :y py :size 15}))]
    (mafs/mafs
     {:height 420 :view-box {:x [-1 16] :y [-1 16]} :preserve-aspect-ratio "contain"}
     (mafs/cartesian {:x-axis {:lines 1} :y-axis {:lines 1}})
     (list 'when (list show :great)
           (mafs/polygon {:points [[0 0] [X 0] [X X] [0 X]] :color (colours :great) :fill-opacity 0.04 :weight 3}))
     (mafs/polygon {:points [[0 0] [x 0] [x x] [0 x]] :color (colours :square) :fill-opacity 0.25})
     (lab "A" -0.5 -0.5) (lab "B" (list '+ x 0.4) (list '+ x 0.4))
     (list 'when (list show :rects)
           [:<>
            (mafs/polygon {:points [[x 0] [X 0] [X x] [x x]] :color (colours :rect) :fill-opacity 0.25})
            (mafs/polygon {:points [[0 x] [x x] [x X] [0 X]] :color (colours :rect) :fill-opacity 0.25})
            (lab "G" (list '+ x (list '* 0.5 h)) (list '* 0.5 x))
            (lab "D" (list '* 0.5 x) (list '+ x (list '* 0.5 h)))])
     (list 'when (list 'or (list show :corner-dashed) (list show :corner))
           (mafs/polygon {:points [[x x] [X x] [X X] [x X]] :color (colours :corner)
                          :fill-opacity (list 'if (list show :corner) 0.35 0) :stroke-style "dashed"}))
     (list 'when (list show :great) [:<> (lab "S" -0.5 (list '+ X 0.4)) (lab "H" (list '+ X 0.4) -0.5)])
     (list 'when (list show :side)
           [:<>
            (mafs/segment {:point1 [0 -0.4] :point2 [X -0.4] :color (colours :great) :weight 3})
            (mafs/segment {:point1 [0 -0.7] :point2 [x -0.7] :color (colours :square) :weight 4})]))))

(defn- square-side [!step]
  (fn [_!s _r]
    [:div {:style {:font-size "0.93em" :line-height 1.5 :margin-top "0.5em"}}
     [:div {:style {:opacity 0.7}} "step " (list 'inc (list 'deref !step)) " of " (count rosen-steps)]
     [:p {:data-step-text true :style {:font-style "italic"}} (list :text (list 'nth rosen-steps (list 'deref !step)))]
     [:p {:style {:margin "0.2em 0" :opacity 0.8}} "now: " (list :now (list 'nth rosen-steps (list 'deref !step)))]
     [:div
      [:button {:type "button" :data-act "prev" :style {:margin "0.2em"}
                :on-click (list 'fn [] (list 'swap! !step '(fn [j] (max 0 (dec j)))))} "◀ prev"]
      [:button {:type "button" :data-act "next" :style {:margin "0.2em"}
                :on-click (list 'fn [] (list 'swap! !step (list 'fn '[j] (list 'min (dec (count rosen-steps)) '(inc j)))))} "next ▶"]]
     (katex (list 'str "x^2 + " (n (si :b) 2) "\\,x = " (n (si :c) 2)
                  ",\\quad \\left(\\tfrac{b}{2}\\right)^2 = " (n (si :half) 2) "^2 = " (n (si :corner) 3)
                  ",\\quad c + \\left(\\tfrac{b}{2}\\right)^2 = " (n (si :great) 3))
            {:data-square-formula true})
     (katex (list 'str "x = \\sqrt{" (n (si :great) 3) "} - " (n (si :half) 2) " = " (n (si :side) 4) " - " (n (si :half) 2)
                  " = " (n (si :x) 4))
            {:data-square-root (list 'str (list 'aget 'r (si :x)))})
     [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.9em"}}
      "check: x² + b·x = " (n (si :square) 3) " + " (n (si :roots) 3) " = " (n (si :c) 3)]]))

(defn square-widget
  "al-Khwarizmi's completing of the square, stepped and tied."
  []
  (let [start {:b 10 :c 39 :x 3}]
    (ev/with-let [!step 4]
      (tied/widget
       {:id :khwarizmi-square
        :keys c/square-keys :start start :ranges c/square-ranges
        :labels {:b "b (roots)" :c "c (number)" :x "x (root)"}
        :sources (c/square-sources)
        :readout (fn [b cc x] (c/square-readout b cc))
        :body (fn [_!s _r] (square-figure !step))
        :side (square-side !step)
        :presets [{:label "ten roots, thirty-nine (his example)" :state start}
                  {:label "six roots, sixteen" :state {:b 6 :c 16 :x 2}}
                  {:label "no roots: x² = c" :state {:b 0 :c 49 :x 7}}]}))))

;; ---------------------------------------------------------------------------
;; 2 and 3. Cardano and Bombelli

(def ^:private ci c/readout-index)

(def irreducible-band "|D| below this names the case D = 0." 0.05)

(defn- case-name []
  (let [band irreducible-band]
    (list 'let ['D (list 'aget 'r (ci :D))]
          [:div {:data-case (list 'cond (list '< '(js/Math.abs D) band) "double"
                                  '(> D 0) "one-real" :else "irreducible")
                 :style {:font-weight 600 :font-size "1.08em" :margin "0.4em 0"
                         :color (list 'if (list '< 'D (- band)) "#C0392B" "#2E8B57")}}
           (list 'cond
                 (list '< '(js/Math.abs D) band)
                 "D = 0: a double root; the two cubes are equal (u = −v), x = 2∛(q/2)"
                 '(> D 0)
                 "D > 0: one real root; the binomium and the apotome are real, and so are their cube roots"
                 :else
                 "D < 0: the irreducible case (casus irreducibilis): three real roots, yet √D is the root of a negative number")])))

(defn- rule-formula []
  (katex (list 'str
               "D = \\left(\\tfrac{q}{2}\\right)^2 + \\left(\\tfrac{p}{3}\\right)^3 = "
               (n (ci :h) 2) "^2 + (" (n (ci :k) 3) ")^3 = " (n (ci :D) 3)
               "\\\\ x = \\sqrt[3]{\\sqrt{D} + \\tfrac{q}{2}} - \\sqrt[3]{\\sqrt{D} - \\tfrac{q}{2}} = "
               (list 'if (list '> (list 'aget 'r (ci :D)) 0)
                     (list 'str "\\sqrt[3]{" (n (ci :A) 3) "} - \\sqrt[3]{" (n (ci :B) 3) "} = "
                           (n (ci :u) 4) " - " (n (ci :v) 4))
                     (list 'str "\\sqrt[3]{" (n (ci :h) 2) " + " (n (ci :cube-im) 3) "\\,i} + \\sqrt[3]{"
                           (n (ci :h) 2) " - " (n (ci :cube-im) 3) "\\,i} = ("
                           (n (ci :t1-re) 4) " + " (n (ci :t1-im) 4) "\\,i) + (" (n (ci :t2-re) 4) " - "
                           (n (ci :t1-im) 4) "\\,i)"))
               " = " (n (ci :x) 4))
         {:data-rule true}))

(defn- check-line []
  [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.9em"}
         :data-check (list 'str (list 'aget 'r (ci :rest-check)))}
   "check: x³ + p·x − q = " (n (ci :rest-check) 6)
   (list 'when (list '< (list 'aget 'r (ci :D)) 0)
         [:span {:style {:opacity 0.7}} " (the cube roots go through the browser's cos and sin, good to about 10⁻⁵)"])
   [:br]
   "the other roots: " (list 'if (list '>= (list 'aget 'r (ci :rest-d2)) 0)
                             (list 'str (fmt (list '+ (list 'aget 'r (ci :rest-re)) (list 'aget 'r (ci :rest-im))) 4)
                                   " and " (fmt (list '- (list 'aget 'r (ci :rest-re)) (list 'aget 'r (ci :rest-im))) 4))
                             (list 'str (fmt (list 'aget 'r (ci :rest-re)) 4) " ± " (fmt (list 'aget 'r (ci :rest-im)) 4) " i (not real)"))])

(defn- cardano-side [!s _r]
  [:div {:style {:font-size "0.93em" :line-height 1.5}}
   (katex (list 'str "x^3 + " (fmt (ev/get !s :p) 2) "\\,x = " (fmt (ev/get !s :q) 2)) {:data-equation true})
   (case-name) (rule-formula) (check-line)])

(defn- cube-scene
  "Cardano's cube AC = u cut at B (AB = x, BC = v): the cube of x (red),
   the cube of v (blue) and the planes x = AB through the whole, drawn as
   edges, from the readout r. Drawn when u, v > 0 (p > 0, D > 0)."
  []
  (let [u (list 'aget 'r (ci :u)) v (list 'aget 'r (ci :v)) x (list 'aget 'r (ci :x))
        box (fn [lo hi]
              (for [[a b] [[[0 0 0] [1 0 0]] [[0 1 0] [1 1 0]] [[0 0 1] [1 0 1]] [[0 1 1] [1 1 1]]
                           [[0 0 0] [0 1 0]] [[1 0 0] [1 1 0]] [[0 0 1] [0 1 1]] [[1 0 1] [1 1 1]]
                           [[0 0 0] [0 0 1]] [[1 0 0] [1 0 1]] [[0 1 0] [0 1 1]] [[1 1 0] [1 1 1]]]]
                [(mapv #(if (zero? %) lo hi) a) (mapv #(if (zero? %) lo hi) b)]))
        cuts (for [axis [0 1 2] i [0 x u] j [0 x u]]
               (let [p0 (assoc [i j 0] axis 0)
                     place (fn [s] (case axis 0 [s i j] 1 [i s j] 2 [i j s]))]
                 [(place 0) (place u)]))]
    (plot/scene
     {:range [[-0.3 4] [-0.3 4] [-0.3 4]] :scale [1 1 1] :camera [2.6 -1.6 1.9]
      :axes [] :grids [] :container {:style {:height "460px" :width "100%"}}}
     (list 'when (list 'and (list '> u 0) (list '> v 0) (list '> (list 'aget 'r (ci :D)) 0))
           (into [:<>]
                 (concat
                  (for [cs cuts] (plot/line {:coords (vec cs) :color (colours :solid) :width 2 :opacity 0.7}))
                  (for [cs (box 0 x)] (plot/line {:coords (vec cs) :color (colours :x) :width 5}))
                  (for [cs (box x u)] (plot/line {:coords (vec cs) :color (colours :v) :width 5}))
                  [(plot/line {:coords [[0 0 0] [x 0 0]] :color (colours :x) :width 8 :label ["A" "B"]})
                   (plot/line {:coords [[x 0 0] [u 0 0]] :color (colours :v) :width 8 :label {:end "C"}})]))))))

(defn- cube-body [_!s _r]
  [:div
   (cube-scene)
   [:div {:style {:font-size "0.88em" :opacity 0.85}}
    "The cube of AC = u (grey grid) cut by the planes through B: the cube of AB = x (red), the cube of BC = v (blue), "
    "three slabs x·x·v and three rods x·v·v. u³ − v³ = q and u·v = p/3, so x = u − v. drag: rotate · scroll: zoom"]
   [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.9em"}}
    "u = AC = " (n (ci :u) 4) ",  v = BC = " (n (ci :v) 4) ",  x = AB = u − v = " (n (ci :x) 4)
    ",  u·v = " (fmt (list '* (list 'aget 'r (ci :u)) (list 'aget 'r (ci :v))) 4) " = p/3,  u³ − v³ = "
    (fmt (list '- (list '* (list 'aget 'r (ci :u)) (list 'aget 'r (ci :u)) (list 'aget 'r (ci :u)))
               (list '* (list 'aget 'r (ci :v)) (list 'aget 'r (ci :v)) (list 'aget 'r (ci :v)))) 4) " = q"]
   (list 'when (list 'not (list 'and (list '> (list 'aget 'r (ci :u)) 0) (list '> (list 'aget 'r (ci :v)) 0)
                            (list '> (list 'aget 'r (ci :D)) 0)))
         [:p {:data-no-cube true :style {:color "#C0392B"}}
          "No cube to draw: Cardano's figure needs p > 0 (u·v = p/3 > 0) and D > 0. Move p up, or see the complex plane below."])])

(defn- plane-body [!s _r]
  (let [g (fn [k] (list 'aget 'r (ci k)))
        pts [[(g :t1-re) (g :t1-im) (colours :t1) "∛A"]
             [(g :t2-re) (g :t2-im) (colours :t2) "−∛B"]]]
    [:div
     (mafs/mafs
      {:height 400 :view-box {:x [-5 5] :y [-4 4]} :preserve-aspect-ratio "contain"}
      (mafs/cartesian {:x-axis {:lines 1} :y-axis {:lines 1}})
      (mafs/of-x {:y (ev/with-params {:atom !s :params [:p :q]}
                       (fn [p q] (fn [x] (c/number-of-cube p x)))) :color "#8A9BB099"})
      (mafs/of-x {:y (ev/with-params {:atom !s :params [:q]} (fn [q] (fn [_] q))) :color "#8A9BB066" :style "dashed"})
      (list 'when (list '< (g :D) 0)
            [:<>
             (mafs/circle {:center [0 0] :radius (list 'js/Math.cbrt (g :r)) :color (colours :t1) :fill-opacity 0})
             (mafs/segment {:point1 [(g :w1-re) (g :w1-im)] :point2 [(g :w1-re) (list '- (g :w1-im))]
                            :color (colours :t1) :opacity 0.5 :style "dashed"})
             (mafs/segment {:point1 [(g :w2-re) (g :w2-im)] :point2 [(g :w2-re) (list '- (g :w2-im))]
                            :color (colours :t1) :opacity 0.5 :style "dashed"})
             (mafs/point {:x (g :w1-re) :y (g :w1-im) :color (colours :t1) :opacity 0.5})
             (mafs/point {:x (g :w2-re) :y (g :w2-im) :color (colours :t1) :opacity 0.5})
             (mafs/point {:x (list '* 2 (g :w1-re)) :y 0 :color (colours :sum) :opacity 0.6})
             (mafs/point {:x (list '* 2 (g :w2-re)) :y 0 :color (colours :sum) :opacity 0.6})])
      (into [:<>] (for [[px py col t] pts]
                    [:<> (mafs/segment {:point1 [0 0] :point2 [px py] :color col})
                     (mafs/point {:x px :y py :color col})
                     (mafs/text t {:x px :y py :attach "ne" :size 14})]))
      (mafs/segment {:point1 [(g :t1-re) (g :t1-im)] :point2 [(g :x) 0] :color (colours :t2) :style "dashed"})
      (mafs/segment {:point1 [(g :t2-re) (g :t2-im)] :point2 [(g :x) 0] :color (colours :t1) :style "dashed"})
      (mafs/point {:x (g :x) :y 0 :color (colours :sum)})
      (mafs/text "x" {:x (g :x) :y 0 :attach "se" :size 15}))
     [:div {:style {:font-size "0.88em" :opacity 0.85}}
      "The complex plane. Violet and green: the two cube roots in Cardano's rule; their sum (black, by the parallelogram) is the root x. "
      "When D < 0 they are conjugate, mirror images in the real axis, so their sum is real; the faint violet points are the other two cube roots of q/2 + i√−D, "
      "each with its conjugate summing to one of the other real roots (black, faint). Grey: the graph of x³ + p·x and the level q."]]))

(defn cardano-widget
  "Cardano's rule, tied. view :cube (Cardano's figure, MathBox) or :plane
   (Bombelli on the complex plane). start {:p :q}; x comes from the rule.
   The kernels are built unsimplified (branch-free sums of abs)."
  ([view] (cardano-widget view (if (= view :plane) {:p -15 :q 4} {:p 6 :q 20})))
  ([view {:keys [p q]}]
   (binding [alexandria.raster/*simplify?* false]
     (let [tied-at (fn [p q] {:p p :q q :x (double (alexandria.raster/value (fn [p q] (c/cardano-x p q)) p q))})]
       (tied/widget
        {:id (if (= view :plane) :bombelli-plane :cardano-cube)
         :keys c/cardano-keys :start (tied-at p q) :ranges c/cardano-ranges
         :labels {:p "p (things)" :q "q (number)" :x "x (root)"}
         :sources (c/cardano-sources)
         :readout (fn [p q _x] (c/cardano-readout p q))
         :body (if (= view :plane) plane-body cube-body)
         :side cardano-side
         :presets (if (= view :plane)
                    [{:label "Bombelli: x³ = 15x + 4" :state (tied-at -15 4)}
                     {:label "x³ = 7x + 6: roots 3, −1, −2" :state (tied-at -7 6)}
                     {:label "D = 0: x³ = 12x + 16" :state (tied-at -12 16)}
                     {:label "Cardano: cube and 6 things = 20" :state (tied-at 6 20)}]
                    [{:label "Cardano: cube and 6 things = 20" :state (tied-at 6 20)}
                     {:label "cube and 3 things = 10" :state (tied-at 3 10)}
                     {:label "cube and 9 things = 26 (x = 2)" :state (tied-at 9 26)}])})))))

;; ---------------------------------------------------------------------------
;; 4. Viete

(defn- viete-body [!s _r !p]
  (let [mp (fn [k col]
             (mafs/movable-point
              {:point [(ev/get !s k) 0] :color col :constrain "horizontal"
               :on-move (list 'fn '[[x _]] (list 'swap! !p 'assoc k '(max -4 (min 4 x))))}))]
    [:div
     (mafs/mafs
      {:height 380 :view-box {:x [-5 5] :y [-20 20]} :preserve-aspect-ratio false}
      (mafs/cartesian {:x-axis {:lines 1} :y-axis {:lines 5}})
      (mafs/of-x {:y (ev/with-params {:atom !s :params [:r1 :r2 :r3]} c/cubic-of-roots) :color "#7B5EA7"})
      (mp :r1 "#C0392B") (mp :r2 "#1F6FB4") (mp :r3 "#2E8B57"))
     [:div {:style {:font-size "0.88em" :opacity 0.85}}
      "Drag the three roots along the axis (or use the sliders): the curve y = (x − r₁)(x − r₂)(x − r₃) passes through them, and its coefficients are recomputed."]]))

(defn- viete-side [_!s _r]
  [:div {:style {:font-size "0.93em" :line-height 1.5}}
   (katex (list 'str "x^3 - (" (n 0 3) ")\\,x^2 + (" (n 1 3) ")\\,x - (" (n 2 3) ")") {:data-coefficients true})
   [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.9em"}}
    [:div "e₁ = r₁ + r₂ + r₃ = " (n 0 3)]
    [:div "e₂ = r₁r₂ + r₁r₃ + r₂r₃ = " (n 1 3)]
    [:div "e₃ = r₁r₂r₃ = " (n 2 3)]
    [:div "with x = y + e₁/3: y³ + p·y = q, p = " (n 3 3) ", q = " (n 4 3)]]
   [:div {:data-case-viete true :style {:margin-top "0.3em" :font-weight 600}}
    (list 'if (list '< (list 'aget 'r 5) (- irreducible-band))
          "three distinct real roots: D < 0, the irreducible case"
          "two roots meet: D = 0")]])

(defn viete-widget
  "The roots dragged, the coefficients following."
  []
  (tied/widget
   {:id :viete-roots
    :keys [:r1 :r2 :r3] :start {:r1 -2 :r2 1 :r3 3}
    :ranges {:r1 [-4 4 0.01] :r2 [-4 4 0.01] :r3 [-4 4 0.01]}
    :labels {:r1 "r₁" :r2 "r₂" :r3 "r₃"}
    :sources (let [id (fn [a b d] [a b d])] {:r1 id :r2 id :r3 id})
    :readout c/viete
    :panel-body viete-body :side viete-side
    :presets [{:label "Bombelli's roots 4, −2 ± √3" :state {:r1 4 :r2 (- -2 (Math/sqrt 3)) :r3 (+ -2 (Math/sqrt 3))}}
              {:label "−2, 1, 3" :state {:r1 -2 :r2 1 :r3 3}}]}))

;; ---------------------------------------------------------------------------
;; Derivations

(defn derivation
  "Emmy's derivation `k` of alexandria.algebra.cubic/derivations, with the
   grades whose labels match `grade-re`."
  [k grade-re]
  (tied/derivation (get (c/derivations) k)
                   (filter #(re-find grade-re (:label %)) (c/graded))))
