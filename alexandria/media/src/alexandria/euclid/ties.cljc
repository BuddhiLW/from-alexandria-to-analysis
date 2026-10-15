(ns alexandria.euclid.ties
  "Two tied widgets for the Euclid notebook (widget brief section 5).

     pythagoras   a triangle ABC with the squares on its sides. a = BC,
                  b = CA, c = AB and the angle A are live Leva controls and
                  B can be dragged; they are tied by Emmy's
                  a^2 = b^2 + c^2 - 2 CA.AD (alexandria.euclid.pythagoras).
                  The proposition is named live: I.47 while the angle at A
                  is right, II.12 (obtuse) or II.13 (acute) when it is not,
                  with the rectangle CA.AD drawn as the excess or the defect.
     application  the application of areas: a rectangle applied to AB = a
                  with area S and height y, falling short (VI.28), exact
                  (I.44) or exceeding (VI.29) by a rectangle of shape k : 1;
                  a, S, y, k tied by S = (a + k y) y
                  (alexandria.euclid.application). The case is named live
                  with the Pythagorean words Apollonius gave the conics:
                  elleipsis, parabole, hyperbole. Its rectangles are the
                  :rect kernel of alexandria.apollonius.areas-steps.

   JVM side: `figures` (Emmy functions for alexandria.medium.kernel).
   Browser side: the render-fns. Every number and point comes from a raster
   kernel (KernelFigure, one batch per frame); the browser compares and
   formats them. The tie: only the touched key is a source (origin guard
   `pushing`); a value Leva echoes back within half a step of a push solves
   nothing (epsilon guard `pushed`)."
  (:require [clojure.string :as str]
            #?(:clj [alexandria.apollonius.areas-steps :as areas])
            #?(:clj [alexandria.euclid.application :as ap])
            #?(:clj [alexandria.euclid.pythagoras :as py])
            #?(:cljs [alexandria.medium.figure :as figure])
            #?(:cljs [leva.core :as leva])
            #?(:cljs [nextjournal.clerk.render :as render])
            #?(:cljs [reagent.core :as rg])))

(defn named-map
  "{name value} of a kernel row: `points` (2 numbers each) then `scalars`."
  [points scalars row]
  (let [n (count points)]
    (merge (zipmap points (map vec (partition 2 (take (* 2 n) row))))
           (zipmap scalars (drop (* 2 n) row)))))

(defn block
  "The solved state for source `src` in a tie kernel's row: the keys
   `tied` from the offset of src in `offsets`."
  [tied offsets row src]
  (let [off (get offsets src)]
    (zipmap tied (map #(nth row (+ off %)) (range (count tied))))))

#?(:clj
   (def figures
     "Every kernel of both widgets, for alexandria.medium.kernel."
     {:tie {:f py/tie :params [5 4 3 90 0 3] :state [0] :opts {:simplify? false}}
      :fig {:f py/figure :params [4 3 90] :state [0] :opts {:simplify? false}}
      :atie {:f ap/tie :params [10 21 3 -1] :state [0] :opts {:simplify? false}}
      :afig {:f ap/figure :params [10 3 -1] :state [0] :opts {:simplify? false}}
      :rect (:rect areas/figures)}))

#?(:cljs
   (do
     (defn- row [fig params] (vec (first (figure/points fig params [[0]]))))

     (defn- fixed [n d] (if (js/isFinite n) (str/replace (.toFixed (js/Number n) d) "-" "−") "—"))

     (defn- tex [s] [:f> render/render-katex s {:inline? true}])

     (defn- pts-attr [ps] (str/join " " (map (fn [[x y]] (str x "," (- y))) ps)))

     (defn- poly [ps colour & [{:keys [fill dashed? opacity] :or {fill 0.25 opacity 1}}]]
       [:polygon {:points (pts-attr ps) :fill colour :fill-opacity fill :stroke colour
                  :stroke-width 0.05 :opacity opacity
                  :stroke-dasharray (when dashed? "0.2 0.12")}])

     (defn- seg [[x1 y1] [x2 y2] colour & [{:keys [width dashed?] :or {width 0.05}}]]
       [:line {:x1 x1 :y1 (- y1) :x2 x2 :y2 (- y2) :stroke colour :stroke-width width
               :stroke-dasharray (when dashed? "0.2 0.12")}])

     (defn- letter [[x y] s & [{:keys [colour size italic?] :or {colour "#ECEEE4" size 0.5}}]]
       [:text {:x x :y (- y) :fill colour :font-size size :text-anchor "middle"
               :dominant-baseline "middle" :font-family "Georgia, 'Times New Roman', serif"
               :font-style (if italic? "italic" "normal")} s])

     (defn- tie-watch!
       "Watch the Leva atom prm: re-solve the exact state ex from the touched
        key with the tie kernel `solve` (a fn of the exact state and the
        source, returning the solved state), push the keys that changed
        into prm after the current notification. `steps` {key step}."
       [prm ex solve steps pushing pushed on-solve]
       (add-watch prm ::tie
                  (fn [_ _ old new]
                    (when-not @pushing
                      (let [src (first (filter #(not= (get old %) (get new %)) (keys steps)))
                            half (fn [k] (+ 1e-9 (/ (get steps k) 2)))
                            echo? (and src
                                       (or (and (contains? @pushed src)
                                                (<= (js/Math.abs (- (get new src) (get @pushed src))) (half src)))
                                           (<= (js/Math.abs (- (get new src) (get @ex src))) (half src))))]
                        (when (and src (not echo?))
                          (let [solved (solve (assoc @ex src (get new src)) src)]
                            (set! (.-__euclidSolves js/globalThis) (inc (or (.-__euclidSolves js/globalThis) 0)))
                            (reset! ex solved)
                            (on-solve solved)
                            (js/setTimeout
                             (fn []
                               (let [shown @prm
                                     push (into {} (filter (fn [[k v]] (> (js/Math.abs (- v (get shown k))) 1e-12)) solved))]
                                 (when (seq push)
                                   (reset! pushed push)
                                   (reset! pushing true)
                                   (try (swap! prm merge push) (finally (reset! pushing false)))
                                   (js/setTimeout #(swap! pushed (fn [m] (apply dissoc m (keys push)))) 600))))
                             0))))))))

     (def ^:private colours
       {:ink "#ECEEE4" :ab "#8FC7E8" :ac "#F2D16B" :bc "#B9A3E3" :excess "#E8735A" :defect "#7FD18B"
        :foot "#9aa7b4" :applied "#F2D16B" :line "#ECEEE4"})

     (def ^:private panel-style
       {:font-size "0.92em" :line-height 1.6 :font-family "ui-monospace, monospace"})

     ;; -----------------------------------------------------------------
     ;; I.47, II.12, II.13

     (defn- svg-point
       "The figure's (x, y) under the pointer event (the SVG's own inverse
        screen transform; y up)."
       [svg e]
       (let [p (.createSVGPoint svg)]
         (set! (.-x p) (.-clientX e)) (set! (.-y p) (.-clientY e))
         (let [q (.matrixTransform p (.inverse (.getScreenCTM svg)))]
           [(.-x q) (- (.-y q))])))

     (defn pythagoras
       "Clerk render-fn. value: {:kernels :points :scalars :tied :offsets
        :ranges :start :names}."
       [{:keys [kernels points scalars tied offsets ranges start names]}]
       (rg/with-let [figs (into {} (map (fn [[k v]] [k (figure/kernel-figure v)])) kernels)
                     state-of (fn [{:keys [b c A]} x y] [0 b c A x y])
                     solve-row (fn [{:keys [a b c A]} x y] (row (:tie figs) [a b c A x y]))
                     ex0 (block tied offsets (solve-row (assoc start :a 1) 0 1) :b)
                     ex (rg/atom ex0)
                     prm (rg/atom ex0)
                     pushing (atom false)
                     pushed (atom {})
                     steps (into {} (map (fn [[k [_ _ s]]] [k s])) ranges)
                     _ (tie-watch! prm ex (fn [st src] (block tied offsets (solve-row st 0 1) src))
                                   steps pushing pushed (fn [_]))
                     drag (atom false)
                     svg-el (atom nil)
                     on-move (fn [e]
                               (when (and @drag @svg-el)
                                 (let [[x y] (svg-point @svg-el e)
                                       solved (block tied offsets (solve-row @ex x y) :B)]
                                   (set! (.-__euclidSolves js/globalThis) (inc (or (.-__euclidSolves js/globalThis) 0)))
                                   (reset! ex solved)
                                   (reset! pushed solved)
                                   (reset! pushing true)
                                   (try (reset! prm solved) (finally (reset! pushing false))))))]
         (let [{:keys [a b c A]} @ex
               P (named-map points scalars (row (:fig figs) [b c A]))
               {:keys [a2 b2 c2 ad rect2 sum cos]} P
               kind (cond (< (js/Math.abs cos) 0.004) :right (pos? cos) :acute :else :obtuse)
               {:keys [prop says] nm :name} (get names kind)
               g (fn [k] (get P k))]
           [:div {:data-figure "euclid-pythagoras" :data-kind (name kind)
                  :data-state (str/join " " (map #(get @ex %) tied))
                  :style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
            [:div {:style {:flex "1 1 460px" :min-width "300px"}}
             [:svg {:viewBox "-9 -12.5 22 18" :ref #(reset! svg-el %)
                    :style {:width "100%" :height "auto" :background "#1d1f21" :border-radius "6px"
                            :touch-action "none" :user-select "none"}
                    :on-pointer-move on-move
                    :on-pointer-up #(reset! drag false)
                    :on-pointer-leave #(reset! drag false)}
              (poly [(g :A) (g :C) (g :ac1) (g :ac2)] (colours :ac))
              (poly [(g :B) (g :A) (g :ab1) (g :ab2)] (colours :ab))
              (poly [(g :C) (g :B) (g :bc1) (g :bc2)] (colours :bc))
              ;; the rectangle contained by CA, AD (II.12-13)
              (when-not (= kind :right)
                (poly [(g :A) (g :D) (g :r1) (g :r2)] (if (= kind :obtuse) (colours :excess) (colours :defect))
                      {:fill 0.55 :dashed? (= kind :acute)}))
              (poly [(g :A) (g :C) (g :B)] (colours :ink) {:fill 0.08})
              ;; the perpendicular BD, and CA produced to D when D falls outside
              (when-not (= kind :right)
                [:g (seg (g :B) (g :D) (colours :foot) {:dashed? true :width 0.04})
                 (seg (g :D) (g :A) (colours :foot) {:width 0.04})])
              (letter (g :l-A) "A") (letter (g :l-C) "C") (letter (g :l-B) "B")
              (when-not (= kind :right) (letter (g :l-D) "D" {:colour (colours :foot)}))
              (letter (g :l-a) "a²" {:italic? true :colour (colours :bc)})
              (letter (g :l-b) "b²" {:italic? true :colour (colours :ac)})
              (letter (g :l-c) "c²" {:italic? true :colour (colours :ab)})
              (let [[x y] (g :B)]
                [:circle {:cx x :cy (- y) :r 0.32 :fill "#E8735A" :stroke "#fff" :stroke-width 0.06
                          :data-drag "B" :style {:cursor "grab"}
                          :on-pointer-down (fn [e] (.preventDefault e) (reset! drag true))}])]
             [:div {:style {:font-size "0.8em" :opacity 0.6 :margin-top "0.3em"}} "drag B, or move any slider"]]
            [:div {:style {:flex "1 1 300px" :min-width "260px"}}
             [leva/SubPanel {:fill true :flat true :titleBar false}
              [leva/Controls {:atom prm
                              :schema (into {} (for [k tied :let [[lo hi s] (get ranges k)]]
                                                 [k {:min lo :max hi :step s
                                                     :label (case k :a "a = BC" :b "b = CA" :c "c = AB" :A "angle A °")}]))}]]
             [:div {:style panel-style}
              [:div {:data-case (name kind) :style {:font-size "1.2em" :font-weight 600 :margin "0.4em 0"}}
               (str prop ": " nm " at A")]
              [:div {:data-formula "true"}
               (tex (str "a^2 = b^2 + c^2 - 2\\,CA\\cdot AD:\\quad " (fixed a2 2) " = " (fixed b2 2) " + "
                         (fixed c2 2) " - " (fixed rect2 2)))]
              [:div {:data-sides (str (fixed a 4) " " (fixed b 4) " " (fixed c 4) " " (fixed A 2))}
               (tex (str "a = " (fixed a 3) ",\\; b = " (fixed b 3) ",\\; c = " (fixed c 3)
                         ",\\; \\angle A = " (fixed A 1) "^\\circ,\\; AD = c\\cos A = " (fixed ad 3)))]
              [:div (case kind
                      :right (tex (str "a^2 = " (fixed a2 2) " = b^2 + c^2 = " (fixed sum 2)))
                      :obtuse (tex (str "a^2 - (b^2 + c^2) = " (fixed (- a2 sum) 2) " = 2\\,CA\\cdot AD = " (fixed (js/Math.abs rect2) 2) " \\text{ (greater)}"))
                      :acute (tex (str "(b^2 + c^2) - a^2 = " (fixed (- sum a2) 2) " = 2\\,CA\\cdot AD = " (fixed rect2 2) " \\text{ (less)}")))]
              [:blockquote {:style {:font-family "Georgia, serif" :font-style "italic" :opacity 0.85
                                    :margin "0.5em 0" :font-size "0.95em"}}
               says " (Heath 1908, " prop ")"]
              [:div {:style {:font-size "0.85em" :opacity 0.75}}
               "Tie: b, c or the angle A moves, the others of them held, a re-solved; "
               "a moves, b and A held, c re-solved (inverse); B dragged, b held, c and A read off B, a re-solved."]]]])
         (finally (remove-watch prm ::tie))))

     ;; -----------------------------------------------------------------
     ;; I.44, VI.28-29

     (defn application
       "Clerk render-fn. value: {:kernels :outputs :tied :offsets :ranges
        :start :names :props}."
       [{:keys [kernels outputs tied offsets ranges start names props]}]
       (rg/with-let [figs (into {} (map (fn [[k v]] [k (figure/kernel-figure v)])) kernels)
                     solve (fn [{:keys [a S y k]} src] (block tied offsets (row (:atie figs) [a S y k]) src))
                     ex0 (solve start :y)
                     ex (rg/atom ex0)
                     prm (rg/atom ex0)
                     pushing (atom false)
                     pushed (atom {})
                     steps (into {} (map (fn [[k [_ _ s]]] [k s])) ranges)
                     _ (tie-watch! prm ex solve steps pushing pushed (fn [_]))]
         (let [{:keys [a S y k]} @ex
               F (zipmap outputs (row (:afig figs) [a y k]))
               {:keys [base applied left left-w top y-max S-max]} F
               kind (cond (< (js/Math.abs k) 0.005) :parabola (pos? k) :hyperbola :else :ellipse)
               {:keys [greek latin meaning]} (get names kind)
               {:keys [prop says]} (get props kind)
               corners [[0 0] [1 0] [1 1] [0 1]]
               rect (fn [u0 u1 h] (mapv (fn [[x y]] [x (- y)]) (figure/points (:rect figs) [u0 u1 h 1 1] corners)))]
           [:div {:data-figure "euclid-application" :data-kind (name kind)
                  :data-state (str/join " " (map #(get @ex %) tied))
                  :style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
            [:div {:style {:flex "1 1 460px" :min-width "300px"}}
             [:svg {:viewBox "-1 -9.5 22 11" :style {:width "100%" :height "auto" :background "#1d1f21" :border-radius "6px"}}
              (poly (rect 0 base y) (colours :applied) {:fill 0.35})
              (case kind
                :ellipse (poly (rect base a y) (colours :excess) {:fill 0.45 :dashed? true})
                :hyperbola (poly (rect a base y) (colours :defect) {:fill 0.45})
                nil)
              (seg [0 0] [a 0] (colours :line) {:width 0.08})
              (letter [0 -0.6] "A") (letter [a -0.6] "B")
              (when-not (= kind :parabola) (letter [base (+ y 0.45)] "X"))
              (letter [(/ base 2) (/ y 2)] "S" {:italic? true})
              (letter [-0.5 (/ y 2)] "y" {:italic? true})]]
            [:div {:style {:flex "1 1 300px" :min-width "260px"}}
             [leva/SubPanel {:fill true :flat true :titleBar false}
              [leva/Controls {:atom prm
                              :schema (into {} (for [kk tied :let [[lo hi s] (get ranges kk)]]
                                                 [kk {:min lo :max hi :step s
                                                      :label (case kk :a "a = AB" :S "S (area)" :y "y (height)" :k "k (shape of the leftover)")}]))}]]
             [:div {:style panel-style}
              [:div {:data-conic-name (name kind) :style {:font-size "1.2em" :font-weight 600 :margin "0.4em 0"}}
               (str latin " (" greek "): " meaning ", " prop)]
              [:div {:data-formula "true"}
               (tex (str "S = (a + k\\,y)\\,y = (" (fixed a 2) (if (neg? k) " - " " + ") (fixed (js/Math.abs k) 2)
                         "\\cdot " (fixed y 2) ")\\cdot " (fixed y 2) " = " (fixed applied 2)))]
              [:div (tex (case kind
                           :parabola (str "\\text{applied exactly: height } S/a = " (fixed y 3))
                           :ellipse (str "a\\,y - S = " (fixed (- left) 2) "\\text{ falls short; at most } (a/2)^2/|k| = "
                                         (fixed S-max 2) " \\text{ at } y = " (fixed y-max 2))
                           :hyperbola (str "S - a\\,y = " (fixed left 2) "\\text{ exceeds, by a rectangle } " (fixed left-w 2) "\\times" (fixed y 2))))]
              [:blockquote {:style {:font-family "Georgia, serif" :font-style "italic" :opacity 0.85 :margin "0.5em 0" :font-size "0.95em"}}
               says " (Heath 1908, " prop ")"]
              [:div {:style {:font-size "0.85em" :opacity 0.75}}
               "Tie: y, a or k moves, the others held, S re-solved; S moves, a and k held, y re-solved "
               "(the near root; for k < 0 S stops at VI.27's greatest)."]]]])
         (finally (remove-watch prm ::tie))))))
