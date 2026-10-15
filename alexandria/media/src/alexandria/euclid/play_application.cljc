(ns alexandria.euclid.play-application
  "The application-of-areas playground (I.44, VI.27-29): the line AB, the
   applied parallelogram's height and angle, the shape of the leftover and
   the given area all free. Drag X along the line (the base AX), A' up and
   down (the height y), B (the line a), the angle handle (theta); Leva
   holds the same five numbers. Drawn: the applied parallelogram AX X'A',
   the leftover XB B'X' (defect dashed, excess solid), VI.27's greatest
   parallelogram dashed in grey, and the parallelogram that applies the
   given area S0 (thin outline). Every point and number is a row of the
   raster kernel of alexandria.euclid.application-playground/figure.

   The name is live (elleipsis / parabole / hyperbole, VI.28 / I.44 /
   VI.29); the badge holds VI.28's diorismos: it turns red when S0 passes
   VI.27's greatest, and says so."
  (:require #?(:cljs [alexandria.medium.figure :as figure])
            #?(:cljs [leva.core :as leva])
            #?(:cljs [reagent.core :as r])
            #?(:cljs [clojure.string :as str])))

#?(:cljs
   (do
     (def ^:private points [:A :X :X' :A' :B :B' :G :G' :Gt :Y :Y' :Yt])
     (def ^:private scalars [:base :S :left :S-max :y-max :y0 :residual :sin])

     (defn- named [row]
       (let [n (count points)]
         (merge (zipmap points (map vec (partition 2 (take (* 2 n) row))))
                (zipmap scalars (drop (* 2 n) row)))))

     (defn- fx [x d] (if (js/isFinite x) (str/replace (.toFixed (js/Number x) d) "-" "−") "—"))

     (defn- at [svg ev]
       (let [p (.createSVGPoint svg)]
         (set! (.-x p) (.-clientX ev)) (set! (.-y p) (.-clientY ev))
         (let [q (.matrixTransform p (.inverse (.getScreenCTM svg)))]
           [(.-x q) (- (.-y q))])))

     (defn- pts [ps] (str/join " " (map (fn [[x y]] (str x "," (- y))) ps)))
     (defn- poly [ps colour & [{:keys [fill dashed? width] :or {fill 0.3 width 0.05}}]]
       [:polygon {:points (pts ps) :fill colour :fill-opacity fill :stroke colour :stroke-width width
                  :stroke-dasharray (when dashed? "0.2 0.12")}])
     (defn- letter [[x y] s & [colour]]
       [:text {:x x :y (- y) :fill (or colour "#ECEEE4") :font-size 0.5 :text-anchor "middle"
               :dominant-baseline "middle" :font-family "Georgia, serif" :font-style "italic"} s])
     (defn- handle [[x y] nm drag]
       [:circle {:cx x :cy (- y) :r 0.28 :fill "#E8735A" :stroke "#fff" :stroke-width 0.05
                 :data-drag nm :style {:cursor "grab"}
                 :on-pointer-down (fn [ev] (.preventDefault ev) (reset! drag nm))}])

     (def ^:private props
       {:ellipse "VI.28: falling short" :parabola "I.44: applied exactly" :hyperbola "VI.29: exceeding"})

     (defn render
       "Clerk render-fn. value: {:kernel :order :ranges :start :presets :names}."
       [{:keys [kernel order ranges start presets names]}]
       (r/with-let [f (figure/kernel-figure kernel)
                    prm (r/atom start)
                    layers (r/atom #{:leftover :greatest :given})
                    drag (atom nil) svg (atom nil)
                    clamp (fn [k v] (let [[lo hi] (get ranges k)] (max lo (min hi v))))]
         (let [{:keys [a y k theta S0] :as st} @prm
               P (named (first (figure/points f (mapv st order) [[0]])))
               {:keys [A X X' A' B B' G G' Gt Y Y' Yt base S left S-max y-max y0 residual sin]} P
               kind (cond (< (js/Math.abs k) 0.005) :parabola (pos? k) :hyperbola :else :ellipse)
               {:keys [greek latin meaning]} (get names kind)
               possible? (or (>= k -0.005) (<= S0 (+ S-max 1e-9)))
               solved? (< (js/Math.abs residual) 0.05)
               on? (fn [l] (contains? @layers l))
               angle-handle (let [rr 1.6 t (* theta (/ js/Math.PI 180))] [(* rr (js/Math.cos t)) (* rr (js/Math.sin t))])]
           [:div {:data-playground "euclid-application" :data-case (name kind)
                  :data-possible (str possible?)
                  :data-state (str/join " " (map #(get st %) order))
                  :style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
            [:div {:style {:flex "1 1 480px" :min-width "300px"}}
             [:svg {:viewBox "-3 -11 26 13" :ref #(reset! svg %)
                    :style {:width "100%" :background "#1d1f21" :border-radius "6px" :touch-action "none" :user-select "none"}
                    :on-pointer-move
                    (fn [ev]
                      (when (and @drag @svg)
                        (let [[x yy] (at @svg ev)]
                          (swap! prm
                                 (fn [{:keys [a y k] :as m}]
                                   (case @drag
                                     "B" (assoc m :a (clamp :a x))
                                     "A'" (assoc m :y (clamp :y (/ yy (max 0.05 sin))))
                                     "X" (assoc m :k (clamp :k (/ (- x a) (max 0.05 y))))
                                     "theta" (assoc m :theta (clamp :theta (* (/ 180 js/Math.PI) (js/Math.atan2 (max 0.01 yy) x))))
                                     m))))))
                    :on-pointer-up #(reset! drag nil) :on-pointer-leave #(reset! drag nil)}
              (when (and (on? :greatest) (neg? k) (< k -0.005))
                [:g {:data-layer "greatest"} (poly [A G G' Gt] "#9aa7b4" {:fill 0.06 :dashed? true :width 0.035})])
              (poly [A X X' A'] "#F2D16B" {:fill 0.35})
              (when (and (on? :leftover) (not= kind :parabola))
                [:g {:data-layer "leftover"}
                 (if (= kind :ellipse)
                   (poly [X B B' X'] "#7FD18B" {:fill 0.35 :dashed? true})
                   (poly [B X X' B'] "#E8735A" {:fill 0.4}))])
              (when (on? :given)
                [:g {:data-layer "given"} (poly [A Y Y' Yt] "#B9A3E3" {:fill 0 :width 0.07})])
              [:line {:x1 -1 :y1 0 :x2 24 :y2 0 :stroke "#555" :stroke-width 0.03}]
              [:line {:x1 0 :y1 0 :x2 (B 0) :y2 0 :stroke "#ECEEE4" :stroke-width 0.09}]
              (letter [-0.4 -0.5] "A") (letter [(B 0) -0.6] "B") (letter [(X 0) -1.1] "X" "#F2D16B")
              (letter [(- (A' 0) 0.5) (A' 1)] "A′")
              (letter [(/ (+ (X 0) (X' 0)) 2) (/ (X' 1) 2)] "S")
              (handle B "B" drag) (handle [(X 0) (- (X 1) 0.0)] "X" drag) (handle A' "A'" drag) (handle angle-handle "theta" drag)
              (letter [(+ (angle-handle 0) 0.55) (+ (angle-handle 1) 0.2)] "θ" "#E8735A")]
             [:div {:style {:font-size "0.8em" :opacity 0.7}} "drag B (the line), X (the base), A′ (the height), θ (the angle)"]
             [:div
              (for [[l label] [[:leftover "defect / excess XB"] [:greatest "VI.27's greatest"] [:given "parallelogram of area S₀"]]]
                ^{:key l}
                [:label {:style {:margin-right "1em" :font-size "0.85em"}}
                 [:input {:type "checkbox" :checked (on? l) :data-layer-toggle (name l)
                          :on-change #(swap! layers (if (on? l) disj conj) l)}] " " label])]]
            [:div {:style {:flex "1 1 300px" :min-width "260px"}}
             [leva/SubPanel {:fill true :flat true :titleBar false}
              [leva/Controls {:atom prm
                              :schema (into {} (for [kk order :let [[lo hi s] (get ranges kk)]]
                                                 [kk {:min lo :max hi :step s
                                                      :label (case kk :a "a = AB" :y "y = AA′" :k "k (shape of XB)" :theta "angle θ °" :S0 "S₀ (given area)")}]))}]]
             [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.92em" :line-height 1.6}}
              [:div {:data-live-case (name kind) :style {:font-size "1.2em" :font-weight 600}}
               (str latin " (" greek "): " meaning)]
              [:div (get props kind)]
              [:div {:data-readout "area"} "S = AX·y·sin θ = " (fx base 3) "·" (fx y 3) "·" (fx sin 4) " = " (fx S 3)]
              [:div {:data-readout "leftover"} (case kind :ellipse "defect XB = " :hyperbola "excess XB = " "leftover = ") (fx (js/Math.abs left) 3)
               ",  strip a·y·sin θ = " (fx (- S left) 3)]
              [:div {:data-readout "given"} "for S₀ = " (fx S0 2) ":  y = " (fx y0 4) (if solved? "  (your y applies it)" (str ",  S − S₀ = " (fx residual 3)))]
              [:div {:data-badge (if possible? "ok" "broken")
                     :style {:margin "0.5em 0" :padding "0.3em 0.6em" :border-radius "4px"
                             :background (if possible? "#2f6b3a" "#7a2b22") :color "#fff"}}
               (cond
                 (not= kind :ellipse) "No limit: an exceeding or exact application exists for every area (I.44, VI.29)."
                 possible? (str "VI.28 is possible: S₀ = " (fx S0 2) " ≤ a² sin θ / 4|k| = " (fx S-max 3) " (VI.27, at y = " (fx y-max 3) ")")
                 :else (str "VI.28 fails its diorismos: S₀ = " (fx S0 2) " is greater than VI.27's greatest " (fx S-max 3)
                            ". No parallelogram falling short by a figure of this shape has that area."))]
              [:div "Try this: "
               (for [[pk label] [[:i44 "k = 0, a slanted angle (I.44)"] [:greatest "The greatest: X at the middle (VI.27)"]
                                 [:impossible "Ask for more than the greatest"] [:exceed "Run past B (VI.29)"] [:euclid "Reset to VI.28"]]]
                 ^{:key pk}
                 [:button {:data-preset (name pk) :style {:margin "0.2em"} :on-click #(reset! prm (get presets pk))} label])]
              [:div {:style {:font-size "0.85em" :opacity 0.75 :margin-top "0.4em"}}
               "Drag X through B and watch the name pass from elleipsis to parabolē to hyperbolē. "
               "Falling short, slide A′ up: the applied area grows, then shrinks; its top is VI.27's greatest."]]]])))))
