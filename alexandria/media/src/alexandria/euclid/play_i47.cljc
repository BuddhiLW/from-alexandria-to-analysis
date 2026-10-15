(ns alexandria.euclid.play-i47
  "The I.47 playground: Euclid's figure with the three vertices A, B, C
   free. Drawn: the squares on BC, CA, AB (Heath's BDEC, HC, GB), AL
   through A parallel to BD cutting BC at K, the rectangles BL and CL of
   the windmill, and, as soon as the angle at A stops being right, Book
   II's perpendicular BD on CA and the rectangle CA.AD. Each layer can be
   switched off. Every point and number is a row of the raster kernel of
   alexandria.euclid.i47-playground/figure.

   The case is named live: I.47 (right at A, BL = AB^2 and CL = AC^2),
   II.12 (obtuse: BC^2 greater by 2 CA.AD), II.13 (acute: less by it), or
   no triangle (A, B, C on one line). The badge holds I.47's hypothesis:
   it turns red when the reader opens the angle, and says by how much the
   windmill fails (AB^2 - BL = AB.AC cos A)."
  (:require #?(:cljs [alexandria.medium.figure :as figure])
            #?(:cljs [reagent.core :as r])
            #?(:cljs [clojure.string :as str])))

#?(:cljs
   (do
     (def ^:private points
       [:A :B :C :ab1 :ab2 :ac1 :ac2 :bc1 :bc2 :K :L :D :r1 :r2])
     (def ^:private scalars
       [:a2 :b2 :c2 :BL :CL :dot :cos :angle :excess :ad])

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
     (defn- poly [ps colour & [{:keys [fill dashed?] :or {fill 0.22}}]]
       [:polygon {:points (pts ps) :fill colour :fill-opacity fill :stroke colour :stroke-width 0.05
                  :stroke-dasharray (when dashed? "0.2 0.12")}])
     (defn- seg [[x1 y1] [x2 y2] colour & [{:keys [dashed? width] :or {width 0.05}}]]
       [:line {:x1 x1 :y1 (- y1) :x2 x2 :y2 (- y2) :stroke colour :stroke-width width
               :stroke-dasharray (when dashed? "0.2 0.12")}])
     (defn- letter [[x y] s & [colour]]
       [:text {:x x :y (- y) :fill (or colour "#ECEEE4") :font-size 0.5 :text-anchor "middle"
               :dominant-baseline "middle" :font-family "Georgia, serif" :font-style "italic"} s])
     (defn- mid [[x1 y1] [x2 y2]] [(/ (+ x1 x2) 2) (/ (+ y1 y2) 2)])
     (defn- away [[x y] [cx cy] d]
       (let [dx (- x cx) dy (- y cy) n (max 1e-9 (js/Math.hypot dx dy))]
         [(+ x (* d (/ dx n))) (+ y (* d (/ dy n)))]))

     (def ^:private names
       {:right {:prop "I.47" :text "right-angled at A: the square on BC is equal to the squares on BA, AC"}
        :obtuse {:prop "II.12" :text "obtuse-angled at A: the square on BC is greater by twice CA·AD (D outside, on CA produced)"}
        :acute {:prop "II.13" :text "acute-angled at A: the square on BC is less by twice CA·AD (D within CA)"}
        :flat {:prop "no triangle" :text "A, B, C lie on one straight line: there is no angle at A and no proposition to apply"}})

     (defn render
       "Clerk render-fn. value: {:kernel :start :presets}."
       [{:keys [kernel start presets]}]
       (r/with-let [f (figure/kernel-figure kernel)
                    state (r/atom start)
                    layers (r/atom #{:squares :windmill :book2})
                    drag (atom nil) svg (atom nil)
                    idx {"A" 0 "B" 2 "C" 4}]
         (let [P (named (first (figure/points f @state [[0]])))
               {:keys [A B C ab1 ab2 ac1 ac2 bc1 bc2 K L D r1 r2 a2 b2 c2 BL CL dot cos angle excess ad]} P
               kind (cond (or (< angle 1) (> angle 179)) :flat
                          (< (js/Math.abs cos) 0.004) :right
                          (pos? cos) :acute :else :obtuse)
               ctr [(/ (+ (A 0) (B 0) (C 0)) 3) (/ (+ (A 1) (B 1) (C 1)) 3)]
               on? (fn [k] (contains? @layers k))
               {:keys [prop text]} (names kind)
               right? (= kind :right)]
           [:div {:data-playground "euclid-i47" :data-case (name kind)
                  :data-state (str/join " " @state)
                  :style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
            [:div {:style {:flex "1 1 460px" :min-width "300px"}}
             [:svg {:viewBox "-10 -14 24 22" :ref #(reset! svg %)
                    :style {:width "100%" :background "#1d1f21" :border-radius "6px" :touch-action "none" :user-select "none"}
                    :on-pointer-move (fn [ev]
                                       (when (and @drag @svg)
                                         (let [[x y] (at @svg ev) i (idx @drag)]
                                           (swap! state assoc i (max -9 (min 13 x)) (inc i) (max -7 (min 13 y))))))
                    :on-pointer-up #(reset! drag nil) :on-pointer-leave #(reset! drag nil)}
              (when (on? :squares)
                [:g (poly [B C bc1 bc2] "#B9A3E3") (poly [A B ab1 ab2] "#8FC7E8") (poly [A C ac1 ac2] "#F2D16B")])
              (when (and (on? :windmill) (not= kind :flat))
                [:g {:data-layer "windmill"}
                 (poly [B K L bc2] "#8FC7E8" {:fill 0.45}) (poly [K C bc1 L] "#F2D16B" {:fill 0.45})
                 (seg A L "#ECEEE4" {:dashed? true})
                 (seg ab2 C "#8FC7E8" {:width 0.035}) (seg A bc2 "#8FC7E8" {:width 0.035})
                 (seg ac2 B "#F2D16B" {:width 0.035}) (seg A bc1 "#F2D16B" {:width 0.035})
                 (letter (away K ctr 0.45) "K") (letter (away L ctr 0.45) "L")])
              (when (and (on? :book2) (#{:acute :obtuse} kind))
                [:g {:data-layer "book2"}
                 (poly [A D r1 r2] (if (= kind :obtuse) "#E8735A" "#7FD18B") {:fill 0.5 :dashed? (= kind :acute)})
                 (seg B D "#9aa7b4" {:dashed? true}) (seg A D "#9aa7b4")
                 (letter (away D ctr 0.5) "D" "#9aa7b4")])
              (poly [A B C] "#ECEEE4" {:fill 0.06})
              (letter (mid B bc1) "a²" "#B9A3E3") (letter (mid A ac1) "b²" "#F2D16B") (letter (mid B ab1) "c²" "#8FC7E8")
              (for [[nm p] [["A" A] ["B" B] ["C" C]]]
                ^{:key nm}
                [:g (letter (away p ctr 0.6) nm)
                 [:circle {:cx (p 0) :cy (- (p 1)) :r 0.3 :fill "#E8735A" :stroke "#fff" :stroke-width 0.05
                           :data-drag nm :style {:cursor "grab"}
                           :on-pointer-down (fn [ev] (.preventDefault ev) (reset! drag nm))}]])]
             [:div {:style {:font-size "0.8em" :opacity 0.7}} "drag A, B or C"]
             [:div {:style {:margin-top "0.3em"}}
              (for [[k label] [[:squares "squares on the sides"] [:windmill "AL and the rectangles BL, CL"] [:book2 "perpendicular BD, rectangle CA·AD"]]]
                ^{:key k}
                [:label {:style {:margin-right "1em" :font-size "0.85em"}}
                 [:input {:type "checkbox" :checked (on? k) :data-layer-toggle (name k)
                          :on-change #(swap! layers (if (on? k) disj conj) k)}] " " label])]]
            [:div {:style {:flex "1 1 300px" :min-width "260px" :font-family "ui-monospace, monospace" :font-size "0.92em" :line-height 1.6}}
             [:div {:data-live-case (name kind) :style {:font-size "1.2em" :font-weight 600}} prop]
             [:div {:style {:font-family "Georgia, serif"}} text]
             [:div {:data-readout "angle"} "∠BAC = " (fx angle 2) "°,  cos A = " (fx cos 4)]
             [:div {:data-readout "squares"} "a² = BC² = " (fx a2 3) ";  b² = CA² = " (fx b2 3) ";  c² = AB² = " (fx c2 3)]
             [:div {:data-readout "windmill"} "BL = " (fx BL 3) ",  CL = " (fx CL 3) ",  BL + CL = " (fx (+ BL CL) 3) " = a²"]
             [:div {:data-readout "excess"} "a² − (b² + c²) = " (fx excess 3) " = −2·CA·AD = −2·" (fx (js/Math.sqrt b2) 3) "·" (fx ad 3)]
             [:div {:data-badge (if right? "ok" "broken")
                    :style {:margin "0.5em 0" :padding "0.3em 0.6em" :border-radius "4px"
                            :background (if right? "#2f6b3a" "#7a2b22") :color "#fff"}}
              (if right?
                (str "I.47 holds: BL = AB² (" (fx BL 3) " = " (fx c2 3) "), CL = AC² (" (fx CL 3) " = " (fx b2 3) ")")
                (str "I.47 needs the angle at A right; you made it " (fx angle 1) "°. BL misses AB² by AB·AC·cos A = " (fx (- c2 BL) 3)
                     ", CL misses AC² by " (fx (- b2 CL) 3)))]
             [:div "Try this: "
              (for [[k label] [[:obtuse "Push B behind A (II.12)"] [:acute "Pull B over AC (II.13)"]
                               [:meno "Isosceles: Meno's doubled square"] [:flat "Flatten the triangle"] [:euclid "Reset to Euclid"]]]
                ^{:key k}
                [:button {:data-preset (name k) :style {:margin "0.2em"} :on-click #(reset! state (get presets k))} label])]
             [:div {:style {:font-size "0.85em" :opacity 0.75 :margin-top "0.4em"}}
              "Drag B until the badge turns green: the two halves of the windmill match the squares only at the right angle. "
              "With AB = AC the square on BC is twice the square on AB: Socrates' doubled square in the Meno."]]])))))
