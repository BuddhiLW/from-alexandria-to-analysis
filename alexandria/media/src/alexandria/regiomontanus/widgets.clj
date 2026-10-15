(ns alexandria.regiomontanus.widgets
  "From chord to sine on the page (widget brief sections 4 and 5).

   `half-chord`   one circle, the arc 2a dragged on the figure or set from
                  any of four tied Leva controls (arc, Ptolemy's chord in
                  parts of 120, Aryabhata's jya in minutes of 3438, the
                  sine); the chord, the half-chord and the arc drawn, and
                  the value in both unit systems, sexagesimal and minutes,
                  live. Kernels: alexandria.regiomontanus.sines/tie,
                  /figure and /arc-of-point on the raster backend.
   `aryabhata`    the 24 sines built one step at a time by the rule of
                  differences (a raster step kernel iterated in the page),
                  divisor 225 (Aryabhata) or 233.5 (exact), each beside his
                  printed value and 3438 sin, the error named.
   `law-of-sines` the triangle in its circumscribed circle, vertices
                  draggable on the circle, A B C a b c R tied
                  (alexandria.regiomontanus.triangles/tie, /drag, /readout);
                  a/sin A = b/sin B = c/sin C = 2R with values.
   `solver`       pick SSS, SAS, ASA, AAS or SSA; three data controls; the
                  case named live; in SSA two, one or no triangles drawn.
   `steps`        the stepped definition: chord, half-chord, jya, sine.

   Every number on the page comes out of one of those kernels."
  (:require [alexandria.medium.flow :as flow]
            [alexandria.medium.tie :as tie]
            [alexandria.regiomontanus.sines :as s]
            [alexandria.regiomontanus.triangles :as t]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [nextjournal.clerk :as clerk]))

(def colours {:circle "#1F6FB4" :chord "#C0392B" :half "#2E8B57" :arc "#E6A700" :ink "#111111" :muted "#888888"})

(defn- katex [tex-form attr]
  (list 'let ['tex tex-form] [:div {attr 'tex} [:f> 'nextjournal.clerk.render/render-katex 'tex {:inline? false}]]))

(defn- kernel-binding [nm f init] (tie/kernel-binding nm f init))

;; ---------------------------------------------------------------------------
;; 1. The chord and the half-chord

(defn half-chord-init []
  (binding [alexandria.raster/*simplify?* false]
    (zipmap s/tied (alexandria.raster/value (fn [a] (s/of-arc a)) 80))))

(defn half-chord
  "One circle, the arc 2a: chord, half-chord, both unit systems, tied."
  ([] (half-chord (half-chord-init)))
  ([start]
   (binding [vc/*backend* :raster]
     (ev/with-let [!p start !s start]
       (let [st (mapv start s/tied)
             [tsym tform] (kernel-binding "tie" s/tie st)
             [fsym fform] (kernel-binding "fig" s/figure [(:arc start)])
             [dsym dform] (kernel-binding "drag" s/arc-of-point [0.5 0.5])
             opts {:tie-sym tsym :p !p :s !s :keys s/tied :ranges s/ranges :counter "__chordSolves"}
             g (fn [k] (list 'aget 'f (s/figure-index k)))
             v (fn [k] (list 'get '@?s k))]
         (flow/template
          {'?s !s '?p !p '?f fsym '?d dsym}
          (list 'reagent.core/with-let (into [tsym tform fsym fform dsym dform] (tie/locals opts))
                (list 'let ['f (list '?f '(array 0) (list 'array (v :arc)))
                            'sexa '(fn [x] (let [t (js/Math.round (* 3600 x)) p (quot t 3600) m (quot (rem t 3600) 60) sec (rem t 60)]
                                             (str p "ᵖ " m "′ " sec "″")))
                            'mins '(fn [x] (let [m (js/Math.floor x) sec (js/Math.round (* 60 (- x m)))] (str m "′ " sec "″")))
                            'drag! (list 'fn '[[x y]]
                                         (list 'let ['arc (list 'aget (list '?d '(array 0) '(array x y)) 0)]
                                               '(swap! ?p assoc :arc arc)))
                            'half-arc '(* (get @?s :arc) (/ js/Math.PI 360))
                            'arc-pts '(vec (for [i (range 41)] (let [t (- half-arc (* i (/ (* 2 half-arc) 40)))] [(js/Math.cos t) (js/Math.sin t)])))]
                      [:div {:data-widget "half-chord" :data-arc (list 'str (v :arc)) :data-crd (list 'str (v :crd)) :data-jya (list 'str (v :jya))}
                       [:style "[data-widget=\"half-chord\"] .MafsView text { pointer-events: none; }"]
                       [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                        [:div {:style {:flex "1 1 380px" :min-width "300px"}}
                         (mafs/mafs {:view-box {:x [-1.25 1.25] :y [-1.25 1.25]} :height 380}
                                    (mafs/circle {:center [0 0] :radius 1 :color (colours :circle) :fill-opacity 0})
                                    '[mafs.plot/Parametric {:t [0 1] :xy (fn [u] (let [t (- half-arc (* u 2 half-arc))] [(js/Math.cos t) (js/Math.sin t)]))
                                                           :color "#E6A700" :weight 6}]
                                    (list 'vector 'mafs.line/Segment {:point1 [0 0] :point2 [(g :Px) (g :Py)] :color (colours :muted) :style "dashed"})
                                    (list 'vector 'mafs.line/Segment {:point1 [0 0] :point2 [(g :Qx) (g :Qy)] :color (colours :muted) :style "dashed"})
                                    (list 'vector 'mafs.line/Segment {:point1 [0 0] :point2 [1 0] :color (colours :muted)})
                                    (list 'vector 'mafs.line/Segment {:point1 [(g :Qx) (g :Qy)] :point2 [(g :Px) (g :Py)] :color (colours :chord) :weight 3})
                                    (list 'vector 'mafs.line/Segment {:point1 [(g :Mx) 0] :point2 [(g :Px) (g :Py)] :color (colours :half) :weight 5})
                                    (mafs/text "P" {:x (g :Px) :y (g :Py) :attach "ne"})
                                    (mafs/text "Q" {:x (g :Qx) :y (g :Qy) :attach "se"})
                                    (mafs/text "M" {:x (g :Mx) :y 0 :attach "e"})
                                    (mafs/text "O" {:x 0 :y 0 :attach "sw"})
                                    (mafs/movable-point {:point [(g :Px) (g :Py)] :color (colours :chord) :on-move 'drag!})
                                    (mafs/movable-point {:point [(g :Qx) (g :Qy)] :color (colours :chord) :on-move 'drag!}))]
                        [:div {:style {:flex "0 1 340px" :min-width "260px"}}
                         (leva/sub-panel {:fill true :flat true :title-bar false}
                                         (leva/controls {:atom !p :schema (tie/schema s/tied s/ranges s/labels)}))
                         (katex (list 'str "\\operatorname{crd}(" (tie/fmt (v :arc) 2) "^\\circ) = 120\\sin("
                                      (tie/fmt '(/ (get @?s :arc) 2) 3) "^\\circ) = " (tie/fmt (v :crd) 4) "^{p}") :data-crd-tex)
                         (katex (list 'str "\\operatorname{jya}(" (tie/fmt '(/ (get @?s :arc) 2) 3) "^\\circ) = 3438\\sin("
                                      (tie/fmt '(/ (get @?s :arc) 2) 3) "^\\circ) = " (tie/fmt (v :jya) 2) "'") :data-jya-tex)
                         (katex (list 'str "\\tfrac12\\operatorname{crd} \\cdot \\tfrac{3438}{60} = "
                                      (tie/fmt (list '* (v :crd) (/ 3438 120.0)) 2) "' = \\operatorname{jya}") :data-same-tex)
                         [:table {:style {:font-size "0.9em" :margin-top "0.4em"}}
                          [:tbody
                           [:tr [:td "Ptolemy, chord PQ (parts of 120):"] [:td {:data-sexa true} (list 'sexa (v :crd))]]
                           [:tr [:td "Aryabhata, half-chord MP (minutes, R = 3438′):"] [:td {:data-minutes true} (list 'mins (v :jya))]]
                           [:tr [:td "sine (radius 1):"] [:td (tie/fmt (v :sin) 6)]]
                           [:tr [:td "cosine OM, versine M to the arc:"] [:td (tie/fmt (g :cos) 4) ", " (tie/fmt (g :versin) 4)]]]]]]
                       [:p {:style {:font-size "0.9em" :opacity 0.85}}
                        "Drag P or Q round the circle, or move any control: the arc, Ptolemy's chord, Aryabhata's jya and the sine are tied, each solved from the one you touch (arcsin as atan2 on raster). The red chord PQ subtends the arc 2a; the green half-chord MP subtends a. Ptolemy measures PQ in parts of which the diameter has 120; Aryabhata measures MP in minutes, on a radius of 3438 minutes, the radius measured by its own circle (21600′/2π)."]])
                (list 'finally (tie/unwatch-form !p)))))))))

;; ---------------------------------------------------------------------------
;; 2. Aryabhata's table by differences

(defn aryabhata
  "The 24 sines by the rule of differences, one step per click (or all),
   each beside Aryabhata's printed sine and 3438 sin, from raster."
  []
  (binding [vc/*backend* :raster]
    (let [rows (s/table)
          printed (mapv :printed rows)
          tru (mapv :raster rows)
          [ksym kform] [(gensym "step") (vc/compiled-fn (fn [divisor] (fn [[n d sn]] ((s/difference-step divisor) [n d sn]))) [225] [0 225 0] {:simplify? false})]
          first-exact (s/first-sine s/exact-divisor)]
      (ev/with-let [!a {:n 0 :divisor 225}]
        (flow/template
         {'?a !a '?k ksym}
         (list 'reagent.core/with-let [ksym kform]
               (list 'let ['dv '(get @?a :divisor)
                           's0 (list 'if '(== dv 225) 225 first-exact)
                           'built '(loop [i 0 st (array 0 s0 0) acc []]
                                     (if (< i (get @?a :n))
                                       (let [nx (?k st (array dv))] (recur (inc i) nx (conj acc [(aget nx 1) (aget nx 2)])))
                                       acc))
                           'printed printed 'tru tru
                           'last-row '(peek built)]
                     [:div {:data-widget "aryabhata" :data-n '(str (count built)) :data-divisor '(str dv)}
                      [:div {:style {:display "flex" :gap "0.5em" :flex-wrap "wrap" :align-items "center"}}
                       [:button {:type "button" :data-step "next" :on-click '(fn [] (swap! ?a update :n (fn [n] (min 24 (inc n)))))} "next sine"]
                       [:button {:type "button" :data-step "all" :on-click '(fn [] (swap! ?a assoc :n 24))} "all 24"]
                       [:button {:type "button" :data-step "reset" :on-click '(fn [] (swap! ?a assoc :n 0))} "start again"]
                       [:label [:input {:type "radio" :name "divisor" :data-divisor "225" :checked '(== dv 225)
                                        :on-change '(fn [_] (swap! ?a assoc :divisor 225))}] " divide by the first sine, 225 (Aryabhata)"]
                       [:label [:input {:type "radio" :name "divisor" :data-divisor "exact" :checked '(not (== dv 225))
                                        :on-change (list 'fn '[_] (list 'swap! '?a 'assoc :divisor s/exact-divisor))}]
                        " by 1/(2 − 2 cos 3°45′) = 233.5 (exact)"]]
                      '(when last-row
                         (let [i (dec (count built)) [d sn] last-row]
                           [:div {:style {:font-family "ui-monospace, monospace" :margin "0.4em 0"} :data-current true}
                            "sine " (inc i) " at " (.toFixed (* 3.75 (inc i)) 2) "°: difference " (.toFixed d 3)
                            " = previous difference − previous sine ÷ " (.toFixed dv 1) "; sine = " (.toFixed sn 3)
                            ";  printed " (nth printed i) ", 3438 sin = " (.toFixed (nth tru i) 3)
                            ";  error of the rule " (.toFixed (- sn (nth tru i)) 3) "′"]))
                      [:svg {:viewBox "0 0 480 220" :style {:width "100%" :max-width "640px"}}
                       '[:line {:x1 30 :y1 200 :x2 470 :y2 200 :stroke "#888"}]
                       '(into [:g] (for [i (range 24)]
                                     [:circle {:cx (+ 30 (* 18 (inc i))) :cy (- 200 (* 0.055 (nth tru i))) :r 2.2 :fill "#1F6FB4"}]))
                       '(into [:g] (for [[i [d sn]] (map-indexed vector built)]
                                     [:rect {:x (- (+ 30 (* 18 (inc i))) 4) :y (- 200 (* 0.055 sn)) :width 8 :height (* 0.055 sn)
                                             :fill "#E6A700" :opacity 0.6}]))]
                      [:table {:style {:font-size "0.85em"} :data-aryabhata-table true}
                       [:thead [:tr [:th "n"] [:th "arc"] [:th "difference"] [:th "sine by the rule"] [:th "printed"] [:th "3438 sin (raster)"] [:th "error of the rule"] [:th "error printed"]]]
                       '(into [:tbody]
                              (for [[i [d sn]] (map-indexed vector built)]
                                [:tr [:td (inc i)] [:td (.toFixed (* 3.75 (inc i)) 2) "°"] [:td (.toFixed d 2)] [:td (.toFixed sn 2)]
                                 [:td (nth printed i)] [:td (.toFixed (nth tru i) 2)]
                                 [:td {:data-error (str (- sn (nth tru i)))} (.toFixed (- sn (nth tru i)) 2)]
                                 [:td (.toFixed (- (nth printed i) (nth tru i)) 2)]]))]
                      [:p {:style {:font-size "0.9em" :opacity 0.85}}
                       "Each click runs one raster step kernel: the next difference is the previous one less the previous sine divided by the divisor, and the sine grows by it. With Aryabhata's divisor 225 the rule drifts below the sines (about a minute per step near 90°); his printed table does not, so he corrected it. With 233.5, which is 1/(2 − 2 cos 3°45′), the rule is the exact recurrence of the sine (Emmy's identity above) and lands on 3438 sin to the last digit. Blue dots: 3438 sin by raster; bars: the rule."]])))))))

;; ---------------------------------------------------------------------------
;; 3. The law of sines

(defn law-of-sines
  "The triangle in its circumscribed circle, A B C a b c R tied, vertices
   draggable on the circle."
  ([] (law-of-sines (t/initial-state)))
  ([start]
   (binding [vc/*backend* :raster alexandria.raster/*simplify?* false]
     (let [st (dissoc start :phi)]
       (ev/with-let [!p st !s st !phi {:phi (:phi start)}]
         (let [[tsym tform] (kernel-binding "tie" t/tie (mapv st t/tied))
               [psym pform] (kernel-binding "pos" (fn [phi A C] (fn [_] (t/positions phi A C))) [110 50 65])
               [rsym rform] (kernel-binding "read" t/readout [110 240 340 1])
               [dsym dform] (kernel-binding "drag" t/drag [110 240 340])
               opts {:tie-sym tsym :p !p :s !s :keys t/tied :ranges t/ranges :counter "__sinesSolves"}
               o (fn [k] (list 'aget 'o (t/readout-index k)))
               f4 (fn [x] (tie/fmt x 4))
               mover (fn [i label px py]
                       [:<>
                        (mafs/text label {:x px :y py :attach "ne"})
                        (mafs/movable-point {:point [px py] :color "#C0392B"
                                             :on-move (list 'fn '[[x y]] (list 'drag! i 'x 'y))})])]
           (flow/template
            {'?s !s '?p !p '?phi !phi '?pos psym '?read rsym '?drag dsym}
            (list 'reagent.core/with-let (into [tsym tform psym pform rsym rform dsym dform] (tie/locals opts))
                  (list 'let ['s '@?s
                              'tt '(?pos (array 0) (array (get @?phi :phi) (:A s) (:C s)))
                              'o '(?read (array 0) (array (aget tt 0) (aget tt 1) (aget tt 2) (:R s)))
                              'drag! '(fn [i x y]
                                        (let [s @?s
                                              tt (?pos (array 0) (array (get @?phi :phi) (:A s) (:C s)))
                                              t (* (/ 180 js/Math.PI) (js/Math.atan2 y x))
                                              _ (aset tt i t)
                                              out (?drag (array 0) tt)
                                              [A B C] [(aget out 0) (aget out 1) (aget out 2)]]
                                          ;; refuse a drag past a neighbour: every angle stays above 1 degree
                                          (when (and (> A 1) (> B 1) (> C 1))
                                            (swap! ?phi assoc :phi (aget out 3))
                                            (swap! ?p assoc :A A)
                                            (js/setTimeout (fn [] (swap! ?p assoc :B B)) 0))))]
                        [:div {:data-widget "law-of-sines" :data-ratio-a (list 'str (o :ra)) :data-ratio-b (list 'str (o :rb))
                               :data-ratio-c (list 'str (o :rc)) :data-diameter (list 'str (o :D))}
                         [:style "[data-widget=\"law-of-sines\"] .MafsView text { pointer-events: none; }"]
                         [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                          [:div {:style {:flex "1 1 400px" :min-width "300px"}}
                           (mafs/mafs {:view-box {:x [-2.2 2.2] :y [-2.2 2.2]} :height 420}
                                      '[mafs.core/Circle {:center [0 0] :radius (:R s) :color "#1F6FB4" :fillOpacity 0}]
                                      (list 'vector 'mafs.core/Polygon {:points [[(o :Ax) (o :Ay)] [(o :Bx) (o :By)] [(o :Cx) (o :Cy)]]
                                                                        :color "#C0392B" :fillOpacity 0.08})
                                      ;; the diameter through B: Regiomontanus' circle, BD = 2R
                                      (list 'vector 'mafs.line/Segment {:point1 [(o :Bx) (o :By)] :point2 [(list '- (o :Bx)) (list '- (o :By))]
                                                                        :color "#2E8B57" :style "dashed"})
                                      (mover 0 "A" (o :Ax) (o :Ay))
                                      (mover 1 "B" (o :Bx) (o :By))
                                      (mover 2 "C" (o :Cx) (o :Cy)))]
                          [:div {:style {:flex "0 1 340px" :min-width "260px"}}
                           (leva/sub-panel {:fill true :flat true :title-bar false}
                                           (leva/controls {:atom !p :schema (tie/schema t/tied t/ranges t/labels)}))
                           (katex (list 'str "\\frac{a}{\\sin A} = \\frac{" (f4 (o :a)) "}{\\sin " (tie/fmt (o :A) 2) "^\\circ} = " (f4 (o :ra))) :data-ra)
                           (katex (list 'str "\\frac{b}{\\sin B} = \\frac{" (f4 (o :b)) "}{\\sin " (tie/fmt (o :B) 2) "^\\circ} = " (f4 (o :rb))) :data-rb)
                           (katex (list 'str "\\frac{c}{\\sin C} = \\frac{" (f4 (o :c)) "}{\\sin " (tie/fmt (o :C) 2) "^\\circ} = " (f4 (o :rc))) :data-rc)
                           (katex (list 'str "2R = 2\\times" (f4 '(:R s)) " = " (f4 (o :D)) ",\\quad A+B+C = " (tie/fmt (o :sum) 2) "^\\circ") :data-2r)
                           [:div {:style {:font-weight 600 :margin-top "0.3em"} :data-triangle-kind
                                  (list 'cond (list '> (list 'max (o :A) (o :B) (o :C)) 90.05) "obtuse"
                                        (list '< (list 'js/Math.abs (list '- (list 'max (o :A) (o :B) (o :C)) 90)) 0.05) "right" :else "acute")}
                            (list 'cond (list '> (list 'max (o :A) (o :B) (o :C)) 90.05) "obtuse triangle: the centre lies outside it"
                                  (list '< (list 'js/Math.abs (list '- (list 'max (o :A) (o :B) (o :C)) 90)) 0.05) "right triangle: the hypotenuse is the diameter"
                                  :else "acute triangle: the centre lies inside it")]]]
                         [:p {:style {:font-size "0.9em" :opacity 0.85}}
                          "Drag a vertex round the circle, or move any control. The three angles, the three sides and R are tied: an angle moves with the first angle held (the third absorbs the change), a side moves its opposite angle through a = 2R sin A (the branch of the angle kept: obtuse stays obtuse), R scales the sides with the angles held. The ratios are measured on the drawn triangle by raster, not assumed; they agree with each other and with the diameter."]])
                  (list 'finally (tie/unwatch-form !p))))))))))

;; ---------------------------------------------------------------------------
;; 4. Solving a triangle

(defn solver
  "Choose the given data; the rest solved on raster, the case named."
  []
  (binding [vc/*backend* :raster alexandria.raster/*simplify?* false]
    (let [ids (mapv :id t/cases)
          ks (into {} (for [{:keys [id init]} t/cases]
                        [id (kernel-binding (name id) (t/solve-kernel id) init)]))
          start (into {:case "SSA"} (for [{:keys [id init]} t/cases] [id init]))
          meta (into {} (for [{:keys [id name data]} t/cases] [(clojure.core/name id) {:name name :data (mapv clojure.core/name data)}]))
          idx (fn [k] (t/solve-index k))
          rng (into {} (for [[k v] t/data-ranges] [(name k) v]))]
      (ev/with-let [!c start]
        (flow/template
         {'?c !c}
         (list 'reagent.core/with-let (vec (mapcat (fn [id] (ks id)) ids))
               (list 'let ['id '(get @?c :case)
                           'kid '(keyword id)
                           'xyz '(get @?c kid)
                           'k (list 'case 'id (name :SSS) (first (ks :SSS)) "SAS" (first (ks :SAS)) "ASA" (first (ks :ASA))
                                    "AAS" (first (ks :AAS)) (first (ks :SSA)))
                           'o '(k (array 0) (apply array xyz))
                           'g (list 'fn '[kw] (list 'aget 'o (list 'get (into {} (for [[k v] t/solve-index] [k v])) 'kw)))
                           'ok '(> (g :ok) 0.5)
                           'ok2 '(and (= id "SSA") (> (g :ok2) 0.5))
                           'verdict '(cond (not ok) "none" ok2 "two" :else "one")
                           'meta meta 'rng rng
                           'f '(fn [x] (if (js/isFinite x) (.toFixed x 3) "—"))
                           'sc '(/ 3.6 (max 0.001 (g :c) (g :b) (if ok2 (g :c2) 0)))
                           'C [(list '* 'sc (list 'g :Cx)) (list '* 'sc (list 'g :Cy))]]
                     [:div {:data-widget "solver" :data-case 'id :data-verdict 'verdict}
                      [:div {:style {:display "flex" :gap "0.4em" :flex-wrap "wrap"}}
                       (list 'into [:<>]
                             (list 'for ['c (mapv name ids)]
                                   '[:button {:type "button" :data-case-button c
                                              :style {:font-weight (if (= c id) 700 400) :border (if (= c id) "2px solid #C0392B" "1px solid #888")
                                                      :border-radius "0.4em" :padding "0.2em 0.6em"}
                                              :on-click (fn [] (swap! ?c assoc :case c))} c]))]
                      [:div {:style {:font-size "1.15em" :font-weight 600 :margin "0.4em 0"} :data-case-name 'id}
                       'id ": " '(get-in meta [id :name]) " — "
                       '(case verdict "none" "no triangle has these parts" "two" "two triangles (B and 180° − B)" "one triangle")]
                      [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                       [:div {:style {:flex "0 1 300px"}}
                        '(into [:div]
                               (for [[i nm] (map-indexed vector (get-in meta [id :data]))
                                     :let [[lo hi stp] (get rng nm)]]
                                 [:label {:style {:display "block"}}
                                  [:span {:style {:display "inline-block" :width "4em" :font-style "italic"}} nm (if (#{"A" "B" "C"} nm) " (°)" "")]
                                  [:input {:type "range" :min lo :max hi :step stp :value (nth xyz i) :data-given nm
                                           :on-change (fn [ev] (swap! ?c assoc-in [kid i] (js/parseFloat (.. ev -target -value))))}]
                                  " " (.toFixed (nth xyz i) 2)]))
                        [:table {:style {:font-size "0.9em"} :data-solution true}
                         [:tbody
                          '[:tr [:td "a, b, c"] [:td (f (g :a)) ", " (f (g :b)) ", " (f (g :c))]]
                          '[:tr [:td "A, B, C"] [:td (f (g :A)) "°, " (f (g :B)) "°, " (f (g :C)) "°"]]
                          '(when ok2 [:tr {:data-second true} [:td "second: c′, B′, C′"] [:td (f (g :c2)) ", " (f (g :B2)) "°, " (f (g :C2)) "°"]])
                          '(when (= id "SSA") [:tr [:td "sin B = b sin A / a"] [:td {:data-sinb (str (g :sinB))} (f (g :sinB))]])]]]
                       [:div {:style {:flex "1 1 380px" :min-width "280px"}}
                        (mafs/mafs {:view-box {:x [-0.5 4.5] :y [-0.8 3.6]} :height 340}
                                   '(when ok [mafs.core/Polygon {:points [[0 0] [(* sc (g :c)) 0] C] :color "#C0392B" :fillOpacity 0.1}])
                                   '(when ok2 [mafs.core/Polygon {:points [[0 0] [(* sc (g :c2)) 0] C] :color "#1F6FB4" :fillOpacity 0.1}])
                                   '(when (and (= id "SSA") ok) [mafs.core/Circle {:center C :radius (* sc (g :a)) :color "#888888" :fillOpacity 0 :strokeStyle "dashed"}])
                                   '(when ok [mafs.core/Text {:x 0 :y 0 :attach "sw"} "A"])
                                   '(when ok [mafs.core/Text {:x (first C) :y (second C) :attach "n"} "C"])
                                   '(when ok [mafs.core/Text {:x (* sc (g :c)) :y 0 :attach "se"} "B"])
                                   '(when ok2 [mafs.core/Text {:x (* sc (g :c2)) :y 0 :attach "s"} "B′"]))]]
                      [:p {:style {:font-size "0.9em" :opacity 0.85}}
                       "Pick which three parts are given. SSS and SAS go through the law of cosines, ASA and AAS through the law of sines, SSA through sin B = b sin A / a, which has two angles (B and 180° − B) with that sine: the dashed circle of radius a about C meets the line AB twice, once, or not at all. Every number comes from the raster kernel of the case chosen."]])))))))

;; ---------------------------------------------------------------------------
;; Derivations

(defn derivation
  "Emmy's identities behind a widget, with TeX and grades."
  [tex-forms graded derived]
  (clerk/col
   (apply clerk/col (map clerk/tex tex-forms))
   (clerk/html [:div {:style {:font-size "0.9em"}} "Emmy's own forms: "
                (into [:span] (interpose ", " (for [[k v] derived] [:code (str (name k) ": " v)])))])
   (clerk/html
    (into [:ul {:style {:font-size "0.9em"}}]
          (for [{:keys [label grade]} graded]
            [:li {:data-grade (name grade)} [:code (name grade)] " " label])))))

(defn sines-derivation []
  (derivation [(:chord s/tex) (:units s/tex) (:inverse s/tex) (:rule s/tex) (:exact s/tex)] (s/graded) (s/derived-tex)))

(defn triangles-derivation []
  (derivation [(:law t/tex) (:cosines t/tex) (:tie t/tex) (:ssa t/tex)] (t/graded) (t/derived-tex)))

;; ---------------------------------------------------------------------------
;; The definition, stepped: chord, half-chord, jya, sine

(def definition-steps
  "One sentence per step; :show names what the step adds to the figure."
  [{:who "Ptolemy, Almagest I.10, tr. Halma (1813), ch. IX"
    :text "« ... en partageant la circonférence en 360 degrés. » (... dividing the circumference into 360 degrees: our translation.)"
    :show #{:circle :arc}}
   {:who "Ptolemy, Almagest I.10, tr. Halma (1813), ch. IX"
    :text "« Faisant donc, comme je l'ai dit, le diamètre du cercle de 120 parties ... » (Making the diameter of the circle, as I said, 120 parts: our translation.) The straight line PQ under the arc 2a is its chord, crd 2a, in those parts."
    :show #{:circle :arc :chord}}
   {:who "Aryabhata, Aryabhatiya (499): the half-chord"
    :text "Halve the arc and the chord together: the half-chord MP stands on the half-arc a. The Sanskrit ardha-jya, 'half bowstring', shortened to jya."
    :show #{:circle :arc :chord :half}}
   {:who "Aryabhata, Aryabhatiya, Dasagitika 10 (Clark 1930, p. 19)"
    :text "The radius is measured in minutes of arc, 3438 of them, and the 24 half-chords of a quadrant are tabulated by their differences."
    :show #{:circle :arc :chord :half :radius}}
   {:who "Regiomontanus, De triangulis II (1533), p. 46"
    :text "« Sinum anguli, ut alibi vocamus, sinum arcus angulum ipsum subtendentis. » (We call the sine of an angle, as elsewhere, the sine of the arc that subtends that angle: our translation.)"
    :show #{:circle :arc :half :radius :angle}}])

(defn steps
  "The definition stepped: one sentence per step, highlighted beside one
   figure that grows with it (the arc of 80 degrees, points by raster)."
  []
  (binding [vc/*backend* :raster]
    (let [f (alexandria.raster/value (fn [a] ((s/figure a) 0)) 80)
          g (fn [k] (nth f (s/figure-index k)))
          [px py qx qy mx] (map g [:Px :Py :Qx :Qy :Mx])
          n (count definition-steps)]
      (ev/with-let [!i {:i 0}]
        (flow/template
         {'?i !i}
         (list 'let ['i '(get @?i :i)
                     'steps definition-steps
                     'show '(:show (nth steps i))
                     'on? '(fn [k] (contains? show k))]
               [:div {:data-widget "definition-steps" :data-step '(str i)}
                [:style "[data-widget=\"definition-steps\"] .MafsView text { pointer-events: none; }"]
                [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                 [:div {:style {:flex "1 1 340px" :min-width "280px"}}
                  (mafs/mafs {:view-box {:x [-1.25 1.25] :y [-1.25 1.25]} :height 340}
                             '(when (on? :circle) [mafs.core/Circle {:center [0 0] :radius 1 :color "#1F6FB4" :fillOpacity 0}])
                             (list 'when '(on? :arc)
                                   ['mafs.plot/Parametric {:t [0 1] :color "#E6A700" :weight 6
                                                           :xy (list 'fn '[u] (list 'let ['t (list '* (/ (* 40 Math/PI) 180) (list '- 1 (list '* 2 'u)))] '[(js/Math.cos t) (js/Math.sin t)]))}])
                             (list 'when '(on? :chord) ['mafs.line/Segment {:point1 [qx qy] :point2 [px py] :color "#C0392B" :weight 3}])
                             (list 'when '(on? :half) ['mafs.line/Segment {:point1 [mx 0] :point2 [px py] :color "#2E8B57" :weight 5}])
                             (list 'when '(on? :radius) ['mafs.line/Segment {:point1 [0 0] :point2 [px py] :color "#7B5EA7" :weight 2}])
                             (list 'when '(on? :angle) ['mafs.line/Segment {:point1 [0 0] :point2 [1 0] :color "#7B5EA7" :weight 2}])
                             (list 'when '(on? :circle) ['mafs.core/Text {:x 0 :y 0 :attach "sw"} "O"])
                             (list 'when '(on? :arc) ['mafs.core/Text {:x px :y py :attach "ne"} "P"])
                             (list 'when '(on? :chord) ['mafs.core/Text {:x qx :y qy :attach "se"} "Q"])
                             (list 'when '(on? :half) ['mafs.core/Text {:x mx :y 0 :attach "e"} "M"])
                             (list 'when '(on? :angle) ['mafs.core/Text {:x 0.25 :y 0.1 :attach "e"} "a"]))]
                 [:div {:style {:flex "1 1 320px"}}
                  '(into [:ol]
                         (for [[j {:keys [who text]}] (map-indexed vector steps)]
                           [:li {:data-step-sentence j
                                 :style {:opacity (if (<= j i) 1 0.35) :font-weight (if (= j i) 600 400)
                                         :background (if (= j i) "#E6A70022" "transparent") :padding "0.2em"}}
                            text [:br] [:small {:style {:opacity 0.7}} who]]))
                  [:div {:style {:display "flex" :gap "0.5em"}}
                   [:button {:type "button" :data-step-prev true :on-click '(fn [] (swap! ?i update :i (fn [k] (max 0 (dec k)))))} "previous"]
                   [:button {:type "button" :data-step-next true :on-click (list 'fn [] (list 'swap! '?i 'update :i (list 'fn '[k] (list 'min (dec n) '(inc k)))))} "next"]]]]]))))))
