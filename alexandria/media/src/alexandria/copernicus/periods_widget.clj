(ns alexandria.copernicus.periods-widget
  "Copernicus' periods and distances on the page.

   `synodic`: the Earth's year E, the planet's sidereal period P and the
   synodic period S tied as three Leva controls (alexandria.copernicus.orbits/tie,
   origin and epsilon guards from alexandria.medium.tie): move any and the
   others are solved. The successive conjunctions are drawn on the planet's
   orbit (orbits/conj-point), the relation printed with the current numbers,
   Copernicus' own synodic period from V.1 beside it.

   `side-by-side`: Copernicus' model and Ptolemy's on ONE time slider. Both
   are integrated by RK4 raster kernels in the page: the heliocentric flow
   (orbits/flow) and Ptolemy's epicycle on a deferent
   (alexandria.ptolemy.epicycle/flow) started by orbits/ptolemy-start at
   his free scale k with the epicycle m times what the Earth fixes. The
   angle between the two lines of sight (orbits/agreement) is shown at
   every instant and at its worst over the run; the case is named live.

   `distances`: the order of the planets. Drag the radius of any orbit or
   set its period; the epicycle ratio Ptolemy would need (orbits/regime) and
   the rule 'the farther, the slower' are checked live, the case named."
  (:require [alexandria.copernicus.orbits :as orb]
            [alexandria.medium.flow :as flow]
            [alexandria.medium.tie :as tie]
            [alexandria.ptolemy.epicycle :as ep]
            [alexandria.raster :as raster]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [nextjournal.clerk :as clerk]))

(def rows (delay (orb/table)))
(defn planet [id] (first (filter #(= id (:id %)) @rows)))

(defn- katex [tex-form attr]
  (list 'let ['tex tex-form] [:div {attr 'tex} [:f> 'nextjournal.clerk.render/render-katex 'tex {:inline? false}]]))

;; ---------------------------------------------------------------------------
;; Synodic and sidereal, tied

(defn init
  "The tied state for a planet of Copernicus' table: E = 1, his P, and S
   solved by the tie (evaluated on numbers; the page re-solves it in the
   raster kernel)."
  [id]
  (let [{:keys [P]} (planet id)
        out ((orb/tie 1.0 (double P) 1.0) nil)]
    (zipmap orb/tied (map double (subvec (vec out) 3 6)))))

(defn synodic
  "E, P, S tied; the conjunctions on the planet's orbit."
  ([] (synodic (init :mars)))
  ([start]
   (binding [vc/*backend* :raster]
     (ev/with-let [!p start !s start]
       (let [s (mapv start orb/tied)
             [tsym tform] (tie/kernel-binding "tie" orb/tie s)
             [rsym rform] (tie/kernel-binding "conj" orb/conjunctions s)
             [csym cform] (tie/kernel-binding "cpt" orb/conj-point [(:P start) (:S start) 1 0])
             [asym aform] (tie/kernel-binding "rad" orb/radius-of [(:P start)])
             opts {:tie-sym tsym :p !p :s !s :keys orb/tied :ranges orb/ranges :counter "__copernicusSolves"}
             n (fn [k d] (tie/fmt (list 'aget 'r (orb/tie-index k)) d))
             cop (into {} (map (juxt :id identity) @rows))
             presets (for [id [:mars :jupiter :saturn :venus :mercury]] [(str (:name (cop id)) " (V.1)") (init id)])]
         (flow/template
          {'?s !s '?p !p '?r rsym '?c csym '?a asym}
          (list 'reagent.core/with-let (into [tsym tform rsym rform csym cform asym aform] (tie/locals opts))
                (list 'let '[{:keys [E P S]} @?s
                             r (?r (array 0) (array E P S))
                             a (aget (?a (array 0) (array (/ P E))) 0)
                             side (if (pos? (aget r 4)) "outer" "inner")
                             pts (mapv (fn [k] (let [q (?c (array 0) (array P S a k))] [(aget q 0) (aget q 1)])) (range 0 8))
                             kind (cond (< (js/Math.abs (- P E)) 0.02) "P ≈ E: the planet keeps pace with the Earth; the synodic period runs off to infinity (capped at 60 years)"
                                        (> P E) "outer planet: 1/S = 1/E − 1/P; the Earth laps it once every S"
                                        :else "inner planet: 1/S = 1/P − 1/E; it laps the Earth once every S")]
                      [:div {:data-figure "copernicus-synodic" :data-case 'kind :data-side 'side
                             :data-residual '(str (aget r 3)) :data-s '(str S) :data-p '(str P)}
                       [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                        [:div {:style {:flex "1 1 360px" :min-width "300px"}}
                         (mafs/mafs {:view-box {:x '[(- (* 1.2 (max a 1))) (* 1.2 (max a 1))] :y '[(- (* 1.2 (max a 1))) (* 1.2 (max a 1))]} :height 360}
                                    '[mafs.core/Circle {:center [0 0] :radius 1 :color "#2E8B57" :fillOpacity 0}]
                                    '[mafs.core/Circle {:center [0 0] :radius a :color "#8A9BB0" :fillOpacity 0}]
                                    '[mafs.core/Point {:x 0 :y 0 :color "#E6A700"}]
                                    '(into [:<>] (map-indexed (fn [k [x y]]
                                                                [:<> {:key k}
                                                                 [mafs.line/Segment {:point1 [0 0] :point2 [x y] :color "#1F6FB4" :opacity 0.35}]
                                                                 [mafs.core/Point {:x x :y y :color (if (zero? k) "#C0392B" "#1F6FB4")}]
                                                                 [mafs.core/Text {:x x :y y :attach "ne"} (str k)]])
                                                              pts)))
                         [:div {:style {:font-size "0.85em" :opacity 0.75}}
                          "The Sun (gold), the Earth's orbit (green) and the planet's (grey, radius by P = a^{3/2}, Kepler's later rule, only to space the drawing). Points 0, 1, 2, …: the planet at successive conjunctions, each 360·S/P degrees on from the last."]]
                        [:div {:style {:flex "0 1 320px" :min-width "260px"}}
                         (leva/sub-panel {:fill true :flat true :title-bar false}
                                         (leva/controls {:atom !p :schema (tie/schema orb/tied orb/ranges orb/labels)}))
                         [:div {:style {:font-weight 600 :margin "0.3em 0"} :data-case-name 'kind} 'kind]
                         (katex (list 'if '(pos? (aget r 4))
                                      (list 'str "\\frac1S = \\frac1E - \\frac1P = " (n :invE 4) " - " (n :invP 4) " = " (n :invS 4))
                                      (list 'str "\\frac1S = \\frac1P - \\frac1E = " (n :invP 4) " - " (n :invE 4) " = " (n :invS 4)))
                                :data-formula)
                         [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.85em"}}
                          [:div "each conjunction advances " (n :advance 2) "° on the planet's orbit"]
                          [:div "the Earth runs " (n :laps 3) " years between conjunctions"]
                          [:div "residual |1/E − 1/P| − 1/S = " [:span {:data-tie-residual true} '(.toExponential (aget r 3) 2)]]]]]
                       [:p "Try this: load a planet's period from Copernicus' V.1 cycles, then move S and watch P follow."]
                       (into [:<>] (for [[label v] presets] (flow/button !p label v)))
                       (flow/button !p "Reset to Mars" start)])
                (list 'finally (tie/unwatch-form !p)))))))))

(defn table
  "Copernicus' V.1 cycles turned into periods by raster, beside the modern
   values and Ptolemy's epicycles turned into distances."
  []
  (clerk/html
   [:table {:data-table "copernicus-periods" :style {:font-size "0.9em"}}
    [:thead [:tr [:th "planet"] [:th "V.1 cycle"] [:th "synodic S (days)"] [:th "his printed S"]
             [:th "sidereal P (yr)"] [:th "modern P"] [:th "Ptolemy's epicycle (parts of 60)"] [:th "radius a (Earth = 1)"]]]
    (into [:tbody]
          (for [{:keys [name years extra laps revs S-days table P modern ptolemy a]} @rows]
            [:tr [:td name]
             [:td (format "%d yr %+.2f d: %d laps, %d revs" years (double extra) laps revs)]
             [:td (format "%.2f" S-days)] [:td table]
             [:td (format "%.4f" P)] [:td (str modern)]
             [:td (str ptolemy)] [:td (format "%.4f" a)]]))]))

;; ---------------------------------------------------------------------------
;; Ptolemy and Copernicus side by side

(def dt 0.004)

(defn- side-state [id]
  (let [{:keys [a P]} (planet id)] {:a a :P P :k 1.0 :m 1.0 :t 0.0 :years 3}))

(def side-schema
  {:t {:min 0 :max 6 :step 0.005 :label "time t (years), both models"}
   :a {:min 0.2 :max 10 :step 0.001 :label "planet's radius a"}
   :P {:min 0.1 :max 30 :step 0.001 :label "planet's period P"}
   :k {:min 0.2 :max 3 :step 0.01 :label "Ptolemy's deferent k (free scale)"}
   :m {:min 0 :max 2 :step 0.005 :label "epicycle ÷ what the Earth fixes, m"}
   :years {:min 1 :max 6 :step 0.5 :label "years integrated"}})

(defn side-by-side
  "One time slider, two models."
  ([] (side-by-side (side-state :mars)))
  ([start]
   (binding [vc/*backend* :raster]
     (ev/with-let [!s start]
       (let [{:keys [a P k m]} start
             [cs csf] (flow/step-kernel "cop" orb/flow [1 (/ 1 P) dt] [1 0 a 0 0])
             [ps psf] (flow/step-kernel "pto" ep/flow [(/ 1 P) 1 dt] [k 0 -1 0 0])
             [c0 c0f] (flow/kernel "c0" orb/start [a] [0])
             [p0 p0f] (flow/kernel "p0" orb/ptolemy-start [k a 1 m] [0])
             [ag agf] (flow/kernel "agree" orb/agreement [1 0 a 0 k 0 -1 0] [0])
             ic (gensym "icop") ip (gensym "ipto")
             presets [["Mars, same sky (m = 1)" (side-state :mars)]
                      ["Mars at Ptolemy's scale k = 2" (assoc (side-state :mars) :k 2.0)]
                      ["Venus (inner)" (side-state :venus)]
                      ["Jupiter, epicycle too small (m = 0.6)" (assoc (side-state :jupiter) :m 0.6)]
                      ["Mars, epicycle too large (m = 1.4)" (assoc (side-state :mars) :m 1.4)]]]
         (flow/template
          {'?s !s '?dt dt 'cs cs 'ps ps 'c0 c0 'p0 p0 'ag ag 'icop ic 'ipto ip}
          (list
           'reagent.core/with-let
           [cs csf ps psf c0 c0f p0 p0f ag agf ic (flow/integrate-form cs) ip (flow/integrate-form ps)
            'cache '(atom nil)
            'run '(fn [a P k m years]
                    (let [key [a P k m years]]
                      (if (= key (first @cache))
                        (second @cache)
                        (let [wP (/ 1 P) outer (if (> a 1) 1 0)
                              n (js/Math.ceil (/ years ?dt))
                              cp (icop (c0 (array 0) (array a)) (array 1 wP ?dt) n (fn [_ _] false))
                              pp (ipto (p0 (array 0) (array k a outer m))
                                       (if (= outer 1) (array wP 1 ?dt) (array 1 wP ?dt)) n (fn [_ _] false))
                              ags (mapv (fn [x y] (ag (array 0) (array (aget x 0) (aget x 1) (aget x 2) (aget x 3)
                                                                       (aget y 0) (aget y 1) (aget y 2) (aget y 3))))
                                        cp pp)
                              worst (reduce max 0 (map (fn [g] (js/Math.abs (aget g 4))) ags))
                              out {:cp cp :pp pp :ags ags :worst (* (/ 180 js/Math.PI) (js/Math.asin (min 1 worst)))}]
                          (reset! cache [key out])
                          out))))]
           (list
            'let '[s @?s
                   a (:a s) P (:P s) k (:k s) m (:m s) years (:years s)
                   t (min (:t s) years)
                   {:keys [cp pp ags worst]} (run a P k m years)
                   i (min (dec (count cp)) (js/Math.round (/ t ?dt)))
                   c (nth cp i) p (nth pp i) g (nth ags i)
                   E [(aget c 0) (aget c 1)] Pl [(aget c 2) (aget c 3)]
                   C [(aget p 0) (aget p 1)] Q [(+ (aget p 0) (aget p 2)) (+ (aget p 1) (aget p 3))]
                   delta (* (/ 180 js/Math.PI) (js/Math.asin (max -1 (min 1 (aget g 4)))))
                   ratio (aget g 5)
                   r-epi (js/Math.hypot (aget p 2) (aget p 3))
                   kind (cond (< worst 1e-4) "same sky: the two models put the planet on the same line of sight at every instant"
                              (< m 1) "the epicycle is smaller than the Earth's orbit makes it: Ptolemy's planet lags at retrograde"
                              :else "the epicycle is larger than the Earth's orbit makes it: the loops overshoot")
                   W (* 1.15 (max (+ a 1) (+ k r-epi)))
                   cpath (mapv (fn [j] (let [x (nth cp j)] [(- (aget x 2) (aget x 0)) (- (aget x 3) (aget x 1))])) (range 0 (count cp) 4))
                   ppath (mapv (fn [j] (let [x (nth pp j)] [(+ (aget x 0) (aget x 2)) (+ (aget x 1) (aget x 3))])) (range 0 (count pp) 4))]
            [:div {:data-playground "copernicus-side-by-side" :data-case 'kind :data-worst '(str worst) :data-delta '(str delta)}
             [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em"}}
              [:div {:style {:flex "1 1 300px" :min-width "260px"}}
               [:div {:style {:font-weight 600}} "Copernicus: the Sun at the centre"]
               (mafs/mafs {:view-box {:x '[(- W) W] :y '[(- W) W]} :height 300}
                          '[mafs.core/Circle {:center [0 0] :radius 1 :color "#2E8B57" :fillOpacity 0}]
                          '[mafs.core/Circle {:center [0 0] :radius a :color "#8A9BB0" :fillOpacity 0}]
                          '[mafs.core/Point {:x 0 :y 0 :color "#E6A700"}]
                          '[mafs.core/Point {:x (first E) :y (second E) :color "#2E8B57"}]
                          '[mafs.core/Point {:x (first Pl) :y (second Pl) :color "#1F6FB4"}]
                          '[mafs.line/Segment {:point1 E :point2 [(+ (first E) (* 2 (- (first Pl) (first E)))) (+ (second E) (* 2 (- (second Pl) (second E))))] :color "#2E8B57"}]
                          (mafs/movable-point {:point 'Pl :color "#1F6FB4"
                                               :on-move '(fn [[x y]] (swap! ?s assoc :a (max 0.2 (min 10 (js/Math.hypot x y)))))}))]
              [:div {:style {:flex "1 1 300px" :min-width "260px"}}
               [:div {:style {:font-weight 600}} "Ptolemy: the Earth at the centre"]
               (mafs/mafs {:view-box {:x '[(- W) W] :y '[(- W) W]} :height 300}
                          '[mafs.core/Circle {:center [0 0] :radius k :color "#8A9BB0" :fillOpacity 0}]
                          '[mafs.core/Circle {:center C :radius (max r-epi 1e-6) :color "#E6A700" :fillOpacity 0}]
                          '[mafs.core/Polyline {:points ppath :color "#8E44AD" :weight 1.5}]
                          '[mafs.core/Polyline {:points (mapv (fn [[x y]] [(* x (/ k (max 1e-6 (if (> a 1) a 1)))) (* y (/ k (max 1e-6 (if (> a 1) a 1))))]) cpath)
                                                :color "#1F6FB4" :weight 1 :strokeOpacity 0.5}]
                          '[mafs.core/Point {:x 0 :y 0 :color "#2E8B57"}]
                          '[mafs.core/Point {:x (first Q) :y (second Q) :color "#8E44AD"}]
                          '[mafs.line/Segment {:point1 [0 0] :point2 [(* 2 (first Q)) (* 2 (second Q))] :color "#8E44AD"}]
                          (mafs/movable-point {:point '[k 0] :color "#8A9BB0"
                                               :constrain '(fn [[x _]] [(max 0.2 (min 3 x)) 0])
                                               :on-move '(fn [[x _]] (swap! ?s assoc :k (max 0.2 (min 3 x))))}))
               [:div {:style {:font-size "0.85em" :opacity 0.75}}
                "Purple: Ptolemy's planet by RK4 on the epicycle flow; faint blue: Copernicus' G = P − E scaled to his deferent. Drag the grey point to change k: the sky does not change."]]
              [:div {:style {:flex "0 1 300px" :min-width "250px"}}
               (leva/sub-panel {:fill true :flat true :title-bar false} (leva/controls {:atom !s :schema side-schema}))
               [:div {:style {:font-weight 600 :margin "0.3em 0"} :data-case-name 'kind} 'kind]
               [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.85em"}}
                [:div "angle between the lines of sight now: " (flow/fmt 'delta 6) "°"]
                [:div "worst over the run: " [:span {:data-worst true} (flow/fmt 'worst 6)] "°"]
                [:div "Ptolemy's distance ÷ Copernicus': " (flow/fmt 'ratio 4) " (his scale is free)"]]]]
             [:p "Try this:"]
             (into [:<>] (for [[label v] presets] (flow/button !s label v)))
             (flow/button !s "Reset" start)]))))))))

;; ---------------------------------------------------------------------------
;; The order of the planets

(def order-ids [:saturn :jupiter :mars :venus :mercury])

(defn order-state
  "Ptolemy's epicycles (parts of 60 over 60) and Copernicus' periods."
  []
  (into {} (mapcat (fn [{:keys [id ptolemy P]}]
                     [[(keyword (str (name id) "-q")) (/ ptolemy 60.0)] [(keyword (str (name id) "-P")) P]])
                   @rows)))

(def order-start (delay (order-state)))

(def order-schema
  (delay
    (into {} (mapcat (fn [{:keys [id name]}]
                       [[(keyword (str (clojure.core/name id) "-q")) {:min 0.01 :max 1 :step 0.0005 :label (str name ": epicycle r/R")}]
                        [(keyword (str (clojure.core/name id) "-P")) {:min 0.1 :max 40 :step 0.001 :label (str name ": period P (yr)")}]])
                     @rows))))

(defn distances
  "The order of the planets the moving Earth fixes: drag any orbit's radius
   (it sets Ptolemy's epicycle ratio) or any period; the order by distance
   and by period are compared live."
  ([] (distances @order-start))
  ([start]
   (binding [vc/*backend* :raster]
     (ev/with-let [!s start]
       (let [qk (mapv #(keyword (str (name %) "-q")) order-ids)
             pk (mapv #(keyword (str (name %) "-P")) order-ids)
             [rsym rform] (tie/kernel-binding "radii" orb/radii (mapv start (concat qk pk)))
             names (mapv (comp :name planet) order-ids)
             outer? [true true true false false]
             presets [["Ptolemy's epicycles, Copernicus' periods" start]
                      ["Make Mars's epicycle smaller than Jupiter's" (assoc start :mars-q 0.15)]
                      ["Give Venus a period longer than the Earth's" (assoc start :venus-P 1.6)]
                      ["Swap the periods of Jupiter and Saturn" (assoc start :jupiter-P (:saturn-P start) :saturn-P (:jupiter-P start))]]]
         (flow/template
          {'?s !s '?r rsym '?qk qk '?pk pk '?names names '?outer outer?}
          (list 'reagent.core/with-let [rsym rform]
                (list 'let '[s @?s
                             o (?r (array 0) (into-array (concat (map s ?qk) (map s ?pk))))
                             rows (mapv (fn [i] {:i i :name (nth ?names i) :a (aget o i) :P (get s (nth ?pk i)) :v (aget o (+ 5 i))})
                                        (range 5))
                             rows (conj rows {:i 5 :name "Earth" :a 1 :P 1 :v (* 2 js/Math.PI)})
                             by-a (sort-by :a rows)
                             by-P (sort-by :P rows)
                             same? (= (map :name by-a) (map :name by-P))
                             outer-ok? (every? (fn [i] (> (aget o i) 1)) [0 1 2])
                             inner-ok? (every? (fn [i] (< (aget o i) 1)) [3 4])
                             kind (cond (not (and outer-ok? inner-ok?)) "broken: an outer planet's orbit inside the Earth's (or an inner one outside); its epicycle would no longer be the Earth's orbit"
                                        same? "Copernicus' order: the farther from the Sun, the longer the period"
                                        :else "the order by distance and the order by period disagree: no single rule ties size and motion")
                             W (* 1.1 (reduce max 1 (map :a rows)))
                             wl (js/Math.log W)
                             sc (fn [a] (/ (js/Math.log (+ 1 a)) (js/Math.log (+ 1 W))))
                             ;; the epicycle ratio a dragged point stands for:
                             ;; q is clamped to [0.01, 1], so an inner planet
                             ;; stops at the Earth's orbit (a = q <= 1) and an
                             ;; outer one at it from outside (a = 1/q >= 1)
                             q-at (fn [i [x y]]
                                    (let [rr (min 0.999 (js/Math.hypot x y))
                                          a' (max 0.01 (- (js/Math.pow (+ 1 W) rr) 1))]
                                      (max 0.01 (min 1 (if (nth ?outer i) (/ 1 a') a')))))]
                      [:div {:data-playground "copernicus-order" :data-case 'kind :data-same '(str same?)
                             :data-order '(clojure.string/join "," (map :name by-a))}
                       [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                        [:div {:style {:flex "1 1 380px" :min-width "300px"}}
                         (mafs/mafs {:view-box {:x [-1.1 1.1] :y [-1.1 1.1]} :height 380}
                                    '[mafs.core/Point {:x 0 :y 0 :color "#E6A700"}]
                                    '(into [:<>]
                                           (for [{:keys [i name a]} rows
                                                 :let [r (sc a) ang (* i 1.05)]]
                                             [:<> {:key name}
                                              [mafs.core/Circle {:center [0 0] :radius r :color (if (= name "Earth") "#2E8B57" "#8A9BB0") :fillOpacity 0}]
                                              [mafs.core/Text {:x (* r (js/Math.cos ang)) :y (* r (js/Math.sin ang)) :attach "ne"} name]]))
                                    ;; the point stays on its planet's orbit: past the clamp
                                    ;; (an inner planet dragged beyond the Earth) it used to
                                    ;; leave the orbit while the state stood still
                                    '(into [:<>]
                                           (for [{:keys [i name a]} (butlast rows)
                                                 :let [r (sc a) ang (* i 1.05)]]
                                             [mafs.core/MovablePoint
                                              {:key name :point [(* r (js/Math.cos ang)) (* r (js/Math.sin ang))] :color "#1F6FB4"
                                               :constrain (fn [p]
                                                            (let [q (q-at i p)
                                                                  r' (sc (if (nth ?outer i) (/ 1 q) q))
                                                                  t (js/Math.atan2 (second p) (first p))]
                                                              [(* r' (js/Math.cos t)) (* r' (js/Math.sin t))]))
                                               :on-move (fn [p] (swap! ?s assoc (nth ?qk i) (q-at i p)))}])))
                         [:div {:style {:font-size "0.85em" :opacity 0.75}}
                          "The orbits on a logarithmic scale, the Sun at the centre, the Earth green. Drag a planet's point in or out: its radius sets the epicycle ratio Ptolemy would need (outer: r/R = 1/a; inner: r/R = a)."]]
                        [:div {:style {:flex "0 1 340px" :min-width "260px"}}
                         [:div {:style {:font-weight 600 :margin "0.3em 0"} :data-case-name 'kind} 'kind]
                         [:table {:style {:font-size "0.85em"}}
                          [:thead [:tr [:th "by distance"] [:th "a"] [:th "P (yr)"] [:th "speed 2πa/P"]]]
                          '(into [:tbody] (for [{:keys [name a P v]} by-a]
                                            [:tr {:key name} [:td name] [:td (.toFixed a 3)] [:td (.toFixed P 3)] [:td (.toFixed v 3)]]))]
                         (leva/sub-panel {:fill true :flat true :title-bar false} (leva/controls {:atom !s :schema @order-schema}))]]
                       [:p "Try this:"]
                       (into [:<>] (for [[label v] presets] (flow/button !s label v)))
                       (flow/button !s "Reset to Ptolemy's epicycles and Copernicus' periods" start)]))))))))
