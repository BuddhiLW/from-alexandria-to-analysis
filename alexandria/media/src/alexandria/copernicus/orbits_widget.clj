(ns alexandria.copernicus.orbits-widget
  "Copernicus' moving Earth on the page (playground brief).

   `playground`: the Earth on its circle (radius 1, one turn a year) and a
   planet on radius a with period P, INTEGRATED in the page: one RK4 step
   of alexandria.copernicus.orbits/flow is a raster kernel
   (alexandria.medium.flow), iterated over the chosen years, the longitude
   seen from the Earth riding along as the fifth coordinate. A frame slider
   f carries the picture from the Sun's frame (f = 1, Copernicus) to the
   Earth's (f = 0, Ptolemy): the same states, drawn about O = (1 - f) E by
   the view kernel. The retrograde stretches are red and named live (their
   stations, width and days, by alexandria.ptolemy.epicycle/arc). The reader
   DRAGS the planet (its distance from the Sun sets a) and the time marker
   on the rate plot. The case is named from the regime kernel: loops when
   the inner body is the faster, none when the reader makes an outer planet
   outrun the Earth.

   `synodic`: E, P and S tied (orbits/tie): move any, the others are
   solved; the conjunctions drawn on the planet's orbit by conj-point.

   `side-by-side`: the heliocentric model and Ptolemy's (the epicycle flow
   of alexandria.ptolemy.epicycle, integrated by the same RK4 kernel
   machinery) on one time slider, Ptolemy at his own free scale k with the
   epicycle m times what the Earth's orbit fixes; the angle between the two
   lines of sight from orbits/agreement, the case named live.

   `distances`: the order and sizes of the orbits that only the
   heliocentric model fixes, from Copernicus' V.1 periods and Ptolemy's
   epicycles (orbits/table, raster).

   Every number shown comes from a raster kernel."
  (:require [alexandria.copernicus.orbits :as orb]
            [alexandria.medium.flow :as flow]
            [alexandria.medium.tie :as tie]
            [alexandria.ptolemy.epicycle :as ep]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [nextjournal.clerk :as clerk]))

(def dt "RK4 step, years." 0.004)
(def samples "Points drawn from each RK4 run." 480)

(def rows "Copernicus' table (V.1), by raster." (delay (orb/table)))

(defn planet [id] (first (filter #(= id (:id %)) @rows)))

(defn- preset-of [id years]
  (let [{:keys [a P]} (planet id)] {:a a :P P :years years :t 0.0 :f 0.0}))

(def colours {:sun "#E6A700" :earth "#2E8B57" :planet "#1F6FB4" :retro "#C0392B" :orbit "#8A9BB0" :ptolemy "#8E44AD"})

(defn- katex [tex-form attr]
  (list 'let ['tex tex-form] [:div {attr 'tex} [:f> 'nextjournal.clerk.render/render-katex 'tex {:inline? false}]]))

(def arcs-form
  "Browser form: the retrograde arcs [i j] of the sampled rates lam'."
  '(loop [i 1 open nil acc []]
     (if (>= i (count lam'))
       (if open (conj acc [open (dec (count lam'))]) acc)
       (let [a (nth lam' (dec i)) b (nth lam' i)]
         (cond (and (>= a 0) (< b 0)) (recur (inc i) i acc)
               (and open (< a 0) (>= b 0)) (recur (inc i) nil (conj acc [open i]))
               :else (recur (inc i) open acc))))))

;; ---------------------------------------------------------------------------
;; 1. Retrograde loops from a moving Earth

(def start-state (assoc (preset-of :mars 4) :play true))

(def presets
  [["Mars (V.1)" (preset-of :mars 4)]
   ["Jupiter (V.1)" (preset-of :jupiter 4)]
   ["Saturn (V.1)" (preset-of :saturn 4)]
   ["Venus, an inner planet" (preset-of :venus 4)]
   ["Mercury" (preset-of :mercury 2)]
   ["Ptolemy's view of Mars (f = 0)" (assoc (preset-of :mars 4) :f 0.0)]
   ["Copernicus' view (f = 1)" (assoc (preset-of :mars 4) :f 1.0)]
   ["Outer, but faster than the Earth" {:a 1.5 :P 0.5 :years 4 :t 0.0 :f 0.0}]])

(def schema
  {:a {:min 0.2 :max 10 :step 0.001 :label "planet's radius a (Earth = 1)"}
   :P {:min 0.1 :max 30 :step 0.001 :label "planet's period P (years)"}
   :years {:min 1 :max 12 :step 0.5 :label "years integrated"}
   :t {:min 0 :max 12 :step 0.005 :label "time t (years)"}
   :f {:min 0 :max 1 :step 0.01 :label "frame: 0 Earth (Ptolemy), 1 Sun (Copernicus)"}
   :play {:label "run"}})

(defn playground
  "Retrograde loops from a moving Earth (Mars by default)."
  ([] (playground start-state))
  ([start]
   (binding [vc/*backend* :raster]
     (ev/with-let [!s start]
       (let [{:keys [a P]} start
             [st sf] (flow/step-kernel "cop" orb/flow [1 (/ 1 P) dt] [1 0 a 0 0])
             [s0 s0f] (flow/kernel "start" orb/start [a] [0])
             [rk rkf] (flow/kernel "rate" orb/rate [1 (/ 1 P)] [1 0 a 0 0])
             [vw vwf] (flow/kernel "view" orb/view [0 1] [1 0 a 0 0])
             [rg rgf] (flow/kernel "regime" orb/regime [a 1 (/ 1 P)] [0])
             [gp gpf] (flow/kernel "gap" orb/gap [a 1 (/ 1 P) 0 0 0] [0])
             [ar arf] (flow/kernel "arc" ep/arc [0 0 0 1] [0])
             integrate (gensym "integrate")
             fmt flow/fmt]
         (flow/template
          {'?s !s '?dt dt '?n samples 'st st 's0 s0 'rk rk 'vw vw 'rg rg 'gp gp 'ar ar 'integrate integrate '?arcs arcs-form}
          (list
           'reagent.core/with-let
           [st sf s0 s0f rk rkf vw vwf rg rgf gp gpf ar arf integrate (flow/integrate-form st)
            'cache '(atom nil)
            'raf '(atom nil) 't-last '(atom nil)
            '_ '(reset! raf (js/requestAnimationFrame
                             (fn step [now]
                               (when (and @t-last (get @?s :play))
                                 (swap! ?s (fn [s] (assoc s :t (mod (+ (:t s) (* (:years s) (/ (- now @t-last) 24000))) (:years s))))))
                               (reset! t-last now)
                               (reset! raf (js/requestAnimationFrame step)))))
            'run '(fn [a P years]
                    (let [k [a P years]]
                      (if (= k (first @cache))
                        (second @cache)
                        (let [wP (/ 1 P)
                              n (js/Math.ceil (/ years ?dt))
                              pts (integrate (s0 (array 0) (array a)) (array 1 wP ?dt) n (fn [_ _] false))
                              rates (mapv (fn [s] (rk s (array 1 wP))) pts)
                              lam' (mapv (fn [q] (* (aget q 2) (/ 360 (* 2 js/Math.PI 365.25)))) rates)
                              arcs ?arcs
                              arcs (mapv (fn [[i j]]
                                           (let [o (ar (array 0) (array (aget (nth rates i) 3) (aget (nth rates j) 3) (* i ?dt) (* j ?dt)))]
                                             {:i i :j j :width (js/Math.abs (aget o 0)) :days (aget o 1)
                                              :L1 (mod (aget (nth rates i) 3) 360) :L2 (mod (aget (nth rates j) 3) 360)}))
                                         arcs)
                              m (count pts)
                              gapmax (reduce max 0 (map (fn [j] (let [q (nth pts j)]
                                                                  (aget (gp (array 0) (array a 1 wP (* j ?dt) (- (aget q 2) (aget q 0)) (- (aget q 3) (aget q 1)))) 0)))
                                                        (range 0 m 25)))
                              out {:pts pts :rates rates :lam lam' :arcs arcs :gap gapmax}]
                          (reset! cache [k out])
                          out))))]
           (list
            'let '[s @?s
                   a (:a s) P (:P s) years (:years s) f (:f s)
                   t (min (:t s) years)
                   outer (if (> a 1) 1 0)
                   {:keys [pts rates lam arcs gap]} (run a P years)
                   m (count pts)
                   i (min (dec m) (js/Math.round (/ t ?dt)))
                   idx (fn [k] (js/Math.min (dec m) (js/Math.round (* k (/ (dec m) (dec ?n))))))
                   in-retro (fn [j] (some (fn [{p :i q :j}] (<= p j q)) arcs))
                   views (mapv (fn [k] (let [j (idx k)] [j (vw (nth pts j) (array f outer))])) (range ?n))
                   path (mapv (fn [[_ o]] [(aget o 4) (aget o 5)]) views)
                   epath (mapv (fn [[_ o]] [(aget o 2) (aget o 3)]) views)
                   retro-segs (->> views (partition-by (fn [[j _]] (boolean (in-retro j))))
                                   (filter (fn [[[j _]]] (in-retro j)))
                                   (mapv (fn [seg] (mapv (fn [[_ o]] [(aget o 4) (aget o 5)]) seg))))
                   now (vw (nth pts i) (array f outer))
                   S [(aget now 0) (aget now 1)] E [(aget now 2) (aget now 3)] Pp [(aget now 4) (aget now 5)]
                   lam-now (nth lam i)
                   L-now (mod (aget (nth rates i) 3) 360)
                   current (first (filter (fn [{p :i q :j}] (<= p i q)) arcs))
                   o (rg (array 0) (array a 1 (/ 1 P)))
                   kind (cond (< (js/Math.abs (- a 1)) 0.01) "a = 1: the planet rides the Earth's own orbit; no nearest point, no loop"
                              (and (> a 1) (< (aget o 3) 0)) "outer planet: the Earth overtakes it at opposition, so it loops backwards"
                              (< (aget o 3) 0) "inner planet: it overtakes the Earth at inferior conjunction, so it loops backwards"
                              :else "no retrograde: the outer body is the faster here, against Copernicus' order (the farther, the slower)")
                   moving (cond (< (js/Math.abs lam-now) 0.003) "stationary" (neg? lam-now) "retrograde" :else "direct")
                   R (* 1.15 (+ (max a 1) (- 1 f)))
                   rate-pts (mapv (fn [j] [(* j ?dt) (nth lam j)]) (range 0 (count lam) 3))
                   lo (reduce min -0.5 lam) hi (reduce max 0.5 lam)]
            [:div {:data-playground "copernicus-retrograde" :data-case 'kind :data-motion 'moving
                   :data-arcs '(count arcs) :data-gap '(str gap) :data-lambda-dot '(str lam-now)
                   :data-nearest '(str (aget o 0)) :data-frame '(str f)}
             [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
              [:div {:style {:flex "1 1 420px" :min-width "300px"}}
               (mafs/mafs {:view-box {:x '[(- R) R] :y '[(- R) R]} :height 420}
                          (mafs/cartesian {:subdivisions 1})
                          '[mafs.core/Circle {:center S :radius 1 :color "#2E8B57" :fillOpacity 0 :strokeOpacity 0.5}]
                          '[mafs.core/Circle {:center S :radius a :color "#8A9BB0" :fillOpacity 0}]
                          '[mafs.core/Polyline {:points epath :color "#2E8B57" :weight 1 :strokeOpacity 0.4}]
                          '[mafs.core/Polyline {:points path :color "#1F6FB4" :weight 1.5}]
                          '(into [:<>] (map-indexed (fn [k seg] [mafs.core/Polyline {:key k :points seg :color "#C0392B" :weight 4}]) retro-segs))
                          '[mafs.line/Segment {:point1 E :point2 [(+ (first E) (* 3 (- (first Pp) (first E)))) (+ (second E) (* 3 (- (second Pp) (second E))))]
                                               :color "#2E8B57" :opacity 0.6}]
                          '[mafs.core/Point {:x (first S) :y (second S) :color "#E6A700"}]
                          '[mafs.core/Point {:x (first E) :y (second E) :color "#2E8B57"}]
                          '[mafs.core/Text {:x (first S) :y (second S) :attach "sw"} "Sun"]
                          '[mafs.core/Text {:x (first E) :y (second E) :attach "se"} "Earth"]
                          (mafs/movable-point {:point 'Pp :color "#111111"
                                               :on-move '(fn [[x y]] (swap! ?s assoc :play false
                                                                            :a (max 0.2 (min 10 (js/Math.hypot (- x (first S)) (- y (second S)))))))}))
               [:div {:style {:font-size "0.85em" :opacity 0.75}}
                "Gold: the Sun. Green: the Earth, its orbit and the line of sight. Blue: the planet's path by RK4 (raster) in the chosen frame; red: where its longitude runs backwards. Drag the planet (black) to change its distance a."]]
              [:div {:style {:flex "0 1 330px" :min-width "270px"}}
               (leva/sub-panel {:fill true :flat true :title-bar false} (leva/controls {:atom !s :schema schema}))
               [:div {:style {:font-size "1.1em" :font-weight 600 :margin "0.3em 0"} :data-case-name 'kind} 'kind]
               [:div {:data-motion-name 'moving :style {:font-weight 600}}
                "now: " 'moving ", λ = " (fmt 'L-now 2) "°, λ′ = " (fmt 'lam-now 4) "°/day"]
               [:div {:data-retro-arc '(if current "1" "0") :style {:margin "0.3em 0"}}
                '(if current
                   (str "crossing a retrograde arc: from λ = " (.toFixed (:L1 current) 2) "° back to "
                        (.toFixed (:L2 current) 2) "°, " (.toFixed (:width current) 2) "° in "
                        (.toFixed (:days current) 1) " days")
                   (str (count arcs) " retrograde arcs in " years " years"
                        (when-let [q (first arcs)] (str "; the first is " (.toFixed (:width q) 2) "° wide, " (.toFixed (:days q) 1) " days"))))]
               (katex '(str "\\dot\\lambda_{\\text{nearest}} = 2\\pi\\,\\frac{a\\,w_P - w_E}{a - 1} = 2\\pi\\,\\frac{"
                            (.toFixed a 3) "\\cdot" (.toFixed (/ 1 P) 3) " - 1}{" (.toFixed (aget o 4) 3) "}\\ \\tfrac{\\text{rad}}{\\text{yr}} = "
                            (.toFixed (aget o 0) 4) "^\\circ/\\text{day}")
                      :data-formula)
               [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.85em"}}
                "planet's speed a·w_P = " (fmt '(aget o 1) 3) " vs the Earth's w_E = " (fmt '(aget o 2) 3) " (radii a year)"]
               [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.85em"}}
                "RK4 (raster kernel, Δt = " (str dt) " yr, " '(count pts) " steps); farthest from the closed form: "
                [:span {:data-gap-text true} '(.toExponential gap 2)]]]]
             [:div {:style {:margin-top "0.5em"}}
              (mafs/mafs {:view-box {:x '[0 years] :y '[lo hi]} :height 260 :preserve-aspect-ratio false}
                         (mafs/cartesian {:subdivisions 1})
                         '[mafs.core/Polyline {:points rate-pts :color "#1F6FB4" :weight 2}]
                         '(into [:<>] (for [{p :i q :j} arcs]
                                        [mafs.core/Polygon {:key p :points [[(* p ?dt) lo] [(* q ?dt) lo] [(* q ?dt) hi] [(* p ?dt) hi]]
                                                            :color "#C0392B" :fillOpacity 0.12}]))
                         (mafs/movable-point {:point '[t lam-now] :color "#111111"
                                              :constrain '(fn [[x _]] [(max 0 (min years x)) lam-now])
                                              :on-move '(fn [[x _]] (swap! ?s assoc :t (max 0 (min years x)) :play false))}))
              [:div {:style {:font-size "0.85em" :opacity 0.75}}
               "λ′ (°/day) against t (years), from the rate kernel at every RK4 state; red bands: the retrograde arcs. Drag the black point along time."]]
             [:p "Try this:"]
             (into [:<>] (for [[label v] presets] (flow/button !s label (assoc v :play true))))
             (flow/button !s "Reset to Copernicus' Mars" start-state)])
           '(finally (some-> @raf js/cancelAnimationFrame)))))))))

(defn derivation
  "Emmy's derivation of the heliocentric rate, its identity with Ptolemy's,
   and the synodic relation, graded."
  []
  (clerk/col
   (clerk/tex (:helio orb/tex))
   (clerk/tex (:rate orb/tex))
   (clerk/tex (:closest orb/tex))
   (clerk/tex (:ptolemy orb/tex))
   (clerk/html
    (into [:ul {:style {:font-size "0.9em"}}]
          (for [{:keys [label grade]} (orb/graded)]
            [:li {:data-grade (name grade)} [:code (name grade)] " " label])))))
