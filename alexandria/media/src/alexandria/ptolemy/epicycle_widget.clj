(ns alexandria.ptolemy.epicycle-widget
  "Ptolemy's epicycle and deferent as a playground (playground brief), the
   planet's path INTEGRATED in the page: one RK4 step of
   alexandria.ptolemy.epicycle/flow is a raster kernel (alexandria.medium.flow
   over alexandria.raster.step), iterated over the chosen years; the
   longitude seen from the Earth rides along as the fifth coordinate.

   Views: MathBox on the raster backend (orbitable): the deferent, the
   epicycle at the current time, the RK4 path with its retrograde stretches
   in red, the closed form dashed beside it (labelled as such), the planet
   and the line of sight from the Earth. A Mafs top view where the reader
   DRAGS the deferent's radius (point on the deferent) and the planet (its
   distance from the epicycle's centre sets r). A Mafs plot of the apparent
   rate lambda' (degrees a day) against time, from the rate kernel at every
   RK4 state: below zero is retrograde; the time marker is draggable.

   Live: the case (no retrograde when r w2 <= R w1, loops otherwise, Earth
   inside the epicycle when r > R), the retrograde arc being crossed (its
   stations, width in degrees and days, by the arc kernel), lambda' at the
   nearest point with R, r, w1, w2 substituted (opposition kernel), and the
   largest gap between RK4 and the closed form."
  (:require [alexandria.medium.flow :as flow]
            [alexandria.ptolemy.epicycle :as ep]
            [emmy.clerk :as ec]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.mathbox.plot :as plot]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [nextjournal.clerk :as clerk]))

(def dt "RK4 step, years." 0.004)
(def samples "Points of the path MathBox draws (resampled from the RK4 run)." 720)

(def start-state
  "Ptolemy's Mars (Almagest X: epicycle 39;30 parts of 60)."
  (let [{:keys [R r w1 w2 years]} (ep/planets :mars)]
    {:R R :r r :w1 w1 :w2 w2 :years years :t 0.0 :play true}))

(defn- planet-preset [k]
  (let [{:keys [R r w1 w2 years]} (ep/planets k)] {:R R :r r :w1 w1 :w2 w2 :years years :t 0.0}))

(def presets
  [["Mars (Almagest X)" (planet-preset :mars)]
   ["Jupiter (XI)" (planet-preset :jupiter)]
   ["Saturn (XI)" (planet-preset :saturn)]
   ["Venus (X)" (planet-preset :venus)]
   ["Shrink the epicycle: no loops" (assoc (planet-preset :mars) :r 0.3)]
   ["Epicycle wider than the deferent" (assoc (planet-preset :mars) :r 1.3)]])

(def schema
  {:R {:min 0.3 :max 2 :step 0.01 :label "deferent R"}
   :r {:min 0 :max 1.8 :step 0.005 :label "epicycle r"}
   :w1 {:min 0.02 :max 2 :step 0.001 :label "deferent speed w₁ (rev/yr)"}
   :w2 {:min 0.1 :max 5 :step 0.001 :label "epicycle speed w₂ (rev/yr)"}
   :years {:min 1 :max 12 :step 0.5 :label "years integrated"}
   :t {:min 0 :max 12 :step 0.005 :label "time t (years)"}
   :play {:label "run"}})

(defn- deferent [R] (fn [s] [(e/* R (e/cos s)) (e/* R (e/sin s)) 0]))

(defn- epicycle-circle [R r' w1 t]
  (fn [s] (let [a (e/* ep/tau w1 t)]
            [(e/+ (e/* R (e/cos a)) (e/* r' (e/cos s))) (e/+ (e/* R (e/sin a)) (e/* r' (e/sin s))) 0])))

(defn- closed-path [R r' w1 w2 Y]
  (fn [s] (let [[x y] ((ep/closed R r' w1 w2) (e/* Y s))] [x y 0.002])))

(defn- line-array
  "A live MathBox line of `samples` points read from the atom `src` (a
   vector of [x y z], NaN breaks the line)."
  [src colour width]
  [:<>
   ['mathbox.primitives/Array {:width samples :channels 3 :items 1 :live true
                                :expr (list 'fn '[emit i _t]
                                            (list 'let ['p (list 'nth (list 'deref src) 'i '[js/NaN js/NaN js/NaN])]
                                                  '(emit (nth p 0) (nth p 1) (nth p 2))))}]
   ['mathbox.primitives/Line {:width width :color colour :zBias 4}]])

(defn playground
  "The epicycle playground (Mars by default)."
  ([] (playground start-state))
  ([start]
   (binding [vc/*backend* :raster]
     (ev/with-let [!s start]
       (let [{:keys [R r w1 w2]} start
             [st sf] (flow/step-kernel "epi" ep/flow [w1 w2 dt] [R 0 r 0 0])
             [s0 s0f] (flow/kernel "start" ep/start [R r] [0])
             [rk rkf] (flow/kernel "rate" ep/rate [w1 w2] [R 0 r 0 0])
             [gp gpf] (flow/kernel "gap" ep/gap [R r w1 w2 0 R 0] [0])
             [ar arf] (flow/kernel "arc" ep/arc [0 0 0 1] [0])
             [op opf] (flow/kernel "opp" ep/opposition [R r w1 w2] [0])
             integrate (gensym "integrate")
             pf (fn [params f] (ev/with-params {:atom !s :params params} f))
             fmt flow/fmt]
         (flow/template
          {'?s !s '?dt dt '?n samples 'st st 's0 s0 'rk rk 'gp gp 'ar ar 'op op 'integrate integrate}
          (list
           'reagent.core/with-let
           [st sf s0 s0f rk rkf gp gpf ar arf op opf integrate (flow/integrate-form st)
            'cache '(atom nil)
            'path '(atom [])
            'retro '(atom [])
            'raf '(atom nil) 't-last '(atom nil)
            '_ '(reset! raf (js/requestAnimationFrame
                             (fn step [now]
                               (when (and @t-last (get @?s :play))
                                 (swap! ?s (fn [s] (assoc s :t (mod (+ (:t s) (* (:years s) (/ (- now @t-last) 24000))) (:years s))))))
                               (reset! t-last now)
                               (reset! raf (js/requestAnimationFrame step)))))
            'run '(fn [R r w1 w2 years]
                    (let [k [R r w1 w2 years]]
                      (if (= k (first @cache))
                        (second @cache)
                        (let [n (js/Math.ceil (/ years ?dt))
                              pts (integrate (s0 (array 0) (array R r)) (array w1 w2 ?dt) n (fn [_ _] false))
                              rates (mapv (fn [s] (rk s (array w1 w2))) pts)
                              lam' (mapv (fn [q] (* (aget q 2) (/ 360 (* 2 js/Math.PI 365.25)))) rates)
                              ;; stations: where lambda' changes sign; arcs: [i-start i-end]
                              arcs (loop [i 1 open nil acc []]
                                     (if (>= i (count lam'))
                                       (if open (conj acc [open (dec (count lam'))]) acc)
                                       (let [a (nth lam' (dec i)) b (nth lam' i)]
                                         (cond (and (>= a 0) (< b 0)) (recur (inc i) i acc)
                                               (and open (< a 0) (>= b 0)) (recur (inc i) nil (conj acc [open i]))
                                               :else (recur (inc i) open acc)))))
                              arcs (mapv (fn [[i j]]
                                           (let [o (ar (array 0) (array (aget (nth rates i) 3) (aget (nth rates j) 3) (* i ?dt) (* j ?dt)))]
                                             {:i i :j j :width (aget o 0) :days (aget o 1)
                                              :L1 (mod (aget (nth rates i) 3) 360) :L2 (mod (aget (nth rates j) 3) 360)}))
                                         arcs)
                              in-retro (fn [i] (some (fn [{a :i b :j}] (<= a i b)) arcs))
                              m (count pts)
                              idx (fn [k] (js/Math.min (dec m) (js/Math.round (* k (/ (dec m) (dec ?n))))))
                              gapmax (reduce max 0 (map (fn [j] (let [q (nth pts j)] (aget (gp (array 0) (array R r w1 w2 (* j ?dt) (+ (aget q 0) (aget q 2)) (+ (aget q 1) (aget q 3)))) 0)))
                                                        (range 0 m 25)))
                              out {:pts pts :rates rates :lam lam' :arcs arcs :gap gapmax}]
                          (reset! path (mapv (fn [k] (let [p (nth pts (idx k))] [(+ (aget p 0) (aget p 2)) (+ (aget p 1) (aget p 3)) 0])) (range ?n)))
                          (reset! retro (mapv (fn [k] (let [i (idx k) p (nth pts i)]
                                                        (if (in-retro i) [(+ (aget p 0) (aget p 2)) (+ (aget p 1) (aget p 3)) 0.004] [js/NaN js/NaN js/NaN])))
                                              (range ?n)))
                          (reset! cache [k out])
                          out))))]
           (list
            'let '[s @?s
                   R (:R s) r (:r s) w1 (:w1 s) w2 (:w2 s) years (:years s)
                   t (min (:t s) years)
                   {:keys [pts rates lam arcs gap]} (run R r w1 w2 years)
                   i (min (dec (count pts)) (js/Math.round (/ t ?dt)))
                   here (nth pts i)
                   P [(+ (aget here 0) (aget here 2)) (+ (aget here 1) (aget here 3))]
                   Cc [(aget here 0) (aget here 1)]
                   lam-now (nth lam i)
                   L-now (mod (aget (nth rates i) 3) 360)
                   current (first (filter (fn [{a :i b :j}] (<= a i b)) arcs))
                   o (op (array 0) (array R r w1 w2))
                   kind (cond (< r 1e-3) "no epicycle: uniform motion on the deferent"
                              (<= (aget o 1) 0) "no retrograde: r·w₂ ≤ R·w₁, the epicycle is too small or too slow"
                              (> r R) "loops, and the Earth inside the epicycle: r > R"
                              :else "retrograde loops: r·w₂ > R·w₁")
                   moving (cond (< (js/Math.abs lam-now) 0.003) "stationary" (neg? lam-now) "retrograde" :else "direct (prograde)")
                   rate-pts (mapv (fn [j] [(* j ?dt) (nth lam j)]) (range 0 (count lam) 3))]
            [:div {:data-playground "ptolemy-epicycle" :data-case 'kind :data-motion 'moving
                   :data-arcs '(count arcs) :data-gap '(str gap) :data-lambda-dot '(str lam-now)
                   :data-opposition '(str (aget o 0))}
             [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
              [:div {:style {:flex "1 1 440px" :min-width "300px"}}
               (plot/scene
                {:range [[-3.2 3.2] [-3.2 3.2] [-1 1]] :scale [1 1 0.3] :camera [0.2 -1.6 1.4]
                 :axes [] :grids [] :container {:style {:height "460px" :width "100%"}}}
                (plot/parametric-curve {:f (pf [:R] deferent) :t [0 (* 2 Math/PI)] :samples 180 :color "#8A9BB0" :width 3})
                (plot/parametric-curve {:f (pf [:R :r :w1 :t] epicycle-circle) :t [0 (* 2 Math/PI)] :samples 120
                                        :color "#E6A700" :width 3 :simplify? false})
                (plot/parametric-curve {:f (pf [:R :r :w1 :w2 :years] closed-path) :t [0 1] :samples 1200
                                        :color "#9AA5B1" :width 1.5 :opacity 0.6 :simplify? false})
                (line-array 'path "#1F6FB4" 4)
                (line-array 'retro "#C0392B" 7)
                (plot/point {:coords [0 0 0] :color "#2E8B57" :size 14})
                '(let [[x y] P [cx cy] Cc]
                   [:<>
                    [emmy.mathbox.components.plot/Point {:coords [x y 0.01] :color "#111111" :size 14}]
                    [emmy.mathbox.components.plot/Point {:coords [cx cy 0] :color "#E6A700" :size 10}]
                    [emmy.mathbox.components.plot/Line {:coords [[0 0 0] [(* 3 x) (* 3 y) 0]] :color "#2E8B57" :width 2 :opacity 0.6}]
                    [emmy.mathbox.components.plot/Line {:coords [[cx cy 0] [x y 0.01]] :color "#E6A700" :width 2}]]))
               [:div {:style {:font-size "0.85em" :opacity 0.75}}
                "Green: the Earth and the line of sight. Grey: the deferent. Gold: the epicycle now. Blue: the path by RK4 (raster); red: where it runs backwards; faint dashed: the closed form, beside it. Drag to orbit."]]
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
                        (when-let [a (first arcs)] (str "; the first is " (.toFixed (:width a) 2) "° wide, " (.toFixed (:days a) 1) " days"))))]
               '(let [tex (str "\\dot\\lambda_{\\text{nearest}} = \\frac{R w_1 - r w_2}{R - r} = \\frac{"
                               (.toFixed R 3) "\\cdot" (.toFixed w1 3) " - " (.toFixed r 3) "\\cdot" (.toFixed w2 3) "}{"
                               (.toFixed (aget o 2) 3) "} \\text{ rev/yr} = " (.toFixed (aget o 0) 4) "^\\circ/\\text{day}")]
                  [:div {:data-formula tex} [:f> nextjournal.clerk.render/render-katex tex {:inline? false}]])
               [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.85em"}}
                "RK4 (raster kernel, Δt = " (str dt) " yr, " '(count pts) " steps); farthest from the closed form: "
                [:span {:data-gap-text true} '(.toExponential gap 2)]]]]
             [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :margin-top "0.5em"}}
              [:div {:style {:flex "1 1 320px" :min-width "280px"}}
               (mafs/mafs {:view-box {:x [-3 3] :y [-3 3]} :height 320}
                          (mafs/cartesian {:subdivisions 1})
                          '[mafs.plot/Parametric {:xy (fn [u] [(* R (js/Math.cos u)) (* R (js/Math.sin u))]) :t [0 (* 2 js/Math.PI)] :color "#8A9BB0"}]
                          '[mafs.core/Polyline {:points (mapv (fn [[x y]] [x y]) @path) :color "#1F6FB4" :weight 1.5}]
                          '[mafs.core/Circle {:center Cc :radius (max r 1e-6) :color "#E6A700" :fillOpacity 0}]
                          (mafs/point {:x 0 :y 0 :color "#2E8B57"})
                          (mafs/text "Earth" {:x 0 :y 0 :attach "sw"})
                          (mafs/movable-point {:point '[R 0] :color "#8A9BB0"
                                               :constrain '(fn [[x _]] [(max 0.3 (min 2 x)) 0])
                                               :on-move '(fn [[x _]] (swap! ?s assoc :R (max 0.3 (min 2 x))))})
                          (mafs/movable-point {:point 'P :color "#111111"
                                               :on-move '(fn [[x y]] (swap! ?s assoc :r (min 1.8 (js/Math.hypot (- x (first Cc)) (- y (second Cc))))))}))
               [:div {:style {:font-size "0.85em" :opacity 0.75}} "Top view: drag the grey point to set R, the planet to set r."]]
              [:div {:style {:flex "1 1 320px" :min-width "280px"}}
               (mafs/mafs {:view-box {:x [0 12] :y [-2 3]} :height 320 :preserve-aspect-ratio false}
                          (mafs/cartesian {:subdivisions 1})
                          '[mafs.core/Polyline {:points rate-pts :color "#1F6FB4" :weight 2}]
                          '(into [:<>] (for [{a :i b :j} arcs]
                                         [mafs.core/Polygon {:key a :points [[(* a ?dt) -2] [(* b ?dt) -2] [(* b ?dt) 3] [(* a ?dt) 3]]
                                                             :color "#C0392B" :fillOpacity 0.12}]))
                          (mafs/movable-point {:point '[t lam-now] :color "#111111"
                                               :constrain '(fn [[x _]] [(max 0 (min years x)) lam-now])
                                               :on-move '(fn [[x _]] (swap! ?s assoc :t (max 0 (min years x)) :play false))})
                          (mafs/text "λ′ (°/day) against t (years)" {:x 0.2 :y 2.8 :attach "e"}))
               [:div {:style {:font-size "0.85em" :opacity 0.75}} "The apparent rate from the rate kernel at every RK4 state; the red bands are the retrograde arcs. Drag the black point along time."]]]
             [:p "Try this:"]
             (into [:<>] (for [[label v] presets] (flow/button !s label (assoc v :play true))))
             (flow/button !s "Reset to Ptolemy's Mars" start-state)])
           '(finally (some-> @raf js/cancelAnimationFrame)))))))))

(defn derivation
  "Emmy's derivation of the apparent rate and the retrograde condition, graded."
  []
  (clerk/col
   (clerk/tex (:model ep/tex))
   (clerk/tex (:rate ep/tex))
   (clerk/tex (:opposition ep/tex))
   (clerk/html
    (into [:ul {:style {:font-size "0.9em"}}]
          (for [{:keys [label grade]} (ep/graded)]
            [:li {:data-grade (name grade)} [:code (name grade)] " " label])))))
