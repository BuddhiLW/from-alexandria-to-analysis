(ns alexandria.noether.widgets
  "The widget of the Noether notebook (1918): a central-force orbit
   integrated by raster (alexandria.noether.orbit), its energy E and angular
   momentum J plotted against time, and a toggle that breaks one symmetry.

   The runs are computed on the JVM by raster's RK4 and the charges by
   raster kernels; the browser only draws them (a Clerk render-fn in SVG)
   and switches between the cases and the perturbation strengths."
  (:require [alexandria.noether.orbit :as orbit]
            [nextjournal.clerk :as clerk]))

(def strengths
  "The perturbation strengths the selector offers. Past about 0.05 the
   rotation-breaking term drives J through 0 and the orbit into the 1/r
   singularity (alexandria.noether.orbit/graded), so the list stops at 0.02."
  [0.005 0.01 0.02])

(defn- thin [samples k] (vec (take-nth k samples)))

(defn data
  "{:runs {kind {eps [{:t :x :y :E :J} ...]}}} over every case and strength."
  []
  {:strengths strengths
   :runs (into {}
               (for [kind [:symmetric :time :rotation]]
                 [kind (into {} (for [eps (if (= kind :symmetric) [0.0] strengths)]
                                  [eps (thin (:samples (orbit/run kind eps)) 2)]))]))})

(def ^:private render
  '(fn [{:keys [strengths runs]}]
     (reagent.core/with-let [kind (reagent.core/atom :symmetric) eps (reagent.core/atom 0.01)]
       (let [e (if (= @kind :symmetric) 0.0 @eps)
             s (get-in runs [@kind e])
             W 300 H 300 sc 70 cx 150 cy 150
             path (fn [pts] (apply str (map-indexed (fn [i [a b]] (str (if (zero? i) "M" "L") (.toFixed a 2) " " (.toFixed b 2) " ")) pts)))
             orbit-d (path (map (fn [{:keys [x y]}] [(+ cx (* sc x)) (- cy (* sc y))]) s))
             t1 (:t (peek s))
             plot (fn [k colour label]
                    (let [vs (map k s) v0 (k (first s)) lo (min (- v0 0.05) (apply min vs)) hi (max (+ v0 0.05) (apply max vs))
                          py (fn [v] (- 110 (* 100 (/ (- v lo) (- hi lo)))))
                          d (path (map (fn [r] [(+ 40 (* 400 (/ (:t r) t1))) (py (k r))]) s))]
                      [:svg {:width 460 :height 130 :style {:display "block"}}
                       [:line {:x1 40 :x2 440 :y1 (py v0) :y2 (py v0) :stroke "#8884" :stroke-dasharray "4 3"}]
                       [:path {:d d :fill "none" :stroke colour :stroke-width 2}]
                       [:text {:x 0 :y 20 :fill colour :font-size 13} label]
                       [:text {:x 0 :y 125 :fill "currentColor" :font-size 11 :opacity 0.7}
                        (str "spread " (.toExponential (- (apply max vs) (apply min vs)) 1))]]))
             btn (fn [k title] [:button {:type "button" :on-click #(reset! kind k)
                                         :style {:margin "0.2em" :padding "0.3em 0.6em" :border-radius "0.3em" :cursor "pointer"
                                                 :border "1px solid currentColor" :color "inherit"
                                                 :background (if (= @kind k) "#1F6FB433" "transparent")}} title])]
         [:div {:style {:margin "1em 0"}}
          [:div (btn :symmetric "symmetric: V = -1/r")
           (btn :time "break time: k(t) = 1 + εt")
           (btn :rotation "break rotation: V = -1/r + εx²/2")]
          (when (not= @kind :symmetric)
            [:div [:label "ε = "]
             (into [:select {:value (str @eps) :on-change #(reset! eps (js/parseFloat (.. % -target -value)))}]
                   (for [v strengths] [:option {:value (str v)} (str v)]))])
          [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "center"}}
           [:svg {:width W :height H}
            [:circle {:cx cx :cy cy :r 4 :fill "#E39B2D"}]
            [:path {:d orbit-d :fill "none" :stroke "#1F6FB4" :stroke-width 1.2}]]
           [:div (plot :E "#C0392B" "energy E(t)") (plot :J "#2E8B57" "angular momentum J(t)")]]
          [:p {:style {:font-size "0.9em"}}
           (case @kind
             :symmetric "Both symmetries hold: E and J stay on their dashed lines."
             :time "Time translation is broken, rotation is not: J stays constant, E drifts."
             :rotation "Rotation is broken, time translation is not: E stays constant, J swings.")]]))))

(defn finite-samples?
  "True when every sample of every run of `data` is a finite number in each
   of :t :x :y :E :J, and every run is non-empty: the widget divides by the
   time span and by the range of E and J, so one NaN or an empty run reaches
   the SVG as NaN coordinates."
  [{:keys [runs]}]
  (boolean
   (and (seq runs)
        (every? (fn [[_ by-eps]]
                  (and (seq by-eps)
                       (every? (fn [[_ samples]]
                                 (and (seq samples)
                                      (every? (fn [s]
                                                (every? #(let [v (get s %)]
                                                           (and (number? v) (Double/isFinite (double v))))
                                                        [:t :x :y :E :J]))
                                              samples)))
                               by-eps)))
                runs))))

(defn orbit-widget
  "The orbit with its two charges and the symmetry toggle. The viewer is
   marked presented so Clerk hands the render-fn the whole value instead of
   eliding it to a page of the vectors."
  []
  (let [d (data)]
    (assert (finite-samples? d) "the orbit runs hold a non-finite or empty sample")
    (clerk/with-viewer {:render-fn render :transform-fn clerk/mark-presented} d)))
