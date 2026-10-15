(ns alexandria.cauchy.limit-widget
  "The epsilon-delta widget of the Cauchy notebook: Cauchy's limit and
   continuity (alexandria.cauchy.limit) with tied controls (widget brief,
   section 5).

   State {:f :a :eps :delta}, every key a live Leva control. eps and delta
   are tied by eps = W(a, delta), the worst increment of f in the window:

     eps moves     a fixed, delta re-solved (forward map: the raster search)
     delta moves   a fixed, eps re-solved (inverse map: W, one kernel call)
     a or f moves  eps fixed, delta re-solved

   Only the touched key is a source. The page pushes the solved key back
   into the panel atom; a push never re-fires the solver: the watch is off
   while the page pushes (origin guard), and a change that comes back
   within half a slider step of the value just pushed is Leva's echo, not
   the reader (epsilon guard).

   Every number is raster's: per function one kernel (`game`) returns f(a)
   and the 33 increments of the window; W is their largest; the search
   halves [0, delta-max] 48 times, one kernel call per halving; the closed
   forms (x^2, 1/x) are kernels too, and the readout shows them beside the
   search. The curves are Mafs of-x plots on emmy-viewers' :raster backend."
  (:require [alexandria.cauchy.limit :as lim]
            [clojure.walk :as walk]
            [emmy.clerk :as ec]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [nextjournal.clerk :as clerk]))

(def ranges
  "Slider [min max step] of each number."
  {:a [0.25 2.5 0.01] :eps [0.01 2.0 0.001] :delta [0.0 1.0 0.0001]})

(def labels {:f "f" :a "a" :eps "ε (the doubter)" :delta "δ (the answer)"})

(def colours {:band "#E8B43A" :window "#3C7DD9" :inside "#2F8A3E" :curve "#444444" :point "#C0392B"})

(def view {:x [-0.6 3.2] :y [-1.3 3.7]})

(defn game
  "The game kernel of function id: [f(a) inc_0 .. inc_32], the increments
   |f(a + s delta) - f(a)| over alexandria.cauchy.limit/grid."
  [id]
  (let [f (:f (lim/functions id))]
    (fn [a d]
      (fn [_] (into [(f a)] (lim/increments f a d))))))

(defn closed-form
  "The forward map's closed form as a kernel [delta] (x^2 and 1/x)."
  [id]
  (fn [a eps] (fn [_] [(lim/forward id a eps)])))

(defn init
  "The starting state: x^2 at a = 1, eps = 0.2, delta by the raster search."
  []
  {:f (get-in lim/functions [:square :label]) :a 1.0 :eps 0.2
   :delta (:delta (lim/search :square 1.0 0.2))})

(defn- compiled
  "f compiled by the raster backend, called in the browser as
   (k (array 0) (array ...params))."
  [f params]
  (vc/compiled-fn f params [0] {:simplify? false}))

(defn- kernel-bindings
  "with-let bindings of every kernel, and the label -> kernel maps."
  []
  (let [ks (into {} (for [id lim/order] [id (symbol (str "game-" (name id)))]))
        cs {:square 'closed-square :reciprocal 'closed-reciprocal}
        curves (into {} (for [id lim/order] [id (symbol (str "curve-" (name id)))]))]
    {:bindings (vec (concat
                     (mapcat (fn [id] [(ks id) (compiled (game id) [1.0 0.1])]) lim/order)
                     (mapcat (fn [[id s]] [s (compiled (closed-form id) [1.0 0.2])]) cs)
                     (mapcat (fn [id]
                               [(curves id)
                                (list 'fn ['domain 'colour 'weight]
                                      (mafs/of-x {:y (:f (lim/functions id)) :domain 'domain
                                                  :color 'colour :weight 'weight}))])
                             lim/order)))
     :games (into {} (for [id lim/order] [(get-in lim/functions [id :label]) (ks id)]))
     :closed (into {} (for [[id s] cs] [(get-in lim/functions [id :label]) s]))
     :curves (into {} (for [id lim/order] [(get-in lim/functions [id :label]) (curves id)]))}))

(defn- schema []
  (into {:f {:options (mapv #(get-in lim/functions [% :label]) lim/order) :label (labels :f)}}
        (for [[k [a b s]] ranges] [k {:min a :max b :step s :label (labels k)}])))

(def template
  "The page, as browser code. Placeholders: !P the panel atom, !S the exact
   state, GAMES CLOSED CURVES the label -> kernel/component maps, STEPS the
   halvings, VIEW, COLOURS, LEVA the panel."
  '(let [{:keys [f a eps delta]} @!S
         game (get GAMES f)
         out (game (array 0) (array a delta))
         fa (aget out 0)
         w (loop [i 1 m 0] (if (> i 33) m (recur (inc i) (max m (aget out i)))))
         closed (when-let [k (get CLOSED f)] (aget (k (array 0) (array a eps)) 0))
         cont? (> delta 1e-9)
         fmt (fn [x n] (if (js/isFinite x) (.replace (.toFixed x n) "-" "−") "∞"))
         [x0 x1] (:x VIEW) [y0 y1] (:y VIEW)
         curve (get CURVES f)
         lo (if (= f "1/x") 0.12 x0)
         tex (case f
               "x²" (str "\\delta = \\sqrt{a^2 + \\varepsilon} - a = \\sqrt{" (fmt a 2) "^2 + " (fmt eps 3) "} - " (fmt a 2) " = " (fmt closed 4))
               "1/x" (str "\\delta = \\frac{\\varepsilon a^2}{1 + \\varepsilon a} = \\frac{" (fmt eps 3) " \\cdot " (fmt a 2) "^2}{1 + " (fmt eps 3) " \\cdot " (fmt a 2) "} = " (fmt closed 4))
               (str "\\delta = \\max\\{\\, d : W(a, d) \\le \\varepsilon \\,\\} = " (fmt delta 4)))]
     [:div {:data-figure "cauchy-eps-delta" :data-f f :data-a (str a) :data-eps (str eps)
            :data-delta (str delta) :data-w (str w) :data-closed (str closed)}
      [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
       [:div {:style {:flex "1 1 420px" :min-width "300px"}}
        [mafs.core/Mafs {:view-box {:x (:x VIEW) :y (:y VIEW)} :height 430 :preserve-aspect-ratio false}
         [mafs.coordinates/Cartesian {}]
         [mafs.core/Polygon {:points [[x0 (- fa eps)] [x1 (- fa eps)] [x1 (+ fa eps)] [x0 (+ fa eps)]]
                             :color (:band COLOURS) :fill-opacity 0.22 :weight 0}]
         [mafs.core/Polygon {:points [[(- a delta) y0] [(+ a delta) y0] [(+ a delta) y1] [(- a delta) y1]]
                             :color (:window COLOURS) :fill-opacity 0.18 :weight 0}]
         (into [:<>] (map-indexed (fn [i m]
                                    [mafs.line/Segment {:point1 [(- a m) (+ y0 0.15)] :point2 [(+ a m) (+ y0 0.15)]
                                                        :color (:window COLOURS) :opacity (/ 1 (+ 1 (* 0.5 i))) :weight 2}])
                                  (take 10 @trials)))
         [curve [lo x1] (:curve COLOURS) 2]
         (when cont? [curve [(max lo (- a delta)) (+ a delta)] (:inside COLOURS) 5])
         [mafs.core/Point {:x a :y fa :color (:point COLOURS)}]]]
       [:div {:style {:flex "0 1 320px" :min-width "270px"}}
        LEVA
        [:div {:data-formula tex :style {:margin-top "0.4em"}}
         [:f> nextjournal.clerk.render/render-katex tex {:inline? false}]]
        [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.88em" :line-height 1.5}}
         [:div "f(a) = " (fmt fa 4)]
         [:div "W(a, δ) = sup |f(a+h) − f(a)| = " [:span {:data-w-shown "1"} (fmt w 4)]]
         [:div "search: " STEPS " halvings of [0, " (fmt (if (= f "1/x") (* 0.9 a) 1) 2) "], δ = " (fmt delta 6)]
         (when closed [:div "formula: δ = " (fmt closed 6) ", |search − formula| = " (.toExponential (js/Math.abs (- closed delta)) 1)])]
        [:div {:data-verdict (if cont? "continuous" "jump")
               :style {:font-size "1.15em" :font-weight 600 :margin "0.4em 0"
                       :color (if cont? (:inside COLOURS) (:point COLOURS))}}
         (if cont?
           (str "continuous at a = " (fmt a 2) ": ε = " (fmt eps 3) " is answered by δ = " (fmt delta 4))
           (str "not continuous at a = " (fmt a 2) ": no window keeps f in the band of ε = " (fmt eps 3)))]]]]))

(def tie-code
  "The with-let bindings of the tie, as browser code (placeholders as in
   `template`)."
  '[W (fn [f a d] (let [out ((get GAMES f) (array 0) (array a d))]
                    (loop [i 1 m 0] (if (> i 33) m (recur (inc i) (max m (aget out i)))))))
    dmax (fn [f a] (if (= f "1/x") (* 0.9 a) 1))
    solve (fn [f a eps]
            (loop [i 0 lo 0 hi (dmax f a) tr []]
              (if (= i STEPS)
                [lo tr]
                (let [mid (* 0.5 (+ lo hi))]
                  (if (> (W f a mid) eps)
                    (recur (inc i) lo mid (conj tr mid))
                    (recur (inc i) mid hi (conj tr mid)))))))
    trials (reagent.core/atom (second (solve (:f @!S) (:a @!S) (:eps @!S))))
    pushing (atom false)
    pushed (atom {})
    from-eps (fn [cur]
               (let [eps (max EPSMIN (:eps cur))
                     [d tr] (solve (:f cur) (:a cur) eps)]
                 (reset! trials tr)
                 (assoc cur :eps eps :delta d)))
    tie (fn [src]
          (js/setTimeout
           (fn []
             (let [cur (assoc @!S src (get @!P src))
                   solved (if (= src :delta)
                            (let [d (min (max 0 (:delta cur)) (dmax (:f cur) (:a cur)))
                                  w (W (:f cur) (:a cur) d)]
                              (cond (> w EPSMAX) (from-eps (assoc cur :eps EPSMAX))
                                    (< w EPSMIN) (from-eps (assoc cur :eps EPSMIN))
                                    :else (do (reset! trials []) (assoc cur :delta d :eps w))))
                            (from-eps cur))
                   shown @!P
                   push (into {} (filter (fn [[k v]] (and (number? v) (> (js/Math.abs (- v (get shown k))) 1e-12))) solved))]
               (set! (.-__cauchySolves js/globalThis) (inc (or (.-__cauchySolves js/globalThis) 0)))
               (reset! !S solved)
               (when (seq push)
                 (reset! pushing true)
                 (swap! !P merge push)
                 (reset! pushing false)
                 (swap! pushed merge push)
                 (js/setTimeout (fn [] (swap! pushed (fn [m] (apply dissoc m (keys push))))) 600))))
           0))
    _ (add-watch !P :cauchy/tie
                 (fn [_ _ old new]
                   (when-not @pushing
                     (let [src (first (filter (fn [k] (not= (get old k) (get new k))) [:f :a :eps :delta]))
                           echo? (and src (contains? @pushed src)
                                      (<= (js/Math.abs (- (get new src) (get @pushed src)))
                                          (+ 1e-9 (get HALF src 0))))]
                       (when (and src (not echo?)) (tie src))))))])

(defn- fill
  "The browser code with its placeholders replaced."
  [code m]
  (walk/postwalk-replace m code))

(defn widget
  "The epsilon-delta widget, an emmy-viewers fragment for a Clerk notebook
   mounted after (history-of-math.widgets/install!). Compiled on the raster
   backend whatever the caller's binding."
  ([] (widget (init)))
  ([start]
   (binding [vc/*backend* :raster]
     (let [{:keys [bindings games closed curves]} (kernel-bindings)]
       (ev/with-let [!p start !s start]
         (let [m {'!P !p '!S !s 'GAMES games 'CLOSED closed 'CURVES curves 'STEPS lim/search-steps
                  'EPSMIN (first (ranges :eps)) 'EPSMAX (second (ranges :eps))
                  'HALF (into {} (for [[k [_ _ s]] ranges] [k (/ s 2)]))
                  'VIEW view 'COLOURS colours
                  'LEVA (leva/controls {:atom !p :schema (schema)})}]
           (list 'reagent.core/with-let
                 (into bindings (fill tie-code m))
                 (fill template m)
                 (list 'finally (list 'remove-watch !p :cauchy/tie)))))))))

(defn derivation
  "Emmy's derivation of the tie as a Clerk column: per closed-form game the
   edge equation, its coefficients read off by Emmy's D, the root; then the
   grades."
  []
  (apply clerk/col
         (concat
          (for [{:keys [label eqs]} (lim/equations)]
            (apply clerk/col
                   (clerk/html [:div {:style {:font-size "0.85em" :opacity 0.7 :margin-top "0.5em"}} label])
                   (for [q eqs] (ec/->TeX q :simplify? true))))
          [(clerk/html
            (into [:ul {:style {:font-size "0.9em"}}]
                  (for [{:keys [label grade]} (lim/grades)]
                    [:li {:data-grade (name grade)} [:code (name grade)] " " label])))])))
