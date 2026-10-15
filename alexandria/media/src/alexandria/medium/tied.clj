(ns alexandria.medium.tied
  "Tied controls (widget brief section 5) as one reusable emmy-viewers
   fragment: every quantity linked by an equation is a Leva control, and
   moving one re-solves the others.

   A widget names its keys, a slider range per key, and a source function
   per key: (fn [& state] state') that holds what the policy holds and
   re-solves the rest by the forward or inverse map (Emmy functions). The
   tie kernel is all the source functions side by side, compiled once by
   emmy.viewer.compile/*backend* :raster to a wasm kernel; the page keeps
   the block of the key the reader touched.

   Two atoms: !p is Leva's (it rounds to the slider step and echoes), !s
   the exact solved state that every plot and the readout kernel read.
   Guards, as in the conics playground (alexandria.apollonius.playground-
   widget): origin (nothing while the page pushes) and epsilon (a change
   within half a step of the value just pushed, or of the exact state, is
   Leva's rounding, not the reader). Solves are counted in
   globalThis[\"__tie_<id>\"] for the e2e (one solve per edit).

   The widget-level twin of alexandria.medium.tie, which holds the lower
   pieces (clamps, kernel binding, schema, watch form) the conic and
   widget-bar notebooks build from. Both arrived on separate branches and
   were kept side by side at the 2026-10-10 integration."
  (:require [clojure.walk :as walk]
            [emmy.leva :as leva]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]))

(defn kernel
  "[sym form]: f compiled by the current backend; in the browser the form
   evaluates to a function called as (sym (array 0) (array ...params))."
  [nm f init]
  [(gensym nm) (vc/compiled-fn f init [0] {:simplify? false})])

(defn tie-fn
  "The tie kernel of `sources` ({key (fn [& state] state')}) in the order
   of `ks`: for a state, every source's solved state, side by side."
  [ks sources]
  (fn [& state]
    (fn [_] (vec (mapcat #(apply (sources %) state) ks)))))

(defn solve
  "The tie on the JVM (tests): the state solved from source `k`, the same
   Emmy functions applied to numbers (Emmy's generic arithmetic; D by dual
   numbers). The browser runs them as the wasm tie kernel."
  [_ks sources state k]
  (mapv double (apply (sources k) state)))

(defn- watch-form
  [ks steps tsym !p !s counter]
  (walk/postwalk-replace
   {'KEYS ks 'HALF (zipmap ks (map #(/ % 2) steps)) 'NK (count ks)
    'TIE tsym 'PANEL !p 'EXACT !s 'COUNTER counter}
   '(add-watch PANEL :alexandria/tie
               (fn [_ _ old new]
                 (when-not @pushing
                   (let [src (first (filter (fn [k] (not= (get old k) (get new k))) KEYS))
                         h (get HALF src)
                         echo? (and src
                                    (or (and (contains? @pushed src)
                                             (<= (js/Math.abs (- (get new src) (get @pushed src))) (+ 1e-9 h)))
                                        (<= (js/Math.abs (- (get new src) (get @EXACT src))) (+ 1e-9 h))))]
                     (when (and src (not echo?))
                       (js/setTimeout
                        (fn []
                          (let [cur (assoc @EXACT src (get @PANEL src))
                                out (TIE (array 0) (apply array (map (fn [k] (get cur k)) KEYS)))
                                off (* NK (.indexOf (to-array (map name KEYS)) (name src)))
                                solved (zipmap KEYS (map (fn [j] (aget out (+ off j))) (range NK)))
                                shown @PANEL
                                push (into {} (filter (fn [[k v]] (> (js/Math.abs (- v (get shown k))) 1e-12)) solved))]
                            (aset js/globalThis COUNTER (inc (or (aget js/globalThis COUNTER) 0)))
                            (reset! EXACT solved)
                            (when (seq push)
                              (reset! pushing true)
                              (swap! PANEL merge push)
                              (reset! pushing false)
                              (swap! pushed merge push)
                              (js/setTimeout (fn [] (swap! pushed (fn [m] (apply dissoc m (keys push))))) 600))))
                        0))))))))

(defn fmt
  "Browser form: x to n places with a true minus sign (or ∞)."
  ([x] (fmt x 4))
  ([x n] (list 'let ['v x] (list 'if '(js/isFinite v) (list '.replace (list '.toFixed 'v n) "-" "−") "∞"))))

(defn widget
  "A tied-controls widget.
     :id       names the figure (data-figure) and the solve counter
     :keys     the tied keys, in order; :start the starting state (tied)
     :ranges   {k [min max step]}; :labels {k label}
     :sources  {k (fn [& state] state')}: the policy for each source
     :readout  (fn [& state] [numbers]) shown on the page (raster kernel)
     :body     (fn [!s r] hiccup): the figure and text; r is the browser
               symbol of the readout array on the exact state
     :side     optional (fn [!s r] hiccup) under the panel (formula, case)
     :presets  optional [{:label :state}]: buttons (data-preset i) that set
               both the panel and the exact state to a tied state
   Compiled on the raster backend whatever the caller's binding."
  [{:keys [id start ranges labels sources readout body panel-body side presets] ks :keys}]
  (binding [vc/*backend* :raster]
    (let [s (mapv start ks)
          [tsym tform] (kernel "tie" (tie-fn ks sources) s)
          [rsym rform] (kernel "readout" readout s)
          counter (str "__tie_" (name id))
          schema (into {} (map (fn [k] (let [[a b st] (ranges k)]
                                         [k {:min a :max b :step st :label (get labels k (name k))}]))
                               ks))]
      (ev/with-let [!p start !s start]
        (list 'reagent.core/with-let
              [tsym tform rsym rform
               'pushing '(atom false)
               'pushed '(atom {})
               '_ (watch-form ks (map #(nth (ranges %) 2) ks) tsym !p !s counter)]
              (list 'let ['r (list rsym '(array 0) (list* 'array (map #(ev/get !s %) ks)))]
                    [:div {:data-figure (name id)}
                     [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                      [:div {:style {:flex "1 1 440px" :min-width "300px"}} (if panel-body (panel-body !s 'r !p) (body !s 'r))]
                      [:div {:style {:flex "0 1 320px" :min-width "260px"}}
                       (leva/sub-panel {:fill true :flat true :title-bar false}
                                       (leva/controls {:atom !p :schema schema}))
                       (when side (side !s 'r))
                       (when (seq presets)
                         (into [:div {:style {:margin-top "0.5em"}} [:strong "Try this:"]]
                               (map-indexed
                                (fn [i {:keys [label state]}]
                                  [:div [:button {:type "button" :data-preset (str i)
                                                  :style {:margin "0.2em 0" :padding "0.25em 0.6em" :cursor "pointer"
                                                          :border-radius "0.3em" :border "1px solid currentColor"
                                                          :background "transparent" :color "inherit" :text-align "left"}
                                                  :on-click (list 'fn []
                                                                  '(reset! pushing true)
                                                                  (list 'reset! !s state)
                                                                  (list 'reset! !p state)
                                                                  '(reset! pushing false))}
                                         label]])
                                presets)))]]])
              (list 'finally (list 'remove-watch !p :alexandria/tie)))))))

(defn derivation
  "A derivation as a Clerk column: each step's label and its Emmy equations
   rendered by emmy.clerk/->TeX, then the grades as a list carrying
   data-grade (the e2e reads them). `equations` is [{:label :eqs}],
   `grades` [{:label :grade}]."
  [equations grades]
  (let [col @(requiring-resolve 'nextjournal.clerk/col)
        html @(requiring-resolve 'nextjournal.clerk/html)
        tex @(requiring-resolve 'emmy.clerk/->TeX)]
    (apply col
           (concat
            (for [{:keys [label eqs]} equations]
              (apply col
                     (html [:div {:style {:font-size "0.85em" :opacity 0.7 :margin-top "0.5em"}} label])
                     (for [q eqs] (tex q :simplify? true))))
            [(html (into [:ul {:style {:font-size "0.9em"}}]
                         (for [{:keys [label grade]} grades]
                           [:li {:data-grade (name grade)} [:code (name grade)] " " label])))]))))
