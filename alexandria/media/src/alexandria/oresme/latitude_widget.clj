(ns alexandria.oresme.latitude-widget
  "Oresme's latitude of forms on the page.

   `player`: the definitions stepped (alexandria.oresme.scenes,
   :oresme/latitude), one sentence of the 1486 text per step.

   `playground` (playground brief): the figure of a quality over the
   extension AB = [0, 4], its top the curve through five control points the
   reader DRAGS (vertically: each sits over a fixed point of the extension;
   the Leva panel holds the same five degrees and the number of strips).
   Live, every number from the raster kernels of alexandria.oresme.latitude:
   the curve (profile kernel at 161 points), the strips and their summed
   area (profile at each strip's midpoint), Boole's exact area, the mean
   degree, the excesses between the control points (summary kernel), and
   the case named as Oresme names it. A badge checks the mean degree
   theorem: the rectangle at the middle degree equals the figure exactly
   when the top is straight; bend it and the badge turns red."
  (:require [alexandria.medium.clerk :as medium]
            [alexandria.medium.flow :as flow]
            [alexandria.oresme.latitude :as lat]
            [alexandria.proofs :as proofs]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [nextjournal.clerk :as clerk]))

(def proofs-resource "alexandria/oresme/latitudes.proofs.edn")

(def render-fn 'alexandria.oresme.scenes/render)

(def player-figures
  "The latitude player's figures: the profile and the summary kernels."
  {:profile {:f lat/profile :params (:difformly-difform lat/kinds) :state [0] :opts {:simplify? false}}
   :summary {:f lat/summary :params (:difformly-difform lat/kinds) :state [0] :opts {:simplify? false}}})

(defn player
  "Oresme's definitions of the latitude, stepped."
  []
  (medium/player render-fn :oresme/latitude
                 (proofs/steps proofs-resource :oresme/latitude)
                 player-figures {}
                 {:window [[-0.7 4.7] [-0.8 5.4]] :height 380
                  :durations {:extension 5000 :uniform 5000 :difform 5000 :uniformly-difform 6000
                              :difformly-difform 6000 :area 7000}}))

;; ---------------------------------------------------------------------------
;; The playground

(def hkeys [:h0 :h1 :h2 :h3 :h4])

(defn- state [hs n] (assoc (zipmap hkeys hs) :n n))

(def author-state "Oresme's own example: a motion from no degree, uniformly difform." (state [0 1 2 3 4] 16))

(def presets
  "The \"try this\" states."
  [["Uniform: a rectangle" (state [2.5 2.5 2.5 2.5 2.5] 16)]
   ["Uniformly difform, from a degree: a trapezium" (state [1 1.75 2.5 3.25 4] 16)]
   ["Bend it: an arch (difformly difform)" (state [0.5 2.6 3.4 2.6 0.5] 16)]
   ["Refine the strips: n = 200" (state [0.5 2.6 3.4 2.6 0.5] 200)]
   ["Dip below zero: a negative degree is no degree" (state [3 0.5 -1 0.5 3] 64)]])

(def schema
  (merge (into {} (map-indexed (fn [i k] [k {:min -1 :max 5 :step 0.01 :label (str "degree at " i)}]) hkeys))
         {:n {:min 1 :max 200 :step 1 :label "strips n"}}))

(defn- preset [state label values]
  [:button {:type "button" :data-preset label :style {:margin "0.2em"}
            :on-click (list 'fn [] (list 'swap! state 'merge values))} label])

(defn playground
  "The latitude of a quality, shaped by the reader."
  ([] (playground author-state))
  ([start]
   (binding [vc/*backend* :raster]
     (ev/with-let [!q start]
       (let [hs0 (mapv start hkeys)
             [psym pform] (flow/kernel "profile" lat/profile hs0 [0])
             [ssym sform] (flow/kernel "summary" lat/summary hs0 [0])
             g (fn [k] (list 'aget 'sm (lat/summary-index k)))
             mover (fn [i k]
                     (mafs/movable-point
                      {:point [i (list 'max -0.9 (list 'min 4.9 (list 'get '@?q k)))] :color "#C0392B"
                       :constrain (list 'fn '[[_ y]] [i '(max -1 (min 5 y))])
                       :on-move (list 'fn '[[_ y]] (list 'swap! '?q 'assoc k '(/ (js/Math.round (* 100 y)) 100)))}))]
         (flow/template
          {'?q !q '?p psym '?s ssym}
          (list 'reagent.core/with-let [psym pform ssym sform]
                (list 'let '[s @?q
                             hs (array (:h0 s) (:h1 s) (:h2 s) (:h3 s) (:h4 s))
                             sm (?s (array 0) hs)
                             curve (mapv (fn [i] (let [o (?p (array (* 0.025 i)) hs)] [(aget o 0) (aget o 1)])) (range 161))
                             raw-neg (some (fn [i] (neg? (aget (?p (array (* 0.025 i)) hs) 2))) (range 161))
                             n (max 1 (js/Math.round (:n s)))
                             w (/ 4 n)
                             strips (mapv (fn [i] (let [t (* w (+ i 0.5))] [t (aget (?p (array t) hs) 1)])) (range n))
                             sum (* w (reduce + (map second strips)))
                             kind (cond (< (aget sm 9) 1e-3) "uniform"
                                        (< (aget sm 10) 1e-3) (if (or (< (js/Math.abs (:h0 s)) 1e-3) (< (js/Math.abs (:h4 s)) 1e-3))
                                                                "uniformly-difform-from-none" "uniformly-difform")
                                        :else "difformly-difform")
                             holds (< (js/Math.abs (- (aget sm 0) (aget sm 11))) 1e-6)]
                      [:div {:data-playground "oresme-latitude" :data-case 'kind :data-area '(str sum)
                             :data-boole '(str (aget sm 0)) :data-strips '(str n)}
                       [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                        [:div {:style {:flex "1 1 440px" :min-width "300px"}}
                         (mafs/mafs {:view-box {:x [-0.4 4.4] :y [-1.2 5.2]} :height 420 :preserve-aspect-ratio false}
                                    (mafs/cartesian {:subdivisions 1})
                                    '(into [:<>]
                                           (for [[t v] strips]
                                             [mafs.core/Polygon {:points [[(- t (/ w 2)) 0] [(+ t (/ w 2)) 0] [(+ t (/ w 2)) v] [(- t (/ w 2)) v]]
                                                                 :color "#E6A700" :fillOpacity 0.25 :weight 0.5}]))
                                    '[mafs.core/Polygon {:points [[0 (aget sm 1)] [4 (aget sm 1)] [4 0] [0 0]]
                                                         :color "#1F6FB4" :fillOpacity 0.0 :strokeStyle "dashed"}]
                                    '[mafs.core/Polyline {:points curve :color "#C0392B" :weight 3}]
                                    '[mafs.line/Segment {:point1 [0 0] :point2 [4 0] :color "#111" :weight 2}]
                                    '[mafs.core/Text {:x 0 :y -0.35 :attach "w" :size 16} "A"]
                                    '[mafs.core/Text {:x 4 :y -0.35 :attach "e" :size 16} "B"]
                                    '[mafs.core/Text {:x 4.05 :y (aget sm 1) :attach "e" :size 13 :color "#1F6FB4"} "mean"]
                                    (mover 0 :h0) (mover 1 :h1) (mover 2 :h2) (mover 3 :h3) (mover 4 :h4))]
                        [:div {:style {:flex "0 1 330px" :min-width "260px"}}
                         (leva/sub-panel {:fill true :flat true :title-bar false}
                                         (leva/controls {:atom !q :schema schema}))
                         [:div {:style {:font-size "1.15em" :font-weight 600 :margin "0.4em 0"} :data-case-name 'kind}
                          '(case kind
                             "uniform" "uniform (uniformis): the same degree throughout"
                             "uniformly-difform-from-none" "uniformly difform, from no degree: a triangle"
                             "uniformly-difform" "uniformly difform (uniformiter difformis): equal excesses, a trapezium"
                             "difformly difform (difformiter difformis): unequal excesses, a curved top")]
                         [:div {:data-invariant '(if holds "pass" "fail")
                                :style {:display "inline-block" :padding "0.15em 0.6em" :border-radius "0.6em" :color "white"
                                        :background '(if holds "#2E8B57" "#C0392B")}}
                          '(if holds "✓ area = middle degree × AB (the mean degree theorem)"
                               "✗ area ≠ middle degree × AB: the theorem needs a straight top")]
                         [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.88em" :margin-top "0.4em"}}
                          [:div "excesses: " (flow/fmt (g :d1) 2) ", " (flow/fmt (g :d2) 2) ", " (flow/fmt (g :d3) 2) ", " (flow/fmt (g :d4) 2)]
                          [:div "area by " 'n " strips (raster) = " [:span {:data-strip-sum true} (flow/fmt 'sum 4)]]
                          [:div "area, Boole's rule (exact) = " (flow/fmt (g :area) 4)]
                          [:div "mean degree = area / AB = " (flow/fmt (g :mean) 4)]
                          [:div "middle degree × AB = " (flow/fmt (g :mid-rect) 4)]
                          '(when raw-neg [:div {:style {:color "#C0392B"}} "the curve dips below AB: there the degree is no degree (clipped to 0), and the strips stop matching Boole's rule"])]]]
                       [:p {:style {:font-size "0.9em" :opacity 0.85}}
                        "Drag the red points up and down: each is the degree over one point of the extension AB, and the top of the figure is the curve through them. The gold strips are the quality measured piece by piece; the dashed blue line is the mean degree, the uniform quality of the same quantity."]
                       [:p "Try this:"]
                       (into [:<>] (for [[label v] presets] (preset !q label v)))
                       (preset !q "Reset to Oresme's triangle" author-state)]))))))))

(defn grades [] (lat/graded))
