(ns alexandria.oresme.series-widget
  "Oresme's infinite series on the page.

   `player`: the harmonic series grouped in halves, stepped
   (alexandria.oresme.scenes, :oresme/harmonic); the group sums and H(2^k)
   by raster on the JVM (alexandria.oresme.series/player-data).

   `playground`: the series 1/n^p (p = 1 is Oresme's) or Oresme's
   convergent n/2^n. The partial sum S(N) is a raster number: the term
   kernel for n <= 16, then the block kernel (16 terms a call) up to N,
   then terms again. Two figures:
   - the terms as bars, n = 1..32, coloured by Oresme's groups, each group
     with its floor (the least term, 2^(-kp)) and the half it guarantees;
     the top of bar 2 is DRAGGABLE (its height 1/2^p sets p);
   - the partial sums against log2 N, at every power of 2 up to N, beside
     ln N + gamma and Oresme's bound 1 + k/2 (compare kernel); the marker
     on the axis is DRAGGABLE (it sets N).
   In Oresme's convergent mode the second figure shows his own figure: the
   degree n over the n-th halving part of the hour, its area n/2^n.
   Live: the case (diverges / diverges faster / converges / the convergent
   series), the group sums, and a badge checking each group >= 1/2, which
   turns red when p > 1."
  (:require [alexandria.medium.clerk :as medium]
            [alexandria.medium.flow :as flow]
            [alexandria.oresme.latitude-widget :as lw]
            [alexandria.oresme.series :as series]
            [alexandria.proofs :as proofs]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [nextjournal.clerk :as clerk]))

(defn player
  "Oresme's grouping of the harmonic series, stepped."
  []
  (medium/player lw/render-fn :oresme/harmonic
                 (proofs/steps lw/proofs-resource :oresme/harmonic)
                 series/figures {:harmonic (series/player-data)}
                 {:window [[-0.8 17] [-1.2 5.6]] :height 360
                  :durations {:terms 5000 :group 5000 :bound 6000}}))

(def author-state "Oresme's series, the first 16 terms." {:N 16 :p 1 :mode "harmonic"})

(def presets
  [["Oresme's four groups: N = 16" {:N 16 :p 1 :mode "harmonic"}]
   ["Push on: N = 65536, the sum passes 1 + 16/2" {:N 65536 :p 1 :mode "harmonic"}]
   ["p = 2: the groups shrink, the sum converges" {:N 4096 :p 2 :mode "harmonic"}]
   ["p = 1/2: diverges faster" {:N 1024 :p 0.5 :mode "harmonic"}]
   ["Oresme's convergent series Σ n/2ⁿ" {:N 32 :p 1 :mode "oresme"}]])

(def schema
  {:N {:min 1 :max 65536 :step 1 :label "terms N"}
   :p {:min 0.3 :max 3 :step 0.01 :label "exponent p (1/nᵖ)"}
   :mode {:options ["harmonic" "oresme"] :label "series"}})

(def ^:private group-colours ["#555555" "#E6A700" "#2E8B57" "#1F6FB4" "#C0392B" "#7B5EA7"])

(defn playground
  "The harmonic series and Oresme's convergent one, N and p free."
  ([] (playground author-state))
  ([start]
   (binding [vc/*backend* :raster]
     (ev/with-let [!q start]
       (let [[tsym tform] (flow/kernel "term" series/term [1] [1])
             [bsym bform] (flow/kernel "block" series/terms [1] [1])
             [csym cform] (flow/kernel "compare" series/compare-kernel [16 4 1] [0])
             [ssym sform] (flow/kernel "stack" series/stack-figure [1] [1])
             B series/block]
         (flow/template
          {'?q !q '?t tsym '?b bsym '?c csym '?st ssym '?B B '?presets presets '?author author-state
           'emmy.leva/SubPanel 'leva.core/SubPanel 'emmy.leva/Controls 'leva.core/Controls}
          (list 'reagent.core/with-let [tsym tform bsym bform csym cform ssym sform]
                '(let [s @?q
                       N (max 1 (js/Math.round (:N s)))
                       p (:p s)
                       oresme? (= "oresme" (:mode s))
                       pa (array p)
                       term (fn [n] (?t (array n) pa))
                       ;; prefix sums at every power of two up to N, and at N
                       sums (loop [n 1 h 0 o 0 acc []]
                              (if (> n N)
                                {:h h :o o :marks acc}
                                (let [blk? (and (> n 16) (<= (+ n ?B -1) N))
                                      r (if blk? (?b (array n) pa) (term n))
                                      n2 (if blk? (+ n ?B) (inc n))
                                      h2 (+ h (aget r 0)) o2 (+ o (aget r 1))
                                      last (dec n2)
                                      acc2 (if (zero? (bit-and last (dec last))) (conj acc [last h2 o2]) acc)]
                                  (recur n2 h2 o2 acc2))))
                       H (:h sums) O (:o sums)
                       k (js/Math.floor (/ (js/Math.log N) (js/Math.log 2)))
                       cmp (?c (array 0) (array N k p))
                       marks (:marks sums)
                       groups (mapv (fn [[[_ h0] [_ h1]]] (- h1 h0)) (partition 2 1 (cons [1 (aget (term 1) 0)] (rest marks))))
                       group-ok (every? (fn [g] (>= g (- 0.5 1e-12))) groups)
                       kind (cond oresme? "oresme"
                                  (< (js/Math.abs (- p 1)) 1e-9) "harmonic"
                                  (< p 1) "faster"
                                  :else "converges")
                       bars (mapv (fn [n] [n (aget (term n) 0)]) (range 1 33))
                       stack (mapv (fn [n] (?st (array n) (array 1))) (range 1 (inc (min N 12))))
                       g-of (fn [n] (cond (= n 1) 0 (<= n 2) 1 (<= n 4) 2 (<= n 8) 3 (<= n 16) 4 :else 5))]
                   [:div {:data-playground "oresme-series" :data-case kind :data-sum (str (if oresme? O H))
                          :data-n (str N) :data-asymptotic (str (aget cmp 0))}
                    [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                     [:div {:style {:flex "1 1 420px" :min-width "300px"}}
                      [mafs.core/Mafs {:view-box {:x [-0.5 33] :y [-0.15 1.1]} :height 230 :preserve-aspect-ratio false}
                       [mafs.coordinates/Cartesian {:subdivisions 1}]
                       (into [:<>]
                             (for [[n v] bars]
                               [mafs.core/Polygon {:key n :points [[(- n 1) 0] [n 0] [n v] [(- n 1) v]]
                                                   :color (nth ["#555555" "#E6A700" "#2E8B57" "#1F6FB4" "#C0392B" "#7B5EA7"] (g-of n))
                                                   :fillOpacity (if (<= n N) 0.45 0.08) :weight 1}]))
                       (into [:<>]
                             (for [kk (range 1 6)
                                   :let [lo (js/Math.pow 2 (dec kk)) hi (js/Math.pow 2 kk)
                                         y (aget (?c (array 0) (array 2 kk p)) 3)]]
                               [mafs.line/Segment {:key kk :point1 [lo (/ y lo)] :point2 [hi (/ y lo)] :color "#111" :weight 2 :style "dashed"}]))
                       [mafs.core/MovablePoint {:point [1.5 (nth (nth bars 1) 1)] :color "#C0392B"
                                                :constrain (fn [[_ y]] [1.5 (max 0.125 (min 0.81 y))])
                                                :on-move (fn [[_ y]] (swap! ?q assoc :p (/ (js/Math.round (* 100 (- (/ (js/Math.log y) (js/Math.log 2))))) 100)))}]
                       [mafs.core/Text {:x 2.2 :y 1.0 :attach "e" :size 13} "drag the top of 1/2ᵖ to set p"]]
                      (if oresme?
                        [mafs.core/Mafs {:view-box {:x [-0.05 1.05] :y [-0.4 12.5]} :height 260 :preserve-aspect-ratio false}
                         [mafs.coordinates/Cartesian {:subdivisions 1}]
                         (into [:<>]
                               (for [o stack]
                                 [mafs.core/Polygon {:key (aget o 2) :points [[(aget o 0) 0] [(aget o 1) 0] [(aget o 1) (aget o 2)] [(aget o 0) (aget o 2)]]
                                                     :color "#7B5EA7" :fillOpacity 0.35 :weight 1}]))
                         [mafs.core/Text {:x 0.02 :y 11.5 :attach "e" :size 13} "degree n on the n-th part (1/2, 1/4, 1/8, …) of the hour"]]
                        [mafs.core/Mafs {:view-box {:x [-0.5 17] :y [-0.3 (+ 2 (max 10 (* 1.1 H)))]} :height 260 :preserve-aspect-ratio false}
                         [mafs.coordinates/Cartesian {:subdivisions 1}]
                         [mafs.core/Polyline {:points (mapv (fn [[m h]] [(/ (js/Math.log m) (js/Math.log 2)) h]) marks) :color "#C0392B" :weight 3}]
                         [mafs.core/Polyline {:points (mapv (fn [j] [j (aget (?c (array 0) (array (js/Math.pow 2 j) j p)) 0)]) (range 0 17)) :color "#1F6FB4" :weight 2 :style "dashed"}]
                         [mafs.core/Polyline {:points (mapv (fn [j] [j (aget (?c (array 0) (array (js/Math.pow 2 j) j p)) 1)]) (range 0 17)) :color "#2E8B57" :weight 2}]
                         [mafs.core/Text {:x 0.3 :y (+ 1 (max 10 (* 1.1 H))) :attach "e" :size 13} "red: Sₙ by raster · blue: ln N + γ · green: 1 + k/2   (x = log₂ N)"]
                         [mafs.core/MovablePoint {:point [(/ (js/Math.log N) (js/Math.log 2)) 0] :color "#111"
                                                  :constrain (fn [[x _]] [(max 0 (min 16 x)) 0])
                                                  :on-move (fn [[x _]] (swap! ?q assoc :N (js/Math.round (js/Math.pow 2 x))))}]])]
                     [:div {:style {:flex "0 1 330px" :min-width "260px"}}
                      [emmy.leva/SubPanel {:fill true :flat true :titleBar false}
                       [emmy.leva/Controls {:atom ?q :schema {:N {:min 1 :max 65536 :step 1 :label "terms N"}
                                                              :p {:min 0.3 :max 3 :step 0.01 :label "exponent p (1/nᵖ)"}
                                                              :mode {:options ["harmonic" "oresme"] :label "series"}}}]]
                      [:div {:style {:font-size "1.15em" :font-weight 600 :margin "0.4em 0"} :data-case-name kind}
                       (case kind
                         "harmonic" "the harmonic series (p = 1): diverges, every group ≥ ½"
                         "faster" "p < 1: the terms are larger, it diverges faster"
                         "converges" "p > 1: the groups shrink like 2^(k(1−p)): it converges"
                         "oresme" "Oresme's series Σ n/2ⁿ: converges to 2")]
                      (when-not oresme?
                        [:div {:data-invariant (if group-ok "pass" "fail")
                               :style {:display "inline-block" :padding "0.15em 0.6em" :border-radius "0.6em" :color "white"
                                       :background (if group-ok "#2E8B57" "#C0392B")}}
                         (if group-ok "✓ every group ≥ ½: Oresme's bound H(2ᵏ) ≥ 1 + k/2 holds"
                             "✗ a group falls below ½: the grouping proof needs p ≤ 1")])
                      [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.88em" :margin-top "0.4em"}}
                       (if oresme?
                         [:<>
                          [:div "S(" N ") = Σ n/2ⁿ (raster) = " [:span {:data-sum-text true} (.toFixed O 10)]]
                          [:div "2 − S(N) = (N + 2)/2ᴺ = " (.toExponential (aget cmp 4) 3)]]
                         [:<>
                          [:div "S(" N ") = Σ 1/nᵖ (raster) = " [:span {:data-sum-text true} (.toFixed H 6)]]
                          [:div "ln N + γ = " (.toFixed (aget cmp 0) 6) (when (< (js/Math.abs (- p 1)) 1e-9) (str ",  difference " (.toExponential (- H (aget cmp 0)) 3)))]
                          [:div "Oresme's bound 1 + k/2 (k = " k ") = " (.toFixed (aget cmp 1) 2)]
                          [:div "groups: " (clojure.string/join ", " (map (fn [g] (.toFixed g 3)) (take 8 groups))) (when (> (count groups) 8) ", …")]])]]]
                    [:p {:style {:font-size "0.9em" :opacity 0.85}}
                     "Top: the terms as bars, coloured by Oresme's groups (1/2; 1/3, 1/4; 1/5 … 1/8; …); the dashed floor of each group is its smallest term, and the group times its floor is the half Oresme counts. Bottom: the partial sums at each doubling of N (red, every term added by raster), the logarithm ln N + γ (blue) and Oresme's staircase 1 + k/2 (green). Drag the black marker to set N, the red handle to set p."]
                    [:p "Try this:"]
                    (into [:<>] (for [[label v] ?presets]
                                  [:button {:type "button" :data-preset label :style {:margin "0.2em"}
                                            :on-click (fn [] (reset! ?q v))} label]))
                    [:button {:type "button" :data-preset "Reset" :style {:margin "0.2em"}
                              :on-click (fn [] (reset! ?q ?author))} "Reset to Oresme's series"]]))))))))

(defn grades [] (series/graded))
