(ns alexandria.oresme.mean-widget
  "The mean degree theorem on the page.

   `player`: Oresme's proof stepped (alexandria.oresme.scenes,
   :oresme/mean), the first and last degree as player controls.

   `tied`: v0, v1, t, vm and d tied (alexandria.medium.tie over
   alexandria.oresme.mean-speed/tie): move any and the others are solved
   (vm moves both speeds with their difference held; d moves t with the
   speeds held, vm kept off 0). The figure is DRAGGABLE: the first degree
   F = (0, v0), the last C = (t, v1) (dragging it sets v1 and t) and a
   bend handle over the middle instant (b, outside the tie: it breaks the
   hypothesis). The distance is the raster sum of 64 strips of the speed
   (strip kernel), the rectangle at the mean speed overlays the figure,
   the formula with the current values updates live, the figure is named
   live, and the badge turns red when the bend makes the figure difformly
   difform. With v0 = 0 and no bend, the distances in four equal times are
   printed as multiples of the first: Galileo's odd numbers."
  (:require [alexandria.medium.clerk :as medium]
            [alexandria.medium.flow :as flow]
            [alexandria.medium.tie :as tie]
            [alexandria.oresme.latitude-widget :as lw]
            [alexandria.oresme.mean-speed :as ms]
            [alexandria.proofs :as proofs]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [nextjournal.clerk :as clerk]))

(defn player
  "Oresme's proof of the mean degree theorem, stepped."
  []
  (medium/player lw/render-fn :oresme/mean
                 (proofs/steps lw/proofs-resource :oresme/mean)
                 ms/figures {}
                 {:window [[-0.7 4.9] [-0.8 5.6]] :height 380
                  :durations {:setup 5000 :midpoint 5000 :rectangle 5000 :equal 6000 :result 6000}
                  :controls [{:id :v0 :label "first degree v₀" :min 0 :max 4 :step 0.05 :init 0}
                             {:id :v1 :label "last degree v₁ = BC" :min 0 :max 4.5 :step 0.05 :init 4}]}))

;; ---------------------------------------------------------------------------
;; The tied widget

(defn init
  "The tied state for Oresme's triangle: from rest to 4 in 4, by raster."
  []
  (zipmap ms/tied (take 5 (ms/tie-values 0 4 4 0 0))))

(def presets
  "The \"try this\" states: [label tied-state bend]."
  [["Oresme's triangle: from rest" {:v0 0 :v1 4 :t 4} 0]
   ["A trapezium: from v₀ = 2" {:v0 2 :v1 6 :t 4} 0]
   ["Uniform: v₀ = v₁" {:v0 3 :v1 3 :t 4} 0]
   ["Break it: bend the top up" {:v0 0 :v1 4 :t 4} 1.5]
   ["Break it: sag below the line" {:v0 1 :v1 5 :t 6} -1]])

(def ^:private colours {:fig "#D98E73" :rect "#1F6FB4" :strip "#E6A700" :ok "#2E8B57" :bad "#C0392B"})

(defn- katex [tex-form attr]
  (list 'let ['tex tex-form] [:div {attr 'tex} [:f> 'nextjournal.clerk.render/render-katex 'tex {:inline? false}]]))

(defn tied
  "v0, v1, t, vm, d tied; the bend breaks the theorem."
  ([] (tied (init)))
  ([start]
   (binding [vc/*backend* :raster]
     (ev/with-let [!p start !s start !b {:b 0}]
       (let [s (mapv start ms/tied)
             [tsym tform] (tie/kernel-binding "tie" ms/tie s)
             [rsym rform] (flow/kernel "readout" ms/readout [0 4 4 0] [0])
             [ksym kform] (flow/kernel "strip" ms/strip [0 4 4 0] [0])
             [csym cform] (flow/kernel "curve" ms/curve [0 4 4 0] [0])
             opts {:tie-sym tsym :p !p :s !s :keys ms/tied :ranges ms/ranges :counter "__oresmeSolves"}
             r (fn [k] (list 'aget 'ro (ms/readout-index k)))
             set-state (fn [vals b] (list 'fn []
                                          (list 'reset! '?b {:b b})
                                          (list 'doseq ['[k v] vals] '(swap! ?p assoc k v))
                                          ;; after the swaps, before their solves (setTimeout 0):
                                          ;; each solve then reads the whole preset, not one key
                                          (list 'swap! '?s 'merge vals)))]
         (flow/template
          {'?p !p '?s !s '?b !b '?r rsym '?k ksym '?c csym}
          (list 'reagent.core/with-let (into [tsym tform rsym rform ksym kform csym cform] (tie/locals opts))
                (list 'let '[st @?s b (:b @?b) v0 (:v0 st) v1 (:v1 st) t (:t st)
                             ps (array v0 v1 t b)
                             ro (?r (array 0) ps)
                             strips (mapv (fn [i] (?k (array i) ps)) (range 64))
                             dist (reduce + (map (fn [o] (aget o 2)) strips))
                             quarters (mapv (fn [q] (reduce + (map (fn [o] (aget o 2)) (subvec strips (* 16 q) (* 16 (inc q)))))) (range 4))
                             curve (mapv (fn [i] (let [o (?c (array (/ i 80)) ps)] [(aget o 0) (aget o 1)])) (range 81))
                             vm (aget ro 0) rect (aget ro 1)
                             bent (> (js/Math.abs b) 1e-3)
                             kind (cond bent "difformly-difform"
                                        (< (js/Math.abs (- v1 v0)) 1e-3) "uniform"
                                        (or (< v0 1e-3) (< v1 1e-3)) "uniformly-difform-from-none"
                                        :else "uniformly-difform")
                             holds (< (js/Math.abs (- dist rect)) 1e-6)
                             xmax (max 4.5 (* 1.08 t)) ymax (max 5 (* 1.15 (max v0 v1 (aget ro 4))))]
                      [:div {:data-figure "oresme-mean" :data-case 'kind :data-distance '(str dist)
                             :data-rect '(str rect) :data-t '(str t)}
                       [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                        [:div {:style {:flex "1 1 440px" :min-width "300px"}}
                         (mafs/mafs {:view-box {:x '[-0.3 xmax] :y '[-0.6 ymax]} :height 400 :preserve-aspect-ratio false}
                                    (mafs/cartesian {:subdivisions 1})
                                    '(into [:<>]
                                           (for [o strips]
                                             (let [sx (aget o 0) h (/ t 128) v (aget o 1)]
                                               [mafs.core/Polygon {:points [[(- sx h) 0] [(+ sx h) 0] [(+ sx h) v] [(- sx h) v]]
                                                                   :color "#E6A700" :fillOpacity 0.2 :weight 0}])))
                                    '[mafs.core/Polygon {:points [[0 0] [t 0] [t vm] [0 vm]] :color "#1F6FB4" :fillOpacity 0.18 :weight 2}]
                                    '[mafs.core/Polyline {:points curve :color "#C0392B" :weight 3}]
                                    '[mafs.line/Segment {:point1 [(/ t 2) 0] :point2 [(/ t 2) vm] :color "#1F6FB4" :style "dashed"}]
                                    '[mafs.core/Text {:x (/ t 2) :y -0.3 :attach "s" :size 14} "D"]
                                    '[mafs.core/Text {:x (/ t 2) :y vm :attach "nw" :size 14 :color "#1F6FB4"} "E"]
                                    '[mafs.core/Text {:x 0 :y -0.3 :attach "s" :size 14} "A"]
                                    '[mafs.core/Text {:x t :y -0.3 :attach "s" :size 14} "B"]
                                    '[mafs.core/Text {:x t :y v1 :attach "ne" :size 14} "C"]
                                    (mafs/movable-point {:point '[0 v0] :color "#C0392B"
                                                         :constrain '(fn [[_ y]] [0 (max 0 (min 10 y))])
                                                         :on-move '(fn [[_ y]] (swap! ?p assoc :v0 (/ (js/Math.round (* 100 y)) 100)))})
                                    (mafs/movable-point {:point '[t v1] :color "#C0392B"
                                                         :constrain '(fn [[x y]] [(max 0.2 (min 10 x)) (max 0 (min 10 y))])
                                                         :on-move '(fn [[x y]]
                                                                     (swap! ?p assoc :v1 (/ (js/Math.round (* 100 y)) 100))
                                                                     (swap! ?p assoc :t (/ (js/Math.round (* 100 x)) 100)))})
                                    (mafs/movable-point {:point '[(/ t 2) (aget ro 4)] :color "#7B5EA7"
                                                         :constrain '(fn [[_ y]] [(/ t 2) y])
                                                         :on-move '(fn [[_ y]] (swap! ?b assoc :b (/ (js/Math.round (* 100 (- y vm))) 100)))}))]
                        [:div {:style {:flex "0 1 330px" :min-width "260px"}}
                         (leva/sub-panel {:fill true :flat true :title-bar false}
                                         (leva/controls {:atom !p :schema (tie/schema ms/tied ms/ranges ms/labels)})
                                         (leva/controls {:atom !b :schema {:b {:min -3 :max 3 :step 0.01 :label "bend b (breaks it)"}}}))
                         [:div {:style {:font-size "1.15em" :font-weight 600 :margin "0.4em 0"} :data-case-name 'kind}
                          '(case kind
                             "uniform" "uniform motion: a rectangle"
                             "uniformly-difform-from-none" "uniformly difform from rest: Oresme's triangle"
                             "uniformly-difform" "uniformly difform: a trapezium"
                             "difformly difform: the top is curved")]
                         [:div {:data-invariant '(if holds "pass" "fail")
                                :style {:display "inline-block" :padding "0.15em 0.6em" :border-radius "0.6em" :color "white"
                                        :background '(if holds "#2E8B57" "#C0392B")}}
                          '(if holds "✓ distance = vₘ · t (the mean speed theorem)"
                               "✗ distance ≠ vₘ · t: the rule needs equal excesses, a straight top")]
                         (katex (list 'str "v_m = \\tfrac{v_0+v_1}{2} = \\tfrac{" (tie/fmt 'v0 2) "+" (tie/fmt 'v1 2) "}{2} = " (tie/fmt 'vm 3)) :data-vm)
                         (katex (list 'str "v_m\\,t = " (tie/fmt 'vm 3) "\\times" (tie/fmt 't 2) " = " (tie/fmt 'rect 3)) :data-rect-tex)
                         (katex (list 'str "\\textstyle\\sum_{64}v\\,\\Delta s = " (tie/fmt 'dist 4)) :data-sum-tex)
                         [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.88em"}}
                          [:div "distance by 64 strips (raster) = " [:span {:data-strip-sum true} (tie/fmt 'dist 4)]]
                          [:div "bend's share 2bt/3 = " (tie/fmt (r :bend) 4)]
                          [:div "each small triangle (v₁ − v₀)t/8 = " (tie/fmt (r :small-triangle) 4)]
                          '(when (and (not bent) (< v0 1e-3))
                             [:div {:data-odd true} "distance in each quarter of t, in units of the first: "
                              (clojure.string/join " : " (map (fn [q] (.toFixed (/ q (first quarters)) 2)) quarters))
                              " (Galileo's odd numbers)"])]]]
                       [:p {:style {:font-size "0.9em" :opacity 0.85}}
                        "The red line is the speed, the gold strips the distance covered strip by strip, the blue rectangle the uniform motion at the mean speed vₘ = DE. Drag the first degree (left red point), the last degree C (right: it moves both v₁ and t), or the purple point over the middle instant to bend the top. Every Leva control is tied: move d and t is solved back; move vₘ and both speeds shift together."]
                       [:p "Try this:"]
                       (into [:<>]
                             (for [[label vals b] presets]
                               [:button {:type "button" :data-preset label :style {:margin "0.2em"}
                                         :on-click (set-state vals b)} label]))
                       [:button {:type "button" :data-preset "Reset" :style {:margin "0.2em"}
                                 :on-click (set-state {:v0 0 :v1 4 :t 4} 0)} "Reset to Oresme's triangle"]])
                (list 'finally (tie/unwatch-form !p)))))))))

;; ---------------------------------------------------------------------------
;; The derivation

(defn derivation
  "Emmy derives the relation and solves it for each quantity, then the
   grades."
  []
  (let [v (ms/speed 'v_0 'v_1 't 0 's)
        F (fn [s] (e/+ (e/* 'v_0 s) (e// (e/* (e/- 'v_1 'v_0) (e/square s)) (e/* 2 't))))
        d (e/simplify (e/- (F 't) (F 0)))]
    (clerk/col
     (clerk/tex (str "v(s) = " (e/->TeX (e/simplify v))))
     (clerk/tex (str "d = \\int_0^t v\\,ds = " (e/->TeX d) " = " (e/->TeX (e/simplify (e/* (ms/mean 'v_0 'v_1) 't)))))
     (clerk/tex (str "t = \\frac{d}{v_m} = " (e/->TeX (e/simplify (e// 'd (ms/mean 'v_0 'v_1))))
                     ",\\qquad v_0 = v_m - \\tfrac{v_1 - v_0}{2},\\quad v_1 = v_m + \\tfrac{v_1 - v_0}{2}"))
     (clerk/html
      (into [:ul {:style {:font-size "0.9em"}}]
            (for [{:keys [label grade]} (ms/graded)]
              [:li {:data-grade (name grade)} [:code (name grade)] " " label]))))))

(defn grades [] (ms/graded))
