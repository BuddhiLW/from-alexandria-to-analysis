(ns alexandria.riemann.playground-scenes
  "Four distinct interactive figures; all sampled coordinates and readouts come from wasm raster."
  (:require [alexandria.medium.figure :as figure]
            [alexandria.medium.svg :as svg]
            [reagent.core :as r]))

(defn num [x] (.toFixed (js/Number x) 3))
(defn button [state label patch]
  [:button {:type "button" :on-click #(swap! state merge patch)} label])
(defn sliders [state specs]
  [:div {:class "playground-controls"}
   (for [[k label lo hi step] specs]
     ^{:key (name k)} [:label label " " (num (get @state k)) " "
                       [:input {:type "range" :min lo :max hi :step step :value (get @state k)
                                :data-play-control (name k)
                                :on-change #(swap! state assoc k (js/parseFloat (.. % -target -value)))}]])])
(defn draggable [state key x y lo hi]
  [:circle {:cx x :cy (- y) :r 0.025 :fill "#e06442" :style {:cursor "ew-resize"}
            :data-drag (name key)
            :on-pointer-down (fn [evt] (.setPointerCapture (.-currentTarget evt) (.-pointerId evt)))
            :on-pointer-move (fn [evt]
                               (when (.hasPointerCapture (.-currentTarget evt) (.-pointerId evt))
                                 (let [box (.getBoundingClientRect (.-ownerSVGElement (.-currentTarget evt)))
                                       fraction (/ (- (.-clientX evt) (.-left box)) (.-width box))]
                                   (swap! state assoc key (max lo (min hi (+ lo (* fraction (- hi lo))))))))) }])
(defn frame [window & children]
  (into [:svg {:view-box (apply svg/view-box window)
               :style {:width "100%" :height "310px" :background "#faf7f0"}}] children))
(defn curve [ps colour]
  (svg/polyline ps {:stroke colour :width 0.007}))
(defn sample [f params lo hi count]
  (figure/points f params (mapv #(vector (+ lo (* (/ % count) (- hi lo)))) (range (inc count)))))
(defn playground [id initial kernel]
  (r/with-let [state (r/atom initial)
               f (figure/kernel-figure kernel)]
    (let [{:keys [kind n tag family eps point a b x zoom]} @state
          params (case id :integral [kind n] :jumps [1 n] :uniform [family n] :monster [a b n])
          heading (case id :integral "Riemann's tagged partition" :jumps "Riemann's dense jumps"
                        :uniform "Weierstrass' uniform tube" :monster "Weierstrass' monster")]
      [:section {:class "rigor-playground" :data-playground (name id)}
       [:h4 heading]
       (case id
         :integral
         (let [m (max 2 (int n)) d (/ 1 m)
               ps (sample f params 0 1 160)
               cells (mapv (fn [i]
                             (let [x0 (* i d) x1 (+ x0 d)
                                   samples (sample f params x0 x1 12)
                                   ys (map second samples)
                                   tagged (second (first (sample f params (+ x0 (* tag d)) (+ x0 (* tag d)) 1)))]
                               {:x0 x0 :x1 x1 :lo (apply min ys) :hi (apply max ys) :tagged tagged})) (range m))
               upper (reduce + (map #(* d (:hi %)) cells))
               lower (reduce + (map #(* d (:lo %)) cells))
               tagged (reduce + (map #(* d (:tagged %)) cells))]
           [:div
            (frame [[-0.04 1.04] [-0.16 1.35]]
                   (for [{:keys [x0 x1 lo hi tagged]} cells]
                     ^{:key x0} [:g
                                 (svg/polygon [[x0 0] [x1 0] [x1 hi] [x0 hi]] {:fill "#dcad43" :opacity 0.16})
                                 (svg/polygon [[x0 0] [x1 0] [x1 tagged] [x0 tagged]] {:fill "#537caa" :opacity 0.25 :stroke "#537caa" :width 0.003})
                                 (svg/polygon [[x0 lo] [x1 lo] [x1 hi] [x0 hi]] {:fill "#e06442" :opacity 0.3})])
                   (curve ps "#204866")
                   (draggable state :tag tag 0 0 1))
            [:p {:data-case true} (if (= kind 0) "x²: integrable — oscillation shrinks" "Riemann's sum: dense jumps, still integrable")]
            [:p {:data-readout true} "L = " (num lower) "; S = " (num tagged) "; U = " (num upper)
             "; ΣδᵢDᵢ = U − L = " (num (- upper lower))]
            (sliders state [[:kind "f: x² (0) / Σ(kx)/k² (1)" 0 1 1] [:n "partition n" 2 32 1] [:tag "εᵢ tags" 0 1 0.01]])
            [:p "Try this: refine to 24 pieces; move every tag to the right; switch to Riemann's jump series."]
            (button state "Refine" {:n 24}) (button state "Right tags" {:tag 1})
            (button state "Dense jumps" {:kind 1 :n 12}) (button state "Reset" initial)])
         :jumps
         (let [ps (sample f params 0 1 220)
               y (second (first (sample f params tag tag 1)))]
           [:div (frame [[-0.04 1.04] [-0.8 0.9]] (curve ps "#204866")
                        (svg/segment [tag -0.7] [tag 0.7] {:stroke "#e06442" :width 0.003 :dash "0.02 0.02"})
                        (draggable state :tag tag y 0 1))
            [:p {:data-case true} "Dense jumps: the partial sum remains Riemann-integrable"]
            [:p {:data-readout true} "Sₙ(" (num tag) ") = " (num y) "; N = " (num n)]
            (sliders state [[:n "terms N" 2 12 1] [:tag "inspect x" 0 1 0.01]])
            [:p "Try this: inspect x = 1/2; increase N; return to the first jump."]
            (button state "Halfway jump" {:tag 0.5}) (button state "More terms" {:n 12}) (button state "Reset" initial)])
         :uniform
         (let [lo 0 hi (if (zero? family) 0.94 js/Math.PI)
               ps (sample f params lo hi 180)
               limit (fn [xx] (if (zero? family) (/ 1 (- 1 xx)) (if (< xx js/Math.PI) (/ xx 2) 0)))
               errors (map (fn [[xx yy]] (js/Math.abs (- yy (limit xx)))) ps)
               sup (apply max errors)
               tube (mapv (fn [[xx _]] [xx (+ (limit xx) eps)]) ps)
               bottom (mapv (fn [[xx _]] [xx (- (limit xx) eps)]) ps)
               scale (if (zero? family) 18 2.5)]
           [:div (frame [[lo hi] [-1 scale]]
                        (curve tube "#70a481") (curve bottom "#70a481")
                        (curve ps "#204866")
                        (draggable state :point point 0 lo hi))
            [:p {:data-case true} (if (zero? family) "Geometric: uniform on [0, 0.94]" "Abel: pointwise but not uniform near π")]
            [:p {:data-readout true} "sup |Sₙ − f| ≈ " (num sup) "; ε = " (num eps)
             (if (<= sup eps) " — inside tube" " — outside tube")]
            (sliders state [[:family "geometric (0) / Abel (1)" 0 1 1] [:n "terms N" 1 16 1]
                            [:eps "ε tube" 0.02 1 0.01] [:point "inspect x" 0 0.94 0.01]])
            [:p "Try this: raise N for geometric convergence; switch to Abel and inspect the endpoint; widen ε."]
            (button state "Large N" {:n 15}) (button state "Abel endpoint" {:family 1 :point 0.94})
            (button state "Wide tube" {:eps 0.8}) (button state "Reset" initial)])
         :monster
         (let [left (max -1 (- x zoom)) right (min 1 (+ x zoom))
               ps (sample f params left right 180)
               y (second (first (sample f params x x 1)))
               h (max 0.001 (/ zoom 5))
               y2 (second (first (sample f params (+ x h) (+ x h) 1)))
               slope (/ (- y2 y) h)
               ys (map second ps)
               yl (- (apply min ys) 0.3) yh (+ (apply max ys) 0.3)]
           [:div (frame [[left right] [yl yh]]
                        (curve ps "#204866")
                        (svg/segment [x y] [(+ x h) y2] {:stroke "#e06442" :width 0.01})
                        (svg/polygon [[left yl] [right yl] [right yh] [left yh]] {:stroke "#70a481" :width 0.003})
                        (draggable state :x x y left right))
            [:p {:data-case true} (if (and (odd? (int a)) (> (* a b) (+ 1 (* 1.5 js/Math.PI))))
                                    "ab > 1 + 3π/2: theorem applies" "hypothesis broken: no nowhere-differentiability conclusion")]
            [:p {:data-readout true} "Wₙ(x) = " (num y) "; ΔW/Δx = " (num slope)
             "; ab = " (num (* a b)) "; threshold = " (num (+ 1 (* 1.5 js/Math.PI))) " (finite secant, not a tangent)"]
            (sliders state [[:a "odd a" 3 21 2] [:b "b" 0.1 0.9 0.01] [:n "terms N" 1 9 1]
                            [:x "point x" -0.8 0.8 0.01] [:zoom "zoom half-width" 0.02 0.8 0.01]])
            [:p "Try this: zoom into the graph; break the ab bound; restore Weierstrass' parameters."]
            (button state "Zoom in" {:zoom 0.03}) (button state "Break hypothesis" {:a 3 :b 0.3})
            (button state "Reset" initial)]))])))

(defn render [value] [playground (:scene value) (:initial value) (get-in value [:kernels :curve])])
