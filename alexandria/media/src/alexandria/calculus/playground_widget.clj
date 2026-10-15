(ns alexandria.calculus.playground-widget
  "Free-form setups beside Leibniz's and Newton's narrated constructions."
  (:require [alexandria.calculus.playground :as math]
            [alexandria.medium.tie :as tie]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [clojure.walk :as walk]))

(def defaults
  {:transmutation {:curve 2 :x 0.4 :dx 0.15}
   :quadrature {:n 8}
   :product {:u 1.2 :v 1.0 :du 0.25 :dv 0.3}
   :lemma {:curve 0 :n 8}
   :impulses {:n 8 :dt 0.2 :sx 0 :sy 0 :offaxis 0}
   :ellipse {:ecc 0.55 :p 1.5 :theta 1 :centre 0}})

(def schemas
  {:transmutation {:curve {:min 0 :max 3 :step 1 :label "curve: 0 parabola, 1 line, 2 circle, 3 cubic"}
                   :x {:min 0.05 :max 1.7 :step 0.01 :label "P: x"}
                   :dx {:min 0 :max 0.2 :step 0.005 :label "dx (Q − P)"}}
   :quadrature {:n {:min 1 :max 200 :step 1 :label "n terms"}}
   :product {:u {:min 0.1 :max 2 :step 0.01 :label "u"}
             :v {:min 0.1 :max 2 :step 0.01 :label "v"}
             :du {:min 0 :max 1 :step 0.01 :label "du (drag width)"}
             :dv {:min 0 :max 1 :step 0.01 :label "dv (drag height)"}}
   :lemma {:curve {:min 0 :max 3 :step 1 :label "curve: parabola / line / circle / cubic"}
           :n {:min 1 :max 100 :step 1 :label "n rectangles"}}
   :impulses {:n {:min 2 :max 30 :step 1 :label "impulse count"}
              :dt {:min 0.05 :max 0.35 :step 0.01 :label "dt"}
              :sx {:min -1 :max 1 :step 0.01 :label "force centre S: x"}
              :sy {:min -1 :max 1 :step 0.01 :label "force centre S: y"}
              :offaxis {:min -1 :max 1 :step 0.01 :label "impulse displaced from S"}}
   :ellipse {:ecc {:min 0 :max 0.9 :step 0.01 :label "eccentricity e"}
             :p {:min 0.5 :max 2 :step 0.01 :label "semilatus rectum"}
             :theta {:min 0 :max 6.28 :step 0.01 :label "P on ellipse: angle"}
             :centre {:min 0 :max 1 :step 1 :label "force point: 0 focus / 1 centre"}}})

(def prompts
  {:transmutation [["the circle: Leibniz's pi/4 series" {:curve 2 :x 0.4 :dx 0.15}]
                   ["try a straight line" {:curve 1 :x 0.6 :dx 0.1}]
                   ["shrink dx to zero" {:curve 2 :x 0.4 :dx 0}]]
   :quadrature [["one term: overshoot" {:n 1}] ["100 terms: slow convergence" {:n 100}]]
   :product [["make the corner visible" {:u 1 :v 1 :du 0.8 :dv 0.8}]
             ["let du vanish" {:u 1.2 :v 1 :du 0 :dv 0.3}]]
   :lemma [["one rectangle" {:curve 0 :n 1}] ["100 rectangles" {:curve 0 :n 100}]
           ["try the circle" {:curve 2 :n 20}]]
   :impulses [["displace the impulse: equal areas break" {:n 8 :dt 0.2 :sx 0 :sy 0 :offaxis 0.7}]
              ["restore the central impulse" {:n 8 :dt 0.2 :sx 0 :sy 0 :offaxis 0}]]
   :ellipse [["force at the centre: r law" {:ecc 0.55 :p 1.5 :theta 1 :centre 1}]
             ["circular orbit around focus" {:ecc 0 :p 1.5 :theta 1 :centre 0}]]})

(defn- figure [kind !s marks]
  (let [pf (fn [params f] (ev/with-params {:atom !s :params params} f))]
    (case kind
      :transmutation
      (mafs/mafs {:view-box {:x [-0.2 2.1] :y [-0.4 1.9]} :height 340}
                 (mafs/cartesian)
                 (mafs/of-x {:y (pf [:curve] (fn [c] (fn [x] (math/height c x)))) :color :blue})
                 (mafs/of-x {:y (pf [:curve :x] (fn [c x0] (fn [x]
                                                            (e/+ (math/height c x0)
                                                                 (e/* (math/slope c x0) (e/- x x0))))))
                             :color :red :style "dashed"})
                 [marks])
      :quadrature
      (mafs/mafs {:view-box {:x [0 202] :y [0.3 1.2]} :height 340}
                 (mafs/cartesian) [marks])
      :product
      (mafs/mafs {:view-box {:x [-0.1 3.2] :y [-0.1 3.2]} :height 340}
                 (mafs/cartesian) [marks])
      :lemma
      (mafs/mafs {:view-box {:x [-0.1 1.15] :y [-0.1 1.6]} :height 340}
                 (mafs/cartesian)
                 (mafs/of-x {:y (pf [:curve] (fn [c] (fn [x] (math/height c x)))) :color :blue})
                 [marks])
      :impulses
      (mafs/mafs {:view-box {:x [-2.3 2.3] :y [-2.3 2.3]} :height 340}
                 (mafs/cartesian) [marks])
      :ellipse
      (mafs/mafs {:view-box {:x [-4 4] :y [-3 3]} :height 340}
                 (mafs/cartesian)
                 (mafs/parametric {:xy (pf [:ecc :p] (fn [ecc p]
                                                       (fn [t] (let [r (e// p (e/+ 1 (e/* ecc (e/cos t))))]
                                                                 [(e/* r (e/cos t)) (e/* r (e/sin t))]))))
                                    :t [0 (* 2 Math/PI)] :color :blue})
                 [marks]))))

(defn- numbers [kind]
  (case kind
    :transmutation math/triangle
    :quadrature math/series-kernel
    :product math/product
    :lemma math/lemma-kernel
    :impulses math/impulse-readout
    :ellipse math/orbit-readout))

(defn- readout [kind r]
  (let [n (fn [i] (tie/fmt (list 'aget 'r i) 4))]
    (case kind
      :transmutation [:div [:div "P, Q: y = " (n 0) ", y+dy = " (n 1) "; dy = " (n 2)]
                      [:div "dy/dx = " (n 3) "; tangent intercept z = " (n 4)]
                      [:div "transmuted strip z·dx/2 = " (n 6) "; triangle OPQ = " (n 5)]
                      [:strong {:data-case '(if (= 2 (:curve @!s)) "circle: Leibniz's π/4 series" "different curve: circle transmutation does not apply")}
                       '(if (= 2 (:curve @!s)) "circle: Leibniz's π/4 series" "different curve: circle transmutation does not apply")]]
      :quadrature [:div [:div "Sₙ = 1 − 1/3 + 1/5 − … = " (n 0) "; π/4 = " (n 1)]
                   [:div "signed error = " (n 2) "; next term = " (n 3)]
                   [:strong {:data-case '(if (pos? (aget r 2)) "overshoot" "undershoot")}
                    '(if (pos? (aget r 2)) "overshoot" "undershoot")]]
      :product [:div [:div "uv = " (n 0) "; u dv = " (n 1) "; v du = " (n 2)]
                [:div "corner du·dv = " (n 3) "; finite Δ(uv) = " (n 4)]
                [:strong {:data-case '(if (< (aget r 3) 0.0001) "d(uv) = u dv + v du" "finite corner: Δ(uv) = u dv + v du + du dv")}
                 '(if (< (aget r 3) 0.0001) "d(uv) = u dv + v du" "finite corner: Δ(uv) = u dv + v du + du dv")]]
      :lemma [:div [:div "inscribed = " (n 0) "; circumscribed = " (n 1)]
              [:strong {:data-case '(if (< (aget r 2) 0.03) "vanishing gap" "visible gap")}
               "difference = " (n 2) "; " '(if (< (aget r 2) 0.03) "vanishing gap" "visible gap")]]
      :impulses [:div [:div "first area = " (n 0) "; maximum = " (n 1) "; minimum = " (n 2)]
                 [:strong {:data-case '(if (< (aget r 3) 1e-7) "equal areas: central impulse" "unequal areas: impulse misses S")}
                  "area spread = " (n 3) "; " '(if (< (aget r 3) 1e-7) "equal areas: central impulse" "unequal areas: impulse misses S")]]
      :ellipse [:div [:div "P = (" (n 0) ", " (n 1) "); SP = " (n 2)]
                [:div "orbit residual = " (n 3) "; measured force = " (n 4)]
                [:strong {:data-case '(if (= 0 (:centre @!s)) "focus: 1/r²" "centre: Hooke's r law")}
                 '(if (= 0 (:centre @!s)) "focus: 1/r²" "centre: Hooke's r law")]])))

(defn- marks-form [kind !s f series-f]
  (let [ks (vec (keys (defaults kind)))
        result (list f '(array 0) (tie/state-array #(ev/get !s %) ks))
        at (fn [i] (list 'aget 'r i))]
    ;; each handle sits at the parameter it shows and writes it back
    ;; on-move: a movable-point's :atom holds the point [x y], so a cursor
    ;; into one scalar of !s cannot carry it
    (list 'fn []
          (list 'let ['r result]
                (case kind
                  :transmutation
                  (let [x (ev/get !s :x) dx (ev/get !s :dx)
                        qx (list '+ x dx) y (at 0) q (at 1) z (at 4)]
                    [:<> (mafs/polygon {:points [[0 0] [x 0] [x y]] :color :blue :fill-opacity 0.15})
                     (mafs/polygon {:points [[x y] [qx y] [qx q]] :color :violet :fill-opacity 0.3})
                     (mafs/polygon {:points [[x 0] [qx 0] [qx z] [x z]] :color :green :fill-opacity 0.24})
                     (mafs/segment {:point1 [x y] :point2 [qx q] :color :red})
                     (mafs/text "O" {:x 0 :y 0}) (mafs/text "P" {:x x :y y})
                     (mafs/text "Q" {:x qx :y q}) (mafs/text "z" {:x x :y z})
                     (mafs/movable-point {:point [x 0] :color :red
                                          :constrain '(fn [[x _]] [(max 0.05 (min 1.7 x)) 0])
                                          :on-move (list 'fn '[[x _]] (list 'swap! !s 'assoc :x '(max 0.05 (min 1.7 x))))})])
                  :quadrature
                  (let [n (ev/get !s :n)]
                    [:<> (mafs/segment {:point1 [0 (at 1)] :point2 [202 (at 1)] :color :red :style "dashed"})
                     (list 'for ['i (list 'range 1 (list 'inc n))]
                           (mafs/point {:x 'i :y (list 'aget (list f '(array 0) (list 'array 'i)) 0) :color :blue}))
                     (mafs/movable-point {:point [n 0.4] :color :violet
                                          :constrain '(fn [[x _]] [(max 1 (min 200 (js/Math.round x))) 0.4])
                                          :on-move (list 'fn '[[x _]] (list 'swap! !s 'assoc :n '(max 1 (min 200 (js/Math.round x)))))})])
                  :product
                  (let [u (ev/get !s :u) v (ev/get !s :v)
                        du (ev/get !s :du) dv (ev/get !s :dv)
                        right (list '+ u du) top (list '+ v dv)]
                    [:<> (mafs/polygon {:points [[0 0] [u 0] [u v] [0 v]] :color :blue :fill-opacity 0.18})
                     (mafs/polygon {:points [[u 0] [right 0] [right v] [u v]] :color :green :fill-opacity 0.3})
                     (mafs/polygon {:points [[0 v] [u v] [u top] [0 top]] :color :violet :fill-opacity 0.3})
                     (mafs/polygon {:points [[u v] [right v] [right top] [u top]] :color :red :fill-opacity 0.5})
                     (mafs/text "uv" {:x 0.3 :y 0.3})
                     (mafs/text "v du" {:x right :y 0.3})
                     (mafs/text "u dv" {:x 0.3 :y top})
                     (mafs/movable-point {:point [right 0] :color :red
                                          :constrain (list 'fn '[[x _]] (list 'vector (list '+ u (list 'max 0 (list 'min 1 (list '- 'x u)))) 0))
                                          :on-move (list 'fn '[[x _]] (list 'swap! !s 'assoc :du (list 'max 0 (list 'min 1 (list '- 'x u)))))})
                     (mafs/movable-point {:point [0 top] :color :violet
                                          :constrain (list 'fn '[[_ y]] (list 'vector 0 (list '+ v (list 'max 0 (list 'min 1 (list '- 'y v))))))
                                          :on-move (list 'fn '[[_ y]] (list 'swap! !s 'assoc :dv (list 'max 0 (list 'min 1 (list '- 'y v)))))})])
                  :lemma [:<>] :impulses [:<>] :ellipse [:<>])))))

(defn- template [kind !s]
  (let [ks (vec (keys (defaults kind)))
        [f compiled] (tie/kernel-binding "play" (numbers kind) (mapv (defaults kind) ks))
        marks (marks-form kind !s f nil)
        text (walk/postwalk-replace {'!s !s} (readout kind 'r))]
    (list 'reagent.core/with-let [f compiled 'marks marks]
          (list 'let ['r (list f '(array 0) (tie/state-array #(ev/get !s %) ks))]
                [:div {:data-playground (name kind)}
                 [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em"}}
                  [:div {:style {:flex "1 1 350px"}} (figure kind !s 'marks)]
                  [:div {:style {:flex "1 1 280px"}}
                   (leva/sub-panel {:fill true :flat true :title-bar false}
                                   (leva/controls {:atom !s :schema (schemas kind)}))
                   text]]
                 ;; a bare lazy seq of buttons inside hiccup is read by sci as
                 ;; a call ("Invalid arity") and the whole playground fails to
                 ;; mount: the buttons go in with into
                 (-> [:div "Try this: "]
                     (into (for [[label state] (prompts kind)]
                             [:button {:type "button" :style {:margin "0.3em"}
                                       :on-click (list 'fn [] (list 'reset! !s state))} label]))
                     (conj [:button {:type "button" :on-click (list 'fn [] (list 'reset! !s (defaults kind)))} "Reset"]))]))))

(defn playground [kind]
  (binding [vc/*backend* :raster]
    (ev/with-let [!s (defaults kind)]
      (template kind !s))))
