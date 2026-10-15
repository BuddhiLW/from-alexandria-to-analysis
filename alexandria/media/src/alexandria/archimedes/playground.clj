(ns alexandria.archimedes.playground
  "Free-form constructions for Archimedes and Eudoxus, on raster."
  (:require [alexandria.archimedes.widgets :as aw]
            [alexandria.medium.tie :as tie]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]))

(def ^:private pi Math/PI)
(defn circle-values [n r]
  (fn [_] [(e/* 2 n r (e/sin (e// pi n)))
           (e/* 2 n r (e/tan (e// pi n)))
           (e/* 2 pi r)]))

(defn circle-figure [!p !drag values]
  (let [at (fn [i] (list 'aget values i))
        fmt (fn [i] (tie/fmt (at i) 5))
        param (fn [f] (ev/with-params {:atom !p :params [:n :r]} f))]
    [:div
     (mafs/mafs {:view-box {:x [-2.3 2.3] :y [-2.1 2.1]} :height 360}
       (mafs/cartesian)
       (mafs/parametric {:xy (ev/with-params {:atom !p :params [:r]}
                                          (fn [r] (fn [t] [(e/* r (e/cos t)) (e/* r (e/sin t))])))
                         :t [0 (* 2 pi)] :color :blue})
       (mafs/parametric {:xy (param (fn [n r] (aw/polygon-curve n (e/* r (e/cos (e// pi n))))))
                         :t [0 (* 2 pi)] :color :green})
       (mafs/parametric {:xy (param (fn [n r] (aw/polygon-curve n r)))
                         :t [0 (* 2 pi)] :color :red})
       (mafs/movable-point {:point [(ev/get !p :r) 0], :color :blue, :constrain '(fn [[x _]] [(max 0.1 (min 1.9 (js/Math.abs x))) 0]), :on-move (list 'fn '[[x _]] (list 'swap! !p 'assoc :r '(max 0.1 (min 1.9 (js/Math.abs x)))))})
       (mafs/text "r" {:x (ev/get !p :r) :y 0.12}))
     [:p "Inscribed perimeter p = " (fmt 0) "; circumscribed perimeter P = " (fmt 1)
      "; circumference C = " (fmt 2) ".  p < C < P."]
     [:p "Archimedes' bracket: 3 10/71 < π < 3 1/7. "
      [:strong {:data-case-circle (list 'if (list '= (ev/get !p :n) 96) "96-gon" "doubling in progress")}
       (list 'if (list '= (ev/get !p :n) 96) "96-gon" "doubling in progress")]]
     [:p "Try this: double the sides from six to 96; compare green and red as the gap closes."]]))

(defn circle []
  (binding [vc/*backend* :raster]
    (ev/with-let [!p {:n 6 :r 1} !drag [1 0]]
      (let [pf (fn [f] (ev/with-params {:atom !p :params [:n :r]} f))
            [bind {out :out}] (vc/compile-1d {:out (pf circle-values)} :out)]
        [:div {:data-figure "circle-playground"}
         (vc/wrap [bind] (list 'let ['v (list out 0)] (circle-figure !p !drag 'v)))
         (leva/controls {:atom !p :schema {:n {:min 6 :max 96 :step 1 :label "polygon sides n"}
                                               :r {:min 0.1 :max 1.9 :step 0.01 :label "radius r"}}})
         [:button {:on-click (list 'fn '[] (list 'reset! !p {:n 96 :r 1}))} "Archimedes' 96-gon"]
         [:button {:on-click (list 'fn '[] (list 'reset! !p {:n 6 :r 1}))} "Reset to hexagon"]]))))

(defn square-values [r]
  (fn [_] [(e/* pi (e/square r)) (e/* 4 (e/square r))
           (e// pi 4) (e/- (e// pi 4) (/ 11 14))]))

(defn square-figure [!p !drag v]
  (let [r (ev/get !p :r) at (fn [i] (list 'aget v i))]
    [:div
     (mafs/mafs {:view-box {:x [-2.3 2.3] :y [-2.3 2.3]} :height 340}
       (mafs/cartesian)
       (mafs/polygon {:points [[(list '- r) (list '- r)] [r (list '- r)] [r r] [(list '- r) r]]
                      :color :red :fill-opacity 0.08})
       (mafs/parametric {:xy (ev/with-params {:atom !p :params [:r]}
                                          (fn [r] (fn [t] [(e/* r (e/cos t)) (e/* r (e/sin t))])))
                         :t [0 (* 2 pi)] :color :blue})
       (mafs/segment {:point1 [(list '- r) 0] :point2 [r 0] :color :orange})
       (mafs/text "diameter" {:x 0 :y 0.15})
       (mafs/movable-point {:point [(ev/get !p :r) 0], :color :blue, :constrain '(fn [[x _]] [(max 0.1 (min 1.9 (js/Math.abs x))) 0]), :on-move (list 'fn '[[x _]] (list 'swap! !p 'assoc :r '(max 0.1 (min 1.9 (js/Math.abs x)))))}))
     [:p "Circle area = " (tie/fmt (at 0)) "; square on the diameter = " (tie/fmt (at 1))
      "; true ratio π/4 = " (tie/fmt (at 2) 6) "; 11:14 = 0.785714; residual = " (tie/fmt (at 3) 6) "."]
     [:strong {:data-case-square "estimate"} "11:14 is an upper estimate, not an equality"]
     [:p "Try this: drag the diameter; areas change, but their ratio does not."]]))

(defn square []
  (binding [vc/*backend* :raster]
    (ev/with-let [!p {:r 1} !drag [1 0]]
      (let [[bind {out :out}] (vc/compile-1d
                               {:out (ev/with-params {:atom !p :params [:r]} square-values)} :out)]
        [:div {:data-figure "square-playground"}
         (vc/wrap [bind] (list 'let ['v (list out 0)] (square-figure !p !drag 'v)))
         (leva/controls {:atom !p :schema {:r {:min 0.1 :max 1.9 :step 0.01 :label "radius r"}}})
         [:button {:on-click (list 'fn '[] (list 'reset! !p {:r 1}))} "Reset to Archimedes' circle"]]))))

(defn lever-values [t h]
  (fn [_] (let [op (e/- 1 (e/square t))
                mo (e/* 2 (e/- 1 t))
                kn (e/+ t 1)]
            [op mo kn (e/* op h) (e/* mo kn) (e/- (e/* op h) (e/* mo kn))])))

(defn lever-figure [!p !slice v]
  (let [t (ev/get !p :t) h (ev/get !p :h)
        at (fn [i] (list 'aget v i))
        y (fn [x] (list '- 1 (list '* x x)))]
    [:div
     (mafs/mafs {:view-box {:x [-3.6 2.4] :y [-0.4 4.7]} :height 400}
       (mafs/cartesian)
       (mafs/parametric {:xy (fn [x] [x (e/- 1 (e/square x))]) :t [-1 1] :color :blue})
       (mafs/polygon {:points [[-1 0] [0 1] [1 0]] :color :blue :fill-opacity 0.12})
       (mafs/segment {:point1 [-1 0] :point2 [1 0] :color :blue})
       (mafs/segment {:point1 [1 0] :point2 [-1 4] :color :orange})
       (mafs/segment {:point1 [-3 2] :point2 [1 2] :color :black :weight 3})
       (mafs/polygon {:points [[-1 2] [-1.15 1.72] [-0.85 1.72]] :color :black})
       (mafs/segment {:point1 [t 0] :point2 [t (at 0)] :color :blue :weight 4})
       (mafs/segment {:point1 [t 0] :point2 [t (at 1)] :color :orange})
       (mafs/segment {:point1 [-3 2] :point2 [-3 (list '+ 2 (at 0))] :color :blue :weight 4})
       (mafs/text "H" {:x -3 :y 1.8})
       (mafs/text "K" {:x -1 :y 1.8})
       (mafs/text "M" {:x t :y (at 1)})
       (mafs/text "N" {:x t :y (list '- 1 t)})
       (mafs/text "O" {:x t :y 0})
       (mafs/text "P" {:x t :y (at 0)})
       (mafs/text "A" {:x -1 :y 0})
       (mafs/text "B" {:x 0 :y 1})
       (mafs/text "C" {:x 1 :y 0})
       (mafs/movable-point {:point [t 0], :color :red, :constrain '(fn [[x _]] [(max -0.99 (min 0.99 x)) 0]), :on-move (list 'fn '[[x _]] (list 'swap! !p 'assoc :t '(max -0.99 (min 0.99 x))))}))
     [:p "OP = " (tie/fmt (at 0)) ", MO = " (tie/fmt (at 1)) ", KN = " (tie/fmt (at 2))
      "; OP × HK = " (tie/fmt (at 3)) ", MO × KN = " (tie/fmt (at 4))
      "; residual = " (tie/fmt (at 5)) "."]
     [:strong {:data-case-lever (list 'if (list '< (list 'js/Math.abs (at 5)) 0.001)
                                       "in equilibrium" (list 'if (list '> (at 5) 0) "tips left" "tips right"))}
      (list 'if (list '< (list 'js/Math.abs (at 5)) 0.001)
            "in equilibrium" (list 'if (list '> (at 5) 0) "tips left" "tips right"))]
     [:p "Try this: drag O to the vertex; move H off its prescribed arm to break the balance."]]))

(defn lever []
  (binding [vc/*backend* :raster]
    (ev/with-let [!p {:t 0.3 :h 2} !slice [0.3 0]]
      (let [[bind {out :out}] (vc/compile-1d
                               {:out (ev/with-params {:atom !p :params [:t :h]} lever-values)} :out)]
        [:div {:data-figure "lever-playground"}
         (vc/wrap [bind] (list 'let ['v (list out 0)] (lever-figure !p !slice 'v)))
         (leva/controls {:atom !p :schema {:t {:min -0.99 :max 0.99 :step 0.01 :label "slice O, x = t"}
                                               :h {:min 0.5 :max 3.5 :step 0.01 :label "arm HK"}}})
         [:button {:on-click (list 'fn '[] (list 'reset! !p {:t 0.3 :h 2}))} "Reset: balanced H"]
         [:button {:on-click (list 'fn '[] (list 'swap! !p 'assoc :h 3))} "Try: H farther left"]]))))

(defn quadrature-values [a b depth]
  (fn [_] (let [width (e/- b a)
                height (e// (e/square width) 4)
                first-area (e// (e/* width height) 2)
                factor (e/exp (e/* (e/+ depth 1) (Math/log 0.25)))
                partial (e/* first-area (e// (e/- 1 factor) 0.75))]
            [first-area partial (e/* first-area 4/3) (e/* first-area (e// factor 0.75))])))

(defn quadrature-figure [!p !a !b v]
  (let [a (ev/get !p :a) b (ev/get !p :b) depth (ev/get !p :depth)
        at (fn [i] (list 'aget v i))
        y (fn [x] (list '- 1 (list '* x x)))
        colours [:blue :green :orange :red :violet :pink]]
    [:div
     (mafs/mafs {:view-box {:x [-1.7 1.7] :y [-0.2 1.5]} :height 370}
       (mafs/cartesian)
       (mafs/parametric {:xy (fn [x] [x (e/- 1 (e/square x))]) :t [-1.6 1.6] :color :blue})
       (mafs/segment {:point1 [a (y a)] :point2 [b (y b)] :color :black :weight 3})
       (for [generation (range 6) i (range (bit-shift-left 1 generation))
             :let [count (bit-shift-left 1 generation)
                   left (list '+ a (list '* (list '- b a) (/ i count)))
                   right (list '+ a (list '* (list '- b a) (/ (inc i) count)))
                   mid (list '/ (list '+ left right) 2)]]
         (list 'when (list '<= generation depth)
               (mafs/polygon {:points [[left (y left)] [mid (y mid)] [right (y right)]]
                              :color (colours generation) :fill-opacity 0.12})))
       (mafs/text "A" {:x a :y (y a)})
       (mafs/text "C" {:x b :y (y b)})
       (mafs/text "B" {:x (list '/ (list '+ a b) 2)
                        :y (y (list '/ (list '+ a b) 2))})
       (mafs/movable-point {:point [a (y a)], :color :red, :constrain '(fn [[x _]] [(max -1.5 (min 0.9 x)) (- 1 (* x x))]), :on-move (list 'fn '[[x _]] (list 'swap! !p 'assoc :a '(max -1.5 (min 0.9 x))))})
       (mafs/movable-point {:point [b (y b)], :color :orange, :constrain '(fn [[x _]] [(max -0.9 (min 1.5 x)) (- 1 (* x x))]), :on-move (list 'fn '[[x _]] (list 'swap! !p 'assoc :b '(max -0.9 (min 1.5 x))))}))
     [:p "First triangle T = " (tie/fmt (at 0)) "; partial sum S = " (tie/fmt (at 1))
      "; 4T/3 = " (tie/fmt (at 2)) "; remainder = " (tie/fmt (at 3)) "."]
     [:strong {:data-case-quadrature (list 'if (list '< (list '- b a) 0.05) "degenerate chord" "one quarter per generation")}
      (list 'if (list '< (list '- b a) 0.05) "degenerate chord" "one quarter per generation")]
     [:p "Try this: drag A and C together; double the triangles to see the uncovered area shrink."]]))

(defn quadrature []
  (binding [vc/*backend* :raster]
    (ev/with-let [!p {:a -1 :b 1 :depth 2} !a [-1 0] !b [1 0]]
      (let [[bind {out :out}] (vc/compile-1d
                               {:out (ev/with-params {:atom !p :params [:a :b :depth]} quadrature-values)} :out)]
        [:div {:data-figure "quadrature-playground"}
         (vc/wrap [bind] (list 'let ['v (list out 0)] (quadrature-figure !p !a !b 'v)))
         (leva/controls {:atom !p :schema {:a {:min -1.5 :max 0.9 :step 0.01 :label "chord A, x"}
                                               :b {:min -0.9 :max 1.5 :step 0.01 :label "chord C, x"}
                                               :depth {:min 0 :max 5 :step 1 :label "triangle generations n"}}})
         [:button {:on-click (list 'fn '[] (list 'reset! !p {:a -1 :b 1 :depth 2}))} "Reset: symmetric chord"]
         [:button {:on-click (list 'fn '[] (list 'swap! !p 'assoc :depth 5))} "Try: six generations"]]))))

(defn eudoxus-values [r s k]
  (fn [_] (let [sides (e/exp (e/* k (Math/log 2)))
                share (e// (e/* sides (e/sin (e// (e/* 2 pi) sides))) (e/* 2 pi))
                ar (e/* pi (e/square r))
                as (e/* pi (e/square s))]
            [ar as (e// ar as) (e// (e/square (e/* 2 r)) (e/square (e/* 2 s)))
             (e/- 1 share) (e/* ar share) (e/* as share)])))

(defn eudoxus-figure [!p !r !s v]
  (let [r (ev/get !p :r) s (ev/get !p :s) k (ev/get !p :k)
        at (fn [i] (list 'aget v i))
        polygon (fn [key offset colour]
                  (mafs/parametric {:xy (ev/with-params {:atom !p :params [:r :s :k]}
                                                      (fn [r s k]
                                                        (let [radius (if (= key :r) r s)
                                                              n (e/exp (e/* k (Math/log 2)))
                                                              poly (aw/polygon-curve n (e/* radius (e/cos (e// pi n))))]
                                                          (fn [t] (let [[x y] (poly t)] [(e/+ offset x) y])))))
                                    :t [0 (* 2 pi)] :color colour}))]
    [:div
     (mafs/mafs {:view-box {:x [-2.1 5] :y [-2 2]} :height 350}
       (mafs/cartesian)
       (mafs/parametric {:xy (ev/with-params {:atom !p :params [:r]}
                                          (fn [r] (fn [t] [(e/* r (e/cos t)) (e/* r (e/sin t))])))
                         :t [0 (* 2 pi)] :color :blue})
       (mafs/parametric {:xy (ev/with-params {:atom !p :params [:s]}
                                          (fn [s] (fn [t] [(e/+ 3 (e/* s (e/cos t))) (e/* s (e/sin t))])))
                         :t [0 (* 2 pi)] :color :red})
       (polygon :r 0 :green)
       (polygon :s 3 :orange)
       (mafs/segment {:point1 [(list '- r) 0] :point2 [r 0] :color :blue})
       (mafs/segment {:point1 [(list '- 3 s) 0] :point2 [(list '+ 3 s) 0] :color :red})
       (mafs/movable-point {:point [r 0], :color :blue, :constrain '(fn [[x _]] [(max 0.1 (min 1.8 (js/Math.abs x))) 0]), :on-move (list 'fn '[[x _]] (list 'swap! !p 'assoc :r '(max 0.1 (min 1.8 (js/Math.abs x)))))})
       (mafs/movable-point {:point [(list '+ 3 s) 0], :color :red, :constrain '(fn [[x _]] [(max 3.1 (min 4.8 x)) 0]), :on-move (list 'fn '[[x _]] (list 'swap! !p 'assoc :s '(max 0.1 (min 1.8 (- x 3)))))}))
     [:p "Circle areas = " (tie/fmt (at 0)) " : " (tie/fmt (at 1))
      "; their ratio = " (tie/fmt (at 2)) "; squares on diameters = " (tie/fmt (at 3))
      "; polygon areas = " (tie/fmt (at 5)) " : " (tie/fmt (at 6))
      "; exhaustion error = " (tie/fmt (at 4) 6) "."]
     [:strong {:data-case-eudoxus (list 'if (list '< (at 4) 0.01) "nearly exhausted" "still uncovered")}
      (list 'if (list '< (at 4) 0.01) "nearly exhausted" "still uncovered")]
     [:p "Try this: change both diameters independently; double the polygons until little remains."]]))

(defn eudoxus []
  (binding [vc/*backend* :raster]
    (ev/with-let [!p {:r 1 :s 0.7 :k 3} !r [1 0] !s [3.7 0]]
      (let [[bind {out :out}] (vc/compile-1d
                               {:out (ev/with-params {:atom !p :params [:r :s :k]} eudoxus-values)} :out)]
        [:div {:data-figure "eudoxus-playground"}
         (vc/wrap [bind] (list 'let ['v (list out 0)] (eudoxus-figure !p !r !s 'v)))
         (leva/controls {:atom !p :schema {:r {:min 0.1 :max 1.8 :step 0.01 :label "first radius"}
                                               :s {:min 0.1 :max 1.8 :step 0.01 :label "second radius"}
                                               :k {:min 2 :max 9 :step 1 :label "doublings k (2^k sides)"}}})
         [:button {:on-click (list 'fn '[] (list 'reset! !p {:r 1 :s 0.7 :k 3}))} "Reset: two circles"]
         [:button {:on-click (list 'fn '[] (list 'swap! !p 'assoc :k 9))} "Try: 512 sides"]]))))
