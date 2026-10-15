(ns alexandria.euclid.playgrounds
  "The I.1 playground: A and B free, the two circles of Post. 3 and their
   meeting point C. JVM side: `kernels` (the Emmy figure for raster).
   Browser side: the render-fn `i1`; every point and length on screen is a
   raster kernel row. The I.47 playground is alexandria.euclid.play-i47."
  (:require #?(:clj [alexandria.medium.kernel :as kernel])
            #?(:clj [emmy.env :as e])
            #?(:cljs [alexandria.medium.figure :as figure])
            #?(:cljs [reagent.core :as r])))

#?(:clj
   (do
     (defn i1-figure
       "A, B, C (C the upper meeting of the circles about A and B with
        radius AB) and the sides AB, AC, BC."
       [ax ay bx by]
       (fn [_]
         (let [dx (e/- bx ax) dy (e/- by ay)
               side (e/sqrt (e/+ (e/square dx) (e/square dy)))
               cx (e/- (e/+ ax (e/* 0.5 dx)) (e/* 0.8660254037844386 dy))
               cy (e/+ ay (e/* 0.5 dy) (e/* 0.8660254037844386 dx))]
           [ax ay bx by cx cy side
            (e/sqrt (e/+ (e/square (e/- cx ax)) (e/square (e/- cy ay))))
            (e/sqrt (e/+ (e/square (e/- cx bx)) (e/square (e/- cy by))))])))

     (def i1-kernel {:f i1-figure :params [-2 0 2 0] :state [0] :opts {:simplify? false}})

     (def kernels (delay (kernel/kernels {:i1 i1-kernel})))))

#?(:cljs
   (do
     (defn- fixed [x] (if (js/isFinite x) (.toFixed (js/Number x) 3) "—"))
     (defn- at [svg ev]
       (let [p (.createSVGPoint svg)]
         (set! (.-x p) (.-clientX ev)) (set! (.-y p) (.-clientY ev))
         (let [q (.matrixTransform p (.inverse (.getScreenCTM svg)))]
           [(.-x q) (- (.-y q))])))
     (defn- line [[x y] [u v] color]
       [:line {:x1 x :y1 (- y) :x2 u :y2 (- v) :stroke color :stroke-width 0.055}])
     (defn- label [[x y] s]
       [:text {:x x :y (- y) :font-size 0.42 :fill "#fff" :font-family "serif"} s])
     (defn- handle [p nm drag]
       [:circle {:cx (first p) :cy (- (second p)) :r 0.26 :fill "#f0c960"
                 :data-drag nm :style {:cursor "grab"}
                 :on-pointer-down (fn [ev] (.preventDefault ev) (reset! drag nm))}])
     (defn- prompt [title k f] [:button {:data-preset k :on-click f :style {:margin "0.25em"}} title])

     (defn i1 [{:keys [kernels]}]
       (r/with-let [f (figure/kernel-figure (:i1 kernels))
                    state (r/atom [-2 0 2 0]) drag (atom nil) svg (atom nil)]
         (let [[ax ay bx by cx cy ab ac bc] (first (figure/points f @state [[0]]))
               A [ax ay] B [bx by] C [cx cy]
               degenerate? (< ab 0.06)]
           [:div {:data-playground "euclid-i1" :data-case (if degenerate? "coincident" "intersect")}
            [:svg {:viewBox "-7 -7 14 14" :ref #(reset! svg %)
                   :style {:width "100%" :max-width "620px" :background "#1d1f21" :touch-action "none"}
                   :on-pointer-move (fn [ev]
                                      (when (and @drag @svg)
                                        (let [[x y] (at @svg ev) a? (= @drag "A")]
                                          (swap! state assoc (if a? 0 2) x (if a? 1 3) y))))
                   :on-pointer-up #(reset! drag nil) :on-pointer-leave #(reset! drag nil)}
             [:circle {:cx ax :cy (- ay) :r ab :fill "none" :stroke "#88c4ec" :stroke-width 0.045}]
             [:circle {:cx bx :cy (- by) :r ab :fill "none" :stroke "#ef9d6a" :stroke-width 0.045}]
             (when-not degenerate? [:g (line A B "#fff") (line A C "#fff") (line B C "#fff") (label C "C")])
             (label A "A") (label B "B") (handle A "A" drag) (handle B "B" drag)]
            [:div {:data-readout true} "AB = " (fixed ab) "; AC = " (fixed ac) "; BC = " (fixed bc)]
            [:strong {:data-live-case true}
             (if degenerate?
               "Coincident circles: no unique meeting C; Hilbert’s continuity axiom makes the missing intersection assumption explicit."
               "Two intersections: choose C; AB = AC = BC (I.1).")]
            [:div "Try this: " (prompt "Collapse B onto A" "collapse" #(reset! state [0 0 0 0]))
             (prompt "Move A and B apart" "apart" #(reset! state [-2 -1 2 1]))
             (prompt "Reset Euclid" "euclid" #(reset! state [-2 0 2 0]))]])))))
