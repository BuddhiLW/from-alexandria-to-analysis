(ns alexandria.galileo.playground
  "Free-form playgrounds for Two New Sciences (playground brief). Every
   motion is INTEGRATED: one RK4 step of the motion's Emmy flow is a raster
   kernel (alexandria.medium.flow), the page iterates it, and every number
   printed comes from those points or from a raster readout kernel. Galileo's
   closed laws (s = aT^2/2, R = v^2 sin 2theta / g) appear only beside the
   integration, labelled as such, so the reader can see them agree, and see
   them part when a hypothesis is broken (a growing acceleration, a resisting
   medium)."
  (:require [alexandria.galileo.motion :as motion]
            [alexandria.galileo.shot :as shot]
            [alexandria.medium.flow :as flow]
            [alexandria.medium.tie :as tie]
            [emmy.env :as e]
            [emmy.leva :as leva] [emmy.mafs :as mafs]
            [emmy.viewer :as ev] [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]))

(defn- controls [state schema]
  (leva/sub-panel {:fill true :flat true :title-bar false}
                  (leva/controls {:atom state :schema schema})))

(def ^:private fmt flow/fmt)
(def ^:private button flow/button)

(defn- badge [ok text]
  [:div {:data-invariant (list 'if ok "pass" "fail")
         :style {:display "inline-block" :padding "0.15em 0.6em" :border-radius "0.6em" :color "white"
                 :background (list 'if ok "#2E8B57" "#C0392B")}}
   (list 'if ok "✓ " "✗ ") text])

;; ---------------------------------------------------------------------------
;; Third Day, Theorem I: the mean speed

(defn acceleration-flow
  "Speed gaining a + j t per unit time (Galileo: j = 0), state [t s v]."
  [a j]
  (fn [[t _s v]] [1 v (e/+ a (e/* j t))]))

(defn acceleration-readout
  "From the RK4 run: [rectangle AGFB = T vT/2, distance - rectangle,
   s(T/4), and the 2nd, 3rd, 4th quarter distances over the first]."
  [T vT s1 s2 s3 s4]
  (fn [_]
    (let [rect (e/* T (e// vT 2)) d (tie/at-least s1 1e-12)]
      [rect (e/- s4 rect) s1 (e// (e/- s2 s1) d) (e// (e/- s3 s2) d) (e// (e/- s4 s3) d)])))

(defn acceleration-values
  "Galileo's closed laws at (a, T), for the label beside the integration:
   [v = aT, s = aT^2/2, rectangle T aT/2, the four quarter distances]."
  [a T]
  (fn [_] (let [s (motion/distance a) v ((motion/speed a) T)]
            [v (s T) (motion/mean-speed-rectangle a T)
             (s (e// T 4)) (e/- (s (e// T 2)) (s (e// T 4)))
             (e/- (s (e/* 3/4 T)) (s (e// T 2))) (e/- (s T) (s (e/* 3/4 T)))])))

(def acceleration-start {:a 2 :T 4 :j 0})

(defn acceleration-playground []
  (binding [vc/*backend* :raster]
    (ev/with-let [!s acceleration-start]
      (let [[st sf] (flow/step-kernel "fall" acceleration-flow [2 0 0.02] [0 0 0])
            [ro rf] (flow/kernel "fallro" acceleration-readout [4 8 1 4 9 16] [0])
            [cl cf] (flow/kernel "fallclosed" acceleration-values [2 4] [0])
            integrate (gensym "integrate")]
        (list 'reagent.core/with-let [st sf ro rf cl cf integrate (flow/integrate-form st)]
          (flow/template
           {'?s !s 'ro ro 'cl cl 'integrate integrate}
           (list 'let '[a (get @?s :a) T (get @?s :T) j (get @?s :j)
                        pts (integrate (array 0 0 0) (array a j (/ T 200)) 200 (fn [_ _] false))
                        q (fn [k] (nth pts (* 50 k)))
                        end (q 4) vT (aget end 2)
                        r (ro (array 0) (array T vT (aget (q 1) 1) (aget (q 2) 1) (aget (q 3) 1) (aget end 1)))
                        c (cl (array 0) (array a T))
                        speed (mapv (fn [p] [(aget p 0) (aget p 2)]) pts)
                        mid (q 2)
                        ok (< (js/Math.abs (aget r 1)) (* 1e-6 (max 1 (aget end 1))))]
                 [:div {:data-playground "galileo-acceleration" :data-distance '(str (aget end 1))
                        :data-rect '(str (aget r 0))}
                  (mafs/mafs {:view-box {:x [-0.5 8.5] :y [-0.5 24]} :height 340 :preserve-aspect-ratio false}
                    (mafs/cartesian)
                    (mafs/polygon {:points '(conj (into [[0 0]] speed) [T 0]) :color "#2472A4" :fill-opacity 0.25})
                    (mafs/polygon {:points '[[0 0] [T 0] [T (/ vT 2)] [0 (/ vT 2)]] :color "#D48632" :fill-opacity 0.2})
                    (mafs/polyline {:points 'speed :color "#2472A4" :weight 3})
                    '(into [:<>] (for [k [1 2 3]] [mafs.line/Segment {:point1 [(aget (q k) 0) 0] :point2 [(aget (q k) 0) (aget (q k) 2)] :color "#888" :style "dashed"}]))
                    (mafs/movable-point {:point '[T vT] :color "#C0392B"
                                         :on-move '(fn [[x y]]
                                                     (let [x (max 0.2 (min 8 x))]
                                                       (swap! ?s assoc :T x :a (max 0 (min 5 (/ (- y (* j x x 0.5)) x))))))})
                    (mafs/text "A" {:x 0 :y 0 :attach "sw"}) (mafs/text "B" {:x 'T :y 0 :attach "se"})
                    (mafs/text "E" {:x 'T :y 'vT :attach "ne"}) (mafs/text "F" {:x 'T :y '(/ vT 2) :attach "e"})
                    (mafs/text "G" {:x 0 :y '(/ vT 2) :attach "w"})
                    (mafs/point {:x '(aget mid 0) :y '(aget mid 2) :color "#111"})
                    (mafs/text "I" {:x '(aget mid 0) :y '(aget mid 2) :attach "nw"}))
                  (controls !s {:a {:min 0 :max 5 :step 0.01 :label "acceleration a"}
                                :T {:min 0.2 :max 8 :step 0.01 :label "time AB"}
                                :j {:min -0.5 :max 1.5 :step 0.01 :label "growth of a (Galileo: 0)"}})
                  [:strong {:data-case true}
                   '(cond (and (< a 1e-3) (< (js/Math.abs j) 1e-3)) "at rest: no speed, no triangle"
                          (> (js/Math.abs j) 1e-3) "not uniformly accelerated: the speed line bends, triangle AEB ≠ rectangle AGFB"
                          :else "uniformly accelerated from rest: triangle AEB = rectangle AGFB")]
                  [:div "RK4 (200 raster steps of ds/dt = v, dv/dt = a + j t): distance AB-run = "
                   (fmt '(aget end 1)) "; speed EB = " (fmt 'vT)]
                  [:div "rectangle AGFB = AB × EB/2 = " (fmt 'T) " × " (fmt '(/ vT 2)) " = " (fmt '(aget r 0))
                   "; distance − rectangle = " (fmt '(aget r 1) 6)]
                  (badge 'ok "Theorem I: the distance equals the mean-speed rectangle")
                  [:div {:data-bars true :style {:display "flex" :align-items "end" :height "110px" :gap "8px" :margin-top "0.5em"}}
                   '(into [:<>] (for [[i h] (map-indexed vector [1 (aget r 3) (aget r 4) (aget r 5)])]
                                  [:div {:key i :style {:height (str (+ 4 (* 12 (if (js/isFinite h) (max 0 h) 0))) "px")
                                                        :background "#2472A4" :width "34px" :color "white" :font-size "0.8em"}}
                                   (if (js/isFinite h) (.toFixed h 2) "–")]))]
                  [:div "Distances in the four quarters of AB, over the first (RK4): Corollary I says 1 : 3 : 5 : 7"]
                  [:div {:style {:opacity 0.7 :font-size "0.9em"}}
                   "Beside it, Galileo's closed law (not the integration): v = aT = " (fmt '(aget c 0))
                   ", s = aT²/2 = " (fmt '(aget c 1))]
                  [:p "Try this:"]
                  (button !s "Double the time AB" {:a 2 :T 8 :j 0})
                  (button !s "Let the acceleration grow" {:a 1 :T 4 :j 1})
                  (button !s "Stop accelerating" {:a 0 :T 4 :j 0})
                  (button !s "Reset Galileo's figure" acceleration-start)])))))))

;; ---------------------------------------------------------------------------
;; The inclined plane

(def g 9.81)

(defn plane-flow
  "The ball on a groove 12 long lifted `lift` at one end, resisted by
   k v (Galileo: k = 0): state [t s v]."
  [lift k]
  (fn [[_t _s v]]
    [1 v (e/- (e/* g (e// lift (e/sqrt (e/+ 144 (e/square lift))))) (e/* k v))]))

(defn plane-readout
  "[a = g sin, t(3)/t(12), slope in degrees] from the RK4 run's
   crossings."
  [lift t3 t12]
  (fn [_]
    [(e/* g (e// lift (e/sqrt (e/+ 144 (e/square lift)))))
     (e// t3 (tie/at-least t12 1e-12))
     (e/* (e// 180 Math/PI) (e/atan (e// lift 12)))]))

(defn quarter-ratios
  "[s2/s1 s3/s1 s4/s1]: s_k the RK4 distance at k equal times."
  [s1 s2 s3 s4]
  (fn [_] (let [d (tie/at-least s1 1e-12)] [(e// s2 d) (e// s3 d) (e// s4 d)])))

(def plane-start {:lift 1 :k 0 :u 0.5})

(defn plane-playground []
  (binding [vc/*backend* :raster]
    (ev/with-let [!s plane-start]
      (let [[st sf] (flow/step-kernel "roll" plane-flow [1 0 0.005] [0 0 0])
            [ro rf] (flow/kernel "rollro" plane-readout [1 1 2] [0])
            [qr qf] (flow/kernel "rollq" quarter-ratios [1 4 9 16] [0])
            [cr cf] (flow/cross-kernel)
            integrate (gensym "integrate") crossing (gensym "crossing")]
        (list 'reagent.core/with-let [st sf ro rf qr qf cr cf integrate (flow/integrate-form st)
                                      crossing (flow/crossing-form cr)]
          (flow/template
           {'?s !s 'ro ro 'qr qr 'integrate integrate 'crossing crossing}
           (list 'let '[lift (get @?s :lift) k (get @?s :k) u (get @?s :u)
                        pts (integrate (array 0 0 0) (array lift k 0.005) 6000 (fn [s _] (> (aget s 1) 12.5)))
                        t12 (crossing pts 0 1 12) t3 (crossing pts 0 1 3)
                        at (fn [t] (if (and t (pos? t)) (crossing pts 1 0 t) 0))
                        sq (mapv (fn [i] (at (when t12 (* i (/ t12 4))))) [1 2 3 4])
                        r (ro (array 0) (array lift (or t3 js/NaN) (or t12 js/NaN)))
                        qs (qr (array 0) (array (nth sq 0) (nth sq 1) (nth sq 2) (nth sq 3)))
                        cosA (/ 12 (js/Math.hypot 12 lift)) sinA (/ lift (js/Math.hypot 12 lift))
                        on (fn [s] [(- 12 (* s cosA)) (- lift (* s sinA))])
                        ball (on (if t12 (or (at (* u t12)) 0) 0))
                        ok (and t12 (< (js/Math.abs (- (aget qs 2) 16)) 2e-3) (< (js/Math.abs (- (aget r 1) 0.5)) 1e-3))]
                 [:div {:data-playground "galileo-plane" :data-t12 '(str t12) :data-ratio '(str (aget qs 2))
                        :data-half '(str (aget r 1))}
                  (mafs/mafs {:view-box {:x [-1 13.5] :y [-1 7]} :height 300}
                    (mafs/cartesian)
                    (mafs/segment {:point1 [0 0] :point2 [12 'lift] :color "#8B5A36" :weight 5})
                    (mafs/segment {:point1 [0 0] :point2 [12 0] :color "#999"})
                    (mafs/segment {:point1 [12 0] :point2 [12 'lift] :color "#999" :style "dashed"})
                    '(into [:<>] (for [[i s] (map-indexed vector sq) :when s]
                                   (let [[x y] (on s)] [mafs.core/Text {:key i :x x :y y :attach "n" :size 14} (str "|" (inc i))])))
                    (mafs/movable-point {:point [12 'lift] :color "#C0392B"
                                         :constrain '(fn [[_ y]] [12 (max 0 (min 6 y))])
                                         :on-move '(fn [[_ y]] (swap! ?s assoc :lift (max 0 (min 6 y))))})
                    (mafs/movable-point {:point 'ball :color "#2472A4"
                                         :constrain '(fn [[x _]] (on (max 0 (min 12 (/ (- 12 x) cosA)))))
                                         :on-move '(fn [[x _]] (when t12 (let [t (crossing pts 0 1 (max 1e-6 (min 12 (/ (- 12 x) cosA))))]
                                                                         (when t (swap! ?s assoc :u (/ t t12))))))})
                    (mafs/text "raised end" {:x 12 :y 'lift :attach "ne"})
                    (mafs/text "groove, 12 long" {:x 6 :y 0 :attach "s"}))
                  (controls !s {:lift {:min 0 :max 6 :step 0.01 :label "lift of the raised end"}
                                :k {:min 0 :max 1.5 :step 0.01 :label "resistance k (Galileo: 0)"}
                                :u {:min 0 :max 1 :step 0.001 :label "time, fraction of the run"}})
                  [:strong {:data-case true}
                   '(cond (< lift 1e-3) "level groove: the ball rests; no times to compare"
                          (nil? t12) "the ball never reaches the end"
                          (> k 1e-4) "resisted roll: equal times no longer give 1 : 4 : 9 : 16"
                          :else "free roll: distances as the squares of the times")]
                  [:div "slope " (fmt '(aget r 2) 2) "°, g sin = " (fmt '(aget r 0))
                   "; RK4 (raster step 0.005 of ds/dt = v, dv/dt = g sin − k v): whole groove in "
                   (fmt 't12) ", its first quarter (3) in " (fmt 't3)]
                  [:div "t(3)/t(12) = " (fmt '(aget r 1) 4) " (Galileo: one half);"
                   " marks |1..|4 at equal times, distances over the first: 1 : "
                   (fmt '(aget qs 0)) " : " (fmt '(aget qs 1)) " : " (fmt '(aget qs 2))]
                  (badge 'ok "a quarter of the length in half the time; 1 : 4 : 9 : 16")
                  [:p "Try this:"]
                  (button !s "Lift two cubits" {:lift 2 :k 0 :u 0.5})
                  (button !s "Almost level" {:lift 0.05 :k 0 :u 0.5})
                  (button !s "Thick air (k = 0.8)" {:lift 2 :k 0.8 :u 0.5})
                  (button !s "Reset Galileo's groove" plane-start)])))))))

;; ---------------------------------------------------------------------------
;; Fourth Day: the projectile

(defn shot-flow
  "Sideways uniform, downward g, both resisted by k v (Galileo: k = 0):
   state [x y vx vy]."
  [k]
  (fn [[_x _y vx vy]] [vx vy (e/- 0 (e/* k vx)) (e/- (e/- 0 g) (e/* k vy))]))

(defn launch [v deg]
  (fn [_] (let [th (e/* deg (/ Math/PI 180))]
            [0 0 (e/* v (e/cos th)) (e/* v (e/sin th))])))

(defn shot-values
  "Galileo's closed forms beside the integration: [R H v^2/g R(90 - deg)]."
  [v deg]
  (fn [_]
    (let [th (e/* deg (/ Math/PI 180))]
      [(shot/range-of v th shot/g) (shot/height-of v th shot/g) (e// (e/square v) shot/g)
       (shot/range-of v (e/* (e/- 90 deg) (/ Math/PI 180)) shot/g)])))

(defn shot-curve [v deg]
  (fn [u]
    (let [th (e/* deg (/ Math/PI 180))
          x (e/* u (shot/range-of v th shot/g))]
      [x ((shot/trajectory v th shot/g) x)])))

(defn parabola-gap
  "How far an RK4 point (x, y) is from Galileo's parabola of (v, deg)."
  [v deg x y]
  (fn [_] [(e/- y ((shot/trajectory v (e/* deg (/ Math/PI 180)) shot/g) x))]))

(def shot-start {:v 18 :angle 35 :k 0})

(defn projectile-playground
  ([] (projectile-playground shot-start))
  ([start]
   (binding [vc/*backend* :raster]
     (ev/with-let [!s start]
       (let [[st sf] (flow/step-kernel "shot" shot-flow [0 0.004] [0 0 1 1])
             [la lf] (flow/kernel "launch" launch [18 35] [0])
             [cl cf] (flow/kernel "shotclosed" shot-values [18 35] [0])
             [gp gf] (flow/kernel "gap" parabola-gap [18 35 1 1] [0])
             [cr cf2] (flow/cross-kernel)
             path (ev/with-params {:atom !s :params [:v :angle]} shot-curve)
             integrate (gensym "integrate") crossing (gensym "crossing")]
         (list 'reagent.core/with-let [st sf la lf cl cf gp gf cr cf2 integrate (flow/integrate-form st)
                                       crossing (flow/crossing-form cr)]
           (flow/template
            {'?s !s 'la la 'cl cl 'gp gp 'integrate integrate 'crossing crossing}
            (list 'let '[v (get @?s :v) angle (get @?s :angle) k (get @?s :k)
                         fly (fn [deg] (integrate (la (array 0) (array v deg)) (array k 0.004) 4000
                                                  (fn [s i] (and (pos? i) (neg? (aget s 1))))))
                         pts (fly angle) other (fly (- 90 angle))
                         R (crossing pts 0 1 0) Rc (crossing other 0 1 0)
                         H (crossing pts 1 3 0) Hx (crossing pts 0 3 0)
                         c (cl (array 0) (array v angle))
                         gap (reduce max 0 (map (fn [p] (js/Math.abs (aget (gp (array 0) (array v angle (aget p 0) (aget p 1))) 0))) pts))
                         xy (fn [ps] (mapv (fn [p] [(aget p 0) (aget p 1)]) ps))
                         vx (* v (js/Math.cos (* angle (/ js/Math.PI 180))))
                         vy (* v (js/Math.sin (* angle (/ js/Math.PI 180))))
                         same (and R Rc (< (js/Math.abs (- R Rc)) (* 2e-3 (max 1 R))))
                         parab (< gap 1e-3)]
                  [:div {:data-playground "galileo-projectile" :data-range '(str R) :data-complement '(str Rc)
                         :data-gap '(str gap)}
                   (mafs/mafs {:view-box {:x [-2 95] :y [-3 48]} :height 370}
                     (mafs/cartesian)
                     (mafs/parametric {:xy path :t [0 1] :color "#E67E22" :weight 1.5 :style "dashed"})
                     (mafs/polyline {:points '(xy other) :color "#999" :weight 2})
                     (mafs/polyline {:points '(xy pts) :color "#2472A4" :weight 3})
                     (mafs/segment {:point1 [0 0] :point2 '[(* 2 vx) (* 2 vy)] :color "#C0392B"})
                     (mafs/movable-point {:point '[(* 2 vx) (* 2 vy)] :color "#C0392B"
                                          :on-move '(fn [[x y]]
                                                      (swap! ?s assoc
                                                             :v (max 1 (min 30 (/ (js/Math.hypot x y) 2)))
                                                             :angle (max 0 (min 90 (* (/ 180 js/Math.PI) (js/Math.atan2 y x))))))})
                     '(when R [mafs.core/Point {:x R :y 0 :color "#111"}])
                     '(when (and H Hx) [mafs.core/Point {:x Hx :y H :color "#555"}])
                     (mafs/text "b" {:x 0 :y 0 :attach "sw"})
                     '(when R [mafs.core/Text {:x R :y 0 :attach "s"} "landing"]))
                   (controls !s {:v {:min 1 :max 30 :step 0.1 :label "launch speed v"}
                                 :angle {:min 0 :max 90 :step 0.1 :label "elevation θ°"}
                                 :k {:min 0 :max 0.3 :step 0.005 :label "air resistance k (Galileo: 0)"}})
                   [:strong {:data-case true}
                    '(cond (> k 1e-4) "resisted: no longer a parabola; complementary shots differ, the longest is below 45°"
                           (< angle 0.1) "horizontal launch from the ground: no flight"
                           (> angle 89.9) "vertical launch: zero range"
                           (< (js/Math.abs (- angle 45)) 0.1) "45°: the longest shot (Prop. VII)"
                           :else "parabola: the complementary elevation reaches the same range (Prop. VIII)")]
                   [:div "RK4 (raster steps of 0.004 s): range " (fmt 'R) ", height " (fmt 'H)
                    "; the complement " (fmt '(- 90 angle) 1) "° → " (fmt 'Rc)]
                   [:div "largest distance of an RK4 point from Galileo's parabola: " (fmt 'gap 5)]
                   (badge 'parab "the path is the parabola (Fourth Day, Prop. I)")
                   " "
                   (badge 'same "complementary elevations, equal ranges (Prop. VIII)")
                   [:div {:style {:opacity 0.7 :font-size "0.9em" :margin-top "0.4em"}}
                    "Beside it, the closed form (dashed orange; not the integration): R = v² sin 2θ/g = " (fmt '(aget c 0))
                    ", H = v² sin²θ/(2g) = " (fmt '(aget c 1)) ", at 45°: v²/g = " (fmt '(aget c 2))]
                   [:p "Try this:"]
                   (button !s "Elevation 30°" {:v 18 :angle 30 :k 0})
                   (button !s "Its complement 60°" {:v 18 :angle 60 :k 0})
                   (button !s "The longest: 45°" {:v 18 :angle 45 :k 0})
                   (button !s "Fire through air (k = 0.15)" {:v 18 :angle 45 :k 0.15})
                   (button !s "Reset" start)]))))))))
