(ns alexandria.kepler.playground
  "Free-form playgrounds for Kepler's laws (playground brief). Every orbit
   is INTEGRATED: one RK4 step of a central-force flow is a raster kernel
   (alexandria.medium.flow over alexandria.raster.step), iterated in the
   page; the swept area rides along as a coordinate. Kepler's closed laws
   (rho = l/(1 - e cos theta), T = a sqrt a) appear only beside the
   integration, labelled. The force law itself is open: its exponent p
   (Newton: 2) and a resisting drag k (Kepler: 0) are controls, so the
   reader can break the hypotheses and watch which law survives:
   any central force keeps the equal areas; only the inverse square keeps
   the ellipse and the harmonic law."
  (:require [alexandria.kepler.focal :as focal]
            [alexandria.kepler.orbit-widget :as widget]
            [alexandria.medium.flow :as flow]
            [emmy.env :as e]
            [emmy.leva :as leva] [emmy.mafs :as mafs]
            [emmy.viewer :as ev] [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]))

(def ^:private fmt flow/fmt)
(def ^:private button flow/button)
(def mu widget/mu)

(def slice "RK4 steps per equal time." 200)

(defn- panel [state schema]
  (leva/sub-panel {:fill true :flat true :title-bar false}
                  (leva/controls {:atom state :schema schema})))

(defn- badge [ok text]
  [:div {:data-invariant (list 'if ok "pass" "fail")
         :style {:display "inline-block" :padding "0.15em 0.6em" :border-radius "0.6em" :color "white"
                 :margin "0.2em 0" :background (list 'if ok "#2E8B57" "#C0392B")}}
   (list 'if ok "✓ " "✗ ") text])

;; ---------------------------------------------------------------------------
;; The flow: a central force mu / r^p, an optional drag k v

(defn- r-power
  "r^(-s) from r^2, written exp(-s/2 log r^2) so a raster kernel takes any s."
  [r2 s]
  (e/exp (e/* (e/- 0 (e// s 2)) (e/log r2))))

(defn sector-flow
  "[x y vx vy A]: pulled toward the Sun by mu / r^p (Newton: p = 2),
   resisted by k v (Kepler: k = 0); A the area swept, dA/dt = (x vy - y vx)/2."
  [mu' p k]
  (fn [[x y vx vy _A]]
    (let [f (e/* mu' (r-power (e/+ (e/square x) (e/square y)) (e/+ p 1)))]
      [vx vy
       (e/- (e/- 0 (e/* f x)) (e/* k vx))
       (e/- (e/- 0 (e/* f y)) (e/* k vy))
       (focal/sweep-rate x y vx vy)])))

(defn sector-setup
  "[T l q dt] for Kepler's ellipse (a, e) cut into n equal times of `slice`
   RK4 steps: the time window T = a sqrt a, l = a(1 - e^2), perihelion
   q = a(1 - e)."
  [a ecc n]
  (fn [_] (let [T (focal/period a)]
            [T (focal/semi-latus a ecc) (e/* a (e/- 1 ecc)) (e// T (e/* slice n))])))

(defn sector-start
  "The RK4 start at perihelion (-q, 0) with the vis-viva speed sqrt(mu l)/q."
  [q l]
  (fn [_] (focal/perihelion-state mu q l :q-l)))

(defn ellipse-gap
  "How far an RK4 point (x, y) lies from rho = l / (1 - e cos theta)."
  [l ecc x y]
  (fn [_] (let [r (e/sqrt (e/+ (e/square x) (e/square y)))]
            [(e/- r (e// l (e/- 1 (e/* ecc (e// x r)))))])))

(def sector-start-state {:a 1.5235 :e 0.09264 :n 8 :p 2 :k 0})

(defn sector-playground []
  (binding [vc/*backend* :raster]
    (ev/with-let [!s sector-start-state]
      (let [[st sf] (flow/step-kernel "orbit" sector-flow [mu 2 0 0.01] [1 0 0 1 0])
            [su suf] (flow/kernel "setup" sector-setup [1.5235 0.09264 8] [0])
            [s0 s0f] (flow/kernel "start" sector-start [1.38 1.51] [0])
            [gp gpf] (flow/kernel "gap" ellipse-gap [1.51 0.09 1 1] [0])
            curve (ev/with-params {:atom !s :params [:a :e]}
                    (fn [a ecc] (fn [t]
                                  (let [rho (focal/focal-rho (focal/semi-latus a ecc) ecc t)]
                                    [(e/* rho (e/cos t)) (e/* rho (e/sin t))]))))
            integrate (gensym "integrate")]
        (list 'reagent.core/with-let [st sf su suf s0 s0f gp gpf integrate (flow/integrate-form st)
                                      'cache '(atom nil)]
          (flow/template
           {'?s !s '?mu mu '?m slice 'st st 'su su 's0 s0 'gp gp 'integrate integrate}
           (list 'let '[a (get @?s :a) ecc (get @?s :e) n (get @?s :n) p (get @?s :p) k (get @?s :k)
                        setup (su (array 0) (array a ecc n))
                        T (aget setup 0) l (aget setup 1) q (aget setup 2) dt (aget setup 3)
                        key [a ecc n p k]
                        pts (if (= key (first @cache)) (second @cache)
                                (let [v (integrate (s0 (array 0) (array q l)) (array ?mu p k dt) (* ?m n) (fn [_ _] false))]
                                  (reset! cache [key v]) v))
                        xy (fn [s] [(aget s 0) (aget s 1)])
                        areas (mapv (fn [i] (- (aget (nth pts (* (inc i) ?m)) 4) (aget (nth pts (* i ?m)) 4))) (range n))
                        spread (- (apply max areas) (apply min areas))
                        rel (/ spread (max 1e-12 (apply max areas)))
                        gap (reduce max 0 (map (fn [j] (js/Math.abs (aget (gp (array 0) (array l ecc (aget (nth pts j) 0) (aget (nth pts j) 1))) 0)))
                                               (range 0 (count pts) 10)))
                        equal (< rel 1e-4)
                        on-ellipse (< gap 1e-3)
                        sectors (into [:<>] (for [i (range n)]
                                              [mafs.core/Polygon
                                               {:points (into [[0 0]] (map (fn [j] (xy (nth pts j)))) (range (* i ?m) (inc (* (inc i) ?m)) 4))
                                                :color (if (even? i) "#C0392B" "#2874A6")
                                                :fill-opacity 0.24}]))
                        path (mapv xy (take-nth 5 pts))]
                 [:div {:data-playground "kepler-sectors" :data-areas '(js/JSON.stringify (clj->js areas))
                        :data-spread '(str spread) :data-gap '(str gap)}
                  (mafs/mafs {:view-box {:x [-9 11] :y [-9 9]} :height 420}
                    (mafs/cartesian)
                    'sectors
                    (mafs/parametric {:xy curve :t [(- Math/PI) Math/PI] :color "#2472A4" :style "dashed"})
                    (mafs/polyline {:points 'path :color "#111" :weight 1.5})
                    (mafs/point {:x 0 :y 0 :color "#E6A700"})
                    (mafs/text "Sun F" {:x 0 :y 0 :attach "se"})
                    (mafs/movable-point {:point '[(- q) 0] :color "#C0392B"
                                         :constrain '(fn [[x _]] [(max (* -0.99 a) (min (* -0.15 a) x)) 0])
                                         :on-move '(fn [[x _]] (swap! ?s assoc :e (max 0 (min 0.85 (- 1 (/ (- x) a))))))})
                    (mafs/text "perihelion" {:x '(- q) :y 0 :attach "nw"}))
                  (panel !s {:a {:min 0.3 :max 5 :step 0.01 :label "semimajor axis a"}
                             :e {:min 0 :max 0.85 :step 0.005 :label "eccentricity e"}
                             :n {:min 4 :max 20 :step 1 :label "equal time slices n"}
                             :p {:min 1 :max 3 :step 0.01 :label "force ∝ 1/rᵖ (Newton: 2)"}
                             :k {:min 0 :max 0.5 :step 0.005 :label "drag k (Kepler: 0)"}})
                  [:strong {:data-case true}
                   '(cond (> k 1e-4) "resisted: the force is no longer central; equal times sweep shrinking areas"
                          (> (js/Math.abs (- p 2)) 1e-3) "central but not inverse-square: equal areas survive, the ellipse does not"
                          (< ecc 0.005) "circle: equal areas, uniform motion"
                          :else "ellipse: equal areas in equal times (Kepler's second law)")]
                  [:div "RK4 (raster kernel, " '(* ?m n) " steps of Δt = " (fmt 'dt 5)
                   " over T = a√a = " (fmt 'T 4) "): areas swept in " 'n " equal times:"]
                  [:div {:data-area-list true} '(for [[i v] (map-indexed vector areas)] [:span {:key i} (.toFixed v 4) " "])]
                  [:div "spread of the areas = " (fmt 'spread 6) "; farthest RK4 point from ρ = ℓ/(1 − e cos θ) = "
                   (fmt 'l 4) "/(1 − " (fmt 'ecc 4) " cos θ): " (fmt 'gap 5)]
                  (badge 'equal "equal areas in equal times")
                  " "
                  (badge 'on-ellipse "the path is the ellipse, Sun at the focus")
                  [:div {:style {:opacity 0.7 :font-size "0.9em"}}
                   "Dashed blue: Kepler's closed ellipse, beside the integration (black, RK4)."]
                  [:p "Try this:"]
                  (button !s "Circular orbit" {:a 1.5235 :e 0 :n 8 :p 2 :k 0})
                  (button !s "Long ellipse" {:a 2 :e 0.7 :n 12 :p 2 :k 0})
                  (button !s "Force 1/r^2.5: areas or ellipse?" {:a 1.5235 :e 0.3 :n 8 :p 2.5 :k 0})
                  (button !s "A resisting medium" {:a 1.5235 :e 0.3 :n 8 :p 2 :k 0.3})
                  (button !s "Reset Mars" sector-start-state)])))))))

;; ---------------------------------------------------------------------------
;; The harmonic law, by integrated periods

(def planets [[:mercury 0.387] [:venus 0.723] [:earth 1.0]
              [:mars 1.5235] [:jupiter 5.203] [:saturn 9.537]])

(defn circle-flow
  "[t x y vx vy] under mu / r^p."
  [mu' p]
  (fn [[_t x y vx vy]]
    (let [f (e/* mu' (r-power (e/+ (e/square x) (e/square y)) (e/+ p 1)))]
      [1 vx vy (e/- 0 (e/* f x)) (e/- 0 (e/* f y))])))

(defn circle-start
  "Start on the circle of radius a at the circular speed v = sqrt(mu a^(1-p))
   (v^2/a = mu/a^p), and an RK4 step of 1/400 of the lap 2 pi a / v:
   [state..., dt]."
  [a p]
  (fn [_] (let [v (e/sqrt (e/* mu (e/exp (e/* (e/- 1 p) (e/log a)))))]
            [0 a 0 0 v (e// (e/* 2 Math/PI a) (e/* 400 v))])))

(defn law-values
  "From a and the RK4 period T: [T^2/a^3, log a^3, log T^2]."
  [a T]
  (fn [_] [(e// (e/square T) (e/cube a)) (e/log (e/cube a)) (e/log (e/square T))]))

(def harmonic-start-state {:a 1.5235 :p 2})

(defn harmonic-playground []
  (binding [vc/*backend* :raster]
    (ev/with-let [!s harmonic-start-state]
      (let [[st sf] (flow/step-kernel "lap" circle-flow [mu 2 0.01] [0 1 0 0 1])
            [cs csf] (flow/kernel "circ" circle-start [1.5235 2] [0])
            [lv lvf] (flow/kernel "law" law-values [1.5235 1.88] [0])
            [cr crf] (flow/cross-kernel)
            integrate (gensym "integrate") crossing (gensym "crossing")]
        (list 'reagent.core/with-let [st sf cs csf lv lvf cr crf integrate (flow/integrate-form st)
                                      crossing (flow/crossing-form cr)]
          (flow/template
           {'?s !s '?mu mu '?planets (mapv (fn [[k a]] [(name k) a]) planets)
            'cs cs 'lv lv 'integrate integrate 'crossing crossing}
           (list 'let '[a (get @?s :a) p (get @?s :p)
                        lap (fn [a] (let [c (cs (array 0) (array a p))
                                          pts (integrate (.slice c 0 5) (array ?mu p (aget c 5)) 2000
                                                         (fn [s i] (and (> i 2) (neg? (aget s 2)))))
                                          T (* 2 (crossing pts 0 2 0))
                                          r (lv (array 0) (array a T))]
                                      {:a a :T T :ratio (aget r 0) :x (aget r 1) :y (aget r 2)}))
                        here (lap a)
                        rows (mapv (fn [[nm ax]] (assoc (lap ax) :name nm)) ?planets)
                        ratios (map :ratio rows)
                        spread (- (apply max ratios) (apply min ratios))
                        same (< spread 1e-3)]
                 [:div {:data-playground "kepler-harmonic" :data-period '(str (:T here))
                        :data-ratio '(str (:ratio here)) :data-spread '(str spread)}
                  (mafs/mafs {:view-box {:x [-4 8] :y [-5 12]} :height 380 :preserve-aspect-ratio false}
                    (mafs/cartesian)
                    (mafs/segment {:point1 [-4 -4] :point2 [8 8] :color "#2472A4" :style "dashed"})
                    '(into [:<>] (for [{:keys [name x y]} rows]
                                   [:<> {:key name} [mafs.core/Point {:x x :y y :color "#777"}]
                                    [mafs.core/Text {:x x :y y :attach "se" :size 12} name]]))
                    (mafs/movable-point {:point '[(:x here) (:y here)] :color "#C0392B"
                                         :constrain '(fn [[x _]] [(max -3.6 (min 6.9 x)) (:y here)])
                                         :on-move '(fn [[x _]] (swap! ?s assoc :a (max 0.3 (min 10 (js/Math.exp (/ x 3))))))})
                    (mafs/text "log a³ →" {:x 6 :y -4.5})
                    (mafs/text "log T²" {:x -3.8 :y 11 :attach "e"}))
                  (panel !s {:a {:min 0.3 :max 10 :step 0.01 :label "semimajor axis a (AU)"}
                             :p {:min 1 :max 3 :step 0.01 :label "force ∝ 1/rᵖ (Newton: 2)"}})
                  [:strong {:data-case true}
                   '(cond (> (js/Math.abs (- p 2)) 1e-3)
                          (str "force 1/r^" (.toFixed p 2) ": T² goes as a^" (.toFixed (+ p 1) 2)
                               ", T²/a³ changes from planet to planet: no harmonic law")
                          (< a 0.4) "inside Mercury: the same harmonic law"
                          (> a 9.5) "beyond Saturn: the same harmonic law"
                          :else "inverse square: T²/a³ the same for every planet (Harmonices Mundi V)")]
                  [:div "RK4 (raster kernel, 400 steps a lap, period from the half-lap crossing): T = "
                   (fmt '(:T here) 4) " years; T²/a³ = " (fmt '(:ratio here) 5)]
                  [:div "log a³ = " (fmt '(:x here) 4) "; log T² = " (fmt '(:y here) 4)]
                  [:table {:data-table true :style {:font-size "0.88em"}}
                   [:thead [:tr [:th "planet"] [:th "a"] [:th "T (RK4)"] [:th "T²/a³"]]]
                   '(into [:tbody] (for [{:keys [name a T ratio]} rows]
                                     [:tr {:key name} [:td name] [:td (.toFixed a 4)] [:td (.toFixed T 4)] [:td (.toFixed ratio 5)]]))]
                  (badge 'same "one constant T²/a³ for all six planets")
                  [:div {:style {:opacity 0.7 :font-size "0.9em"}}
                   "Dashed blue: the line of slope 1, Kepler's law written as a closed form, beside the RK4 dots."]
                  [:p "Try this:"]
                  '(into [:<>] (for [[nm ax] ?planets]
                                 [:button {:key nm :type "button" :data-preset nm :style {:margin "0.2em"}
                                           :on-click (fn [] (swap! ?s assoc :a ax))} nm]))
                  (button !s "Beyond Saturn" {:a 10 :p 2})
                  (button !s "Force 1/r: what slope?" {:a 1.5235 :p 1})
                  (button !s "Force 1/r³" {:a 1.5235 :p 3})
                  (button !s "Reset Mars" harmonic-start-state)])))))))
