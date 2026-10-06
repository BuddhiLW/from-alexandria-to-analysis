(ns alexandria.lagrange.mechanics-scenes
  "The figures Lagrange left out of the Mecanique analytique (1788), drawn:
   the virtual displacement of a double pendulum, the pendulum's phase
   portrait, the double pendulum in motion, the Kepler orbit sweeping equal
   areas; and the secant with its parallel tangent of the Theorie des
   fonctions analytiques (1797). One method per stage
   (alexandria.medium.scene/draw).

   ctx :data, from alexandria.lagrange.mechanics-view:
     :portrait  phase curves [[theta theta'] ...] integrated by raster (RK4)
     :swing     one swing [theta ...] from 1.1 rad, raster RK4
     :double    double-pendulum samples [t a b a' b'] integrated by raster
     :phis      the conic's angle at 241 equal times (raster's Brent)
     :cubic     {:a :b :u} of the mean value scene
   ctx :figures the :conic figure (params [e], state [phi]) and :cubic."
  (:require [alexandria.medium.anim :as a]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.math :as m]
            [alexandria.medium.plane :as plane]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            #?(:cljs [alexandria.medium.player :as player])))

#?(:cljs (def render "Clerk's render-fn for these scenes." player/render))

(def ^:private vv :lagrange/virtual-velocities)
(def ^:private eom :lagrange/equations-of-motion)
(def ^:private kep :lagrange/kepler)
(def ^:private mvt :lagrange/mean-value)

(defn- write [palette at s opts]
  (svg/text at s (merge {:colour (:ink palette) :size 0.2} opts)))

(defn- readout [palette f at lines]
  (svg/layer (a/fade f [0.2 0])
             (into [:g] (map-indexed (fn [i s] (write palette (plane/translate at [0 (* -0.3 i)]) s
                                                      {:anchor "start" :size 0.17 :mono? true}))
                                     lines))))

(defn- stroke [ps colour width f]
  (when (pos? f) (svg/polyline ps {:stroke colour :width width :attrs (a/create f)})))

;; ---------------------------------------------------------------------------
;; Pendulums: the bobs are the :bobs kernel (state [a b end]); the motions
;; are integrated by raster on the JVM (ctx :data :swing, :double).

(defn- bobs [figures [a b]]
  (figure/points (:bobs figures) [] [[a b 0] [a b 1]]))

(defn- double-pendulum [palette figures ab colour]
  (let [[p1 p2] (bobs figures ab)]
    [:g (svg/polyline [[0 0] p1 p2] {:stroke colour :width 0.03})
     (svg/circle [0 0] 0.04 {:fill (:ink palette)})
     (svg/circle p1 0.09 {:fill colour}) (svg/circle p2 0.09 {:fill colour})]))

(defmethod scene/draw [vv :statics] [_ _ p {:keys [palette figures]}]
  (let [w (* 0.2 (a/wiggle p))]
    [:g (double-pendulum palette figures [0.5 0.9] (:muted palette))
     (double-pendulum palette figures [(+ 0.5 w) (- 0.9 w)] (:found palette))
     (readout palette (a/play p 0 0.2) [1.6 0.6] ["a small motion δq" "Σ F · δr = 0 in equilibrium"])]))

(defmethod scene/draw [vv :dalembert] [_ _ p {:keys [palette figures]}]
  (let [[_ p2] (bobs figures [0.5 0.9]) f (a/play p 0.1 0.5)]
    [:g (double-pendulum palette figures [0.5 0.9] (:found palette))
     (stroke [p2 (plane/translate p2 [0 -0.6])] (:construction palette) 0.03 f)
     (stroke [p2 (plane/translate p2 [-0.4 0.25])] (:muted palette) 0.03 f)
     (readout palette (a/play p 0.3 0.6) [1.6 0.6] ["F - m a: the force lost" "virtual work of it = 0"])]))

(defmethod scene/draw [vv :coordinates] [_ _ p {:keys [palette figures]}]
  (let [ab [(* 0.5 (a/play p 0 0.5)) (* 0.9 (a/play p 0.3 0.8))]]
    [:g (double-pendulum palette figures ab (:found palette))
     (svg/segment [0 0] [0 -2.1] {:stroke (:muted palette) :width 0.01 :dash "0.05 0.05"})
     (readout palette (a/play p 0.5 0.8) [1.6 0.6] ["coordinates: the angles a, b" "δr = (∂r/∂q) δq"])]))

(defmethod scene/draw [vv :lagrange-form] [_ _ p {:keys [palette figures]}]
  [:g (double-pendulum palette figures [0.5 0.9] (:found palette))
   (readout palette (a/play p 0 0.3) [1.6 0.6] ["Σ (F - m a)·∂r/∂q" "  = ∂L/∂q - d/dt ∂L/∂q'" "Emmy: proved"])])

(defn- to-phase [[th om]] [(* 0.6 th) (* 0.25 om)])

(defn- at-fraction
  "The sample of xs at fraction s of the run."
  [xs s]
  (xs (min (dec (count xs)) (int (* (count xs) s)))))

(defmethod scene/draw [eom :pendulum] [_ _ p {:keys [palette figures data]}]
  ;; theta(t) of one swing from 1.1 rad, raster RK4 (ctx :data :swing);
  ;; the bob is the :bobs kernel's first bob on a rod scaled to 1.8
  (let [th (at-fraction (:swing data) (let [s (* 2 p)] (- s (m/floor s))))
        [p1] (bobs figures [th 0])
        bob [(* 1.8 (first p1)) (* 1.8 (second p1))]]
    [:g (svg/polyline [[0 0] bob] {:stroke (:ink palette) :width 0.03})
     (svg/circle bob 0.12 {:fill (:found palette)})
     (readout palette (a/play p 0 0.3) [1.4 0] ["θ'' + (g/l) sin θ = 0" "integrated by raster (RK4)"])]))

(defmethod scene/draw [eom :portrait] [_ _ p {:keys [palette data]}]
  (let [curves (:portrait data) fs (a/lagged p (count curves) 0.4 0 0.9)]
    (into [:g (svg/segment [-2.1 0] [2.1 0] {:stroke (:muted palette) :width 0.01})
           (svg/segment [0 -2] [0 2] {:stroke (:muted palette) :width 0.01})
           (write palette [2.0 0.1] "θ" {:italic? true}) (write palette [0.1 1.85] "θ'" {:italic? true :anchor "start"})]
          (map (fn [c f] (stroke (mapv to-phase c) (:found palette) 0.02 f)) curves fs))))

(defmethod scene/draw [eom :double] [_ _ p {:keys [palette figures]}]
  [:g (double-pendulum palette figures [(* 1.2 (a/play p 0 0.6)) (* 0.4 (a/play p 0.2 0.8))] (:found palette))
   (readout palette (a/play p 0.5 0.8) [1.6 0.6] ["L(a, b, a', b')" "through Emmy's F->C"])])

(defmethod scene/draw [eom :double-run] [_ _ p {:keys [palette figures data]}]
  (let [run (:double data)
        i (min (dec (count run)) (int (* (count run) (a/play p 0 1 a/linear))))
        trail (figure/points (:bobs figures) [] (mapv (fn [[_ a b]] [a b 1]) (take (inc i) run)))
        [_ a b] (run i)]
    [:g (svg/polyline trail {:stroke (:construction palette) :width 0.012})
     (double-pendulum palette figures [a b] (:found palette))]))

;; ---------------------------------------------------------------------------
;; Kepler: the orbit is the :conic kernel; the angle after a fraction of the
;; period is ctx :data :phis, Kepler's equation solved by raster on the JVM.

(def ^:private ecc 0.5)

(defn- polars [figures phis] (figure/points (:conic figures) [ecc] (mapv vector phis)))

(defn- phi-at
  "The angle at fraction s of a period: the raster table, interpolated."
  [{:keys [phis]} s]
  (let [n (dec (count phis))
        x (* (max 0 (min 1 s)) n)
        i (min (dec n) (int (m/floor x)))
        a (phis i) b (phis (inc i))
        b (if (< b a) (+ b (* 2 m/pi)) b)]
    (+ a (* (- x i) (- b a)))))

(defn- orbit [figures]
  (polars figures (mapv (fn [i] (* 2 m/pi (/ i 120))) (range 121))))

(defmethod scene/draw [kep :lagrangian] [_ _ p {:keys [palette figures]}]
  [:g (svg/circle [0 0] 0.08 {:fill (:found palette)})
   (stroke (orbit figures) (:ink palette) 0.02 (a/play p 0.2 0.9))
   (readout palette (a/play p 0 0.3) [1.2 1.1] ["L = m(r'² + r²φ'²)/2 + GMm/r"])])

(defmethod scene/draw [kep :cyclic] [_ _ p {:keys [palette figures]}]
  (let [phi (* 2 m/pi p)]
    [:g (svg/circle [0 0] 0.08 {:fill (:found palette)})
     (svg/polyline (orbit figures) {:stroke (:ink palette) :width 0.02})
     (svg/segment [0 0] (first (polars figures [phi])) {:stroke (:construction palette) :width 0.02})
     (readout palette (a/play p 0 0.3) [1.2 1.1] ["∂L/∂φ = 0" "so m r² φ' is constant"])]))

(defn- sector [figures phi0 phi1]
  (let [phi1 (if (< phi1 phi0) (+ phi1 (* 2 m/pi)) phi1)]
    (into [[0 0]] (polars figures (mapv (fn [i] (+ phi0 (* (- phi1 phi0) (/ i 24)))) (range 25))))))

(defmethod scene/draw [kep :areas] [_ _ p {:keys [palette figures data]}]
  (let [s (a/play p 0 1 a/linear)
        planet (first (polars figures [(phi-at data s)]))
        done (int (* 6 s))]
    (into [:g (svg/circle [0 0] 0.08 {:fill (:found palette)})
           (svg/polyline (orbit figures) {:stroke (:ink palette) :width 0.02})
           (svg/circle planet 0.07 {:fill (:construction palette)})
           (readout palette 1 [1.2 1.1] ["six equal times," "six equal areas"])]
          (for [k (range done)]
            (svg/polygon (sector figures (phi-at data (/ k 6)) (phi-at data (/ (+ k 0.35) 6)))
                         {:fill (:found palette) :opacity 0.35})))))

(defmethod scene/draw [kep :conic] [_ _ p {:keys [palette figures]}]
  (let [es [0 0.5 0.9]
        fs (a/lagged p 3 0.5 0 0.9)]
    (into [:g (svg/circle [0 0] 0.08 {:fill (:found palette)})
           (readout palette (a/play p 0.6 0.9) [1.2 1.1] ["r = p / (1 + e cos φ)" "solves both equations"])]
          (map (fn [e f] (stroke (figure/points (:conic figures) [e] (mapv (fn [i] [(* 2 m/pi (/ i 120))]) (range 121)))
                                 (:construction palette) 0.02 f))
               es fs))))

;; ---------------------------------------------------------------------------
;; The mean value theorem

(defn- curve-pts [figures]
  (figure/points (:cubic figures) [] (mapv (fn [i] [(- (* 3 (/ i 90)) 1.5)]) (range 91))))

(defn- on-cubic
  "The points of the cubic at xs: the :cubic kernel."
  [figures xs]
  (figure/points (:cubic figures) [] (mapv vector xs)))

(defn- tangent-at
  "The tangent at x, half-length half: its slope from the kernel's
   symmetric difference over 1e-4 (exact for a cubic up to 1e-8)."
  [figures x half]
  (let [[[_ y] [_ y0] [_ y1]] (on-cubic figures [x (- x 1e-4) (+ x 1e-4)])
        s (/ (- y1 y0) 2e-4)]
    [[(- x half) (- y (* s half))] [(+ x half) (+ y (* s half))]]))

(defn- chord [figures {:keys [a b]}] (on-cubic figures [a b]))

(defmethod scene/draw [mvt :secant] [_ _ p {:keys [palette figures data]}]
  (let [[A B] (chord figures (:cubic data))]
    [:g (svg/polyline (curve-pts figures) {:stroke (:ink palette) :width 0.02})
     (stroke [A B] (:construction palette) 0.025 (a/play p 0.2 0.7))
     (svg/circle A 0.05 {:fill (:ink palette)}) (svg/circle B 0.05 {:fill (:ink palette)})]))

(defmethod scene/draw [mvt :parallel] [_ _ p {:keys [palette figures data]}]
  (let [{:keys [a u]} (:cubic data)
        x (plane/lerp a u (a/play p 0.05 0.85))]
    [:g (svg/polyline (curve-pts figures) {:stroke (:ink palette) :width 0.02})
     (svg/polyline (chord figures (:cubic data)) {:stroke (:construction palette) :width 0.025})
     (svg/polyline (tangent-at figures x 0.8) {:stroke (:found palette) :width 0.025})
     (svg/circle (first (on-cubic figures [x])) 0.06 {:fill (:found palette)})]))

(defmethod scene/draw [mvt :cube] [_ _ p {:keys [palette figures data]}]
  (let [{:keys [u]} (:cubic data)]
    [:g (svg/polyline (curve-pts figures) {:stroke (:ink palette) :width 0.02})
     (svg/polyline (chord figures (:cubic data)) {:stroke (:construction palette) :width 0.025})
     (svg/polyline (tangent-at figures u 0.8) {:stroke (:found palette) :width 0.025})
     (readout palette (a/play p 0 0.3) [-1.5 2.0] ["x³: u² = (a² + ab + b²)/3" "Emmy: proved"])]))

(defmethod scene/draw [mvt :remainder] [_ _ p {:keys [palette figures]}]
  ;; exp and its partial sums: the :exp-0 .. :exp-4 kernels (Emmy's Taylor
  ;; polynomials compiled by raster)
  (let [n (inc (int (* 3.99 (a/play p 0 1 a/linear))))
        xs (mapv (fn [i] [(- (* 3 (/ i 60)) 1.5)]) (range 61))]
    [:g (svg/polyline (figure/points (:exp-0 figures) [] xs) {:stroke (:ink palette) :width 0.02})
     (svg/polyline (figure/points ((keyword (str "exp-" n)) figures) [] xs) {:stroke (:found palette) :width 0.025})
     (readout palette 1 [-1.5 2.0] [(str n " terms; the rest = xⁿ/n! f⁽ⁿ⁾(u)")])]))

(defmethod scene/draw [mvt :bounds] [_ _ p {:keys [palette]}]
  [:g (readout palette (a/play p 0 0.4) [-1.5 1.0] ["0 < u < x: the rest lies between" "the least and greatest of xⁿ/n! f⁽ⁿ⁾"])])

(defmethod scene/draw [mvt :cauchy] [_ _ p {:keys [palette figures data]}]
  (let [{:keys [u]} (:cubic data)]
    [:g (svg/polyline (curve-pts figures) {:stroke (:ink palette) :width 0.02})
     (svg/polyline (tangent-at figures u 0.8) {:stroke (:found palette) :width 0.025})
     (readout palette (a/play p 0 0.4) [-1.5 2.0] ["Cauchy 1823: the same theorem," "from limits"])]))
