(ns alexandria.lagrange.variations-scenes
  "The drawings of the brachistochrone (Bernoulli 1696, with Huygens'
   tautochrone) and of Lagrange's delta (1755), one method per stage
   (alexandria.medium.scene/draw), in Manim's idiom (alexandria.medium.anim).

   ctx :data carries, from alexandria.lagrange.variations-view:
     :race   {:line :arc :cycloid} samples [t x depth], times by raster's quadrature
     :times  {:line :arc :cycloid} descent times in seconds
   ctx :figures the :cycloid and :wheel figures (raster kernels in the
   browser)."
  (:require [alexandria.medium.anim :as a]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.math :as m]
            [alexandria.medium.plane :as plane]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            #?(:cljs [alexandria.medium.player :as player])))

#?(:cljs (def render "Clerk's render-fn for these scenes." player/render))

(def ^:private brach :bernoulli/brachistochrone)
(def ^:private delta-id :lagrange/delta-1755)

(defn- fmt [x digits]
  #?(:clj (format (str "%." digits "f") (double x))
     :cljs (.toFixed (js/Number x) digits)))

(defn- write
  ([palette at s] (write palette at s {}))
  ([palette at s opts]
   (svg/text at s (merge {:colour (:ink palette) :size 0.2} opts))))

(defn- stroke [ps colour width f]
  (when (pos? f) (svg/polyline ps {:stroke colour :width width :attrs (a/create f)})))

(defn- label [palette at s f]
  (svg/layer (a/fade f [0 -0.15]) (write palette (plane/translate at [0.1 0.1]) s {:italic? true :anchor "start"})))

(defn- readout [palette f at lines]
  (svg/layer (a/fade f [0.2 0])
             (into [:g] (map-indexed (fn [i s] (write palette (plane/translate at [0 (* -0.3 i)]) s
                                                      {:anchor "start" :size 0.17 :mono? true}))
                                     lines))))

;; ---------------------------------------------------------------------------
;; The tracks (y up on the page: depth d is drawn at -d)

(def ^:private colours {:line :muted :arc :construction :cycloid :found})

(defn- track [samples] (mapv (fn [[_ x d]] [x (- d)]) samples))

(defn- bead-at
  "Where the bead on `samples` is at time t: interpolated, held at B after."
  [samples t]
  (let [[before after] (split-with (fn [[ts]] (<= ts t)) samples)
        [t0 x0 d0] (or (last before) (first samples))
        [t1 x1 d1] (or (first after) (last samples))
        f (if (> t1 t0) (/ (- t t0) (- t1 t0)) 1)]
    [(plane/lerp x0 x1 (min 1 f)) (- (plane/lerp d0 d1 (min 1 f)))]))

(defn- endpoints [palette race f]
  (let [[_ bx bd] (last (:line race))]
    [:g (svg/circle [0 0] 0.06 {:fill (:ink palette)}) (svg/circle [bx (- bd)] 0.06 {:fill (:ink palette)})
     (label palette [0 0] "A" f) (label palette [bx (- bd)] "B" f)]))

(defn- tracks [palette race ks fs]
  (into [:g] (map (fn [k f] (stroke (track (race k)) ((colours k) palette) 0.035 f)) ks fs)))

(defmethod scene/draw [brach :challenge] [_ _ p {:keys [palette data]}]
  (let [race (:race data)]
    [:g (endpoints palette race (a/play p 0 0.3))
     (tracks palette race [:line] [(a/play p 0.3 0.7)])
     (svg/layer (a/fade (a/play p 0.5 0.9))
                (write palette [1.6 0.5] "brevissimo tempore: in the shortest time" {:italic? true :size 0.18}))]))

(defmethod scene/draw [brach :tracks] [_ _ p {:keys [palette data]}]
  (let [race (:race data)]
    [:g (endpoints palette race 1)
     (tracks palette race [:line :arc] [1 (a/play p 0.1 0.6)])
     (readout palette (a/play p 0.5 0.8) [3.3 -0.2] ["Galileo 1638:" "the arc beats the chord"])]))

(defmethod scene/draw [brach :refraction] [_ _ p {:keys [palette data]}]
  (let [race (:race data)
        layers (a/lagged p 8 0.3 0 0.6)]
    (into [:g (endpoints palette race 1)
           (tracks palette race [:cycloid] [(a/play p 0.5 1)])
           (readout palette (a/play p 0.4 0.7) [3.4 -0.2] ["layers of speed sqrt(2gy)" "sin(angle)/v = const"])]
          (map-indexed (fn [i f] (svg/layer (a/fade f)
                                            (svg/segment [-0.2 (* -0.25 (inc i))] [3.3 (* -0.25 (inc i))]
                                                         {:stroke (:muted palette) :width 0.01 :dash "0.05 0.05"})))
                       layers))))

(defmethod scene/draw [brach :cycloid] [_ _ p {:keys [palette data figures]}]
  (let [race (:race data)
        th (* m/pi (a/play p 0.05 0.95 a/linear))
        n 64
        drawn (figure/points (:cycloid figures) [1] (mapv (fn [i] [(* th (/ i n))]) (range (inc n))))
        wheel (figure/points (:wheel figures) [th] (mapv (fn [i] [(* 2 m/pi (/ i 48))]) (range 49)))]
    [:g (endpoints palette race 1)
     (svg/segment [-0.3 0] [3.6 0] {:stroke (:muted palette) :width 0.012})
     (svg/polyline wheel {:stroke (:construction palette) :width 0.02})
     (svg/polyline drawn {:stroke (:found palette) :width 0.04})
     (svg/circle (last drawn) 0.05 {:fill (:found palette)})]))

(defmethod scene/draw [brach :euler-lagrange] [_ _ p {:keys [palette data]}]
  (let [race (:race data)]
    [:g (endpoints palette race 1)
     (tracks palette race [:cycloid] [1])
     (readout palette (a/play p 0.1 0.4) [0.4 -2.4]
              ["T = integral of ds / sqrt(2 g y)"
               "Emmy: Lagrange-equations along the cycloid = (down 0 0)"])]))

(defmethod scene/draw [brach :race] [_ _ p {:keys [palette data]}]
  (let [{:keys [race times]} data
        t (* 1.35 (:line times) (a/play p 0.05 0.95 a/linear))]
    (into [:g (endpoints palette race 1)
           (tracks palette race [:line :arc :cycloid] [1 1 1])
           (readout palette 1 [3.4 -0.2]
                    (for [k [:cycloid :arc :line]]
                      (str (name k) " " (if (>= t (k times)) (fmt (k times) 3) (fmt t 3)) " s")))]
          (for [k [:line :arc :cycloid]]
            (svg/circle (bead-at (race k) t) 0.08 {:fill ((colours k) palette)})))))

(defmethod scene/draw [brach :tautochrone] [_ _ p {:keys [palette figures]}]
  ;; the bowl and the beads are the :bowl kernel (params [r tau], state
  ;; [u0]); r = 1/2. Every bead moves as s(t) = s0 cos(omega t).
  (let [r 0.5
        pts (figure/points (:bowl figures) [r 0] (mapv (fn [i] [(* m/pi (/ i 64))]) (range 65)))
        tau (* 4 m/pi (a/play p 0 1 a/linear))
        [b1 b2] (figure/points (:bowl figures) [r tau] [[0.35] [1.0]])]
    [:g (svg/polyline pts {:stroke (:found palette) :width 0.04})
     (svg/circle b1 0.07 {:fill (:construction palette)})
     (svg/circle b2 0.07 {:fill (:ink palette)})
     (readout palette (a/play p 0 0.2) [0.1 -0.6]
              ["height = s^2 / 8a: a harmonic oscillator"
               "both beads reach the bottom together"])]))

;; ---------------------------------------------------------------------------
;; Lagrange's delta: the paths are the :varied kernel (params [eps])

(defn- path-pts [figures eps]
  (figure/points (:varied figures) [eps] (mapv (fn [i] [(/ i 60)]) (range 61))))

(defmethod scene/draw [delta-id :euler-polygon] [_ _ p {:keys [palette figures]}]
  (let [base (figure/points (:varied figures) [0] (mapv (fn [i] [(/ i 8)]) (range 9)))
        k 4
        bump (* 0.3 (a/there-and-back p))
        moved (assoc base k (plane/translate (base k) [0 bump]))]
    [:g (stroke (path-pts figures 0) (:muted palette) 0.015 1)
     (svg/polyline moved {:stroke (:construction palette) :width 0.03})
     (svg/circle (moved k) 0.06 {:fill (:found palette)})
     (readout palette (a/play p 0 0.2) [0 -0.35] ["Euler 1744: move one vertex"])]))

(defmethod scene/draw [delta-id :delta] [_ _ p {:keys [palette figures]}]
  (let [eps (* 0.35 (a/wiggle p))]
    [:g (stroke (path-pts figures 0) (:muted palette) 0.015 1)
     (svg/polyline (path-pts figures eps) {:stroke (:found palette) :width 0.035})
     (svg/circle [0 0] 0.05 {:fill (:ink palette)}) (svg/circle [3 0] 0.05 {:fill (:ink palette)})
     (readout palette (a/play p 0 0.2) [0 -0.35] ["Lagrange 1755: y + δy, the whole curve" "δy = 0 at the ends"])]))

(defmethod scene/draw [delta-id :first-variation] [_ _ p {:keys [palette figures]}]
  [:g (stroke (path-pts figures 0) (:muted palette) 0.015 1)
   (stroke (path-pts figures 0.25) (:found palette) 0.03 (a/play p 0 0.5))
   (readout palette (a/play p 0.3 0.6) [0 -0.35]
            ["δ∫L = ∫[d/dt(η ∂L/∂q') - η E(L)] dt" "Emmy: proved for every L, q, η"])])

(defmethod scene/draw [delta-id :integrate] [_ _ p {:keys [palette figures]}]
  (let [f (a/play p 0.1 0.6)]
    [:g (stroke (path-pts figures 0.25) (:found palette) 0.03 1)
     (svg/circle [0 0] (* 0.12 f) {:stroke (:construction palette) :width 0.02})
     (svg/circle [3 0] (* 0.12 f) {:stroke (:construction palette) :width 0.02})
     (readout palette (a/play p 0.3 0.7) [0 -0.35] ["η ∂L/∂q' vanishes at both ends"])]))

(defmethod scene/draw [delta-id :equation] [_ _ p {:keys [palette figures]}]
  [:g (stroke (path-pts figures 0) (:found palette) 0.04 1)
   (svg/layer (a/fade (a/play p 0.1 0.5) [0 -0.2])
              (write palette [1.5 -0.5] "d/dt ∂L/∂q' - ∂L/∂q = 0" {:size 0.26}))])
