(ns alexandria.oresme.scenes
  "The drawings of Oresme's three passages, one sentence per step
   (alexandria/oresme/latitudes.proofs.edn).

     :oresme/latitude  the figure of a quality: the extension along a line,
                       the degree raised at each point; uniform, uniformly
                       difform, difformly difform; the area is the quantity
     :oresme/mean      the mean degree theorem: triangle ABC, the middle
                       degree DE, the rectangle and its two equal triangles
     :oresme/harmonic  the harmonic series grouped in halves

   ctx :figures
     :profile   alexandria.oresme.latitude/profile (params five heights,
                state [t]: [t degree raw])
     :summary   alexandria.oresme.latitude/summary (params five heights:
                area, mean, differences)
     :proof     alexandria.oresme.mean-speed/proof-figure (params [v0 v1 T])
     :bar       alexandria.oresme.series/bar-figure (params [p sx sy],
                state [n])
   ctx :data :harmonic {:groups [g1 ..] :H [H(2) H(4) ..]} by raster on the
   JVM; ctx :controls the reader's v0 and v1 for the mean player. Every
   coordinate and number drawn comes out of those kernels."
  (:require [alexandria.medium.anim :as a]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            #?(:cljs [alexandria.medium.player :as player])))

#?(:cljs (def render "Clerk's render-fn for these scenes." player/render))

(def kinds
  "The heights of each stage's figure (alexandria.oresme.latitude/kinds)."
  {:uniform [2.5 2.5 2.5 2.5 2.5]
   :uniformly-difform [0 1 2 3 4]
   :difform [1 1.75 2.5 3.25 4]
   :difformly-difform [0.5 2.6 3.4 2.6 0.5]})

(def colours {:figure "#D98E73" :rect "#8FC7E8" :area "#F2D16B" :bad "#E74C3C" :ok "#9FD38A"})

(defn- fmt [x digits]
  #?(:clj (format (str "%." digits "f") (double x))
     :cljs (.toFixed (js/Number x) digits)))

(defn- write [palette at s size]
  (svg/text at s {:colour (:ink palette) :size size :anchor "start" :mono? true}))

(defn- letter [palette [x y] s dx dy size]
  (svg/text [(+ x dx) (+ y dy)] s {:colour (:ink palette) :size size :italic? true}))

(defn- readout [palette at f size lines]
  (svg/layer (a/fade f [0 0])
             (into [:g] (map-indexed (fn [i s] (write palette (update at 1 - (* 1.3 size i)) s size)) lines))))

;; ---------------------------------------------------------------------------
;; The latitude of forms

(def ^:private ts (mapv (fn [i] [(* 0.05 i)]) (range 81)))
(def ^:private strip-ts (mapv (fn [i] [(* 0.25 (+ i 0.5))]) (range 16)))

(defn- curve [{:keys [figures]} hs] (figure/points (:profile figures) hs ts))

(defn- lat-axes [palette f]
  [:g (svg/segment [0 0] [4 0] {:stroke (:ink palette) :width 0.03 :attrs (a/create f)})
   (svg/layer (a/fade f) (letter palette [0 0] "A" -0.12 -0.32 0.26) (letter palette [4 0] "B" 0.1 -0.32 0.26)
              (svg/text [2 -0.45] "extension (longitudo)" {:colour (:muted palette) :size 0.2}))])

(defn- lat-figure [palette ctx hs f fill?]
  (let [pts (curve ctx hs)
        top (mapv (fn [[t v]] [t v]) pts)]
    [:g (when fill? (svg/polygon (concat [[0 0]] top [[4 0]]) {:fill (:figure colours) :opacity (* 0.25 f)}))
     (svg/polyline top {:stroke (:figure colours) :width 0.05 :attrs (a/create f)})]))

(defn- perpendiculars [palette ctx hs f]
  (let [pts (figure/points (:profile (:figures ctx)) hs (mapv vector [0 1 2 3 4]))]
    (into [:g] (map-indexed (fn [i [t v]] (svg/layer (a/fade (nth f i [0 0]))
                                                     (svg/segment [t 0] [t v] {:stroke (:rect colours) :width 0.035})))
                            pts))))

(defn- summary-of [ctx hs] (first (figure/points (:summary (:figures ctx)) hs [[0]])))

(defn- lat-stage [palette ctx p hs label extra]
  (let [[area mean] (summary-of ctx hs)]
    [:g (lat-axes palette 1)
     (lat-figure palette ctx hs (a/play p 0 0.6) true)
     (perpendiculars palette ctx hs (a/lagged p 5 0.3 0.2 0.8))
     extra
     (readout palette [-0.5 5.0] (a/play p 0.5 0.8) 0.22
              [label (str "quantity = area = " (fmt area 3) ",  mean degree = " (fmt mean 3))])]))

(defmethod scene/draw [:oresme/latitude :extension] [_ _ p {:keys [palette] :as ctx}]
  (let [hs (:difformly-difform kinds)]
    [:g (lat-axes palette (a/play p 0 0.4))
     (perpendiculars palette ctx hs (a/lagged p 5 0.4 0.3 0.9))
     (readout palette [-0.5 5.0] (a/play p 0.4 0.8) 0.22
              ["the extension A..B, and at each point" "its degree, raised as a perpendicular (latitudo)"])]))

(defmethod scene/draw [:oresme/latitude :uniform] [_ _ p {:keys [palette] :as ctx}]
  (lat-stage palette ctx p (:uniform kinds) "uniform: one degree throughout, a rectangle" nil))

(defmethod scene/draw [:oresme/latitude :difform] [_ _ p {:keys [palette] :as ctx}]
  (lat-stage palette ctx p (:difform kinds) "difform: the degree changes along the extension" nil))

(defn- excess-steps [palette ctx hs f]
  (let [pts (figure/points (:profile (:figures ctx)) hs (mapv vector [0 1 2 3 4]))]
    (svg/layer (a/fade f)
               (into [:g] (map (fn [[[t0 v0] [t1 v1]]]
                                 [:g (svg/segment [t0 v0] [t1 v0] {:stroke (:muted palette) :width 0.025 :dash "0.08 0.06"})
                                  (svg/segment [t1 v0] [t1 v1] {:stroke (:found palette) :width 0.06})])
                               (partition 2 1 pts))))))

(defmethod scene/draw [:oresme/latitude :uniformly-difform] [_ _ p {:keys [palette] :as ctx}]
  (let [hs (:uniformly-difform kinds)]
    (lat-stage palette ctx p hs "uniformly difform: equal excesses (gold), a straight top"
               (excess-steps palette ctx hs (a/play p 0.6 0.9)))))

(defmethod scene/draw [:oresme/latitude :difformly-difform] [_ _ p {:keys [palette] :as ctx}]
  (let [hs (:difformly-difform kinds)]
    (lat-stage palette ctx p hs "difformly difform: unequal excesses, a curved top"
               (excess-steps palette ctx hs (a/play p 0.6 0.9)))))

(defmethod scene/draw [:oresme/latitude :area] [_ _ p {:keys [palette] :as ctx}]
  (let [hs (:difformly-difform kinds)
        strips (figure/points (:profile (:figures ctx)) hs strip-ts)
        fs (a/lagged p 16 0.4 0 0.7)]
    [:g (lat-axes palette 1)
     (into [:g] (map-indexed (fn [i [t v]]
                               (svg/polygon [[(- t 0.125) 0] [(+ t 0.125) 0] [(+ t 0.125) v] [(- t 0.125) v]]
                                            {:fill (:area colours) :opacity (* 0.35 (nth fs i)) :stroke (:area colours) :width 0.01}))
                             strips))
     (lat-figure palette ctx hs 1 false)
     (let [[area mean] (summary-of ctx hs)]
       (readout palette [-0.5 5.0] (a/play p 0.6 0.9) 0.22
                ["the quantity of the quality is the area of its figure"
                 (str "16 strips → area = " (fmt area 3) " (Boole's rule, raster)")]))]))

;; ---------------------------------------------------------------------------
;; The mean degree theorem

(defn- mean-state [{:keys [figures controls]}]
  (let [v0 (get controls :v0 0) v1 (get controls :v1 4) T 4
        [[_ _ vm rect area half small]] (figure/points (:proof figures) [v0 v1 T] [[0.5]])]
    {:v0 v0 :v1 v1 :T T :vm vm :rect rect :area area :half half :small small
     :A [0 0] :B [T 0] :C [T v1] :F [0 v0] :D [half 0] :E [half vm]}))

(defn- mean-base [palette {:keys [A B C F]} f]
  [:g (svg/polygon [A B C F] {:fill (:figure colours) :opacity (* 0.22 f) :stroke (:figure colours) :width 0.04
                              :attrs (a/create f)})
   (svg/layer (a/fade f)
              (letter palette A "A" -0.15 -0.32 0.26) (letter palette B "B" 0.12 -0.32 0.26)
              (letter palette C "C" 0.15 0.05 0.26)
              (svg/text [2 -0.5] "time (extension)" {:colour (:muted palette) :size 0.2}))])

(defmethod scene/draw [:oresme/mean :setup] [_ _ p {:keys [palette] :as ctx}]
  (let [{:keys [area v0 v1] :as s} (mean-state ctx)]
    [:g (mean-base palette s (a/play p 0 0.6))
     (readout palette [-0.5 5.2] (a/play p 0.5 0.8) 0.22
              [(str "first degree " (fmt v0 2) ", last degree BC = " (fmt v1 2))
               (str "the figure ABC: area = " (fmt area 3))])]))

(defmethod scene/draw [:oresme/mean :midpoint] [_ _ p {:keys [palette] :as ctx}]
  (let [{:keys [D E vm v1] :as s} (mean-state ctx)
        f (a/play p 0 0.6)]
    [:g (mean-base palette s 1)
     (svg/layer (a/fade f)
                (svg/segment D E {:stroke (:rect colours) :width 0.06})
                (letter palette D "D" 0 -0.32 0.26) (letter palette E "E" -0.1 0.15 0.26))
     (readout palette [-0.5 5.2] (a/play p 0.5 0.8) 0.22
              [(str "DE = " (fmt vm 3) " = (first + last)/2, the middle degree")])]))

(defn- rect-of [{:keys [T vm]}] [[0 0] [T 0] [T vm] [0 vm]])

(defmethod scene/draw [:oresme/mean :rectangle] [_ _ p {:keys [palette] :as ctx}]
  (let [{:keys [D E rect] :as s} (mean-state ctx)
        f (a/play p 0 0.6)]
    [:g (mean-base palette s 1)
     (svg/segment D E {:stroke (:rect colours) :width 0.05})
     (svg/polygon (rect-of s) {:fill (:rect colours) :opacity (* 0.25 f) :stroke (:rect colours) :width 0.04
                               :attrs (a/create f)})
     (readout palette [-0.5 5.2] (a/play p 0.5 0.8) 0.22
              [(str "rectangle on AB at the middle degree: " (fmt rect 3))])]))

(defmethod scene/draw [:oresme/mean :equal] [_ _ p {:keys [palette] :as ctx}]
  (let [{:keys [T vm v0 v1 E F C small] :as s} (mean-state ctx)
        f (a/play p 0 0.5)]
    [:g (mean-base palette s 1)
     (svg/polygon (rect-of s) {:fill (:rect colours) :opacity 0.15 :stroke (:rect colours) :width 0.03})
     (svg/polygon [E [T vm] C] {:fill (:bad colours) :opacity (* 0.5 f) :stroke (:bad colours) :width 0.03})
     (svg/polygon [F [0 vm] E] {:fill (:ok colours) :opacity (* 0.5 f) :stroke (:ok colours) :width 0.03})
     (readout palette [-0.5 5.2] (a/play p 0.4 0.8) 0.22
              [(str "cut off above (red) = " (fmt small 3) ",  added below (green) = " (fmt small 3))])]))

(defmethod scene/draw [:oresme/mean :result] [_ _ p {:keys [palette] :as ctx}]
  (let [{:keys [area rect vm T] :as s} (mean-state ctx)]
    [:g (mean-base palette s 1)
     (svg/polygon (rect-of s) {:fill (:rect colours) :opacity (* 0.3 (a/play p 0 0.5)) :stroke (:rect colours) :width 0.04})
     (readout palette [-0.5 5.2] (a/play p 0.3 0.7) 0.22
              [(str "figure " (fmt area 3) " = rectangle " (fmt rect 3))
               (str "d = vₘ t = " (fmt vm 3) " × " (fmt T 0))])]))

;; ---------------------------------------------------------------------------
;; The harmonic series

(def ^:private bar-params [1 1 4])
(def ^:private bar-ns (mapv vector (range 1 17)))

(defn- bars [{:keys [figures]}] (figure/points (:bar figures) bar-params bar-ns))

(defn- group-of [n] (cond (= n 1) 0 (<= n 2) 1 (<= n 4) 2 (<= n 8) 3 :else 4))

(def ^:private group-colours ["#ECEEE4" "#F2D16B" "#9FD38A" "#8FC7E8" "#D98E73"])

(defn- draw-bars [palette ctx upto f]
  (into [:g] (map-indexed
              (fn [i [x0 h]]
                (let [n (inc i) g (group-of n)]
                  (svg/polygon [[x0 0] [(+ x0 1) 0] [(+ x0 1) h] [x0 h]]
                               {:fill (group-colours g) :opacity (if (<= g upto) (* 0.55 f) 0.12)
                                :stroke (:ink palette) :width 0.03})))
              (bars ctx))))

(defn- floor-of
  "The level each term of group k is pushed down to: the bar of n = 2^k."
  [ctx k]
  (second (nth (bars ctx) (dec (bit-shift-left 1 k)))))

(defn- floors [palette ctx upto f]
  (into [:g] (for [k (range 1 (inc upto))
                   :let [lo (bit-shift-left 1 (dec k)) hi (bit-shift-left 1 k) y (floor-of ctx k)]]
               (svg/layer (a/fade (if (= k upto) f 1))
                          (svg/segment [lo y] [hi y] {:stroke (:found palette) :width 0.08})
                          (svg/text [(/ (+ lo hi) 2) (+ y 0.25)] "½" {:colour (:found palette) :size 0.55})))))

(defn- axis [palette]
  [:g (svg/segment [0 0] [16.5 0] {:stroke (:ink palette) :width 0.04})
   (into [:g] (for [n [1 2 4 8 16]] (svg/text [(- n 0.5) -0.6] (str n) {:colour (:muted palette) :size 0.45})))])

(defmethod scene/draw [:oresme/harmonic :terms] [_ _ p {:keys [palette] :as ctx}]
  [:g (axis palette) (draw-bars palette ctx 4 (a/play p 0 0.7))
   (readout palette [3 4.6] (a/play p 0.5 0.8) 0.5 ["1, 1/2, 1/3, … : bars of height 1/n"])])

(defmethod scene/draw [:oresme/harmonic :group] [_ stage p {:keys [palette data] :as ctx}]
  (let [k (second stage)
        g (nth (get-in data [:harmonic :groups]) (dec k))
        H (nth (get-in data [:harmonic :H]) (dec k))]
    [:g (axis palette) (draw-bars palette ctx k 1)
     (floors palette ctx k (a/play p 0.2 0.6))
     (readout palette [3 4.6] (a/play p 0.5 0.8) 0.5
              [(str "group " k ": " (bit-shift-left 1 (dec k)) " term(s), sum = " (fmt g 4) " ≥ 0.5")
               (str "H(" (bit-shift-left 1 k) ") = " (fmt H 4) " ≥ 1 + " k "/2 = " (fmt (+ 1 (/ k 2)) 1))])]))

(defmethod scene/draw [:oresme/harmonic :bound] [_ _ p {:keys [palette data] :as ctx}]
  (let [Hs (get-in data [:harmonic :H])
        big (get-in data [:harmonic :big])]
    [:g (axis palette) (draw-bars palette ctx 4 1) (floors palette ctx 4 1)
     (readout palette [3 4.6] (a/play p 0.2 0.7) 0.5
              [(str "H(16) = " (fmt (nth Hs 3) 4) " ≥ 3")
               (str "H(2^20) = " (fmt big 4) " ≥ 1 + 20/2 = 11")
               "every doubling adds at least ½: no bound holds"])]))
