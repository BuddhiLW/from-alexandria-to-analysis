(ns alexandria.archimedes.quadrature-scenes
  "Quadrature of the Parabola, Props 21-24 (scene :archimedes/parabola):
   the segment of y = 1 - x^2 exhausted by triangles, stage by stage. Every
   triangle's corners come from the :stage figure of
   alexandria.archimedes.quadrature through ctx :figures (a raster kernel
   in the browser); the new stage grows up from its chords with Manim's
   smooth. ctx :controls :stages is the player's slider; ctx :data :rows
   carries the exact areas (strings) of each stage."
  (:require [alexandria.medium.anim :as a]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            #?(:cljs [alexandria.medium.player :as player])))

#?(:cljs (def render "Clerk's render-fn for these scenes." player/render))

(def ^:private scene-id :archimedes/parabola)
(def ^:private colours [:found :construction :ink :muted])

(def ^:private corner-states
  (memoize (fn [s] (vec (for [k (range (bit-shift-left 1 s)) j (range 3)] [k j])))))

(defn- triangles
  "The triangles of stage s grown by t, as vectors of three corners."
  [figures s t]
  (partition 3 (figure/points (:stage figures) [s t] (corner-states s))))

(defn- stage-layer [palette figures s t]
  (let [c (get palette (nth colours (mod s (count colours))))]
    (into [:g] (for [tri (triangles figures s t)]
                 (svg/polygon tri {:stroke c :fill c :opacity 0.45 :width 0.006})))))

(defn- readout [palette f lines]
  (svg/layer (a/fade f [0.2 0])
             (into [:g] (map-indexed (fn [i s] (svg/text [1.08 (- 1.05 (* 0.13 i))] s
                                                         {:anchor "start" :size 0.075 :mono? true
                                                          :colour (:ink palette)}))
                                     lines))))

(defn- segment
  "The segment: the arc (ctx :data :arc, sampled by alexandria.raster) and its chord."
  [palette arc]
  [:g (svg/polygon arc {:fill (:muted palette) :opacity 0.18})
   (svg/polyline arc {:stroke (:ink palette) :width 0.012})
   (svg/segment [-1 0] [1 0] {:stroke (:ink palette) :width 0.012})])

(defn- upto
  "Stages 0..n-1 in place and stage n grown by t, with the exact rows."
  [palette figures {:keys [rows arc]} n t]
  (into [:g (segment palette arc)]
        (concat (for [s (range n)] (stage-layer palette figures s 1))
                [(stage-layer palette figures n t)
                 (let [{:keys [stage polygon left]} (nth rows (min n (dec (count rows))))]
                   (readout palette 1 [(str "stage " stage) (str "polygon = " polygon)
                                       "segment = 4/3" (str "left = " left)]))])))

(defmethod scene/draw [scene-id :stage] [_ [_ n] p {:keys [palette figures data controls]}]
  (let [n (or (some-> controls :stages long) n)]
    (upto palette figures data n (a/play p 0 0.6))))

(defmethod scene/draw [scene-id :sum] [_ [_ k] p {:keys [palette figures data]}]
  (let [k (or k 0)]
    [:g (upto palette figures data 3 1)
     (svg/layer (a/fade (a/play p 0 0.4) [0 -0.1])
                (svg/text [0 1.32] (nth ["b + B = A/3, c + C = B/3, ..."
                                         "(B + ... + Z) + (b + ... + z) = (A + ... + Y)/3"
                                         "B + ... + Z + Z/3 = A/3"
                                         "A + B + ... + Z + Z/3 = 4A/3"] (min k 3))
                          {:size 0.08 :colour (:found palette)}))]))
