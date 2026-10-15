(ns alexandria.bernoulli.design-view
  "MathBox projection of the backend-neutral design's sampled scene graph.
   Uses exactly the track-node points consumed by the Blender emitter."
  (:require [alexandria.design :as design]
            [emmy.mathbox.plot :as plot]
            [emmy.viewer.compile :as vc]))

(def colours {:cycloid "#C0392B" :arc "#1F6FB4" :line "#7f8c8d"})

(defn- segments [points colour]
  (for [[a b] (partition 2 1 points)]
    (plot/line {:coords [a b] :width 4 :color colour})))

(defn scene
  "The computed printable tracks in millimetres, with the original three
   raster-sampled coordinates projected directly into the orbitable view."
  ([] (scene (design/make-design)))
  ([d]
   (binding [vc/*backend* :raster]
     (let [[w h depth] (:dimensions d)
           nodes (into {} (map (juxt :id :opts) (:nodes d)))
           paths (for [kind [:cycloid :arc :line]
                       :let [pts (:points (nodes kind))]]
                   (segments pts (colours kind)))]
       (apply plot/scene
              {:range [[(- (/ (- w (get-in d [:constraints :span-mm])) 2)) w]
                       [-5 (+ h 8)] [(- (/ depth 2) 8) (+ (/ depth 2) 8)]]
               :scale [1 0.85 0.75]
               :camera [0.9 -1.6 1.2]
               :axes [] :grids []
               :container {:style {:height "460px" :width "100%"}}}
              (concat (apply concat paths)
                      [(plot/point {:coords (first (:points (nodes :cycloid)))
                                    :size 12 :color "#dba627"})
                       (plot/point {:coords (last (:points (nodes :cycloid)))
                                    :size 12 :color "#dba627"})]))))))
