(ns alexandria.euclid.elements-view
  "Elements I.1 and I.47 as proof players for a Clerk notebook: Heath's
   steps (elements_1.proofs.edn) over a figure whose given points come
   from the board's raster frame and whose moving points come from the
   :motion raster kernel."
  (:require [alexandria.euclid.elements :as el]
            [alexandria.medium.clerk :as medium]
            [alexandria.proofs :as proofs]))

(def ^:private render-fn 'alexandria.euclid.elements-scenes/render)

(defn data
  "The numbers scene id draws, all from raster: the points for I.1, and
   the windmill for I.47."
  [id]
  (case id
    :euclid/I.1 {:points (el/figure :euclid/I.1)}
    :euclid/I.47 (el/windmill)))

(defn player
  "The player of proposition id (:euclid/I.1 or :euclid/I.47)."
  [id]
  (medium/player render-fn id
                 (proofs/steps el/proofs-resource id)
                 el/figures (data id)
                 (case id
                   :euclid/I.1 {:window [[-1.65 1.65] [-1.15 1.15]] :height 320
                                :durations {:circle-a 3000 :circle-b 3000 :join 2000}}
                   :euclid/I.47 {:window [[-1.4 3.4] [-2.3 2.85]] :height 420
                                 :durations {:shear-left 6000 :shear-right 6000}})))
