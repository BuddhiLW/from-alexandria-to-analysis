(ns alexandria.fermat.maxima-view
  "Fermat's Methodus as proof players for a Clerk notebook, over the scenes
   of alexandria.fermat.maxima-scenes."
  (:require [alexandria.fermat.maxima :as fm]
            [alexandria.medium.clerk :as medium]
            [alexandria.proofs :as proofs]
            [hive-dsl.result :as r]))

(def ^:private render-fn 'alexandria.fermat.maxima-scenes/render)

(defn section
  "A section's data (:rule :source :steps), or nil."
  [id]
  (let [res (fm/proof)] (when (r/ok? res) (get-in res [:ok id]))))

(defn rectangle
  "The greatest rectangle, by adequality; a slider for the cut A. The curve,
   the chord and Fermat's numbers are raster kernels."
  []
  (medium/player render-fn :fermat/rectangle (proofs/steps fm/proofs-resource :fermat/rectangle)
                 (select-keys fm/figures [:secant :rectangle :adequation]) {}
                 {:window [[-0.8 6.6] [-1.4 2.3]] :height 330
                  :controls [{:id :a :label "A" :min 0.2 :max 3.8 :step 0.05 :init 1}]
                  :durations {:divide 6000 :strike 7000 :derivative 6000}}))

(defn tangent
  "The tangent to the parabola: CE = 2 CD."
  []
  (medium/player render-fn :fermat/tangent (proofs/steps fm/proofs-resource :fermat/tangent)
                 (select-keys fm/figures [:tangent :parabola :outside]) {}
                 {:window [[-1.6 3.4] [-1.7 1.95]] :height 330 :durations {:outside 7000}}))
