(ns alexandria.archimedes.quadrature-view
  "Quadrature of the Parabola, Props 21-24, as proof players for a Clerk
   notebook: the steps of each proposition (quadrature_of_the_parabola.proofs.edn)
   over the exhaustion figure, with a slider for the stage."
  (:require [alexandria.archimedes.quadrature :as q]
            [alexandria.medium.clerk :as medium]
            [alexandria.proofs :as proofs]
            [hive-dsl.result :as r]))

(def ^:private render-fn 'alexandria.archimedes.quadrature-scenes/render)

(defn rows
  "The exact areas of stages 0-6, as the scene prints them."
  []
  (mapv (fn [n] {:stage n :polygon (str (q/partial-area n)) :left (str (q/uncovered n))})
        (range 7)))

(def data
  "What the scene shows: the exact rows and the arc sampled by alexandria.raster."
  (memoize (fn [] {:rows (rows) :arc (q/arc 80)})))

(defn proposition
  "Heath's statement and source of proposition id (:archimedes/parabola-21 ..)."
  [id]
  (let [res (q/proof)]
    (when (r/ok? res) (select-keys (get-in res [:ok id]) [:statement :source]))))

(defn player
  "The player of proposition id; Props 22 and 24 carry the stage slider."
  [id]
  (medium/player render-fn :archimedes/parabola
                 (proofs/steps q/proofs-resource id)
                 q/figures (data)
                 (cond-> {:window [[-1.15 2.0] [-0.12 1.45]] :height 360
                          :durations {:stage 3000 :sum 5000}}
                   (#{:archimedes/parabola-22 :archimedes/parabola-24} id)
                   (assoc :controls [{:id :stages :label "stage" :min 0 :max 6 :step 1 :init 3}]))))
