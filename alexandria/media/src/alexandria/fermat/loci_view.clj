(ns alexandria.fermat.loci-view
  "Fermat's Isagoge as a proof player for a Clerk notebook, over the scenes
   of alexandria.fermat.loci-scenes: one sentence of Fermat per step."
  (:require [alexandria.fermat.loci :as loci]
            [alexandria.medium.clerk :as medium]
            [alexandria.proofs :as proofs]
            [hive-dsl.result :as r]))

(def ^:private render-fn 'alexandria.fermat.loci-scenes/render)

(defn section
  "The Isagoge's data (:passage :french :source :steps), or nil."
  []
  (let [res (loci/proof)] (when (r/ok? res) (get-in res [:ok :fermat/loci]))))

(defn loci
  "The five loci, stepped: the frame NZ = A, ZI = E, then each equation
   with I running along its curve (raster kernels)."
  []
  (medium/player render-fn :fermat/loci (proofs/steps loci/proofs-resource :fermat/loci)
                 loci/figures {}
                 {:window [[-2 3.1] [-2.1 2.5]] :height 360
                  :durations {:setup 6000 :line 6000 :hyperbola 6000 :parabola 6000
                              :circle 6000 :ellipse 6000 :descartes 5000}}))
