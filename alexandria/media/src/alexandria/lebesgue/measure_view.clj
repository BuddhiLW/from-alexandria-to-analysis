(ns alexandria.lebesgue.measure-view
  "Lebesgue's measure zero and Dirichlet's function as proof players for a
   Clerk notebook, and the French passages with the series' rendering as
   data a page can quote."
  (:require [alexandria.lebesgue.measure :as measure]
            [alexandria.medium.clerk :as medium]
            [alexandria.proofs :as proofs]
            [hive-dsl.result :as r]))

(def ^:private render-fn 'alexandria.lebesgue.measure-scenes/render)

(defn entry
  "The proof file's entry id (:lebesgue/introduction, ...), or nil."
  [id]
  (let [res (measure/proof)]
    (when (r/ok? res) (get-in res [:ok id]))))

(defn- data [k] {:rationals (mapv double (take k (measure/rationals-01)))})

(defn measure-zero
  "The rationals of [0, 1] covered by intervals of total length < eps."
  []
  (medium/player render-fn :lebesgue/measure-zero
                 (proofs/steps measure/proofs-resource :lebesgue/measure-zero)
                 measure/figures (data 16)
                 {:window [[-2.8 2.8] [-1.95 1.65]] :height 340
                  :durations {:cover 7000 :sum 7000 :shrink 7000}}))

(defn dirichlet
  "Dirichlet's function: Riemann's vertical slices against Lebesgue's
   horizontal ones."
  []
  (medium/player render-fn :lebesgue/dirichlet
                 (proofs/steps measure/proofs-resource :lebesgue/dirichlet)
                 {} (data 60)
                 {:window [[-2.9 4.6] [-1.6 1.85]] :height 340
                  :durations {:riemann-upper 7000 :riemann-lower 7000 :coins 7000}}))
