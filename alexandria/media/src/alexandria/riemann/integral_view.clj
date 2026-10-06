(ns alexandria.riemann.integral-view
  "Riemann's sections 4 to 6 as proof players for a Clerk notebook, and the
   sections' text as data a page can quote."
  (:require [alexandria.medium.clerk :as medium]
            [alexandria.proofs :as proofs]
            [alexandria.riemann.integral :as integral]
            [hive-dsl.result :as r]))

(def ^:private render-fn 'alexandria.riemann.integral-scenes/render)

(defn section
  "The data of section id (:riemann/integral or :riemann/pathological)."
  [id]
  (let [res (integral/proof)]
    (when (r/ok? res) (get-in res [:ok id]))))

(defn pathological-data
  "The exact upper and lower sums of S_8 on 4 to 64 pieces, as the scene
   draws them: {:cells [{:n :upper :lower :cells [[x0 x1 sup inf] ...]}]}."
  []
  {:cells (vec (for [n [4 8 16 32 64]
                     :let [xs (integral/partition-points 0 1 n)
                           si (integral/partial-sum-sup-inf 8)
                           cells (mapv (fn [x0 x1] (let [[s i] (si x0 x1)] (mapv double [x0 x1 s i])))
                                       xs (rest xs))
                           {:keys [upper lower]} (integral/sums-of-partial 8 n)]]
                 {:n n :upper (double upper) :lower (double lower) :cells cells}))})

(defn integral-player
  "Sections 4 and 5: tagged sums, refinement, upper and lower sums, the
   oscillation criterion."
  []
  (medium/player render-fn :riemann/integral
                 (proofs/steps integral/proofs-resource :riemann/integral)
                 {} {}
                 {:window [[-0.08 2.05] [-0.16 1.12]] :height 380
                  :durations {:tags 7000 :refine 8000 :upper-lower 7000 :oscillation 6000 :sigma 9000}}))

(defn pathological-player
  "Section 6: sum (nx)/n^2, its dense jumps, and its integral."
  []
  (medium/player render-fn :riemann/pathological
                 (proofs/steps integral/proofs-resource :riemann/pathological)
                 (select-keys integral/figures [:graph])
                 (pathological-data)
                 {:window [[-0.1 1.08] [-0.72 0.9]] :height 400
                  :durations {:terms 8000 :dense 7000 :finite-jumps 8000 :integrable 9000}}))
