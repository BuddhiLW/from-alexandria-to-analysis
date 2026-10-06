(ns alexandria.weierstrass.monster-view
  "Weierstrass' uniform convergence and his function without a derivative
   as proof players for a Clerk notebook, and the address as data a page
   can quote."
  (:require [alexandria.medium.clerk :as medium]
            [alexandria.proofs :as proofs]
            [alexandria.weierstrass.monster :as monster]
            [hive-dsl.result :as r]))

(def ^:private render-fn 'alexandria.weierstrass.monster-scenes/render)

(def x0 "The point the notebook zooms into." 3/10)

(defn part
  "The data of part id (:weierstrass/uniform or :weierstrass/monster)."
  [id]
  (let [res (monster/proof)]
    (when (r/ok? res) (get-in res [:ok id]))))

(defn rows
  "Weierstrass' quotients at x0 for m = 1..6: m and alpha_m whole, the
   rest as plain doubles."
  []
  (mapv (fn [m] (-> (monster/quotients x0 m)
                    (update-vals #(if (ratio? %) (double %) %))
                    (update :alpha long)))
        (range 1 7)))

(defn uniform-player
  "Cauchy's sum theorem, Abel's exception, the tube, the M-test."
  []
  (medium/player render-fn :weierstrass/uniform
                 (proofs/steps monster/proofs-resource :weierstrass/uniform)
                 (select-keys monster/figures [:abel :geometric]) {}
                 {:window [[-0.2 3.75] [-0.75 2.5]] :height 380
                  :durations {:cauchy 6000 :abel 8000 :tube-abel 9000 :tube-geometric 9000 :m-test 7000 :repaired 7000}}))

(defn monster-player
  "The zoom and Weierstrass' estimate."
  []
  (medium/player render-fn :weierstrass/monster
                 (proofs/steps monster/proofs-resource :weierstrass/monster)
                 (select-keys monster/figures [:monster])
                 {:x0 (double x0) :a monster/a :roughness monster/roughness :rows (rows)}
                 {:window [[-2.05 2.05] [-3.5 2.45]] :height 520
                  :durations {:terms 7000 :zoom 12000 :points 9000 :quotients 9000}}))
