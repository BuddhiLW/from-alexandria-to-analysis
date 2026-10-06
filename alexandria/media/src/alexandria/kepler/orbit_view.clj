(ns alexandria.kepler.orbit-view
  "Kepler's laws as proof players for a Clerk notebook: Mars on its ellipse
   (Astronomia Nova) and the third law (Harmonices Mundi), with the
   passages a page can quote.

   The planet figure is alexandria.kepler.orbit/planet, compiled to a raster
   kernel: Kepler's equation is solved inside the kernel by fixed-point
   steps, one batch per frame."
  (:require [alexandria.kepler.orbit :as orbit]
            [alexandria.medium.clerk :as medium]
            [alexandria.proofs :as proofs]
            [hive-dsl.result :as r]))

(def ^:private render-fn 'alexandria.kepler.orbit-scenes/render)

(def figures
  "Every orbit figure as a raster kernel; the nested sines of the planet and
   the fixed-point steps compile unsimplified (no simplifier helps them)."
  (-> orbit/figures
      (update-vals (fn [fig] (assoc fig :opts {:simplify? false})))
      (update :circle dissoc :opts)))

(defn proposition
  "The data of :kepler/war-with-mars or :kepler/harmonice: :passages,
   :source, :steps."
  [id]
  (let [res (orbit/proof)]
    (when (r/ok? res) (get-in res [:ok id]))))

(defn data
  "Everything the scenes show, as plain numbers."
  []
  (let [{ecc :e a :a} orbit/mars]
    {:e ecc :a a :b (double (orbit/minor a ecc))
     :rows (orbit/third-law)
     :spread (orbit/spread)}))

(defn war-with-mars
  "Mars on its ellipse, the sectors of equal times filling."
  []
  (medium/player render-fn :kepler/war-with-mars
                 (proofs/steps orbit/proofs-resource :kepler/war-with-mars)
                 figures (data)
                 {:window [[-2.0 3.9] [-1.75 1.75]] :height 440
                  :durations {:circle 6000 :eight 7000 :ellipse 7000 :sweep 6000
                              :equal-areas 12000 :solve 8000}}))

(defn harmonice
  "The six planets on one line, T^2 against a^3."
  []
  (medium/player render-fn :kepler/harmonice
                 (proofs/steps orbit/proofs-resource :kepler/harmonice)
                 figures (data)
                 {:window [[-2.0 4.6] [-2.1 3.5]] :height 440
                  :durations {:table 6000 :points 6000 :line 6000 :newton 6000}}))
