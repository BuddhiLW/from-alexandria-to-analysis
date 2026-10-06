(ns alexandria.kepler.mysterium-view
  "Kepler's Mysterium Cosmographicum as a proof player for a Clerk notebook,
   and its numbers as data a page can show.

     project   the figure: a vertex [x y z] of a solid, turned by angle about
               the vertical axis, tilted toward the viewer, scaled, and
               projected orthographically to the plane; compiled to a raster
               kernel, one batch per frame for every vertex of a solid"
  (:require [alexandria.kepler.mysterium :as my]
            [alexandria.medium.clerk :as medium]
            [alexandria.proofs :as proofs]
            [alexandria.solids :as solids]
            [emmy.env :as e]
            [hive-dsl.result :as r]
            [alexandria.raster :as raster]))

(def ^:private render-fn 'alexandria.kepler.mysterium-scenes/render)

(defn project
  "(fn [angle tilt scale] (fn [[x y z]] [u v])): orthographic projection of
   the solid turned by angle about the y axis and tilted by tilt about the x
   axis."
  [angle tilt scale]
  (fn [[x y z]]
    (let [x1 (e/+ (e/* (e/cos angle) x) (e/* (e/sin angle) z))
          z1 (e/- (e/* (e/cos angle) z) (e/* (e/sin angle) x))
          y2 (e/- (e/* (e/cos tilt) y) (e/* (e/sin tilt) z1))]
      [(e/* scale x1) (e/* scale y2)])))

(def figures
  {:project {:f project :params [0 0.42 1] :state [0 0 0]}})

(defn proposition
  "The proposition's data: :passages, :source, :steps."
  []
  (let [res (my/proof)]
    (when (r/ok? res) (get-in res [:ok :kepler/mysterium]))))

(defn- plain-solid
  "Solid id as numbers for the browser: the exact Emmy vertices and
   circumradius evaluated by raster kernels (alexandria.raster/value); the
   projection of every vertex per frame is the :project kernel."
  [id]
  (let [s (solids/solids id)]
    {:vertices (mapv (fn [v] (raster/value (fn [] (vec v)))) (:vertices s))
     :edges (:edges s) :faces (:faces s)
     :circum (raster/value (fn [] (my/radius id :circum)))}))

(defn data
  "Everything the scenes show, as plain numbers."
  []
  (let [rows (my/comparison)]
    {:solids (into {} (map (juxt identity plain-solid)) solids/solid-ids)
     :radii (zipmap my/planets (my/nested-radii))
     :ratios (into {} (map (juxt identity my/ratio-value)) solids/solid-ids)
     :mid-ratio (my/ratio-value :octahedron :mid)
     :rows (mapv #(select-keys % [:solid :computed :copernicus :midsphere]) rows)
     :table (into {} (map (juxt :solid #(select-keys % [:computed :copernicus]))) rows)}))

(defn player
  "The nested solids, played shell by shell."
  []
  (medium/player render-fn :kepler/mysterium
                 (proofs/steps my/proofs-resource :kepler/mysterium)
                 figures
                 (data)
                 {:window [[-1.1 2.15] [-1.02 1.02]] :height 460
                  :durations {:spheres 6000 :five 6000 :cube 7000 :tetrahedron 7000
                              :dodecahedron 7000 :icosahedron 7000 :octahedron 7000
                              :table 7000 :mercury 7000 :verdict 7000}}))
