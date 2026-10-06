(ns alexandria.lagrange.lagrange-view
  "The calculus of variations and analytical mechanics as proof players for a
   Clerk notebook, and the passages a page can quote. Each player plays the
   steps of a *.proofs.edn shelf over its scene; the numbers the scenes show
   are computed here by alexandria.lagrange.* (Emmy), the figures compile to
   raster kernels."
  (:require [alexandria.lagrange.functions :as functions]
            [alexandria.lagrange.mechanics :as mechanics]
            [alexandria.lagrange.variations :as variations]
            [alexandria.medium.clerk :as medium]
            [alexandria.proofs :as proofs]
            [hive-dsl.result :as r]
            [alexandria.raster :as raster]))

(def ^:private var-render 'alexandria.lagrange.variations-scenes/render)
(def ^:private mech-render 'alexandria.lagrange.mechanics-scenes/render)

(defn- entry [res id] (when (r/ok? res) (get-in res [:ok id])))

(defn brachistochrone-text [] (entry (variations/brachistochrone-proof) :bernoulli/brachistochrone))
(defn delta-text [] (entry (variations/delta-proof) :lagrange/delta-1755))
(defn preface [] (entry (mechanics/proof) :preface))
(defn mean-value-text [] (entry (functions/proof) :lagrange/mean-value))

(defn graded
  "Every graded claim of the era, by section id."
  []
  (group-by :id (concat (variations/graded) (mechanics/graded) (functions/graded))))

(def ^:private race-data
  (delay {:race (variations/race) :times (variations/descent-times)}))

(defn brachistochrone []
  (medium/player var-render :bernoulli/brachistochrone
                 (proofs/steps variations/brachistochrone-resource :bernoulli/brachistochrone)
                 variations/figures @race-data
                 {:window [[-0.4 5.6] [-2.6 0.9]] :height 380
                  :durations {:cycloid 6000 :race 8000 :tautochrone 8000}}))

(defn delta []
  (medium/player var-render :lagrange/delta-1755
                 (proofs/steps variations/delta-resource :lagrange/delta-1755)
                 (select-keys variations/figures [:varied]) {}
                 {:window [[-0.3 3.4] [-1.1 1.5]] :height 300}))

(def ^:private mech-data
  ;; every number from raster: the runs by RK4, the Kepler clock by Brent,
  ;; the mean point as Emmy's closed form u = sqrt((a^2 + ab + b^2)/3)
  ;; evaluated by a raster kernel
  (delay {:portrait (mechanics/pendulum-portrait)
          :swing (mechanics/pendulum-swing 1.1)
          :double (mechanics/double-pendulum-run 1.9 2.4 12.0 0.03)
          :phis (mapv #(mechanics/conic-anomaly 0.5 (/ % 240)) (range 241))
          :cubic (let [a -0.4 b 1.4]
                   {:a a :b b :u (raster/value functions/cube-mean-point a b)})}))

(defn- mech-player [id window extra]
  (medium/player mech-render id (proofs/steps mechanics/proofs-resource id)
                 (merge mechanics/figures functions/figures) @mech-data
                 (merge {:window window :height 360} extra)))

(defn virtual-velocities [] (mech-player :lagrange/virtual-velocities [[-2.2 4.2] [-2.3 1.0]] {}))

(defn equations-of-motion []
  (mech-player :lagrange/equations-of-motion [[-2.2 4.2] [-2.3 2.1]]
               {:durations {:portrait 7000 :double-run 12000}}))

(defn kepler []
  (mech-player :lagrange/kepler [[-2.4 3.6] [-1.9 1.9]] {:durations {:areas 9000 :conic 6000}}))

(defn mean-value []
  (medium/player mech-render :lagrange/mean-value
                 (proofs/steps functions/proofs-resource :lagrange/mean-value)
                 (merge mechanics/figures functions/figures) @mech-data
                 {:window [[-1.7 2.0] [-0.6 2.3]] :height 340}))
