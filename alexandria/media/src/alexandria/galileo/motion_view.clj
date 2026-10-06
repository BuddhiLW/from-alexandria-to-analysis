(ns alexandria.galileo.motion-view
  "Two New Sciences as proof players for a Clerk notebook: each function
   returns a Clerk value, the steps of one proposition
   (alexandria.galileo.motion/proof) played over its scene
   (alexandria.galileo.motion-scenes)."
  (:require [alexandria.galileo.motion :as motion]
            [alexandria.medium.clerk :as medium]
            [alexandria.proofs :as proofs]
            [hive-dsl.result :as r]))

(def ^:private render-fn 'alexandria.galileo.motion-scenes/render)

(defn proposition
  "The data of proposition id: :statement, :quote..., :source, :steps."
  [id]
  (let [res (motion/proof)]
    (when (r/ok? res) (get-in res [:ok id]))))

(defn- player [id figures opts]
  (medium/player render-fn id (proofs/steps motion/proofs-resource id) figures {} opts))

(defn mean-speed
  "Third Day, Theorem I: the speed triangle and the mean-speed rectangle."
  []
  (player :galileo/third-day-1 {}
          {:window [[-0.6 7.4] [-0.6 3.7]] :height 340
           :durations {:triangle 6000 :parallels 6000 :swap 7000}}))

(defn squares
  "Third Day, Theorem II: two times, two triangles, the squared ratio."
  []
  (player :galileo/third-day-2 {}
          {:window [[-0.6 7.4] [-0.6 3.7]] :height 320
           :durations {:squares 7000}}))

(defn odd-numbers
  "Third Day, Corollary I: the staircase 1 3 5 7 and the gnomons."
  []
  (player :galileo/corollary-1 {}
          {:window [[-0.6 9.4] [-0.2 7.6]] :height 400
           :durations {:staircase 6000 :gnomons 7000}}))

(defn inclined-plane
  "The groove, the water clock, and the marks at 1 : 4 : 9 : 16."
  []
  (player :galileo/inclined-plane (select-keys motion/figures [:roll])
          {:window [[-0.5 12.5] [0 3.8]] :height 300
           :durations {:clock 6000 :roll 8000 :quarter 6000}}))

(defn projectile
  "Fourth Day, Theorem I: the semi-parabola and Apollonius' ordinate check."
  []
  (player :galileo/fourth-day-1 (select-keys motion/figures [:throw])
          {:window [[-1.8 8.6] [-4.6 1.4]] :height 380
           :durations {:trace 7000 :symptom 9000}}))

(defn forty-five
  "Fourth Day, Proposition VII: every elevation, the longest at 45 degrees.
   The arcs are the :shot raster kernel, the speed's components c, w the
   :speed raster kernel."
  []
  (player :galileo/fourth-day-7 (select-keys motion/figures [:shot :speed])
          {:window [[-0.3 6.4] [-0.3 3.0]] :height 320
           :durations {:square 8000 :pairs 6000}}))
