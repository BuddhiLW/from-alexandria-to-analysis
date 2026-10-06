(ns alexandria.delian.duplication-view
  "The doubling of the cube as proof players and one MathBox scene for a
   Clerk notebook. Players: steps from duplication.proofs.edn over the scenes
   of alexandria.delian.duplication-scenes (figures as raster kernels,
   numbers from alexandria.raster on the JVM). Archytas' three surfaces are
   an emmy-viewers MathBox scene compiled with *backend* :raster, so MathBox
   samples raster kernels; the notebook that mounts it calls
   emmy.clerk/install!."
  (:require [alexandria.delian.duplication :as d]
            [alexandria.medium.clerk :as medium]
            [alexandria.proofs :as proofs]
            [emmy.env :as e]
            [emmy.mathbox.plot :as plot]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]))

(def ^:private render-fn 'alexandria.delian.duplication-scenes/render)

(defn- steps [id] (proofs/steps d/proofs-resource id))

(def data (memoize d/scene-data))

(defn- player [id figures opts]
  (medium/player render-fn id (steps id) (select-keys d/figures figures) (data) opts))

(defn hippocrates []
  (player :delian/hippocrates [] {:window [[-1.4 4.6] [-0.6 2.5]] :height 320
                                  :durations {:cube 3000 :eight 4000 :square 4000 :means 6000}}))

(defn menaechmus
  "The two parabolas and the hyperbola, the slider a moving their meeting
   point; its abscissa is a times the cube root of 2."
  []
  (player :delian/menaechmus [:parabola-x :parabola-y :hyperbola :meeting]
          {:window [[-0.5 4.6] [-0.7 4.4]] :height 460
           :controls [{:id :a :label "a" :min 0.6 :max 2.2 :step 0.01 :init 1.4}]
           :durations {:given 3000 :lines 4000 :parabola 5000 :hyperbola 5000
                       :meet 4000 :second 5000 :cube-root 5000}}))

(defn mesolabe []
  (player :delian/mesolabe [:mesolabe :frame-edge]
          {:window [[-0.4 3.6] [-0.4 2.8]] :height 360
           :durations {:frames 3000 :slide 7000 :line 4000 :means 5000}}))

(defn conchoid []
  (player :delian/conchoid [:conchoid] {:window [[-3.6 3.6] [-1.5 2.9]] :height 320
                                        :durations {:trace 8000 :neusis 6000}}))

(defn cissoid []
  (player :delian/cissoid [:cissoid] {:window [[-0.4 2.6] [-1.3 1.6]] :height 300
                                      :durations {:trace 7000 :proportion 6000}}))

;; ---------------------------------------------------------------------------
;; Archytas in MathBox (AC = 2 on the x axis, A at the origin, AB = 1)

(defn- cylinder [[u v]] [(e/+ 1 (e/cos u)) (e/sin u) v])
(defn- torus [[u v]] (let [rho (e/* 2 (e/square (e/cos v)))]
                       [(e/* rho (e/cos u)) (e/* rho (e/sin u)) (e/* 2 (e/cos v) (e/sin v))]))
(defn- cone [[u v]] [v (e/* (e/sqrt 3) v (e/cos u)) (e/* (e/sqrt 3) v (e/sin u))])

(defn archytas
  "MathBox scene of the half-cylinder, the torus and the cone, and the
   meeting point P (from raster's Brent root, alexandria.delian.duplication/
   archytas-numbers). Orbit camera: drag rotates, scroll or CTRL+scroll
   zooms, shift-drag pans, double-click resets (MathBox's controls)."
  []
  (let [{:keys [x y z]} (d/archytas-numbers 2 1)]
    (binding [vc/*backend* :raster]
      (plot/scene
       {:range [[-0.2 2.2] [-1.2 1.2] [0 2]] :scale [1 1 0.9] :camera [1.6 -2.6 1.6]
        :axes {:x {:label "AC"} :y true :z true}}
       (plot/parametric-surface {:f cylinder :u [0 Math/PI] :v [0 2] :color "#4a7fb0" :opacity 0.35
                                 :u-samples 48 :v-samples 12 :grid-u 8 :grid-v 4})
       (plot/parametric-surface {:f torus :u [0 (/ Math/PI 2)] :v [0 (/ Math/PI 2)] :color "#c8913a" :opacity 0.45
                                 :u-samples 40 :v-samples 40 :grid-u 6 :grid-v 6})
       (plot/parametric-surface {:f cone :u [0 Math/PI] :v [0 1.0] :color "#2f8a3e" :opacity 0.3
                                 :u-samples 48 :v-samples 16 :grid-u 8 :grid-v 4})
       (plot/point {:coords [x y z] :label "P" :color "#c0392b" :size 18})))))
