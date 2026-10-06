(ns alexandria.apollonius.conics-view
  "Apollonius' Conics as proof players for a Clerk notebook: each function
   returns a Clerk value, the steps of alexandria.apollonius.conics/proof
   played over alexandria.apollonius.conics-scenes. Also the letters and
   the grades as data a page can quote."
  (:require [alexandria.apollonius.conics :as conics]
            [alexandria.medium.clerk :as medium]
            [alexandria.proofs :as proofs]
            [hive-dsl.result :as r]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [alexandria.medium.kernel :as kernel]
            [nextjournal.clerk :as clerk]))

(def ^:private render-fn 'alexandria.apollonius.conics-scenes/render)

(def view
  "The cone's slope, the height of P (the cutting plane sits low on the
   cone, so the section closes inside the drawn nappe), and the eye."
  {:k 0.6 :h 0.7 :yaw 0.5 :pitch 0.32})

(def dandelin-tilt 0.55)

(def focal-axes
  "The semi-axes of the focal scenes' ellipse and hyperbola."
  {:ellipse [1.5 1.2] :hyperbola [1 1.118]})

(defn- focal-data
  "The foci and the numbers of III.45, each by a raster kernel of its Emmy
   expression."
  []
  (let [[ea eb] (:ellipse focal-axes) [ha hb] (:hyperbola focal-axes)
        [ec as-sa cb2] (raster/value (fn [a b] (let [c (e/sqrt (e/- (e/square a) (e/square b)))]
                                                 [c (e/* (e/- a c) (e/+ a c)) (e/square b)]))
                                     ea eb)]
    {:ellipse [ea eb] :hyperbola [ha hb]
     :ellipse-c ec :as-sa as-sa :cb2 cb2
     :hyperbola-c (raster/value (fn [a b] (e/sqrt (e/+ (e/square a) (e/square b)))) ha hb)
     :source raster/source}))

(defn data
  "The numbers the scenes read, every one from a raster kernel."
  []
  (let [{:keys [k h]} view]
    (assoc view
           :parabola-tilt (raster/value (fn [k] (conics/parabola-tilt k)) k)
           :dandelin-tilt dandelin-tilt
           :spheres (conics/dandelin-spheres k dandelin-tilt h)
           :focal (focal-data))))

(defn passages
  "The quoted passages of entry id (:apollonius/preface, :apollonius/menaechmus)."
  [id]
  (let [res (conics/proof)]
    (when (r/ok? res) (get-in res [:ok id]))))

(defn grades
  "[{:label :grade}] of proposition id, graded now by Emmy."
  [id]
  (get (conics/graded) id))

(defn- steps [id] (proofs/steps conics/proofs-resource id))

(def ^:private section-figures [:cone :section :plane :height :plane-height :symptoma :space :trace :application])

(defn sections
  "I.11-13: the plane through the double cone in Apollonius' frame (apex
   A, axial triangle ABC, trace DME), the circle, the symptoma, the
   application of areas drawn to scale, the three names. The tilt slider
   moves the plane (degrees from the horizontal: 0 cuts a circle; the
   parabola sits at the generator's tilt). The window fits the whole double
   cone with a margin and keeps a caption band on its right."
  []
  (let [d (data)
        t0 (* (/ 180 Math/PI) (:parabola-tilt d))]
    (medium/player render-fn :apollonius/sections (steps :apollonius/I.11-13)
                   (select-keys conics/figures section-figures) d
                   {:window [[-3 6.2] [-3.1 3.1]] :height 560
                    :controls [{:id :tilt :label "tilt of the plane (deg)" :min 0 :max 85 :step 0.5
                                :init (Math/round t0)}]
                    :durations {:cone 4000 :diameter 5000 :circle 6000 :similar 6000
                                :symptoma 12000 :apply 12000 :names 6000}})))

(defn sections-3d
  "I.11 in space on emmy-viewers' MathBox (alexandria.apollonius.conics-3d):
   the shaded double cone, the translucent cutting plane, the section on
   it, the circle HK through V and the ordinate QV, from raster kernels;
   orbit camera. Needs a page that loads the emmy-viewers bundle (the
   notebook calls emmy.clerk/install! and is built with that bundle's JS)."
  []
  (let [d (data)
        t0 (* (/ 180 Math/PI) (:parabola-tilt d))]
    (clerk/with-viewer {:name `sections-3d
                        :require-cljs true
                        :transform-fn clerk/mark-presented
                        :render-fn 'alexandria.apollonius.conics-3d/render}
      {:kernels (kernel/kernels (select-keys conics/figures [:cone3 :section3 :plane3 :ordinate3 :height]))
       :data (select-keys d [:k :h :parabola-tilt])
       :controls [{:id :tilt :label "tilt of the plane (deg)" :min 0 :max 85 :step 0.5
                   :init (Math/round t0)}]})))

(defn focal
  "III.45-52: the foci, the tangent, the string, the hyperbola."
  []
  (medium/player render-fn :apollonius/focal (steps :apollonius/focal)
                 (select-keys conics/figures [:ellipse :ellipse-tangent :ellipse-focal :hyperbola :hyperbola-focal])
                 (data)
                 {:window [[-2.5 2.5] [-1.6 2.1]] :height 360
                  :durations {:foci 5000 :tangent 8000 :sum 8000 :string 8000 :difference 9000}}))

(defn dandelin
  "Dandelin's spheres (1822): III.52 in one picture."
  []
  (medium/player render-fn :apollonius/dandelin (steps :apollonius/dandelin)
                 (select-keys conics/figures [:cone :section :plane :height :space :dandelin]) (data)
                 {:window [[-3.6 3.6] [-2.6 3.4]] :height 460
                  :durations {:spheres 6000 :tangents 10000 :generator 10000}}))
