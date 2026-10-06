(ns alexandria.apollonius.conics-scenes-test
  (:require [alexandria.apollonius.conics :as conics]
            [alexandria.apollonius.conics-scenes]
            [alexandria.apollonius.conics-view :as view]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.scene :as scene]
            [alexandria.palette :as palette]
            [alexandria.proofs :as proofs]
            [clojure.test :refer [deftest is testing]]
            [alexandria.medium.kernel-oracle :as ko]))

(def ctx
  {:figures (update-vals conics/figures (comp figure/->FnFigure :f))
   :palette (palette/palette :deck)
   :data (view/data)})

(def scenes
  {:apollonius/sections :apollonius/I.11-13
   :apollonius/focal :apollonius/focal
   :apollonius/dandelin :apollonius/dandelin})

(deftest every-stage-is-drawn
  (doseq [[scene-id proof-id] scenes
          {:keys [stage]} (proofs/steps conics/proofs-resource proof-id)
          p [0 0.37 1]
          c [ctx (assoc ctx :controls {:tilt 40}) (assoc ctx :controls {:tilt 75})]]
    (let [h (scene/draw scene-id stage p c)]
      (is (and (vector? h) (not= :text (first h))) (str scene-id " " stage " at " p)))))

(deftest the-foci-are-dandelins-points-of-contact
  (testing "the feet of the spheres lie on the cutting plane and on its diameter"
    (let [{:keys [k h dandelin-tilt spheres]} (view/data)
          [[x1 y1 z1] [x2 _ z2]] (:feet spheres)
          s (fn [x] (/ (- (* k h) x) (Math/cos dandelin-tilt)))]
      (is (< (Math/abs y1) 1e-12))
      (is (< (Math/abs (- z1 (+ h (* (s x1) (Math/sin dandelin-tilt))))) 1e-9))
      (is (< (Math/abs (- z2 (+ h (* (s x2) (Math/sin dandelin-tilt))))) 1e-9)))))

(deftest kernels-compile
  (let [kernels (requiring-resolve 'alexandria.medium.kernel/kernels)
        ks (kernels conics/figures)]
    (is (= (set (keys conics/figures)) (set (keys ks))))
    (is (every? (fn [{:keys [glue fallback]}] (and (string? glue) (seq fallback))) (vals ks)))))

(def ^:private oracle-cases
  "[figure params states] per figure: the points each scene feeds it."
  (let [{:keys [k h yaw pitch dandelin-tilt]} (view/data)
        phis (mapv (fn [i] [(+ 0.13 (* 0.5 i))]) (range 12))]
    {:cone [[k yaw pitch] [[0 2.4] [1.3 -2.4] [3.0 1.0]]]
     :section [[k 0.7 h yaw pitch] phis]
     :plane [[k 0.7 h yaw pitch] [[-1.2 -1.5] [3.6 1.5] [0 0]]]
     :height [[k 0.7 h] phis]
     :plane-height [[k 0.7 h] [[0] [0.9] [2.5]]]
     :space [[yaw pitch] [[0 0 0.85] [0.23 0 1.23] [-0.93 0 1.94]]]
     :symptoma [[1 -0.35] [[0.4] [1.0] [1.4]]]
     :application [[k 0.3 h] [[0.35] [0.6] [0.85]]]
     :trace [[k 0.7 h yaw pitch] [[2.4] [1.8]]]
     :ellipse [[1.5 1.2] phis]
     :ellipse-tangent [[1.5 1.2] [[0.5 -0.9] [0.5 0.9] [2.1 0.9]]]
     :ellipse-focal [[1.5 1.2] [[0.5 0] [0.5 1] [2.1 0] [2.1 1]]]
     :hyperbola [[1 1.118] [[-1.15] [0] [1.15]]]
     :hyperbola-focal [[1 1.118] [[-1.15] [0.3] [1.15]]]
     :dandelin [[k dandelin-tilt h] phis]
     :cone3 [[k] [[0 2.4] [1.3 -2.4] [3.0 1.0]]]
     :section3 [[k 0.7 h] phis]
     :plane3 [[k 0.7 h] [[-1.9 -1.75] [3.4 1.75] [0 0]]]
     :ordinate3 [[k 0.4 h] [[0.85] [0.3]]]}))

(deftest every-figure-kernel-matches-its-emmy-oracle
  (testing "each figure's wasm kernel, run in node, against the same Emmy function on the JVM"
    (is (= (set (keys conics/figures)) (set (keys oracle-cases))) "every figure has a case")
    (doseq [[id [params states]] oracle-cases]
      (when-let [{:keys [wasm max-error]} (ko/deviation (conics/figures id) params states)]
        (is wasm (str id))
        (is (< max-error 1e-4) (str id " " max-error))))))

(deftest the-numbers-the-scenes-print-come-from-raster
  (let [d (view/data)]
    (is (= :raster (get-in d [:spheres :source])))
    (is (= :raster (get-in d [:focal :source])))
    (is (< (Math/abs (- (get-in d [:focal :ellipse-c]) 0.9)) 1e-12))
    (is (< (Math/abs (- (get-in d [:focal :as-sa]) 1.44)) 1e-12))))

(deftest the-application-panel-is-to-scale-and-names-by-the-sign
  (testing "QV^2 = PV.VR at every tilt, and the name follows the sign of c"
    (let [{:keys [k h parabola-tilt]} (view/data)
          f (figure/->FnFigure (:f (conics/figures :application)))]
      (doseq [[deg kind] [[0 :ellipse] [10 :ellipse] [25 :ellipse]
                          [(* (/ 180 Math/PI) parabola-tilt) :parabola] [75 :hyperbola]]
              t [0.35 0.85]]
        (let [[x p vr y c] (mapv double (first (figure/points f [k (* deg (/ Math/PI 180)) h] [[t]])))]
          (is (< (Math/abs (- (* y y) (* x vr))) 1e-9) (str deg))
          (is (pos? vr) (str "VR > 0 at " deg))
          (is (= kind (conics/kind c)) (str deg " " c))
          (case kind
            :ellipse (is (< vr p)) :hyperbola (is (> vr p)) :parabola (is (< (Math/abs (- vr p)) 1e-9))))))))
