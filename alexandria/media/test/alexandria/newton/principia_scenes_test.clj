(ns alexandria.newton.principia-scenes-test
  (:require [alexandria.medium.figure :as figure]
            [alexandria.medium.kernel-oracle :as ko]
            [alexandria.medium.scene :as scene]
            [alexandria.newton.principia :as principia]
            [alexandria.newton.principia-scenes]
            [alexandria.palette :as palette]
            [alexandria.proofs :as proofs]
            [clojure.test :refer [deftest is testing]]))

(def ctx
  {:figures (update-vals principia/figures (comp figure/->FnFigure :f))
   :palette (palette/palette :deck)
   :data (merge (principia/polygon-data) (principia/prop-11-data 48))})

(deftest the-orbit-kernel-matches-emmy
  (when-let [{:keys [wasm max-error]} (ko/deviation (:orbit principia/figures)
                                                    (vec (for [th (range 0 6.3 0.7) end [0 1]] [th end])))]
    (is wasm "the wasm module loaded")
    (is (< max-error 1e-4) "raster's libm tolerance")))

(deftest the-focal-kernel-matches-emmy-and-sums-to-the-axis
  (let [fig (:focal principia/figures)
        [p ecc] (:params fig)
        axis (/ (* 2 p) (- 1 (* ecc ecc)))
        states (mapv vector (range 0 6.3 0.7))]
    (when-let [{:keys [wasm max-error nan-states]} (ko/deviation fig states)]
      (is wasm "the wasm module loaded")
      (is (empty? nan-states))
      (is (< max-error 1e-4) (str "max error " max-error)))
    (doseq [[sp ph] (figure/points (figure/->FnFigure (:f fig)) (:params fig) states)]
      (is (< (Math/abs (- (+ sp ph) axis)) 1e-9) "SP + PH = 2a"))))

(deftest prop-11-angles-sweep-equal-areas
  (testing "Kepler's equation by raster: the area from perihelion grows linearly in t"
    (let [ecc 0.6 p 1.0
          a (/ p (- 1 (* ecc ecc))) b (* a (Math/sqrt (- 1 (* ecc ecc))))
          area (fn [th] (let [E (* 2 (Math/atan2 (* (Math/sqrt (- 1 ecc)) (Math/sin (/ th 2)))
                                                 (* (Math/sqrt (+ 1 ecc)) (Math/cos (/ th 2)))))]
                          (* 0.5 a b (- E (* ecc (Math/sin E))))))]
      (doseq [t [0.1 0.25 0.4]]
        (is (< (Math/abs (- (area (principia/true-anomaly ecc t)) (* t Math/PI a b))) 1e-9) (str t))))))


(deftest every-stage-is-drawn
  (doseq [id [:newton/lemma-1 :newton/prop-1 :newton/prop-11]
          {:keys [stage]} (proofs/steps principia/proofs-resource id)
          p [0 0.37 1]]
    (let [h (scene/draw id stage p ctx)]
      (is (and (vector? h) (not= :text (first h))) (str id " " stage " at " p)))))

(deftest the-force-arrow-scales-as-the-inverse-square
  (let [f (figure/->FnFigure principia/orbit-figure)
        len (fn [th] (let [[[x0 y0] [x1 y1]] (figure/points f [1.0 0.6 0.5] [[th 0] [th 1]])]
                       (Math/hypot (- x1 x0) (- y1 y0))))
        r (fn [th] (/ 1.0 (+ 1 (* 0.6 (Math/cos th)))))]
    (is (< (Math/abs (- (* (len 0.0) (Math/pow (r 0.0) 2)) (* (len 2.5) (Math/pow (r 2.5) 2)))) 1e-9))))
