(ns alexandria.lagrange.scenes-test
  (:require [alexandria.lagrange.functions :as functions]
            [alexandria.lagrange.mechanics :as mechanics]
            [alexandria.lagrange.mechanics-scenes]
            [alexandria.lagrange.variations :as variations]
            [alexandria.lagrange.variations-scenes]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.kernel-oracle :as ko]
            [alexandria.medium.scene :as scene]
            [alexandria.palette :as palette]
            [alexandria.proofs :as proofs]
            [clojure.test :refer [deftest is testing]]))

(defn- fn-figures [figs] (update-vals figs (comp figure/->FnFigure :f)))

(def var-ctx
  {:figures (fn-figures variations/figures)
   :palette (palette/palette :deck)
   :data {:race (variations/race variations/race-params 12) :times (variations/descent-times)}})

(def mech-ctx
  {:figures (fn-figures (merge mechanics/figures functions/figures))
   :palette (palette/palette :deck)
   :data {:portrait (take 2 (mechanics/pendulum-portrait))
          :swing (mechanics/pendulum-swing 1.1)
          :double (mechanics/double-pendulum-run 1.9 2.4 1.0 0.1)
          :phis (mapv #(mechanics/conic-anomaly 0.5 (/ % 24)) (range 25))
          :cubic {:a -0.4 :b 1.4 :u (double (functions/cube-mean-point -0.4 1.4))}}})

(deftest every-kernel-matches-its-emmy-oracle
  (testing "each Lagrange figure, run as wasm in node, within raster's libm tolerance, on the states its scene draws"
    (let [line (mapv vector (range -1.5 1.6 0.25))
          unit (mapv vector (range 0 1.01 0.1))
          bowl (mapv vector (range 0.05 3.1 0.2))]
      (doseq [[k fig params states]
              (concat
               (for [k [:cycloid :wheel]] [k (k variations/figures) (:params (k variations/figures)) line])
               [[:varied (:varied variations/figures) [0.3] unit]
                [:bowl (:bowl variations/figures) [0.5 0] bowl]
                [:bowl-moving (:bowl variations/figures) [0.5 2.0] bowl]
                [:conic (:conic mechanics/figures) [0.5] line]
                [:bobs (:bobs mechanics/figures) []
                 (vec (for [a [-1.0 0.4 2.0] b [0.3 -2.5] end [0 1]] [a b end]))]]
               (for [[k fig] functions/figures] [k fig [] line]))
              :let [res (ko/deviation fig params states)]
              :when res]
        (is (:wasm res) (str k))
        (is (< (:max-error res) 1e-4) (str k " " (:max-error res)))))))

(deftest the-kepler-clock-sweeps-equal-areas
  (testing "conic-anomaly (raster): the focal area grows linearly in t"
    (let [ecc 0.5 a (/ 1 (- 1 (* ecc ecc))) b (* a (Math/sqrt (- 1 (* ecc ecc))))
          area (fn [phi] (let [E (* 2 (Math/atan2 (* (Math/sqrt (- 1 ecc)) (Math/sin (/ phi 2)))
                                                  (* (Math/sqrt (+ 1 ecc)) (Math/cos (/ phi 2)))))]
                           (* 0.5 a b (- E (* ecc (Math/sin E))))))]
      (doseq [t [0.1 0.3 0.45]]
        (is (< (Math/abs (- (area (mechanics/conic-anomaly ecc t)) (* t Math/PI a b))) 1e-9))))))

(defn- drawn? [h] (and (vector? h) (not= :text (first h))))

(deftest every-stage-is-drawn
  (doseq [[resource id ctx] [[variations/brachistochrone-resource :bernoulli/brachistochrone var-ctx]
                             [variations/delta-resource :lagrange/delta-1755 var-ctx]
                             [mechanics/proofs-resource :lagrange/virtual-velocities mech-ctx]
                             [mechanics/proofs-resource :lagrange/equations-of-motion mech-ctx]
                             [mechanics/proofs-resource :lagrange/kepler mech-ctx]
                             [functions/proofs-resource :lagrange/mean-value mech-ctx]]
          {:keys [stage]} (proofs/steps resource id)
          p [0 0.37 1]]
    (is (drawn? (scene/draw id stage p ctx)) (str id " " stage " at " p))))

(deftest the-cycloid-figure-starts-at-the-cusp
  (let [[[x0 y0] [x1 y1]] (figure/points (:cycloid (:figures var-ctx)) [1] [[0] [Math/PI]])]
    (is (= [0.0 0.0] [x0 y0]))
    (is (< (Math/abs (- x1 Math/PI)) 1e-12))
    (is (< (Math/abs (+ y1 2)) 1e-12))))

(deftest the-orbit-figure-is-the-conic
  (let [[[x y]] (figure/points (:conic (:figures mech-ctx)) [0.5] [[0]])]
    (is (< (Math/abs (- x (/ 1 1.5))) 1e-12))
    (is (< (Math/abs y) 1e-12))))
