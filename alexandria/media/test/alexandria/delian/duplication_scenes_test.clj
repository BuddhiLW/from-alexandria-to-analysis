(ns alexandria.delian.duplication-scenes-test
  (:require [alexandria.delian.duplication :as d]
            [alexandria.delian.duplication-scenes]
            [alexandria.delian.duplication-view :as view]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.kernel-oracle :as ko]
            [alexandria.medium.scene :as scene]
            [alexandria.palette :as palette]
            [alexandria.proofs :as proofs]
            [clojure.test :refer [deftest is testing]]))

(defn- ctx [controls]
  {:figures (update-vals d/figures (comp figure/->FnFigure :f))
   :palette (palette/palette :deck) :data (view/data) :controls controls})

(deftest every-delian-stage-is-drawn
  (doseq [id [:delian/hippocrates :delian/menaechmus :delian/mesolabe :delian/conchoid :delian/cissoid]
          a [0.6 1.4 2.2]
          {:keys [stage]} (proofs/steps d/proofs-resource id)
          p [0 0.37 1]]
    (let [h (scene/draw id stage p (ctx {:a a}))]
      (is (and (vector? h) (not= :text (first h))) (str id " " stage " at " p)))))

(deftest the-meeting-point-is-a-times-the-cube-root-of-two
  (testing "the meeting kernel's oracle at the slider's values"
    (let [c (:cube-root-2 (view/data))]
      (doseq [a [0.6 1.4 2.2]]
        (let [[[x y]] (figure/points (figure/->FnFigure d/meeting) [a c] [[0]])]
          (is (< (abs (- (* x x) (* a y))) 1e-9) "on x^2 = a y")
          (is (< (abs (- (* y y) (* 2 a x))) 1e-9) "on y^2 = 2a x")
          (is (< (abs (- (* x x x) (* 2 a a a))) 1e-9) "x^3 = 2 a^3"))))))

(def ^:private oracle-cases
  {:parabola-x [[1.4] [[0] [1.2] [2.4]]]
   :parabola-y [[2.8] [[0] [1.4] [2.8]]]
   :hyperbola [[1.4 2.8] [[0.9] [2] [4.2]]]
   :meeting [[1.4 1.2599] [[0]]]
   :mesolabe [[2 1 0.8] [[0] [1] [2] [3]]]
   :frame-edge [[1 0.8] [[0] [1] [2]]]
   :conchoid [[1 2] [[0.35] [1.57] [2.79]]]
   :cissoid [[1] [[0.01] [0.8] [1.6]]]})

(deftest every-figure-kernel-matches-its-emmy-oracle
  (is (= (set (keys d/figures)) (set (keys oracle-cases))) "every figure has a case")
  (doseq [[id [params states]] oracle-cases]
    (when-let [{:keys [wasm max-error]} (ko/deviation (d/figures id) params states)]
      (is wasm (str id))
      (is (< max-error 1e-4) (str id " " max-error)))))
