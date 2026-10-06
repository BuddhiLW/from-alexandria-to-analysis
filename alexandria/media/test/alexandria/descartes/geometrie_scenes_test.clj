(ns alexandria.descartes.geometrie-scenes-test
  (:require [alexandria.descartes.geometrie :as g]
            [alexandria.descartes.geometrie-scenes]
            [alexandria.descartes.geometrie-view :as view]
            [alexandria.fermat.maxima :as fm]
            [alexandria.fermat.maxima-scenes]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.scene :as scene]
            [alexandria.palette :as palette]
            [alexandria.proofs :as proofs]
            [clojure.test :refer [deftest is testing]]
            [alexandria.medium.kernel-oracle :as ko]))

(defn- ctx [figures data controls]
  {:figures (update-vals figures (comp figure/->FnFigure :f))
   :palette (palette/palette :deck) :data data :controls controls})

(defn- drawn? [scene-id resource id c]
  (doseq [{:keys [stage]} (proofs/steps resource id) p [0 0.37 1]]
    (let [h (scene/draw scene-id stage p c)]
      (is (and (vector? h) (not= :text (first h))) (str scene-id " " stage " at " p)))))

(deftest every-descartes-stage-is-drawn
  (doseq [ab [{:a 2 :b 1.5} {:a 0.4 :b 1.9}]]
    (drawn? :descartes/unit g/proofs-resource :descartes/unit (ctx g/figures {} ab))
    (drawn? :descartes/plane g/proofs-resource :descartes/plane (ctx g/figures {} ab)))
  (doseq [lam [1 -2 -0.5]]
    (testing lam
      (drawn? :descartes/pappus g/proofs-resource :descartes/pappus (ctx g/figures (view/pappus-data) {:lam lam}))))
  (drawn? :descartes/normal g/proofs-resource :descartes/normal (ctx g/figures {} {}))
  (drawn? :descartes/ellipse g/proofs-resource :descartes/ellipse (ctx g/figures {} {}))
  (drawn? :descartes/signs g/proofs-resource :descartes/signs (ctx g/figures {} {}))
  (drawn? :descartes/construction g/proofs-resource :descartes/construction
          (ctx g/figures (view/construction-data) {})))

(deftest every-fermat-stage-is-drawn
  (drawn? :fermat/rectangle fm/proofs-resource :fermat/rectangle (ctx fm/figures {} {:a 1}))
  (drawn? :fermat/tangent fm/proofs-resource :fermat/tangent (ctx fm/figures {} {})))
(deftest the-pappus-data-carries-both-parabolas
  (let [{:keys [parabola-at lines]} (view/pappus-data)]
    (is (= 4 (count lines)))
    (is (= 2 (count parabola-at)))
    (testing "the raster roots of the discriminant agree with Emmy's evaluation of the same expressions"
      (doseq [[r x] (map vector parabola-at (sort (map #(double (emmy.env/simplify %)) (:root-exprs (g/parabola-ratios)))))]
        (is (< (Math/abs (- r x)) 1e-9))))))

(def ^:private oracle-cases
  "{figures {id [params states]}}: the points the scenes feed each kernel."
  {g/figures
   {:multiplication [[2 1.5 0.9] (mapv #(assoc [0 0 0 0 0] % 1) (range 5))]
    :square-root [[2.5] (mapv #(assoc [0 0 0 0 0] % 1) (range 5))]
    :root-arc [[2] [[0] [1.5] [3.1]]]
    :plane-root [[2 1] (mapv #(assoc [0 0 0 0 0] % 1) (range 5))]
    :chord-root [[3 1] (mapv #(assoc [0 0 0 0 0] % 1) (range 5))]
    :circle [[1 0.5 1.2] [[0] [1.5] [4]]]
    :pappus [[1] [[-1.2] [-0.3] [0.4]]]
    :pappus-foot [[] [[1 1 0] [2 0.5 1] [3 2 2] [0.5 2 3]]]
    :pappus-products [[] [[1 1] [2 0.5] [3 2]]]
    :pappus-discriminant [[] [[1] [-2] [-0.5]]]
    :normal [[2.2 1 1] [[0] [1.5] [4]]]
    :normal-meet [[2.2 1 1] [[1] [-1]]]
    :parabola [[1] [[-1.6] [0] [1.2]]]
    :ellipse [[2 4] [[0] [1.5] [4]]]
    :ellipse-circle [[2 4 2.2 1] [[0] [1.5] [4]]]
    :ellipse-meet [[2 4 2.0 1] [[1] [-1]]]
    :quartic [[0.01] [[-6] [0.5] [5]]]
    :parabola-iii [[] [[-2] [0.3] [1.7]]]
    :construction-circle [[3 1.68 0] [[0] [1.5] [4]]]
    :construction-poly [[3 1.68 0] [[-2] [0.65] [1.3]]]}
   fm/figures
   {:secant [[4 1 0.8] [[-0.6] [1.6]]]
    :rectangle [[4 0.25] [[0] [1.3] [4]]]
    :adequation [[4] [[1 0.8 0] [1 0.8 1] [1 0.8 2] [2.5 1e-4 2]]]
    :parabola [[1] [[-1.5] [0.4]]]
    :tangent [[1 1] [[-0.1] [0.6] [1.6]]]
    :outside [[] [[0.35] [0.85]]]}})

(deftest every-figure-kernel-matches-its-emmy-oracle
  (doseq [[figures cases] oracle-cases]
    (is (= (set (keys figures)) (set (keys cases))) "every figure has a case")
    (doseq [[id [params states]] cases]
      (when-let [{:keys [wasm max-error]} (ko/deviation (figures id) params states)]
        (is wasm (str id))
        (is (< max-error 1e-4) (str id " " max-error))))))
