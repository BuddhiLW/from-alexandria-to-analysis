(ns alexandria.cauchy.analysis-scenes-test
  (:require [alexandria.cauchy.analysis :as cauchy]
            [alexandria.cauchy.analysis-scenes]
            [alexandria.cauchy.analysis-view :as view]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.scene :as scene]
            [alexandria.palette :as palette]
            [alexandria.proofs :as proofs]
            [clojure.test :refer [deftest is testing]]))

(def figures (update-vals cauchy/figures (comp figure/->FnFigure :f)))

(def parts
  {:cauchy/continuity (view/continuity-data)
   :cauchy/ivt (view/ivt-data)
   :cauchy/series (view/series-data)
   :cauchy/root-test (view/root-test-data)
   :cauchy/sum-theorem (view/sum-theorem-data)
   :cauchy/derivative (view/derivative-data)
   :cauchy/integral (view/integral-data)
   :cauchy/refinement (view/integral-data)
   :cauchy/fundamental (view/fundamental-data)
   :cauchy/flat (view/flat-data)})

(deftest the-tenths-search-closes-on-the-root
  (let [{:keys [rounds root]} (view/ivt-data)]
    (testing "each round's kept pair brackets the root, and is a tenth of the last"
      (doseq [[r s] (partition 2 1 rounds)]
        (is (<= (:x s) root (:X s)))
        (is (< (Math/abs (- (* 10 (- (:X s) (:x s))) (- (:X r) (:x r)))) 1e-12))))
    (testing "the kept pair has contrary signs"
      (doseq [{:keys [values j]} (butlast rounds)]
        (is (<= (* (nth values j) (nth values (inc j))) 0))))))

(deftest the-chords-tend-to-the-tangent
  (let [{:keys [chords slope]} (view/derivative-data)
        errs (map #(Math/abs (- (:q %) slope)) chords)]
    (is (apply > errs))
    (is (< (last errs) 0.01))))

(deftest every-stage-is-drawn
  (doseq [[id data] parts
          {:keys [stage]} (proofs/steps cauchy/proofs-resource id)
          p [0 0.37 1]]
    (let [h (scene/draw id stage p {:figures figures :palette (palette/palette :deck) :data data})]
      (is (and (vector? h) (not= :text (first h))) (str id " " stage " at " p)))))

(deftest the-abel-figure-is-the-partial-sum
  (testing "at a whole number of terms the figure is S_n"
    (doseq [n [1 3 10] x [0.5 2.0 3.0]]
      (let [[[_ y]] (figure/points (:abel figures) [n] [[x]])]
        (is (< (Math/abs (- y (cauchy/abel-sum n x))) 1e-12)))))
  (testing "it vanishes at pi for every n"
    (is (< (Math/abs (second (first (figure/points (:abel figures) [17.5] [[Math/PI]])))) 1e-12))))

(deftest the-game-delta-keeps-the-graph-in-the-band
  (let [{:keys [a fa rounds]} (view/continuity-data)]
    (doseq [{:keys [eps delta]} rounds
            h (map #(* delta (- (/ % 50.0) 1)) (range 1 100))]
      (is (< (Math/abs (- (* (+ a h) (+ a h)) fa)) (+ eps 1e-12))))))

(deftest the-sums-refine-to-the-integral
  (let [{:keys [levels integral]} (view/integral-data)
        errs (map #(Math/abs (- (:S %) integral)) levels)]
    (is (apply > errs))
    (is (< (last errs) 0.02))))

(deftest the-slider-sets-n-and-the-error-stays
  (let [ctx (fn [n] {:figures figures :palette (palette/palette :deck)
                     :data (view/sum-theorem-data) :controls {:n n}})
        err (fn [n] (let [x (- Math/PI (/ 1.0 n))
                          [[_ y]] (figure/points (:abel figures) [n] [[x]])]
                      (Math/abs (- y (/ x 2)))))
        ns [10 24 cauchy/abel-terms]]
    (testing "the player carries one slider, :n, no further than the kernel's terms"
      (let [v (view/sum-theorem)
            cs (or (get-in v [:nextjournal/value :controls]) (:controls v)
                   (some :controls (tree-seq coll? seq v)))]
        (is (= [:n] (map :id cs)))
        (is (= cauchy/abel-terms (:max (first cs))))))
    (testing "the uniform stage reads it and the readout shows that n"
      (doseq [n ns]
        (is (re-find (re-pattern (str "n = " n ",")) (pr-str (scene/draw :cauchy/sum-theorem :uniform 0.2 (ctx n)))))))
    (testing "the error at x = pi - 1/n does not shrink with n: about 0.6 every time"
      (doseq [n ns] (is (< 0.55 (err n) 0.66) (str "n = " n))))))
