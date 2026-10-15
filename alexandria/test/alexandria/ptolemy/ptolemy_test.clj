(ns alexandria.ptolemy.ptolemy-test
  "Ptolemy's shelf: the chord table against raster, the lemma and its
   tie, the epicycle flow integrated by RK4."
  (:require [alexandria.ptolemy.chords :as chords]
            [alexandria.ptolemy.epicycle :as ep]
            [alexandria.ptolemy.theorem :as th]
            [alexandria.raster :as raster]
            [alexandria.raster.step :as step]
            [clojure.test :refer [deftest is testing]]))

(defn- close? [a b tol] (< (Math/abs (- (double a) (double b))) tol))

(deftest chord-table
  (testing "Ptolemy's values within 1 second of 120 sin(a/2)"
    (doseq [{:keys [arc error-seconds]} (chords/table)]
      (is (< (Math/abs error-seconds) 1.0) (str arc))))
  (testing "sexagesimal round trip"
    (is (= "70ᵖ 32′ 3″" (chords/sexagesimal-str (chords/sexagesimal [70 32 3])))))
  (testing "every grade passes"
    (is (every? #{:grade/proved :grade/numeric} (map :grade (chords/graded))))))

(deftest constructions
  (testing "half arc: GD^2 = AG.GZ and GD = crd(beta/2)"
    (let [[_ _ _ _ _ _ _ _ GD GZ half-diff AGGZ GD2 crd] (raster/value (fn [b] ((chords/half-figure 60) [b])) 72)]
      (is (close? AGGZ GD2 1e-9)) (is (close? GD crd 1e-9)) (is (close? GZ half-diff 1e-9))
      (is (close? GD 37.0820393 1e-6))))
  (testing "sum: AG = crd(alpha + beta) from Ptolemy's lemma"
    (let [out (raster/value (fn [a b] ((chords/sum-figure 60) [a b])) 72 60)]
      (is (close? (nth out 13) (nth out 14) 1e-9))
      (is (close? (nth out 14) (* 120 (Math/sin (Math/toRadians 66))) 1e-9)))))

(defn- quad [& xs]
  (binding [raster/*simplify?* false]
    (let [ks (raster/kernels (fn [a b c d e f g h] ((th/quad a b c d e f g h) 0)) 8)
          [a b c d e f g h] (map double xs)]
      (mapv #(% a b c d e f g h) ks))))

(deftest lemma
  (is (every? #{:grade/proved} (map :grade (th/graded))))
  (testing "the tie solves back to the same d from every source"
    (let [state (raster/value (fn [d] (th/chords-of d)) 40)
          out (binding [raster/*simplify?* false] (apply raster/value (fn [& xs] ((apply th/tie xs) 0)) state))]
      (doseq [i (range 4)] (is (close? (nth out (* 4 i)) 40 1e-6)))))
  (testing "the residual is 0 on the circle"
    (is (close? 0 (nth (raster/value (fn [d] ((th/tie-readout d) 0)) 40) 10) 1e-12)))
  (testing "the cases"
    (let [p (fn [deg] [(Math/cos (Math/toRadians deg)) (Math/sin (Math/toRadians deg))])
          [A B C D] (map p [200 80 20 -20])]
      (is (= :cyclic (th/case-of (apply quad (concat A B C D)))))
      (is (= :inside (th/case-of (apply quad (concat A B C [0.3 -0.5])))))
      (is (= :outside (th/case-of (apply quad (concat A B C [1.3 -1.0])))))
      (is (neg? (nth (apply quad (concat A B C [1.3 -1.0])) 8)) "Ptolemy's inequality")
      (is (= :cyclic-crossed (th/case-of (apply quad (concat A C B D))))))))

(deftest epicycle
  (is (every? #{:grade/proved :grade/numeric} (map :grade (ep/graded))))
  (testing "the RK4 step the page compiles: Mars retrogrades near opposition"
    (let [{:keys [R r w1 w2]} (ep/planets :mars)
          dt 0.004
          pts (step/run (ep/flow w1 w2) dt [R 0 r 0 0] 1000)
          rates (map (fn [s] (nth (mapv double ((ep/rate w1 w2) s)) 2)) pts)]
      (is (some neg? rates))
      (is (some pos? rates))))
  (testing "no retrograde when r w2 < R w1"
    (let [pts (step/run (ep/flow 0.5 1) 0.004 [1 0 0.3 0 0] 1000)]
      (is (every? pos? (map (fn [s] (nth (mapv double ((ep/rate 0.5 1) s)) 2)) pts)))))
  (testing "lambda' at the nearest point"
    (let [{:keys [R r w1 w2]} (ep/planets :mars)
          [v] (raster/value (fn [a b c d] ((ep/opposition a b c d) 0)) R r w1 w2)]
      (is (neg? v)))))
