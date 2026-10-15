(ns alexandria.regiomontanus.regiomontanus-test
  "From chord to sine: the chord/jya tie, Aryabhata's table by
   differences, Regiomontanus' law of sines and the five cases."
  (:require [alexandria.raster :as raster]
            [alexandria.regiomontanus.sines :as s]
            [alexandria.regiomontanus.triangles :as t]
            [clojure.test :refer [deftest is testing]]))

(defn- close? [a b tol] (< (Math/abs (- (double a) (double b))) tol))

(deftest chord-is-twice-the-sine
  (let [[arc crd jya sn] (raster/value (fn [a] (s/of-arc a)) 60)]
    (is (= 60.0 arc))
    (is (close? crd 60 1e-9) "crd 60 deg is the radius, 60 parts")
    (is (close? jya 1719 1e-9) "jya 30 deg is half of 3438")
    (is (close? sn 0.5 1e-12)))
  (testing "every source of the tie solves back to the same state"
    (is (< (s/tie-round-trip) 1e-9)))
  (testing "a dragged point gives its arc"
    (is (close? 80 (first (raster/value (fn [x y] ((s/arc-of-point x y) 0)) (Math/cos (Math/toRadians 40)) (- (Math/sin (Math/toRadians 40))))) 1e-9))))

(deftest aryabhata-table
  (let [rows (s/table)]
    (is (= 24 (count rows)))
    (is (= 3438 (:printed (peek rows))) "the table ends at the radius")
    (is (every? #(< (Math/abs (:printed-error %)) 1.5) rows) "printed within 1.5 minutes")
    (is (every? #(< (Math/abs (:exact-error %)) 1e-6) rows) "the exact divisor reproduces 3438 sin")
    (is (= [225.0 449.0] (mapv :rule (take 2 rows))) "his rule starts 225, 449 as printed")
    (is (> (Math/abs (:rule-error (peek rows))) 50) "his divisor 225 drifts to about a minute per step near 90 deg"))
  (is (every? #{:grade/proved :grade/numeric} (map :grade (s/graded)))))

(deftest law-of-sines
  (let [o (t/value (fn [& xs] ((apply t/readout xs) 0)) 10 140 200 1.7)
        g (fn [k] (nth o (t/readout-index k)))]
    (doseq [k [:ra :rb :rc]] (is (close? (g k) 3.4 1e-9)))
    (is (close? (g :sum) 180 1e-9)))
  (testing "the tie, from a side: a back to A on the branch of A"
    (let [st (t/initial-state)
          out (apply t/value (fn [& xs] ((apply t/tie xs) 0)) (map (assoc st :a 1.2) t/tied))
          n (count t/tied)
          [A B C a] (subvec out (* 3 n) (+ (* 3 n) 4))]
      (is (close? a 1.2 1e-9))
      (is (close? B (:B st) 1e-9) "B held")
      (is (close? (+ A B C) 180 1e-9))))
  (testing "the drag reads the angles back from the vertices"
    (let [st (t/initial-state)
          [tA tB tC] (t/value (fn [p a c] (t/positions p a c)) (:phi st) (:A st) (:C st))
          [A B C] (t/value (fn [x y z] ((t/drag x y z) 0)) tA tB tC)]
      (is (close? A (:A st) 1e-9)) (is (close? B (:B st) 1e-9)) (is (close? C (:C st) 1e-9)))))

(deftest five-cases
  (is (= :one (t/case-name :SSS (t/solve :SSS [4 5 6]))))
  (is (= :none (t/case-name :SSS (t/solve :SSS [1 2 5]))) "no triangle: 1 + 2 < 5")
  (is (= :none (t/case-name :ASA (t/solve :ASA [100 5 90]))) "angles past 180")
  (is (= :two (t/case-name :SSA (t/solve :SSA [4 5 40]))))
  (is (= :one (t/case-name :SSA (t/solve :SSA [6 5 40]))))
  (is (= :none (t/case-name :SSA (t/solve :SSA [2 5 40]))) "b sin A > a")
  (is (= :none (t/case-name :SSA (t/solve :SSA [4 5 120]))) "obtuse A needs a > b")
  (let [{:keys [c c2 B B2]} (t/solve :SSA [4 5 40])]
    (is (close? (+ B B2) 180 1e-9))
    (is (> c c2)))
  (is (every? #{:grade/proved :grade/numeric} (map :grade (t/graded)))))
