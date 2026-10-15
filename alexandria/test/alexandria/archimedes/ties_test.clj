(ns alexandria.archimedes.ties-test
  "The Archimedes ties: every derivation step grades :grade/proved, and the
   numeric inverses (Newton on Emmy's D, run by raster) land back on the
   value they came from."
  (:require [alexandria.archimedes.ties :as t]
            [alexandria.eudoxus.ties :as et]
            [alexandria.raster :as raster]
            [clojure.test :refer [deftest is testing]]))

(defn- close? [a b tol] (<= (Math/abs (- (double a) (double b))) tol))

(deftest every-grade-proved
  (doseq [[nm gs] {:circle (t/circle-grades) :parabola (t/parabola-grades)
                   :sphere (t/sphere-grades) :eudoxus (et/grades)}]
    (testing (name nm)
      (is (seq gs))
      (doseq [{:keys [label grade]} gs]
        (is (= :grade/proved grade) label)))))

(deftest archimedes-96-gon
  (testing "the 96-gon brackets pi inside Archimedes' 3 10/71 and 3 1/7"
    (let [[p P] (raster/value (fn [n] [(t/inscribed-perimeter n 1) (t/circumscribed-perimeter n 1)]) 96)]
      (is (< (/ 223.0 71) p Math/PI P (/ 22.0 7))))))

(defn- solve
  "Newton by raster on the JVM: start, then t/newton-steps steps, each a
   kernel call."
  [start step v]
  (nth (iterate #(raster/value step % v) (raster/value start v)) t/newton-steps))

(deftest newton-inverses
  (testing "n from the error bound: back to n for n >= 6"
    (doseq [n [6 12 24 48 96 200 1000]]
      (let [eps (raster/value t/gap n)]
        (is (close? n (solve t/gap-start t/gap-step eps) (* 1e-9 n n)) n))))
  (testing "k from the remainder, exact form"
    (doseq [k [0 1 2 5 9]]
      (let [R (raster/value (fn [T k] (t/remainder T k)) 0.7 k)]
        (is (close? k (raster/value (fn [T R] (t/k-of-remainder T R)) 0.7 R) 1e-9)))))
  (testing "XII.2: n from the leftover"
    (doseq [k [2 3 5 8]]
      (let [n (Math/pow 2 k)
            eps (raster/value et/leftover n)]
        (is (close? n (solve et/leftover-start et/leftover-step eps) (* 1e-6 n)) k)))))
