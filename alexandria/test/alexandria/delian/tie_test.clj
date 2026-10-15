(ns alexandria.delian.tie-test
  (:require [alexandria.delian.tie :as t]
            [alexandria.raster :as raster]
            [clojure.test :refer [deftest is testing]]))

(deftest every-tie-step-is-proved
  (doseq [[section gs] (t/grades)
          {:keys [label grade]} gs]
    (is (= :grade/proved grade) (str section ": " label)))
  (is (= [12 6 7] (map (comp count (t/grades)) [:menaechmus :archytas :mesolabe]))))

(deftest emmy-derives-the-cube-from-the-parabolas
  (let [d @t/derivation]
    (is (= 0 (:remainder d)) "x divides the eliminated polynomial")
    (is (= 7 (count (mapcat :eqs (t/equations)))) "the page shows every step as an equation")))

(defn- close? [x y] (< (Math/abs (- (double x) (double y))) 1e-12))

(deftest the-maps-run-on-raster
  (testing "a -> x -> V and back, compiled by alexandria.raster"
    (doseq [a [0.6 1.0 1.7 2.2]]
      (let [x (raster/value t/x-of-a a)
            v (raster/value t/v-of-a a)]
        (is (close? x (* a (Math/cbrt 2))))
        (is (close? v (* x x x)))
        (is (close? a (raster/value t/a-of-x x)))
        (is (close? a (raster/value t/a-of-v v)))
        (is (close? x (raster/value t/x-of-v v)))))))
