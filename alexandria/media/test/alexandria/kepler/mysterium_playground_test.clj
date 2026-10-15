(ns alexandria.kepler.mysterium-playground-test
  "The Mysterium playground's kernels by raster on the JVM: the readout of a
   nest for any order of the solids, the inverse (Saturn's radius back from
   a dragged planet), and the verdict: Kepler's order is the best of 120."
  (:require [alexandria.kepler.mysterium :as my]
            [alexandria.kepler.mysterium-playground :as p]
            [alexandria.raster :as raster]
            [clojure.test :refer [deftest is testing]]))

(defn- close? [a b tol] (< (Math/abs (- (double a) (double b))) tol))

(deftest readout
  (testing "Kepler's order: the radii are Saturn's times the exact chain, the misses Kepler's table"
    (let [v (p/readout 0.9 p/kepler-order false)
          {:keys [radius miss rms]} p/readout-index]
      (doseq [i (range 6)]
        (is (close? (nth v (+ radius i)) (* 0.9 (raster/value (fn [] (my/chain i)))) 1e-12)))
      (doseq [[j {:keys [miss] :as row}] (map-indexed vector (my/comparison))]
        (is (close? (nth v (+ (:miss p/readout-index) j)) (/ (- (:value row) (:copernicus row)) (:copernicus row)) 1e-9)))
      (is (close? (nth v rms) 0.1013 1e-3))))
  (testing "Mercury on the midsphere: the last miss drops from -20% to -2%"
    (let [v (p/readout 0.9 p/kepler-order true)]
      (is (close? (nth v (+ (:miss p/readout-index) 4)) (/ (- 707.1067811865 723) 723) 1e-6)))))

(deftest inverse
  (testing "forward then inverse: Saturn's radius back from any planet"
    (let [v (p/readout 0.7 [:tetrahedron :cube :dodecahedron :icosahedron :octahedron] false)]
      (doseq [i (range 6)]
        (let [r (nth v i) k (nth v (+ (:chain p/readout-index) i))
              rs (first (raster/value (fn [a b] ((p/saturn-of a b) 0)) r k))]
          (is (close? rs 0.7 1e-12)))))))

(deftest verdict
  (testing "Kepler's order ranks first of the 120; swapping cube and tetrahedron does not"
    (is (= 120 (count p/orders)))
    (is (= 1 (first (p/rank p/kepler-order false))))
    (is (= 1 (first (p/rank p/kepler-order true))))
    (is (< 1 (first (p/rank [:tetrahedron :cube :dodecahedron :icosahedron :octahedron] false))))
    (testing "the dual pair dodecahedron | icosahedron share r/R: swapping them ties"
      (is (= 1 (first (p/rank [:cube :tetrahedron :icosahedron :dodecahedron :octahedron] false)))))))

(deftest playground-builds
  (is (some? (p/playground))))
