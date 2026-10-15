(ns alexandria.euler.widgets-test
  (:require [alexandria.euler.widgets :as w]
            [alexandria.raster :as raster]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn- close? [a b tol] (< (Math/abs (- (double a) (double b))) tol))

(deftest every-widget-ships-wasm-kernels
  (doseq [[nm f min-k] [[:basel w/basel 4] [:e w/e-limit 6] [:cis w/cis 2] [:base w/base-steps 4]
                        [:polyhedron w/polyhedron 7] [:terms w/terms-steps 12]]]
    (let [frag (f) s (pr-str frag)]
      (testing (name nm)
        (is (<= min-k (count (w/kernel-forms frag))))
        (is (str/includes? s "data-figure"))
        (is (not (str/includes? s "http")) "no remote script in the fragment")))))

(deftest the-kernels-solve-the-ties
  (testing "Basel: S_n, the gap inside its bounds, n from b"
    (let [[S target gap b lower] ((w/basel-readout 10.0) nil)]
      (is (close? S 1.5497677311665408 1e-12))
      (is (close? target (/ (* Math/PI Math/PI) 6) 1e-12))
      (is (< lower gap b))
      (is (close? 25.0 (nth (raster/value (fn [n b] ((w/basel-solve n b) nil)) 3.0 0.04) 2) 1e-9))))
  (testing "e: forward"
    (let [[n o eps] (raster/value (fn [n] ((w/e-forward n) nil)) 4.0)]
      (is (= 4.0 n)) (is (close? o 0.25 1e-15))
      (is (close? eps (- Math/E (Math/pow 1.25 4)) 1e-12))))
  (testing "cis: the three blocks agree on the point"
    (let [x 2.4 o (raster/value (fn [x c s] ((w/cis-solve x c s) nil)) x (Math/cos x) (Math/sin x))]
      (is (close? (o 3) x 1e-9)) (is (close? (o 6) x 1e-9))))
  (testing "base: a = e^k, k = log a"
    (let [[a k a' k'] (raster/value (fn [a k] ((w/base-solve a k) nil)) 10.0 1.0)]
      (is (close? a Math/E 1e-12)) (is (= k 1.0)) (is (= a' 10.0)) (is (close? k' (Math/log 10) 1e-12)))))

(deftest the-cut-solids-keep-euler
  (doseq [{:keys [name counts degree segments points]} @w/cut-data]
    (let [{:keys [V E F]} counts]
      (testing name
        (is (= 2 (+ (- V E) F)))
        (is (= (* 2 E) (count points) (reduce + degree)))
        (is (= (* 3 E) (count segments)) "every vertex cut: E + 2E edges")))))
