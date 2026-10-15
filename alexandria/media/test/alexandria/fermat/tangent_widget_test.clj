(ns alexandria.fermat.tangent-widget-test
  (:require [alexandria.fermat.tangent-widget :as w]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

;; The widget's kernels ship to the browser as wasm, compiled without
;; simplification (the clamped tie is too deep for Emmy's simplifier, so
;; also for the JVM raster compile); here the same Emmy functions run on
;; numbers, and the e2e reads the wasm results.
(defn- kernel [f] (fn [& xs] ((apply f xs) 0)))
(defn- at-numbers [f & xs] (mapv double (apply f xs)))
(defn- close? [a b] (< (Math/abs (- (double a) (double b))) 1e-9))

(deftest every-key-solves-the-others-by-raster
  ;; Descartes' ellipse p = 2, c = -1/2 (centre at x = 2): the state as typed
  ;; has CD = 1.2, CE = 3, DP = 1.5, and each source re-solves the others
  (let [out (at-numbers (kernel w/tie-kernel) 2 -0.5 1.2 3 1.5)
        block (fn [k] (subvec out (w/source-block k) (+ 5 (w/source-block k))))]
    (testing "d moved: CE = 2 CD (p + c CD) / (p + 2c CD), DP = CD (1 + c) + p/2"
      (is (every? true? (map close? (block :d) [2 -0.5 1.2 4.2 1.6]))))
    (testing "ce moved: CD by the inverse map"
      (is (every? true? (map close? (block :ce) [2 -0.5 1 3 1.5]))))
    (testing "np moved: CD = (DP - p/2) / (1 + c)"
      (is (every? true? (map close? (block :np) [2 -0.5 1 3 1.5]))))
    (testing "p or c moved: CD held"
      (is (every? true? (map close? (block :p) (block :d))))
      (is (every? true? (map close? (block :c) (block :d)))))
    (testing "past the ellipse's centre CE would blow up: CD is held short of it, CE at its top"
      (let [s (subvec (at-numbers (kernel w/tie-kernel) 2 -0.5 1.9 3 1.5) 10 15)]
        (is (close? 8 (nth s 3)) (pr-str s))))))

(deftest the-tie-keeps-every-key-in-range
  (doseq [[p c d ce np] [[3 1.5 2.9 8 6] [0.2 -0.9 3 0.1 0.1] [1 0 1 2 1.5]]
          :let [out (at-numbers (kernel w/tie-kernel) p c d ce np)]
          [k off] w/source-block
          [key v] (map vector w/tied (subvec out off (+ off 5)))
          :let [[lo hi] (w/ranges key)]]
    (is (<= (- lo 1e-9) v (+ hi 1e-9)) (str "source " k ": " key " = " v))))

(deftest descartes-circle-touches-only-at-the-normal
  (let [idx w/readout-index]
    (doseq [[p c] [[1 0] [2 -0.5] [1 0.5]]
            :let [at (fn [off] (at-numbers (kernel w/readout) p c 1 0.6 off))]]
      (is (close? 0 (nth (at 0) (idx :delta))) "centre at P: Δ = 0")
      (is (close? (nth (at 0) (idx :x1)) (nth (at 0) (idx :x2))) "one double root")
      (is (close? 1 (nth (at 0) (idx :x1))) "at x = CD")
      (is (pos? (nth (at 0.5) (idx :delta))) "centre past P: two roots")
      (is (close? (* 4 0.25) (nth (at 0.5) (idx :delta))) "Δ = 4 (v - DP)^2")
      (is (close? (nth (at 0) (idx :ec-cp)) (nth (at 0) (idx :bc2))) "EC.CP = BC^2: the normal"))
    (is (close? 1.5 (nth (at-numbers (kernel w/readout) 2 -0.5 1 0.6 0) (idx :np))) "Descartes' ellipse: DP = 3/2")
    (is (close? 2 (nth (at-numbers (kernel w/readout) 1 0 1 0.6 0) (idx :ce))) "Fermat's parabola: CE = 2 CD")))

(deftest the-loci-kernel-names-each-equation
  (let [q (at-numbers (kernel w/loci-readout) 1 1.5 1 2.5 0.7)
        inv (fn [i] (subvec q (* 7 i) (+ 4 (* 7 i))))]
    (is (= [:line :hyperbola :parabola :circle :ellipse :hyperbola]
           (mapv #(alexandria.fermat.tie/kind (inv %)) (range 6))))
    (doseq [i (range 6)] (is (close? 0 (nth q (+ 6 (* 7 i)))) (str "I on the curve, kind " i)))))

(deftest the-widgets-ship-wasm-kernels
  (doseq [[nm frag n] [["tangent-normal" (w/tangent-normal) 3]
                       ["tangent-normal descartes" (w/tangent-normal w/descartes-init :descartes) 3]
                       ["loci-panel" (w/loci-panel) 9]]]
    (let [ks (w/kernel-forms frag)]
      (is (<= n (count ks)) nm)
      (is (every? #(str/includes? % "WebAssembly") ks) nm))))

(deftest the-section-is-named-with-apollonius-names
  (is (str/starts-with? (w/names :parabola) "παραβολή"))
  (is (str/starts-with? (w/names :ellipse) "ἔλλειψις"))
  (is (str/starts-with? (w/names :hyperbola) "ὑπερβολή")))

(deftest the-derivation-renders
  (is (some? (w/derivation))))
