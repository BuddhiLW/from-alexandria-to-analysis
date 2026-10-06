(ns alexandria.delian.duplication-widgets-test
  (:require [alexandria.delian.duplication :as d]
            [alexandria.delian.duplication-widgets :as w]
            [alexandria.medium.kernel-oracle :as ko]
            [clojure.test :refer [deftest is testing]]
            [emmy.viewer.compile :as vc]))

(defn- fig
  "A widget function of the sliders as a figure for the kernel oracle."
  [f params state & {:keys [scalar?]}]
  {:f (if scalar? (fn [& ps] (let [g (apply f ps)] (fn [[t]] (g t)))) f)
   :params params :state state :opts {:simplify? false}})

(deftest the-widgets-compile-every-kernel-on-raster
  (testing "built under a :js binding, the widgets still ship raster kernels only"
    (let [a (binding [vc/*backend* :js] (w/archytas))
          m (binding [vc/*backend* :js] (w/menaechmus))]
      (is (= 7 (count (w/kernel-forms a))) "circle, cylinder, ring, its curve, cone, AB produced, letters")
      (is (= 4 (count (w/kernel-forms m))) "two parabolas, hyperbola, letters")
      (is (every? #(re-find #"WebAssembly" %) (concat (w/kernel-forms a) (w/kernel-forms m)))))))

(deftest the-sentences-match-the-steps
  (is (= 6 (count w/archytas-sentences)))
  (is (= 5 (count w/menaechmus-sentences))))

(defn- close? [x y tol] (< (Math/abs (- x y)) tol))

(deftest archytas-letters-meet-on-the-three-surfaces
  (when @ko/node?
    (doseq [b [0.8 1.0 1.3]]
      (let [a w/archytas-a
            out (first (:points (ko/run-kernel (fig w/archytas-letters [a b] [0]) [a b] [[0]])))
            at #(subvec (vec out) (w/archytas-index %) (+ 3 (w/archytas-index %)))
            n #(nth out (w/archytas-index %))
            [x y z] (at "K")]
        (testing (str "AB = " b)
          (is (close? (+ (* x x) (* y y)) (* a x) 1e-9) "K on the half-cylinder")
          (is (close? (+ (* x x) (* y y) (* z z)) (* a (Math/sqrt (+ (* x x) (* y y)))) 1e-9) "K on the ring")
          (is (close? (* b (Math/sqrt (+ (* x x) (* y y) (* z z)))) (* a x) 1e-9) "K on the cone (AB.AK = AD.x)")
          (is (close? (n :ai3) (n :ad-ab2) 1e-9) "AI^3 = AD.AB^2")
          (is (close? (/ (n :ab) (n :ai)) (/ (n :ai) (n :ak)) 1e-9))
          (is (close? (/ (n :ai) (n :ak)) (/ (n :ak) (n :ad)) 1e-9)))))
    (testing "at AB = 1 the kernel agrees with raster's Brent root on the JVM"
      (let [{:keys [m x y z]} (d/archytas-numbers 2 1)
            out (first (:points (ko/run-kernel (fig w/archytas-letters [2 1.0] [0]) [2 1.0] [[0]])))]
        (is (close? m (nth out (w/archytas-index :ai)) 1e-9))
        (is (every? true? (map #(close? %1 %2 1e-9) [x y z] (subvec (vec out) 6 9))))))))

(deftest every-curve-kernel-matches-its-emmy-oracle
  (doseq [[id f params states scalar?]
          [[:cylinder w/archytas-cylinder [2] [[0.0 0.0] [1.0 1.5] [3.0 2.0]] false]
           [:torus w/archytas-torus [2] [[0.0 0.0] [0.7 0.4] [1.5 1.5]] false]
           [:curve w/archytas-curve [2] [[0.0] [0.6] [1.5]] true]
           [:cone w/archytas-cone [2 1.0] [[0.0 0.0] [1.0 0.5] [3.0 1.0]] false]
           [:generator w/archytas-generator [2 1.2] [[0.0] [1.0]] true]
           [:menaechmus w/menaechmus-letters [1.4] [[0]] false]
           [:parabola-y w/menaechmus-parabola-y [1.4] [[-2.0] [0.5] [3.0]] true]
           [:hyperbola w/menaechmus-hyperbola [1.4] [[0.3] [1.0] [4.0]] true]]]
    (when-let [{:keys [wasm max-error]} (ko/deviation (fig f params (first states) :scalar? scalar?) params states)]
      (is wasm (str id))
      ;; raster's sin/cos are good to ~2e-5 (cone-widget-test allows 3e-4)
      (is (< max-error 1e-4) (str id ": " max-error)))))

(deftest menaechmus-meeting-point-is-on-all-three-curves
  (when @ko/node?
    (doseq [a [0.6 1.4 2.2]]
      (let [out (first (:points (ko/run-kernel (fig w/menaechmus-letters [a] [0]) [a] [[0]])))
            [x y] out]
        (is (close? (* x x) (* a y) 1e-9) "x^2 = a y")
        (is (close? (* y y) (* 2 a x) 1e-9) "y^2 = 2a x")
        (is (close? (* x y) (* 2 a a) 1e-9) "x y = 2a^2")
        (is (close? (/ x a) (Math/cbrt 2) 1e-12) "x = a cbrt 2")))))
