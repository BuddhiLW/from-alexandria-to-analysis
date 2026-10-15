(ns alexandria.delian.tie-widgets-test
  (:require [alexandria.delian.tie-widgets :as tw]
            [alexandria.medium.kernel-oracle :as ko]
            [clojure.test :refer [deftest is testing]]
            [emmy.viewer.compile :as vc]))

(defn- fig [f params] {:f f :params params :state [0] :opts {:simplify? false}})

(defn- run [f params] (vec (first (:points (ko/run-kernel (fig f params) params [[0]])))))

(defn- close? [x y tol] (< (Math/abs (- (double x) (double y))) tol))

(deftest the-widgets-ship-raster-kernels-only
  (doseq [[nm build n] [["menaechmus" tw/menaechmus 5] ["archytas" tw/archytas 8] ["mesolabe" tw/mesolabe 2]]]
    (let [form (binding [vc/*backend* :js] (build))
          ks (tw/kernel-forms form)]
      (is (= n (count ks)) nm)
      (is (every? #(re-find #"WebAssembly" %) ks) nm))))

(defn- blocks [out n] (partition n out))

(deftest menaechmus-tie-round-trips
  (when @ko/node?
    (doseq [a [0.6 1.0 1.4 2.2]]
      (let [x (* a (Math/cbrt 2)) v (* 2 a a a)
            [from-a from-x from-v] (blocks (run tw/menaechmus-tie [a x v]) 3)]
        (testing (str "a = " a)
          (is (every? true? (map #(close? %1 %2 1e-9) from-a [a x v])) "from a")
          (is (every? true? (map #(close? %1 %2 1e-9) from-x [a x v])) "from x")
          (is (every? true? (map #(close? %1 %2 1e-9) from-v [a x v])) "from V"))))
    (testing "a source past its slider is held at the edge"
      (let [[from-a] (blocks (run tw/menaechmus-tie [5.0 1.0 1.0]) 3)]
        (is (close? 2.2 (first from-a) 1e-12))))))

(deftest archytas-tie-round-trips
  (when @ko/node?
    (doseq [b [0.7 1.0 1.6]]
      (let [m (Math/cbrt (* 2 b b)) k (Math/sqrt (* 2 m))
            bs (blocks (run tw/archytas-tie [b m k]) 3)]
        (doseq [blk bs]
          (is (every? true? (map #(close? %1 %2 1e-9) blk [b m k])) (str "AB = " b)))))))

(deftest mesolabe-tie-round-trips
  (when @ko/node?
    (doseq [s [0.5 0.8 1.0]]
      (let [st [s (* 2 s s s) (* 2 s) (* 2 s s)]
            bs (blocks (run tw/mesolabe-tie st) 4)]
        (doseq [blk bs]
          (is (every? true? (map #(close? %1 %2 1e-9) blk st)) (str "s = " s)))))
    (testing "the letters lie on one line and the ratios agree"
      (let [v (run tw/mesolabe-letters [0.8])
            r #(nth v (tw/s-index %))]
        (is (close? (r :r1) (r :r2) 1e-12))
        (is (close? (r :r2) (r :r3) 1e-12))
        (let [[ax ay bx by gx gy dx dy] (subvec v 0 8)
              sl (fn [x0 y0 x1 y1] (/ (- y1 y0) (- x1 x0)))]
          (is (close? (sl ax ay bx by) (sl bx by gx gy) 1e-12))
          (is (close? (sl bx by gx gy) (sl gx gy dx dy) 1e-12)))))))
