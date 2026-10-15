(ns alexandria.galileo.playground-test
  "The playgrounds' kernels on the JVM: the RK4 flows the browser iterates
   (alexandria.raster.step, the same Emmy step the page compiles), and the
   readouts by raster."
  (:require [alexandria.galileo.playground :as p]
            [alexandria.raster :as raster]
            [alexandria.raster.step :as step]
            [clojure.test :refer [deftest is testing]]))

(defn- close? [a b tol] (< (Math/abs (- (double a) (double b))) tol))

(defn- crossing
  "Linear crossing of coordinate k's level m, coordinate j returned."
  [pts j k m]
  (some (fn [[p q]]
          (when (neg? (* (- (q k) m) (- (p k) m)))
            (+ (p j) (* (- m (p k)) (/ (- (q j) (p j)) (- (q k) (p k)))))))
        (partition 2 1 pts)))

(deftest acceleration-areas
  (testing "Galileo's closed laws, by raster"
    (let [[v s rect & bars] (raster/value (fn [a T] ((p/acceleration-values a T) 0)) 2 4)]
      (is (close? v 8 1e-12)) (is (close? s rect 1e-9))
      (is (every? true? (map #(close? %1 %2 1e-9) bars [1 3 5 7])))))
  (testing "RK4 of the uniform acceleration: distance = rectangle, 1:3:5:7"
    (let [pts (step/run (p/acceleration-flow 2 0) 0.02 [0 0 0] 200)
          q #(nth pts (* 50 %))
          [rect diff _ r2 r3 r4] (raster/value (fn [& xs] ((apply p/acceleration-readout xs) 0))
                                              4 ((q 4) 2) ((q 1) 1) ((q 2) 1) ((q 3) 1) ((q 4) 1))]
      (is (close? rect 16 1e-9)) (is (close? diff 0 1e-9))
      (is (close? r2 3 1e-9)) (is (close? r3 5 1e-9)) (is (close? r4 7 1e-9))))
  (testing "a growing acceleration breaks the theorem"
    (let [pts (step/run (p/acceleration-flow 1 1) 0.02 [0 0 0] 200)
          [_ diff] (raster/value (fn [& xs] ((apply p/acceleration-readout xs) 0))
                                 4 ((last pts) 2) 1 2 3 ((last pts) 1))]
      (is (> (Math/abs diff) 0.1)))))

(deftest inclined-plane
  (let [roll (fn [lift k] (step/run (p/plane-flow lift k) 0.005 [0 0 0] 6000))
        pts (roll 1 0)
        t12 (crossing pts 0 1 12) t3 (crossing pts 0 1 3)]
    (testing "RK4 roll: a quarter of the groove in half the time"
      (is (close? (/ t3 t12) 0.5 1e-4)))
    (testing "the readout kernels"
      (let [s (fn [t] (crossing pts 1 0 t))
            [a ratio deg] (raster/value (fn [& xs] ((apply p/plane-readout xs) 0)) 1 t3 t12)
            [r2 r3 r4] (raster/value (fn [& xs] ((apply p/quarter-ratios xs) 0))
                                     (s (/ t12 4)) (s (/ t12 2)) (s (* 3/4 t12)) (s (* 0.99999 t12)))]
        (is (close? a (* 9.81 (/ 1 (Math/hypot 12 1))) 1e-9))
        (is (close? ratio 0.5 1e-4)) (is (close? r2 4 1e-3)) (is (close? r3 9 1e-3)) (is (close? r4 16 1e-3))
        (is (close? deg 4.7636 1e-3))))
    (testing "resistance breaks the half-time law"
      (let [pts (roll 2 0.8)]
        (is (> (Math/abs (- (/ (crossing pts 0 1 3) (crossing pts 0 1 12)) 0.5)) 0.01))))))

(deftest projectile
  (let [fly (fn [v deg k] (step/run (p/shot-flow k) 0.004 (raster/value (fn [v d] ((p/launch v d) 0)) v deg) 4000))
        R (fn [v deg k] (crossing (fly v deg k) 0 1 0))
        [Rc Hc vg Rcomp] (raster/value (fn [v d] ((p/shot-values v d) 0)) 18 30)]
    (testing "RK4 lands where Galileo's closed form says"
      (is (close? (R 18 30 0) Rc 1e-3)) (is (close? Rc Rcomp 1e-9)) (is (< Rc vg)) (is (pos? Hc)))
    (testing "complementary elevations, equal RK4 ranges; 45 longest"
      (is (close? (R 18 30 0) (R 18 60 0) 1e-3))
      (is (> (R 18 45 0) (R 18 40 0))))
    (testing "every RK4 point lies on the parabola"
      (is (every? (fn [[x y]] (close? 0 (first (raster/value (fn [v d x y] ((p/parabola-gap v d x y) 0)) 18 35 x y)) 1e-6))
                  (take-while #(>= (% 1) 0) (fly 18 35 0)))))
    (testing "air resistance: complements differ"
      (is (> (Math/abs (- (R 18 30 0.15) (R 18 60 0.15))) 0.1)))))

(deftest playgrounds-build
  (doseq [f [p/acceleration-playground p/plane-playground p/projectile-playground]]
    (is (some? (f)))))
