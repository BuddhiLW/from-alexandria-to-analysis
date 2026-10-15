(ns alexandria.kepler.orbit-widget-test
  "Kepler's tied orbit: the tie re-solves (a, T, e, l) by T^2 = a^3 and
   l = a (1 - e^2) for each source and keeps every key in its slider range;
   it is idempotent on a tied state; the browser's RK4 step kernel (run here
   by raster on the JVM, the same Emmy function) sweeps equal areas in
   equal times and closes the orbit after T; the start kernel's speed puts
   the RK4 points on rho = l / (1 - e cos theta); every function compiles to
   a raster wasm kernel."
  (:require [alexandria.kepler.orbit-widget :as ow]
            [alexandria.medium.tie :as tie]
            [alexandria.raster :as raster]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]))

(defn- close? [a b tol]
  (<= (Math/abs (- (double a) (double b))) (* tol (max 1.0 (Math/abs (double a)) (Math/abs (double b))))))

(defn- tie [state] (ow/solve state))

(defn- tied? [[a T ecc l]]
  (and (close? (* T T) (* a a a) 1e-9)
       (close? l (* a (- 1 (* ecc ecc))) 1e-9)))

(defn- in-range? [state]
  (every? (fn [[k x]] (let [[lo hi] (ow/ranges k)] (<= (- lo 1e-9) x (+ hi 1e-9))))
          (map vector ow/tied state)))

(def start (mapv (ow/init) ow/tied))

(deftest init-is-tied
  (is (tied? start))
  (is (in-range? start))
  (is (close? (start 1) 1.8805 1e-4) "Mars: T = 1.5235^1.5 years"))

(deftest each-source-re-solves-the-others
  (testing "a moves: e fixed, T = a^1.5"
    (let [[a T ecc :as s] ((tie (assoc start 0 2.0)) :a)]
      (is (tied? s)) (is (close? a 2.0 1e-12)) (is (close? ecc (start 2) 1e-12))
      (is (close? T (Math/pow 2 1.5) 1e-12))))
  (testing "T moves: a = T^(2/3)"
    (let [[a T :as s] ((tie (assoc start 1 8.0)) :T)]
      (is (tied? s)) (is (close? T 8.0 1e-9)) (is (close? a 4.0 1e-9))))
  (testing "e moves: a and T fixed, l follows"
    (let [[a T ecc l :as s] ((tie (assoc start 2 0.6)) :e)]
      (is (tied? s)) (is (close? ecc 0.6 1e-12)) (is (close? a (start 0) 1e-12))
      (is (close? l (* (start 0) 0.64) 1e-12))))
  (testing "l moves: e fixed, a = l / (1 - e^2)"
    (let [[a _ ecc l :as s] ((tie (assoc start 3 1.0)) :l)]
      (is (tied? s)) (is (close? l 1.0 1e-12)) (is (close? ecc (start 2) 1e-12))
      (is (close? a (/ 1.0 (- 1 (* ecc ecc))) 1e-12)))))

(deftest the-tie-clamps-at-the-sliders-edges
  (doseq [[src i v] [[:a 0 4.2] [:a 0 0.3] [:T 1 8.6] [:T 1 0.17] [:e 2 0.95] [:e 2 0.0]
                     [:l 3 4.2] [:l 3 0.02]]
          base [start ((tie (assoc start 2 0.9)) :e)]
          :let [s ((tie (assoc base i v)) src)]]
    (is (in-range? s) (str src "=" v " -> " s))
    (is (tied? s) (str src "=" v " -> " s))))

(deftest the-tie-is-idempotent
  (doseq [[src s] (tie start)]
    (is (every? true? (map #(close? %1 %2 1e-9) s start)) (str src " " s))))

(defn- rk4-run
  "The browser's loop, on the JVM: the start kernel, then n*per calls of
   the step kernel (compiled unsimplified, as emmy-viewers ships it)."
  [[a T ecc l] n per]
  (binding [raster/*simplify?* false]
    (let [s0 (raster/value (fn [a e l] ((ow/start a e l) 0)) a ecc l)
          dt (/ T (* n per))
          step (fn [s] (raster/value (fn [x y vx vy A] ((ow/step ow/mu dt) [x y vx vy A])) (s 0) (s 1) (s 2) (s 3) (s 4)))]
      (vec (take (inc (* n per)) (iterate step s0))))))

(deftest rk4-sweeps-equal-areas-on-the-focal-conic
  (doseq [st [start ((tie (assoc start 2 0.6)) :e)]
          :let [[a _ ecc l] st
                n 8 per 60
                pts (rk4-run st n per)
                As (map #(nth (pts (* % per)) 4) (range (inc n)))
                areas (map - (rest As) As)
                expected (/ (* Math/PI a a (Math/sqrt (- 1 (* ecc ecc)))) n)]]
    (testing (str "e = " ecc)
      (is (every? #(close? % expected 1e-6) areas) (str areas))
      (is (< (Math/hypot (- ((peek pts) 0) ((pts 0) 0)) (- ((peek pts) 1) ((pts 0) 1))) 1e-4) "closed after T")
      (doseq [[x y] (take-nth 37 pts)
              :let [rho (Math/hypot x y) c (/ x rho)]]
        (is (close? rho (/ l (- 1 (* ecc c))) 1e-6) "on rho = l/(1 - e cos theta)")))))

(defn- kernel-forms [form]
  (let [acc (volatile! [])]
    (walk/postwalk (fn [x] (when (and (seq? x) (= 'js/Function. (first x)) (= "fb" (second x))) (vswap! acc conj (nth x 2))) x) form)
    @acc))

(deftest every-function-compiles-to-a-raster-kernel
  (let [ks (kernel-forms (binding [vc/*backend* :js] (ow/widget)))]
    (is (= 6 (count ks)) "tie, readout, start, step, directrix, orbit curve")
    (doseq [g ks] (is (str/includes? g "new WebAssembly.Instance")))))
