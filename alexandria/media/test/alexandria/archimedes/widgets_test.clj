(ns alexandria.archimedes.widgets-test
  "The tied widgets of the Archimedes and Eudoxus notebooks: each source
   re-solves the state by the equations (raster on the JVM, the same Emmy
   functions the browser kernel runs), round trips land back, and every
   widget builds with wasm kernels."
  (:require [alexandria.archimedes.widgets :as w]
            [alexandria.eudoxus.widgets :as ew]
            [alexandria.medium.tied :as tie]
            [clojure.test :refer [deftest is testing]]))

(defn- close? [a b tol] (<= (Math/abs (- (double a) (double b))) tol))
(defn- st [ks m] (mapv m ks))

(deftest circle-tie
  (let [s0 (st w/circle-keys w/circle-start)
        solve (fn [s k] (tie/solve w/circle-keys w/circle-sources s k))]
    (testing "n = 96: Archimedes' bracket"
      (let [[_ d p P] (solve s0 :n)]
        (is (< (/ 223.0 71) (/ p d) Math/PI (/ P d) (/ 22.0 7)))))
    (testing "eps -> n -> eps round trip (d held)"
      (let [s1 (solve (assoc s0 4 0.01) :eps)]
        (is (close? 1 (nth s1 1) 1e-12))
        (is (close? 0.01 (nth s1 4) 1e-9))
        (is (close? 0.01 (nth (solve s1 :n) 4) 1e-9))))
    (testing "p moves: n held, d back, then forward lands on p"
      (let [s1 (solve (assoc s0 2 4.0) :p)]
        (is (= 96.0 (double (first s1))))
        (is (close? 4.0 (nth s1 2) 1e-9))
        (is (close? (* (nth s1 1) 96 (Math/tan (/ Math/PI 96))) (nth s1 3) 1e-9))))
    (testing "P moves: p and d back"
      (let [s1 (solve (assoc s0 3 6.4) :P)]
        (is (close? 6.4 (nth s1 3) 1e-9))))))

(deftest parabola-tie
  (let [s0 (st w/parabola-keys w/parabola-start)
        solve (fn [s k] (tie/solve w/parabola-keys w/parabola-sources s k))]
    (testing "k moves: S + R = 4/3 T"
      (doseq [k [0 1 4 8]]
        (let [[_ T _ S R] (solve (assoc s0 2 k) :k)]
          (is (close? (* 4/3 T) (+ S R) 1e-12)))))
    (testing "T moves: a = T^(1/3)"
      (let [[a T] (solve (assoc s0 1 0.512) :T)]
        (is (close? 0.8 a 1e-12)) (is (close? 0.512 T 1e-12))))
    (testing "R moves: k back exactly"
      (let [R (nth (solve (assoc s0 2 5) :k) 4)
            s1 (solve (assoc s0 4 R) :R)]
        (is (close? 5 (nth s1 2) 1e-9))))))

(deftest sphere-tie
  (let [s0 (st w/sphere-keys w/sphere-start)
        solve (fn [s k] (tie/solve w/sphere-keys w/sphere-sources s k))]
    (doseq [[k i v] [[:V 1 2.0] [:C 2 3.0] [:K 3 0.5] [:r 0 1.2]]]
      (let [[_ V C K :as s] (solve (assoc s0 i v) k)]
        (is (close? v (nth s i) 1e-9) k)
        (is (close? (/ 2 3.0) (/ V C) 1e-12) k)
        (is (close? 4 (/ V K) 1e-12) k)))))

(deftest eudoxus-ties
  (let [s0 (st ew/def5-keys ew/def5-start)
        solve (fn [s k] (tie/solve ew/def5-keys ew/def5-sources s k))]
    (testing "D follows C; C follows D"
      (let [[A B C D] (solve (assoc s0 2 2.5) :C)]
        (is (close? (/ C D) (/ A B) 1e-12)))
      (let [[A B C D] (solve (assoc s0 3 3.3) :D)]
        (is (close? 3.3 D 1e-12)) (is (close? (/ C D) (/ A B) 1e-12)))))
  (let [s0 (st ew/xii2-keys ew/xii2-start)
        solve (fn [s k] (tie/solve ew/xii2-keys ew/xii2-sources s k))]
    (testing "k -> eps -> k"
      (doseq [k [2 4 7]]
        (let [[_ eps] (solve (assoc s0 0 k) :k)]
          (is (close? k (first (solve [0 eps] :eps)) 1e-6) k))))))

(defn- wasm-count [form]
  (count (re-seq #"WebAssembly" (pr-str form))))

(deftest widgets-compile-to-wasm
  (doseq [[nm f] {:circle w/circle :parabola w/parabola :sphere w/sphere :def-5 ew/def-5 :xii-2 ew/xii-2}]
    (let [form (f)]
      (is (pos? (wasm-count form)) nm))))
