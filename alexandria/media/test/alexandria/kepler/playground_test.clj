(ns alexandria.kepler.playground-test
  "The Kepler playgrounds' kernels on the JVM: the RK4 flows the browser
   iterates (alexandria.raster.step, the same Emmy step the page compiles),
   and the readouts by raster."
  (:require [alexandria.kepler.playground :as p]
            [alexandria.raster :as raster]
            [alexandria.raster.step :as step]
            [clojure.test :refer [deftest is testing]]))

(defn- close? [a b tol] (< (Math/abs (- (double a) (double b))) tol))

(defn- kv [f & args] (vec (raster/value (fn [& xs] ((apply f xs) 0)) args)))

(defn- kval [f & args] (vec (apply raster/value (fn [& xs] ((apply f xs) 0)) args)))

(defn- orbit [a ecc n pw k]
  (let [[T l q dt] (kval p/sector-setup a ecc n)
        s0 (kval p/sector-start q l)]
    {:T T :l l :pts (step/run (p/sector-flow p/mu pw k) dt s0 (* p/slice n))}))

(defn- areas [pts n]
  (mapv (fn [i] (- ((nth pts (* (inc i) p/slice)) 4) ((nth pts (* i p/slice)) 4))) (range n)))

(defn- rel-spread [xs] (/ (- (apply max xs) (apply min xs)) (apply max xs)))

(deftest equal-areas
  (testing "inverse square: equal areas, the RK4 path on the ellipse, closed after T"
    (let [{:keys [pts l]} (orbit 1.5235 0.3 8 2 0)
          gap (apply max (map (fn [[x y]] (Math/abs (first (kval p/ellipse-gap l 0.3 x y)))) (take-nth 10 pts)))
          [x y] (peek pts)]
      (is (< (rel-spread (areas pts 8)) 1e-6))
      (is (< gap 1e-4))
      (is (close? x (- (* 1.5235 0.7)) 1e-3)) (is (close? y 0 1e-3))))
  (testing "another central force: equal areas survive, the ellipse does not"
    (let [{:keys [pts l]} (orbit 1.5235 0.3 8 2.5 0)
          gap (apply max (map (fn [[x y]] (Math/abs (first (kval p/ellipse-gap l 0.3 x y)))) (take-nth 10 pts)))]
      (is (< (rel-spread (areas pts 8)) 1e-6))
      (is (> gap 1e-2))))
  (testing "drag: the areas shrink"
    (is (> (rel-spread (areas (:pts (orbit 1.5235 0.3 8 2 0.3)) 8)) 1e-2))))

(defn- period [a pw]
  (let [[t x y vx vy dt] (kval p/circle-start a pw)
        pts (step/run (p/circle-flow p/mu pw) dt [t x y vx vy] 260)
        [[t0 _ y0] [t1 _ y1]] (some (fn [[u w]] (when (and (pos? (u 2)) (neg? (w 2))) [u w])) (partition 2 1 (drop 2 pts)))]
    (* 2 (+ t0 (* (- y0) (/ (- t1 t0) (- y1 y0)))))))

(deftest harmonic-law
  (testing "inverse square: T^2/a^3 = 1 from the RK4 period"
    (doseq [[_ a] p/planets]
      (let [T (period a 2)
            [ratio x y] (kval p/law-values a T)]
        (is (close? ratio 1 1e-4))
        (is (close? x y 1e-3)))))
  (testing "force 1/r: the ratio changes from planet to planet"
    (let [r (map #(first (kval p/law-values % (period % 1))) [0.387 9.537])]
      (is (> (Math/abs (- (first r) (second r))) 0.1)))))

(deftest playgrounds-build
  (is (some? (p/sector-playground)))
  (is (some? (p/harmonic-playground))))
