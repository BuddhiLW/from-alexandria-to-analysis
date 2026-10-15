(ns alexandria.kepler.mysterium-widget-test
  "Kepler's nest, tied: each source re-solves the six radii so that every
   neighbouring pair keeps its solid's r/R; the tie is idempotent and keeps
   every radius in its slider range; the readout's Copernican radii follow
   Kepler's table; the chain is graded proved; every function compiles to a
   raster wasm kernel."
  (:require [alexandria.kepler.mysterium :as my]
            [alexandria.kepler.mysterium-widget :as mw]
            [alexandria.raster :as raster]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]))

(defn- close? [a b tol]
  (<= (Math/abs (- (double a) (double b))) (* tol (max 1.0 (Math/abs (double a)) (Math/abs (double b))))))

(def start (mapv (mw/init) mw/tied))

(defn- nested? [rs]
  (every? true? (map-indexed (fn [j [_ id _]] (close? (/ (rs (inc j)) (rs j)) (my/ratio-value id) 1e-12))
                             my/shells)))

(defn- in-range? [rs]
  (every? (fn [[k x]] (let [[lo hi] (mw/ranges k)] (<= (- lo 1e-12) x (+ hi 1e-12)))) (map vector mw/tied rs)))

(deftest init-is-nested
  (is (nested? start))
  (is (in-range? start))
  (is (close? (start 0) 0.9 1e-12)))

(deftest each-source-re-solves-the-others
  (doseq [[i k] (map-indexed vector mw/tied)
          :let [[lo hi] (mw/ranges k)
                v (+ lo (* 0.3 (- hi lo)))
                s ((mw/solve (assoc start i v)) k)]]
    (testing (name k)
      (is (close? (s i) v 1e-12) "the source keeps its value")
      (is (nested? s))
      (is (in-range? s)))))

(deftest the-tie-clamps-at-the-sliders-edges
  (doseq [[i k] (map-indexed vector mw/tied)
          v [-1.0 0.0 5.0]
          :let [s ((mw/solve (assoc start i v)) k)]]
    (is (nested? s) (str k "=" v))
    (is (in-range? s) (str k "=" v))))

(deftest the-tie-is-idempotent
  (doseq [[src s] (mw/solve start)]
    (is (every? true? (map #(close? %1 %2 1e-12) s start)) (str src))))

(deftest readout-follows-kepler-and-copernicus
  (let [v (vec (binding [raster/*simplify?* false] (raster/value (fn [rs] ((mw/readout rs) 0)) 1.0)))
        ix mw/readout-index]
    (is (every? true? (map #(close? (v (+ (:kepler ix) %)) (mw/chain-value %) 1e-12) (range 6))))
    (is (close? (v (+ (:copernicus ix) 1)) 0.635 1e-12) "Jupiter at 635/1000 of Saturn")
    (is (close? (v (+ (:copernicus-ratio ix) 1)) 0.333 1e-12))
    (is (close? (v (+ (:ratio ix) 1)) (/ 1.0 3) 1e-12) "tetrahedron r/R = 1/3")
    (is (close? (v (:mercury-mid ix)) (* (mw/chain-value 4) (Math/sqrt 0.5)) 1e-12) "Mercury on the midsphere")
    (doseq [[j {:keys [computed copernicus]}] (map-indexed vector my/kepler-table)]
      (is (< (Math/abs (- (* 1000 (v (+ (:ratio ix) j))) computed)) 0.5) "Kepler's computed column")
      (is (close? (v (+ (:miss ix) j)) (/ (- (v (+ (:ratio ix) j)) (/ copernicus 1000.0)) (/ copernicus 1000.0)) 1e-9)))))

(deftest the-chain-is-proved
  (let [gs (my/chain-grades)]
    (is (= 10 (count gs)))
    (is (every? #(= :grade/proved (:grade %)) gs))))

(deftest unit-solids-are-on-the-unit-sphere
  (doseq [id [:cube :tetrahedron :dodecahedron :icosahedron :octahedron]
          p (:vertices (mw/unit-solid id))]
    (is (close? (Math/sqrt (reduce + (map * p p))) 1.0 1e-12) (str id))))

(defn- kernel-forms [form]
  (let [acc (volatile! [])]
    (walk/postwalk (fn [x] (when (and (seq? x) (= 'js/Function. (first x)) (= "fb" (second x))) (vswap! acc conj (nth x 2))) x) form)
    @acc))

(deftest every-function-compiles-to-a-raster-kernel
  (let [ks (kernel-forms (binding [vc/*backend* :js] (mw/widget)))]
    (is (= 2 (count (filter #(str/includes? % "new WebAssembly.Instance") (take 2 ks)))) "tie and readout")
    (is (<= 14 (count ks)) "tie, readout, 6 spheres, 6 + 5 + 1 rings")
    (doseq [g ks] (is (str/includes? g "new WebAssembly.Instance")))))
