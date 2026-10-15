(ns alexandria.playgrounds-test
  (:require [alexandria.fermat.playgrounds :as p]
            [alexandria.raster :as raster]
            [clojure.test :refer [deftest is]]))

(deftest normal-circle-and-cubic
  (let [f (fn [p c h d v] ((p/normal-values p c h d v) nil))]
    (let [[_ target residual] (raster/value f 2 -0.5 0 1 1.5)]
      (is (< (Math/abs (- target 1.5)) 1e-6))
      (is (< (Math/abs residual) 1e-6)))
    (let [[_ target residual] (raster/value f 2 -0.4 0.12 1 1.78)]
      (is (< (Math/abs (- target 1.78)) 1e-6))
      (is (< (Math/abs residual) 1e-6)))))

(deftest adequality-and-loci
  (is (< (Math/abs (nth (raster/value (fn [k b a ex] ((p/adequate-values k b a ex) nil)) 0 4 2 0.2) 3)) 1e-5))
  (is (= -8.0 (first (raster/value (fn [al be ga de ep ze]
                                     ((p/locus-values al be ga de ep ze 1 0.5) nil))
                                   1 0 2 0 0 -2.25)))))