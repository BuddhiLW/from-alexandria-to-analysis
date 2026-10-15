(ns alexandria.riemann.playground-test
  (:require [alexandria.riemann.playground :as pg]
            [alexandria.medium.kernel :as kernel]
            [clojure.test :refer [deftest is testing]]))

(deftest four-independent-raster-figures
  (doseq [[id f params] [[:integral pg/integral-curve [0 8]]
                         [:jumps pg/integral-curve [1 8]]
                         [:uniform pg/uniform-curve [0 6]]
                         [:monster pg/monster-curve [13 0.5 7]]]]
    (testing (name id)
      (let [v ((apply f params) [0.2])]
        (is (= 2 (count v)))
        (is (every? #(Double/isFinite (double %)) v)))
      (let [k (kernel/kernel {:f f :params params :state [0.2]})]
        (is (string? (:glue k)))
        (is (seq (:fallback k)))))))

(deftest playgrounds-ship-distinct-kernels
  (doseq [mount [pg/integral pg/jumps pg/uniform pg/monster]]
    (let [v (mount)]
      (is (some? v)))))
