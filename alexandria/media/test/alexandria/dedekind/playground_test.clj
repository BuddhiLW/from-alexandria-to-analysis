(ns alexandria.dedekind.playground-test
  (:require [alexandria.dedekind.playground :as pg]
            [alexandria.medium.kernel :as kernel]
            [clojure.test :refer [deftest is testing]]))

(deftest raster-geometry
  (doseq [[f ps] [[pg/line [1.414 2]] [pg/location [0.5]] [pg/cover [0.5 8]]]]
    (testing (str f)
      (is (every? #(Double/isFinite (double %)) ((apply f ps) [0.2])))
      (let [k (kernel/kernel {:f f :params ps :state [0.2]})]
        (is (string? (:glue k)))
        (is (seq (:fallback k)))))))
(deftest all-mounts
  (doseq [m [pg/cut pg/zigzag pg/heights pg/diagonal pg/measure-zero pg/dirichlet]]
    (is (some? (m)))))
