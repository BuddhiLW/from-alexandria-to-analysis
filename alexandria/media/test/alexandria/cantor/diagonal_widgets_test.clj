(ns alexandria.cantor.diagonal-widgets-test
  "The data of the Cantor 1891 widgets: the table is 8 x 8 of m and w and
   starts with the diagonal in no row; the theorem report has all maps missed."
  (:require [alexandria.cantor.diagonal-widgets :as w]
            [alexandria.cantor.sets :as sets]
            [clojure.test :refer [deftest is]]))

(deftest the-table-data-is-eight-by-eight
  (let [d (w/table-data)]
    (is (w/valid-table? d))
    (is (sets/in-no-row? (mapv (fn [r] (mapv keyword r)) (:rows d))))
    (is (not (w/valid-table? (assoc-in d [:rows 0 0] "x"))))
    (is (not (w/valid-table? {:rows []})))))

(deftest the-theorem-report-misses-every-map
  (let [d (w/theorem-data)]
    (is (w/valid-report? d))
    (is (= [1 2 3] (mapv :n (:report d))))
    (is (= [2 16 512] (mapv :maps (:report d))))
    (is (not (w/valid-report? (assoc-in d [:report 2 :missed] 511))))))
