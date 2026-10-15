(ns alexandria.noether.widgets-test
  "The data of the Noether orbit widget: the browser divides by the time span
   and by the range of E and J, so every sample must be finite and every run
   non-empty, or the SVG gets NaN coordinates."
  (:require [alexandria.noether.widgets :as w]
            [clojure.test :refer [deftest is testing]]))

(deftest the-widget-data-is-finite-and-not-empty
  (let [d (w/data)]
    (is (w/finite-samples? d))
    (is (= #{:symmetric :time :rotation} (set (keys (:runs d)))))
    (doseq [[kind by-eps] (:runs d) [eps samples] by-eps]
      (testing (str kind " " eps)
        (is (< 100 (count samples)))
        (is (pos? (:t (peek samples))) "a time span to divide by")
        (is (every? #(Double/isFinite (double (:E %))) samples))
        (is (every? #(Double/isFinite (double (:J %))) samples))))))

(deftest non-finite-or-empty-data-is-rejected
  (let [d (w/data)]
    (is (not (w/finite-samples? (assoc-in d [:runs :symmetric 0.0 0 :E] Double/NaN))))
    (is (not (w/finite-samples? (assoc-in d [:runs :symmetric 0.0] []))))
    (is (not (w/finite-samples? {:runs {}})))
    (is (not (w/finite-samples? (assoc-in d [:runs :time 0.01 3 :J] Double/POSITIVE_INFINITY))))))
