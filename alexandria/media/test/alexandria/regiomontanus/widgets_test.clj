(ns alexandria.regiomontanus.widgets-test
  "The chord-to-sine widgets build on the raster backend, carry their
   data attributes, and their derivations grade."
  (:require [alexandria.regiomontanus.widgets :as w]
            [clojure.test :refer [deftest is testing]]))

(defn- text [v] (pr-str v))

(deftest widgets-build
  (testing "each widget is a viewer value whose browser form names its figure"
    (is (re-find #"data-widget \"half-chord\"" (text (w/half-chord))))
    (is (re-find #"data-widget \"aryabhata\"" (text (w/aryabhata))))
    (is (re-find #"data-widget \"law-of-sines\"" (text (w/law-of-sines))))
    (is (re-find #"data-widget \"solver\"" (text (w/solver))))
    (is (re-find #"data-widget \"definition-steps\"" (text (w/steps)))))
  (testing "the half-chord starts at the arc of 80 degrees, values from raster"
    (let [st (w/half-chord-init)]
      (is (= 80.0 (:arc st)))
      (is (< (Math/abs (- (:crd st) (* 120 (Math/sin (Math/toRadians 40))))) 1e-9))))
  (testing "the stepped definition quotes each author once per step"
    (is (= 5 (count w/definition-steps)))
    (is (every? :who w/definition-steps))))

(deftest derivations-grade
  (doseq [d [(w/sines-derivation) (w/triangles-derivation)]]
    (is (not (re-find #"grade/fails|\"fails\"" (text d))))))
