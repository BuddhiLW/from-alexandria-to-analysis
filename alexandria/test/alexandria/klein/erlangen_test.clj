(ns alexandria.klein.erlangen-test
  (:require [alexandria.klein.erlangen :as k]
            [clojure.test :refer [deftest is testing]]))

(def ok #{:grade/proved :grade/numeric})

(deftest grades
  (doseq [{:keys [label grade]} (k/graded)]
    (is (ok grade) label)))

(deftest invariants-per-group
  (let [by (into {} (map (juxt :group :invariant) (k/invariant-table)))]
    (testing "Euclid keeps everything planar, but a motion moves the disk"
      (is (= #{:length :angle :ratio :parallel :midpoint :cross-ratio :circle} (by :euclid))))
    (testing "a similarity loses length only"
      (is (= #{:angle :ratio :parallel :midpoint :cross-ratio :circle} (by :similarity))))
    (testing "an affine map keeps parallels, midpoints, cross-ratio"
      (is (= #{:parallel :midpoint :cross-ratio} (by :affine))))
    (testing "a projective map keeps only the cross-ratio"
      (is (= #{:cross-ratio} (by :projective))))
    (testing "a Moebius map keeps angles, circles, cross-ratio and hyperbolic length"
      (is (= #{:angle :circle :cross-ratio :hyp} (by :hyperbolic))))))

(deftest identity-keeps-all
  (doseq [g k/groups]
    (let [{:keys [invariant]} (k/evaluate g k/identity-params)]
      (is (every? true? (vals invariant)) (str g)))))
