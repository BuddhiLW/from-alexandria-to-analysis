(ns alexandria.grade-test
  (:require [alexandria.grade :as grade]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [desargues.board.construction]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

(defn- variant [result]
  (:adt/variant (:ok result)))

(defn- poly-gen [vars]
  (letfn [(poly [depth]
            (if (zero? depth)
              (gen/one-of [(gen/elements vars)
                           (gen/choose -5 5)])
              (gen/one-of [(gen/elements vars)
                           (gen/choose -5 5)
                           (gen/fmap (fn [[a b]] (e/+ a b))
                                     (gen/tuple (poly (dec depth)) (poly (dec depth))))
                           (gen/fmap (fn [[a b]] (e/- a b))
                                     (gen/tuple (poly (dec depth)) (poly (dec depth))))
                           (gen/fmap (fn [[a b]] (e/* a b))
                                     (gen/tuple (poly (dec depth)) (poly (dec depth))))])))]
    (poly 3)))

(defn- env-for [board]
  {:points (into {}
                 (map-indexed (fn [i {:keys [id at]}]
                                [id {:x (first at)
                                     :y (second at)}])
                              (:points board)))})

(def tiny-distance-board
  {:kind :construction
   :points [{:id :A :op :free :at [0 0]}
            {:id :B :op :free :at [3 4]}]
   :checks [{:distance [:A :B]
             :against {:value 5}}]})

(defspec random-polynomial-identities-grade-proved 50
  (prop/for-all [p (poly-gen ['a 'b 'c])]
    (= :grade/proved
       (variant (grade/grade :symbolic [(e/- p p)])))))

(deftest symbolic-identities-grade-proved
  (is (= :grade/proved
         (variant (grade/grade :symbolic [(e/- (e/* 'x 'x) (e/expt 'x 2))])))))

(deftest tolerance-and-fidelity-ordering
  (testing "inexact zero is not proved"
    (is (= :grade/closed-form
           (variant (grade/grade :closed-form [0.0] 1.0e-9)))))
  (testing "within tolerance follows fidelity"
    (is (= :grade/closed-form
           (variant (grade/grade :closed-form [1.0e-12] 1.0e-9))))
    (is (= :grade/numeric
           (variant (grade/grade :numeric [1.0e-12] 1.0e-9)))))
  (testing "outside tolerance fails for every fidelity"
    (doseq [fidelity [:symbolic :closed-form :numeric]]
      (is (= :grade/fails
             (variant (grade/grade fidelity [1.0] 1.0e-9)))))))

(deftest false-symbolic-identity-fails
  (is (= :grade/fails
         (variant (grade/grade :symbolic [(e/- (e/+ 'x 1) 'x)])))))

(defspec small-numeric-zeroes-grade-closed-form 50
  (prop/for-all [x (gen/double* {:min -1.0e-10 :max 1.0e-10 :NaN? false :infinite? false})]
    (= :grade/closed-form
       (variant (grade/grade :closed-form [x] 1.0e-9)))))

(deftest grade-board-checks-results
  (let [env (env-for tiny-distance-board)]
    (is (= :grade/proved
           (variant (grade/grade-board-checks :symbolic env tiny-distance-board))))
    (is (= :grade/proved
           (variant (grade/grade-board-checks :numeric env tiny-distance-board))))))

(deftest broken-board-returns-check-failed-err
  (let [broken (assoc tiny-distance-board :checks [{:distance [:A :missing]
                                                   :against {:value 5}}])]
    (is (= :grade/check-failed
           (:error (grade/grade-board-checks :symbolic (env-for broken) broken))))))
