(ns alexandria.newton.principia-test
  (:require [alexandria.grade :as grade]
            [alexandria.library :as library]
            [alexandria.newton.principia :as p]
            [alexandria.vocab :as vocab]
            [clojure.test :refer [deftest is testing]]
            [desargues.board.construction :as c]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

(defn- close? [a b tol] (< (Math/abs (- (double a) (double b))) tol))

(defn- board-env
  "The check environment of a construction whose points are all given :at,
   each coordinate an Emmy expression of the free letters."
  [{:keys [points]}]
  {:points (into {} (map (fn [{:keys [id at]}]
                           [id {:x (vocab/evaluate (first at) {}) :y (vocab/evaluate (second at) {})}]))
                 points)})

(defn- variant [diff] (get-in (grade/grade :symbolic [diff]) [:ok :adt/variant]))

(deftest every-check-grades-as-expected
  (is (= [:grade/proved :grade/proved :grade/proved :grade/numeric :grade/proved
          :grade/proved :grade/proved :grade/proved :grade/proved]
         (mapv :grade (p/graded)))))

(deftest prop-1-shelf-claims-are-proved
  (let [prop (get-in (library/shelves) [:ok :newton/prop-1])
        env (board-env (:construction prop))]
    (is (= 3 (count (:claims prop))))
    (doseq [claim (:claims prop)]
      (is (= :grade/proved (variant (c/check claim env))) (:label claim)))))

(deftest equal-areas-without-the-parallel-fail
  (testing "an impulse not toward S breaks the equality: C moved off the parallel"
    (let [env (update-in (p/impulse-env) [:points :C :x] e/+ 1)]
      (is (not= :grade/proved (variant (c/check {:claim '(= (area S B c) (area S B C))} env)))))))

(deftest the-polygon-sweeps-equal-areas
  (let [ps (p/polygon [1.0 0.0] [0.0 1.2] 1.0 0.05 120)
        areas (p/swept-areas ps)]
    (is (= 121 (count ps)))
    (is (every? #(close? (first areas) % 1e-12) areas))
    (testing "and stays bound: a loop around S"
      (is (every? #(< 0.5 (Math/hypot (first %) (second %)) 3.0) ps)))))

(deftest lemma-2-bars-squeeze
  (is (= 1/2 (- (p/circumscribed-sum 2) (p/inscribed-sum 2))))
  (is (< (p/inscribed-sum 100) 2/3 (p/circumscribed-sum 100))))

(deftest the-force-is-inverse-square
  (let [f (fn [th] (p/force-at 1.0 0.5 1.0 th))
        r (fn [th] ((p/orbit-r 1.0 0.5) th))]
    (is (close? (* (f 0.0) (Math/pow (r 0.0) 2)) (* (f 2.0) (Math/pow (r 2.0) 2)) 1e-12))
    (is (> (f 0.0) (f Math/PI)) "strongest at perihelion")))

(deftest the-proofs-are-shelf-data
  (let [res (p/proof)]
    (is (r/ok? res))
    (is (= #{:newton/lemma-1 :newton/prop-1 :newton/prop-11} (set (keys (:ok res)))))
    (is (every? (fn [{:keys [claim why stage]}] (and (string? claim) (string? why) (keyword? stage)))
                (mapcat :steps (vals (:ok res)))))))
