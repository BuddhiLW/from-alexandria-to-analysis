(ns alexandria.dedekind.cuts-test
  (:require [alexandria.dedekind.cuts :as cuts]
            [alexandria.library :as library]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-dsl.result :as r]))

(deftest every-check-is-proved
  (is (= 8 (count (cuts/graded))))
  (is (every? #{:grade/proved} (map :grade (cuts/graded)))))

(def pos-rational
  (gen/let [m (gen/choose 1 400) n (gen/choose 1 400)] (/ m n)))

(defspec the-classes-are-a-cut 200
  (prop/for-all [a pos-rational b pos-rational]
    (let [[lo hi] (sort [a b])]
      (not (and (cuts/A2? 2 lo) (cuts/A1? 2 hi) (< lo hi))))))

(defspec y-beats-every-candidate 200
  (prop/for-all [x pos-rational]
    (let [y (cuts/y-map 2 x)]
      (if (cuts/A1? 2 x)
        (and (> y x) (cuts/A1? 2 y))
        (and (< y x) (cuts/A2? 2 y))))))

(defspec descent-keeps-the-pell-value-up-to-sign 200
  (prop/for-all [t (gen/choose 2 500) u (gen/choose 1 300)]
    (let [[t' u'] (cuts/descent-step 2 [t u])]
      (= (cuts/pell-value 2 [t' u']) (* -1 (cuts/pell-value 2 [t u]))))))

(deftest no-rational-on-the-cut
  (is (not-any? #(= 2 (* % %)) (cuts/rationals-in 1 2 80))))

(deftest the-line-figure-separates-the-classes
  (let [f (cuts/on-the-line 1.4142135623730951 1 0.08)]
    (is (pos? (second (f [7 5]))) "7/5 is in A1, above")
    (is (neg? (second (f [3 2]))) "3/2 is in A2, below")
    (is (< (abs (first (f [99 70]))) 1e-4) "99/70 sits next to sqrt 2")))

(deftest the-shelf-and-proofs-are-data
  (let [shelves (:ok (library/shelves))]
    (is (= "Richard Dedekind" (get-in shelves [:dedekind/cut-sqrt-d :author]))))
  (let [p (:ok (cuts/proof))]
    (is (r/ok? (cuts/proof)))
    (is (= 8 (count (get-in p [:dedekind/cut-sqrt-2 :steps]))))
    (is (every? string? (get-in p [:dedekind/preface :passages])))
    (is (string? (get-in p [:dedekind/eudoxus :euclid])))))
