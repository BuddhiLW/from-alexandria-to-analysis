(ns alexandria.cantor.sets-test
  (:require [alexandria.cantor.sets :as sets]
            [alexandria.library :as library]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-dsl.result :as r]))

(deftest every-check-is-proved
  (is (every? #{:grade/proved} (map :grade (sets/graded)))))

(deftest the-zig-zag-starts-as-drawn
  (is (= [1 2 1/2 1/3 3 4 3/2 2/3 1/4 1/5] (take 10 (sets/zig-zag)))))

(defspec every-rational-has-its-place 100
  (prop/for-all [p (gen/choose 1 15) q (gen/choose 1 15)]
    (let [x (/ p q)]
      (= x (nth (sets/zig-zag) (dec (sets/rational-index x)))))))

(deftest cantor-heights
  (testing "Cantor's own values, and the next"
    (is (= [1 2 4 12] (map sets/phi [1 2 3 4]))))
  (testing "the heights of his examples"
    (is (= 3 (sets/height [2 -1])))
    (is (= 4 (sets/height [1 0 -2]))))
  (testing "height 4 holds sqrt 2 and the golden ratio"
    (let [xs (map (comp double :root) (sets/numbers-of-height 4))]
      (is (some #(< (abs (- % (Math/sqrt 2))) 1e-8) xs))
      (is (some #(< (abs (- % (/ (+ 1 (Math/sqrt 5)) 2))) 1e-8) xs)))))

(deftest sturm-counts
  (is (= 2 (sets/real-root-count [1 0 -2] -2 2)))
  (is (= 0 (sets/real-root-count [1 0 1] -2 2)))
  (is (= 3 (sets/real-root-count [1 0 -1 0] -2 2))))

(deftest nested-intervals-miss-the-sequence
  (let [xs (vec (take 400 (sets/zig-zag)))
        steps (sets/nested-intervals xs [0 1] 6)
        {[lo hi] :interval [_ j] :used} (last steps)]
    (is (apply < (map (comp first :interval) (rest steps))) "left ends climb")
    (is (apply > (map (comp second :interval) (rest steps))) "right ends fall")
    (is (not-any? #(< lo % hi) (subvec xs 0 (inc j))) "nothing used so far lies inside")))

(defspec the-diagonal-is-in-no-row 100
  (prop/for-all [rows (gen/let [n (gen/choose 1 12)]
                        (gen/vector (gen/vector (gen/elements [:m :w]) n) n))]
    (and (sets/in-no-row? rows)
         (not-any? #(= (sets/diagonal-element rows) %) rows))))

(deftest the-walk-figure-follows-the-walk
  (let [f (sets/walk-pointer 0)]
    (is (= [0.0 0.0] (mapv double (f [0])))))
  (is (= [0.0 -0.63] (mapv double ((sets/walk-pointer 2.5) [0])))))

(deftest the-shelf-and-proofs-are-data
  (is (= "Georg Cantor" (get-in (:ok (library/shelves)) [:cantor/diagonal :author])))
  (let [p (:ok (sets/proof))]
    (is (r/ok? (sets/proof)))
    (is (every? (fn [{:keys [original rendering]}] (and (string? original) (string? rendering)))
                (mapcat :passages (vals (select-keys p [:cantor/heights :cantor/diagonal])))))
    (is (= [4 5 6] (map #(count (get-in p [% :steps])) [:cantor/zigzag :cantor/heights :cantor/diagonal])))))
