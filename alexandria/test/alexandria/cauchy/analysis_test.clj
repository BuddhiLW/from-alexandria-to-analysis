(ns alexandria.cauchy.analysis-test
  (:require [alexandria.cauchy.analysis :as cauchy]
            [alexandria.library :as library]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]))

(def graded (delay (cauchy/graded)))

(defn- grades [part] (mapv :grade (get @graded part)))

(deftest every-check-grades-as-expected
  (testing "continuity: closed-form deltas proved, the sine's delta numeric"
    (is (= [:grade/proved :grade/proved :grade/proved :grade/numeric] (grades :continuity))))
  (testing "series: criterion and tests proved, the limit e numeric"
    (is (= (conj (vec (repeat 5 :grade/proved)) :grade/numeric) (grades :series))))
  (testing "sum theorem: the terms vanish at pi (proved), jump, overshoot and non-uniformity numeric"
    (is (= (into (vec (repeat 7 :grade/proved)) (repeat 4 :grade/numeric)) (grades :sum-theorem))))
  (testing "integral: closed-form sums proved, transcendental integrands numeric"
    (is (= (into (vec (repeat 9 :grade/proved)) (repeat 3 :grade/numeric)) (grades :integral))))
  (testing "flat: the derivative recurrence proved, the limits at 0 numeric, f(1) = 1/e in floating point"
    (is (= (into (vec (repeat 5 :grade/proved)) [:grade/numeric :grade/numeric :grade/closed-form])
           (grades :flat)))))

(deftest cauchys-order-grades-as-expected
  (testing "Note III: signs and widths proved exactly, the half-sum against raster's root numeric"
    (is (= [:grade/proved :grade/proved :grade/proved :grade/numeric] (grades :ivt))))
  (testing "ch. VI Th. 1: the comparison with U^n proved exactly, the roots and the sum numeric"
    (is (= [:grade/numeric :grade/proved :grade/proved :grade/numeric :grade/proved] (grades :root-test))))
  (testing "Lesson 3: the quotients of x^5 and sin x proved, the raster quotients numeric"
    (is (= [:grade/proved :grade/proved :grade/proved :grade/numeric :grade/proved] (grades :derivative))))
  (testing "Lesson 21: equations (4) and (7) proved, the subdivision bound decided, the limits numeric"
    (is (= [:grade/proved :grade/proved :grade/proved :grade/numeric :grade/numeric] (grades :refinement))))
  (testing "Lesson 26: the polynomial quotient proved, theta in (0, 1) decided, the limit numeric"
    (is (= [:grade/proved :grade/proved :grade/numeric :grade/proved] (grades :fundamental)))))

(deftest the-tenths-search
  (let [rounds (cauchy/tenths-search cauchy/ivt-example 2 3 10 3)]
    (is (= [[2 3] [2 21/10] [209/100 21/10] [2094/1000 2095/1000]] (mapv (juxt :x :X) rounds)))))

(deftest the-epsilon-delta-game
  (testing "a found delta keeps every sampled increment inside the band"
    (let [f (get-in cauchy/games [:sine :f])
          d (cauchy/find-delta f 1.0 0.1)]
      (is (< 0 d 1))
      (is (<= (cauchy/worst-increment f 1.0 (* 0.99 d)) 0.1))
      (is (> (cauchy/worst-increment f 1.0 (* 1.05 d)) 0.1))))
  (testing "the closed-form delta for x^2 at a = 1"
    (let [delta (get-in cauchy/games [:square :delta])]
      (doseq [eps [0.5 0.1 0.01]]
        (is (< (Math/abs (- (+ 1 (double (delta 1 eps))) (Math/sqrt (+ 1 eps)))) 1e-12))))))

(deftest the-numbers-come-from-raster
  (let [numeric (filter #(= :grade/numeric (:grade %)) (apply concat (vals (cauchy/graded))))]
    (is (= 18 (count numeric)))
    (is (every? #(= :raster (:source %)) numeric)))
  (testing "Abel's partial sum by the raster loop kernel equals the Emmy sum term by term"
    (doseq [n [1 5 12] x [0.3 2.0]]
      (is (< (Math/abs (- (cauchy/abel-sum n x) (double (cauchy/abel-partial n x)))) 1e-12)))))

(deftest abels-series
  (is (= 0.0 (cauchy/sawtooth Math/PI)))
  (is (< (Math/abs (- (cauchy/sawtooth 1.0) 0.5)) 1e-12))
  (testing "the error near the jump does not shrink"
    (is (every? #(> (cauchy/error-near-jump %) 0.6) [50 500 5000]))))

(deftest cauchys-sums
  (testing "S for x^2 on [0, 1] in exact rationals tends to 1/3 from below"
    (let [S (fn [n] (cauchy/left-sum #(* % %) (cauchy/equal-points 0 1 n)))]
      (is (= (S 4) 7/32))
      (is (= (S 4) (cauchy/square-sum-closed 0 1 4)))
      (is (< (S 10) (S 100) 1/3)))))

(deftest the-shelf-and-the-proofs
  (let [shelves (:ok (library/shelves))]
    (is (= "Niels Henrik Abel" (get-in shelves [:abel/exception :author])))
    (is (= "Augustin-Louis Cauchy" (get-in shelves [:cauchy/continuity :author]))))
  (let [res (cauchy/proof)]
    (is (r/ok? res))
    (doseq [id [:cauchy/continuity :cauchy/series :cauchy/sum-theorem :cauchy/integral :cauchy/flat]
            :let [part (get-in res [:ok id])]]
      (is (seq (:steps part)) (str id))
      (is (every? (every-pred :fr :en :source) (:quotes part)) (str id)))))
