(ns alexandria.lagrange.variations-test
  (:require [alexandria.lagrange.variations :as v]
            [alexandria.library :as library]
            [clojure.test :refer [deftest is testing]]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

(defn- close? [a b tol] (< (Math/abs (- (double a) (double b))) tol))

(def ^:private numeric-labels
  #{"cycloid time by raster's quadrature = pi sqrt(a/g)"
    "cycloid < circle arc < straight line (raster's quadrature)"})

(deftest every-claim-grades-as-expected
  (let [gs (v/graded)
        by-label (into {} (map (juxt :label :grade)) gs)]
    (is (= #{:grade/numeric} (set (vals (select-keys by-label numeric-labels)))))
    (is (= 2 (count (select-keys by-label numeric-labels))))
    (is (= #{:grade/proved} (set (vals (apply dissoc by-label numeric-labels)))))
    (testing "every numeric grade records raster as its engine"
      (is (every? #(= :raster (:engine %)) (filter #(= :grade/numeric (:grade %)) gs))))))

(deftest the-cycloid-wins-the-race
  (let [{:keys [line arc cycloid]} (v/descent-times)]
    (is (close? cycloid (* Math/PI (Math/sqrt (/ 1.0 9.81))) 1e-9))
    (testing "the chord in closed form: t = sqrt(2 L / (g sin alpha)), L^2 = pi^2 + 4"
      (is (close? line (Math/sqrt (/ (* 2 (+ (* Math/PI Math/PI) 4)) (* 9.81 2))) 1e-9)))
    (is (< cycloid arc line))
    (testing "all three tracks join A to B"
      (doseq [[k f] (v/curves)
              :let [[[x0 y0] [x1 y1]] (map #(mapv double %) [(f 0) (f 1)])]]
        (is (close? 0 x0 1e-12) (str k))
        (is (close? 0 y0 1e-12) (str k))
        (is (close? Math/PI x1 1e-9) (str k))
        (is (close? 2 y1 1e-9) (str k))))))

(deftest a-straight-line-is-not-an-extremal
  (testing "the Euler-Lagrange expression does not vanish on the chord"
    (let [line (fn [s] (e/up (e/* Math/PI s) (e/* 2 s)))
          res (((emmy.mechanics.lagrange/Lagrange-equations (v/descent-L 9.81)) line) 0.5)]
      (is (not (every? #(close? 0 % 1e-9) (flatten (seq res))))))))

(deftest the-tautochrone-period-is-independent-of-amplitude
  (is (close? (v/tautochrone-period 1 9.81) (* 4 Math/PI (Math/sqrt (/ 1 9.81))) 1e-12)))

(deftest the-race-is-sampled-in-time-order
  (doseq [[k samples] (v/race v/race-params 12)]
    (is (apply <= (map first samples)) (str k))))

(deftest the-proofs-are-shelf-data
  (let [b (get-in (v/brachistochrone-proof) [:ok :bernoulli/brachistochrone])
        d (get-in (v/delta-proof) [:ok :lagrange/delta-1755])]
    (is (r/ok? (v/brachistochrone-proof)))
    (is (= 7 (count (:steps b))))
    (is (re-find #"brevissimo tempore" (get-in b [:challenge :original])))
    (is (= 5 (count (:steps d))))
    (is (re-find #"denotabo per δ" (get-in d [:lagrange :original])))))

(deftest the-shelf-is-catalogued
  (let [res (library/shelves)]
    (is (r/ok? res))
    (is (every? (:ok res) [:bernoulli/brachistochrone :lagrange/delta-1755 :lagrange/kepler
                           :lagrange/mean-value]))))
