(ns alexandria.kepler.orbit-test
  (:require [alexandria.kepler.mysterium :as my]
            [alexandria.kepler.orbit :as orbit]
            [alexandria.library :as library]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]))

(defn- close? [a b tol] (< (Math/abs (- (double a) (double b))) tol))

(deftest the-laws-are-graded
  (let [g (orbit/graded)
        n (count orbit/identities)]
    (is (every? #{:grade/proved} (map :grade (take n g))) "the ellipse and the areas, by Emmy")
    (is (every? #{:grade/numeric} (map :grade (drop n g))) "Kepler's equation and the third law")))

(deftest keplers-equation
  (testing "the root satisfies E - e sin E = M over a revolution"
    (doseq [M (range 0 6.3 0.3)
            :let [E (orbit/eccentric-anomaly 0.09264 M)]]
      (is (close? M (- E (* 0.09264 (Math/sin E))) 1e-12))))
  (testing "circular orbit: E = M"
    (is (close? 1.234 (orbit/eccentric-anomaly 0 1.234) 1e-12))))

(deftest equal-areas-in-equal-times
  (let [{:keys [formula quadrature E]} (orbit/equal-time-areas 12)
        steps (map - (rest E) E)]
    (is (apply < (map double E)) "E advances")
    (testing "the planet moves unequally: E-steps are longest near perihelion"
      (is (> (first steps) (nth steps 6))))
    (testing "the areas are equal, by the formula and by raster's quadrature"
      (is (< (- (apply max quadrature) (apply min quadrature)) 1e-9))
      (is (< (- (apply max formula) (apply min formula)) 1e-9))
      (is (every? #(< (Math/abs (double %)) 1e-9) (map - formula quadrature))))))

(deftest the-numbers-come-from-raster
  (let [g (orbit/graded)
        numeric (drop (count orbit/identities) g)]
    (is (seq numeric) "there are numeric grades")
    (is (every? #(= :raster (:engine %)) numeric) "every numeric grade records :engine :raster")
    (is (every? #{:raster} (keep :engine g)) "no grade records another engine")
    (is (some #(re-find #"^third law" (:label %)) numeric) "the third-law grade is among them")))

(deftest the-figure-moves-on-the-ellipse
  (let [{ecc :e a :a} orbit/mars
        f (orbit/planet ecc a)
        b (* a (Math/sqrt (- 1 (* ecc ecc))))]
    (doseq [m [0 0.5 2 3.1 5]
            :let [[x y] (mapv double (f [m]))]]
      (is (close? 1 (+ (/ (* x x) (* a a)) (/ (* y y) (* b b))) 1e-12)))
    (is (close? a (first (f [0])) 1e-12) "perihelion at M = 0")))

(deftest third-law-spread
  (let [{:keys [min max]} (orbit/spread)]
    (is (< 0.99 min 1 max 1.01))))

(deftest third-law-numbers-come-from-raster
  (doseq [{:keys [T a k log-a3 log-T2 planet]} (orbit/third-law)]
    (is (< (Math/abs (- k (/ (* T T) (* a a a)))) 1e-9) (str planet " T^2/a^3"))
    (is (< (Math/abs (- log-a3 (Math/log10 (* a a a)))) 1e-4) (str planet " log a^3"))
    (is (< (Math/abs (- log-T2 (Math/log10 (* T T)))) 1e-4) (str planet " log T^2"))))

(deftest the-shelf-claims-grade
  (let [res (library/shelves)
        claims (for [[id p] (:ok res) :when (#{:kepler/first-law :kepler/second-law} id) c (:claims p)] c)]
    (is (r/ok? res))
    (is (= 2 (count claims)))
    (is (every? #{:grade/proved} (map my/claim-grade claims)))))

(deftest the-proofs-are-shelf-data
  (let [p (:ok (orbit/proof))]
    (is (= 6 (count (get-in p [:kepler/war-with-mars :steps]))))
    (is (= 4 (count (get-in p [:kepler/harmonice :steps]))))))
