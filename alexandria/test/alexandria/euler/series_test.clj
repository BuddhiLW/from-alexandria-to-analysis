(ns alexandria.euler.series-test
  (:require [alexandria.euler.series :as s]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]))

(defn- close? [a b tol] (< (Math/abs (- (double a) (double b))) tol))

(deftest basel-coefficients
  (is (= -1/6 (s/sinc-coefficient 1)))
  (is (= 1/120 (s/sinc-coefficient 2)))
  (is (= 1/6 (:zeta-2 s/basel)))
  (is (= 1/90 (:zeta-4 s/basel)) "the same move, one coefficient on"))

(deftest basel-graded
  (let [gs (s/graded-basel)]
    (is (= 3 (count (filter #{:grade/numeric} (map :grade gs)))) "the two partial sums and the raster tail")
    (is (every? #{:grade/proved :grade/numeric} (map :grade gs)))
    (is (every? #(= :raster (:source %)) (filter #(= :grade/numeric (:grade %)) gs))
        "every numeric grade records that its number came from raster")
    (is (= 22 (count (filter #{:grade/proved} (map :grade gs)))))))

(deftest partial-sums-close-in-from-below
  (doseq [N [1 10 100 1000]]
    (let [gap (s/basel-gap 2 N)]
      (is (< 0 gap (double (s/tail-bound 2 N))) (str "N = " N)))))

(deftest euler-formula-on-truncations
  (is (every? #{:grade/proved} (map :grade (s/graded-euler-formula 10))))
  (testing "the spiral's partial sums (raster kernels) land on the unit circle at angle x, within the series tail x^17/17!"
    (doseq [x [0.5 1.0 Math/PI]]
      (let [[re im] (last (s/spiral-partial-sums x s/max-terms))
            tail (/ (Math/pow x 17) (reduce * (range 1.0 18.0)))]
        (is (close? re (Math/cos x) (+ tail 1e-12)))
        (is (close? im (Math/sin x) (+ tail 1e-12)))))))

(deftest figures
  (testing "the spiral figure at integer k is the k-th partial sum"
    (let [f (s/spiral 1.0)]
      (doseq [k [0 1 2 5]]
        (let [[a b] (mapv double (f [k])) [c d] (nth (s/spiral-partial-sums 1.0 k) k)]
          (is (close? a c 1e-9))
          (is (close? b d 1e-9))))))
  (testing "the partial products approach sin x / x"
    (let [[_ y] ((s/partial-product s/max-terms) [1.0])]
      (is (close? y (Math/sin 1.0) 0.01)))
    (is (close? 0 (second ((s/partial-product 3) [(* 2 Math/PI)])) 1e-9) "the factor n = 2 vanishes at 2 pi")))

(deftest e41-paragraph-by-paragraph
  (testing "par. 8: Euler's rule turns alpha, beta, gamma, ... into the power sums"
    (is (= [3 5 9] (s/newton-sums [3 2] 3)) "roots 1 and 2: 1 + 2, 1 + 4, 1 + 8"))
  (testing "par. 12: P, Q, R, S, T for y = 1, Euler's values"
    (is (= [1 1 1/2 1/3 5/24] (s/euler-par-9))))
  (let [gs (s/graded-e41)]
    (is (= 9 (count (filter #{:grade/proved} (map :grade gs)))))
    (is (every? #(= :raster (:source %)) (filter #(= :grade/numeric (:grade %)) gs))
        "the partial sums come from raster")
    (is (every? #{:grade/proved :grade/numeric} (map :grade gs))))
  (testing "the partial sums by raster agree with the exact rationals"
    (doseq [[N S] (map vector [1 2 3 10] (s/partial-sums-raster 2 [1 2 3 10]))]
      (is (close? S (s/partial-sum 2 N) 1e-12)))))

(deftest the-proofs-are-shelf-data
  (is (r/ok? (s/proof)))
  (is (= 14 (count (get-in (s/proof) [:ok :euler/basel :steps]))) "E41 par. 1 to 19")
  (is (= 6 (count (get-in (s/proof) [:ok :euler/exponential :steps])))))
