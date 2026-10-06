(ns alexandria.fermat.maxima-test
  (:require [alexandria.fermat.maxima :as fm]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

(deftest every-claim-of-the-methodus-is-proved
  (let [g (fm/graded)]
    (is (= 10 (count g)))
    (is (every? #{:grade/proved} (map :grade g)) (pr-str (remove (comp #{:grade/proved} :grade) g)))))

(deftest adequality-steps-on-numbers
  (testing "B = 10: the quotient B - 2A is zero only at the half"
    (is (e/= 0 (:struck (fm/adequate (fm/rectangle 10) 5))))
    (is (e/= 4 (:struck (fm/adequate (fm/rectangle 10) 3))))))

(def polys (gen/vector (gen/choose -9 9) 1 6))

(deftest adequality-equals-the-derivative-for-random-polynomials
  (let [result (tc/quick-check 30
                 (prop/for-all [cs polys a (gen/choose -5 5)]
                   (let [f (fn [x] (reduce (fn [acc c] (e/+ (e/* acc x) c)) 0 cs))]
                     (e/zero? (e/simplify (e/- (:struck (fm/adequate f a)) ((e/D f) a)))))))]
    (is (:pass? result) (pr-str (dissoc result :result-data)))))

(deftest the-rectangle-is-greatest-at-the-half
  (let [f (fm/rectangle 4)]
    (is (every? #(<= (f %) (f 2)) (range 0 4 1/8)))))

(deftest the-tangent-and-the-secant
  (let [[x y] ((fm/tangent-line 1 1) [0])]
    (is (= [-1 0] [x y]) "E sits at CE = 2 CD to the left of C"))
  (let [[x0 y0] ((fm/secant-point 4 1 1) [0]) [x1 y1] ((fm/secant-point 4 1 1) [1])]
    (is (= [1 3] [x0 y0]))
    (is (= [2 4] [x1 y1]))))

(deftest the-propositions-are-shelf-data
  (let [res (fm/proof)]
    (is (r/ok? res))
    (doseq [id [:fermat/rectangle :fermat/tangent]]
      (is (every? (every-pred :claim :why :stage) (get-in res [:ok id :steps])) id))))
