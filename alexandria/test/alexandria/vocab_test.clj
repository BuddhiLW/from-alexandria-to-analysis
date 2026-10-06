(ns alexandria.vocab-test
  (:refer-clojure :exclude [angle])
  (:require [alexandria.vocab :as vocab]
            [clojure.test :refer [deftest is testing]]
            [desargues.board.construction :as c]
            [desargues.board.euclid]
            [emmy.env :as e]))

(def tiny-env
  {:points {:A {:x 0 :y 0}
            :B {:x 4 :y 0}
            :C {:x 0 :y 3}
            :L {:x 2 :y 0}
            :V {:x 2 :y 1}}})

(deftest realize-reuses-desargues-checks
  (testing "length delegates to :distance"
    (is (= (e/freeze (c/check {:distance [:A :B]} tiny-env))
           (e/freeze (vocab/evaluate '(length A B) tiny-env)))))
  (testing "area delegates to desargues.board.euclid :area"
    (is (= (e/freeze (c/check {:area [:A :B :C]} tiny-env))
           (e/freeze (vocab/evaluate '(area A B C) tiny-env))))))

(deftest claim-check-compares-vocabulary-expressions
  (is (e/zero? (c/check {:claim '(= (area A B C) (area A B C))} tiny-env)))
  (is (e/zero? (c/check {:claim ['(length A B) '(length A B)]} tiny-env))))

(deftest arithmetic-claims-evaluate-and-unknown-ops-throw
  (is (e/zero?
       (c/check {:claim '(= (area A B C)
                            (* 1/2 (length A B) 3))}
                tiny-env)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"Unknown Alexandria vocabulary operation"
                        (vocab/evaluate '(mystery-op 1 2) tiny-env)))
  (is (= :vocab/unknown-op
         (get-in (vocab/evaluate-result '(mystery-op 1 2) tiny-env)
                 [:error :alexandria/error]))))

(defn- near-zero? [x]
  (< (Math/abs (double x)) 1.0e-6))

(deftest archimedes-quadrature-prop-17-segment-area
  (doseq [[x1 x2] [[-1 1] [0 2] [-2 3]]]
    (let [mid (/ (+ x1 x2) 2.0)
          env {:points {:A {:x x1 :y (* x1 x1)}
                        :B {:x x2 :y (* x2 x2)}
                        :V {:x mid :y (* mid mid)}}}]
      (is (near-zero?
           (c/check {:claim '(= (segment-area (parabola 1 0 0) A B)
                                (* 4/3 (area A V B)))}
                    env))))
    (let [mid (/ (+ x1 x2) 2.0)
          env {:points {:A {:x x1 :y (* x1 x1)}
                        :B {:x x2 :y (* x2 x2)}
                        :V {:x mid :y (inc (* mid mid))}}}]
      (is (not (near-zero?
                (c/check {:claim '(= (segment-area (parabola 1 0 0) A B)
                                     (* 4/3 (area A V B)))}
                         env)))))))
