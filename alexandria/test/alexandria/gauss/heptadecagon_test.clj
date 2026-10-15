(ns alexandria.gauss.heptadecagon-test
  (:require [alexandria.gauss.heptadecagon :as h]
            [clojure.test :refer [deftest is testing]]))

(deftest integers
  (is (= 3 (h/primitive-root 17)))
  (is (= [1 2 4 8] (h/chain 17)))
  (is (= {2 4} (h/factorize 16)))
  (testing "constructible n (Gauss art. 365-366)"
    (doseq [n [3 4 5 6 8 10 12 15 16 17 20 30 34 51 60 85 255 257]]
      (is (:constructible? (h/classify n)) (str n))))
  (testing "not constructible, with the obstruction named"
    (is (= [:not-fermat 7 3] ((juxt :obstruction :culprit :degree) (h/classify 7))))
    (is (= [:odd-prime-power 3 3] ((juxt :obstruction :culprit :degree) (h/classify 9))))
    (is (= [:not-fermat 11 5] ((juxt :obstruction :culprit :degree) (h/classify 11))))
    (is (= :not-fermat (:obstruction (h/classify 13))))
    (is (= :odd-prime-power (:obstruction (h/classify 25))))))

(deftest periods
  (let [t (h/periods 17)]
    (is (= [[1 2 3 4 5 6 7 8]] (map sort (:exps (first t)))))
    (is (= [[1 2 4 8] [3 5 6 7]] (:exps (second t))))
    (is (= [1 4] (first (:exps (nth t 2)))))
    (is (= [[1] [3] [8] [7] [4] [5] [2] [6]] (:exps (nth t 3))))))

(deftest values
  (let [v (h/nested-values)]
    (is (< (Math/abs (- (:cos v) (Math/cos (/ (* 2 Math/PI) 17)))) 1e-12))
    (is (< (Math/abs (- (:y1 v) (/ (+ -1 (Math/sqrt 17)) 2))) 1e-12)))
  (is (every? #{:grade/proved :grade/numeric} (map :grade (h/graded)))))
