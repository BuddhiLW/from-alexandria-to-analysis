(ns alexandria.algebra.cubic-test
  (:require [alexandria.algebra.cubic :as c]
            [alexandria.raster :as raster]
            [clojure.test :refer [deftest is testing]]))

(defn- near [a b] (< (Math/abs (- a b)) 1e-9))

(deftest khwarizmi
  (testing "a square and ten roots equal thirty-nine: the root is three"
    (let [r (raster/value (fn [b cc] ((c/square-readout b cc) nil)) 10 39)]
      (is (near 5 (nth r (c/square-index :half))))
      (is (near 25 (nth r (c/square-index :corner))))
      (is (near 64 (nth r (c/square-index :great))))
      (is (near 8 (nth r (c/square-index :side))))
      (is (near 3 (nth r (c/square-index :x))))
      (is (near 9 (nth r (c/square-index :square))))))
  (testing "the tie: x moved, c follows; b moved, x follows"
    (let [t (raster/value (fn [b cc x] ((c/khwarizmi-tie b cc x) nil)) 10 39 4)]
      (is (near 56 (nth t (+ (c/square-blocks :x) 1))))
      (is (near 3 (nth t (+ (c/square-blocks :b) 2)))))))

(deftest cardano
  (binding [raster/*simplify?* false]
    (testing "Cardano's example: the cube and six things equal twenty"
      (let [r (raster/value (fn [p q] ((c/cardano p q) nil)) 6 20)]
        (is (near 108 (nth r (c/cardano-index :D))))
        (is (near 1 (nth r (c/cardano-index :real?))))
        (is (near 2 (nth r (c/cardano-index :x))))))
    (testing "Bombelli: x^3 = 15x + 4, cube roots 2 +- i, x = 4"
      (let [r (raster/value (fn [p q] ((c/cardano p q) nil)) -15 4)]
        (is (neg? (nth r (c/cardano-index :D))))
        (is (near 2 (nth r (c/cardano-index :t1-re))))
        (is (near 1 (nth r (c/cardano-index :t1-im))))
        (is (near -1 (nth r (c/cardano-index :t2-im))))
        (is (near 4 (nth r (c/cardano-index :x))))
        (let [rr (raster/value (fn [p q x] ((c/rest-roots p q x) nil)) -15 4 4)]
          (is (near 3 (nth rr (c/rest-index :d2))))
          (is (near 0 (nth rr (c/rest-index :check)))))))))

(deftest viete
  (let [v (raster/value (fn [a b d] ((c/viete a b d) nil)) 4 -2 -2)]
    (is (= [0 -12 16] (mapv #(Math/round (double %)) (take 3 v))))))

(deftest grades
  (is (every? #{:grade/proved :grade/numeric} (map :grade (c/graded)))))
