(ns alexandria.euler.graphs-test
  (:require [alexandria.euler.graphs :as g]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]))

(deftest koenigsberg-by-parity
  (is (= {:A 5 :B 3 :C 3 :D 3} (g/degrees g/koenigsberg)))
  (is (= [:A :B :C :D] (g/odd-regions g/koenigsberg)))
  (is (= :none (g/euler-rule g/koenigsberg)))
  (let [{:keys [prefixed sum]} (g/euler-table g/koenigsberg)]
    (is (= 8 prefixed))
    (is (= 9 sum) "par. 9: three A's and two each of B, C, D")))

(deftest the-count-agrees-with-exhaustion
  (is (= 6 (g/longest-trail g/koenigsberg)))
  (testing "adding one bridge between two odd regions leaves two odd: a walk exists"
    (let [g' (update g/koenigsberg :bridges conj {:id :h :ends [:B :C]})]
      (is (= :from-odd (g/euler-rule g')))
      (is (= 8 (g/longest-trail g'))))))

(deftest eulers-fifteen-bridges
  (is (= 15 (count (:bridges g/fifteen-bridges))))
  (is (= {:A 8 :B 4 :C 4 :D 3 :E 5 :F 6} (g/degrees g/fifteen-bridges)) "par. 15's table")
  (is (= [:D :E] (g/odd-regions g/fifteen-bridges)))
  (is (= 16 (:sum (g/euler-table g/fifteen-bridges))))
  (is (g/walk? g/fifteen-bridges (g/route->steps g/route-15)))
  (is (not (g/walk? g/fifteen-bridges (rest (g/route->steps g/route-15))))))

(deftest every-claim-is-proved
  (is (= 14 (count (g/graded))))
  (is (every? #{:grade/proved} (map :grade (g/graded)))))

(deftest eulers-letter-counts
  (testing "par. 8, 11-12: odd d gives (d + 1)/2 wherever the walk starts; even d gives d/2, or d/2 + 1 from there"
    (is (= [1 1 2 2 3 3] (for [d [1 3 5] s? [false true]] (g/letter-count-from d s?))))
    (is (= [1 2 2 3 3 4] (for [d [2 4 6] s? [false true]] (g/letter-count-from d s?)))))
  (testing "par. 18-19: the excess is (odd regions)/2 - 1"
    (is (= 1 (g/excess g/koenigsberg)))
    (is (= 0 (g/excess g/fifteen-bridges)))
    (is (= -1 (g/excess (g/doubled g/koenigsberg))))
    (is (= :anywhere (g/euler-rule (g/doubled g/koenigsberg))))))

(deftest the-proof-is-shelf-data
  (let [p (get-in (g/proof) [:ok :euler/koenigsberg])]
    (is (r/ok? (g/proof)))
    (is (= 19 (count (:steps p))) "par. 1 to 21, one step per paragraph or pair")
    (is (= (count (:passages (:latin p))) (count (:english p)) (count (:locus (:latin p)))))
    (is (string? (:english-source p)) "the English is the Wikisource translation, named")))
