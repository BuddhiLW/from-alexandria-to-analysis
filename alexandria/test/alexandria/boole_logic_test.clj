(ns alexandria.boole-logic-test
  "Boole's syllogism as elimination of the middle term, verified by the
   truth table over 2^3 assignments and by models; the 256 moods."
  (:require [alexandria.boole.logic :as l]
            [clojure.test :refer [deftest is testing]]))

(deftest barbara-by-elimination
  (testing "All M are P, All S are M: eliminate m, read the residue"
    (let [r (l/eliminate-middle 1 :A :A)]
      (is (= [[1 0]] (:empty r)) "the only vanishing constituent is s(1-p)")
      (is (= ["All S are P"] (:conclusions r))))))

(deftest celarent-and-camestres
  (is (= ["No S are P"] (:conclusions (l/eliminate-middle 1 :E :A))))
  (is (= ["No S are P"] (:conclusions (l/eliminate-middle 2 :A :E)))))

(deftest truth-table-verifies
  (let [rows (l/truth-table 1 :A :A :A)]
    (is (= 8 (count rows)))
    (is (every? :ok? rows))
    (is (some :premises-hold? rows)))
  (is (not (every? :ok? (l/truth-table 1 :A :A :E))) "AAE is not valid"))

(deftest the-moods
  (let [ms (l/all-moods)
        fig1 (into {} (map (juxt :name identity)) (filter #(= 1 (:figure %)) ms))]
    (is (= 256 (count ms)))
    (testing "first figure: Barbara, Celarent, Darii, Ferio need no import"
      (is (every? #(:valid? (fig1 %)) ["AAA" "EAE" "AII" "EIO"])))
    (testing "Barbari and Celaront need the subject class to be populated"
      (is (every? #(and (not (:valid? (fig1 %))) (:import? (fig1 %))) ["AAI" "EAO"])))
    (is (not (:valid? (fig1 "AEE"))))
    (is (= 4 (count (filter :valid? (vals fig1)))))
    (is (= 256 (reduce + (vals (l/tally ms)))))
    (testing "Aristotle's 24 moods = 15 unconditional + 9 with import (Boole's reading of existence)"
      (is (= 15 (:valid (l/tally ms))))
      (is (= 9 (:import (l/tally ms)))))))
