(ns alexandria.boole.widgets-test
  "The data of the Boole syllogism widget: 64 premise pairs, the
   elimination residue of Barbara, the truth table, the tally of the 256
   moods, and that incomplete data are rejected."
  (:require [alexandria.boole.widgets :as w]
            [clojure.test :refer [deftest is testing]]))

(deftest data-is-complete
  (let [d (w/syllogism-data)]
    (is (w/finite-syllogism-data? d))
    (is (= {:valid 15 :import 9 :invalid 232} (:tally d)))))

(deftest barbara-row
  (let [r (first (filter #(and (= 1 (:figure %)) (= "A" (:f1 %)) (= "A" (:f2 %))) (:pairs (w/syllogism-data))))]
    (is (= ["All S are P"] (get-in r [:eliminated :conclusions])))
    (is (= "All S are P" (:table-conclusion r)))
    (is (= 8 (count (:table r))))
    (is (every? :ok? (:table r)))
    (is (= ["m(1 - p) = 0" "s(1 - m) = 0"] (map :eq (:premises r))))))

(deftest particular-premises-are-not-eliminated
  (let [r (first (filter #(and (= 1 (:figure %)) (= "A" (:f1 %)) (= "I" (:f2 %))) (:pairs (w/syllogism-data))))]
    (is (nil? (:eliminated r)))
    (is (nil? (:table r)))
    (is (some :valid (:moods r)) "Darii: some S are P")))

(deftest incomplete-data-rejected
  (let [d (w/syllogism-data)]
    (is (not (w/finite-syllogism-data? (update d :pairs butlast))))
    (is (not (w/finite-syllogism-data? (assoc-in d [:pairs 0 :table] [])))
        "a table that is not 8 rows")
    (is (not (w/finite-syllogism-data? (assoc d :tally {:valid 1}))))))
