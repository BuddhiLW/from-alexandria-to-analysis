(ns alexandria.moebius.widgets-test
  "The data of the Möbius widgets: the band report for each half-twist count
   (chi 0 throughout, orientable exactly for even t), the cut pieces, and the
   content rows (the cube's volume is 1 from any apex, the flipped cube's is
   not, the decahedron and the twisted strip have none)."
  (:require [alexandria.moebius.widgets :as w]
            [clojure.test :refer [deftest is testing]]))

(deftest band-data-is-complete
  (let [d (w/band-data)]
    (is (w/finite-band-data? d))
    (is (= [0 1 2 3] (:twists d)))
    (doseq [t (:twists d) :let [r (get-in d [:reports t])]]
      (testing (str t " half twists")
        (is (= 0 (:chi r)))
        (is (= (even? t) (:orientable? r)))
        (is (= (if (even? t) 2 1) (:boundary r)))
        (is (= (if (even? t) 2 1) (count (get-in d [:cuts t]))))))))

(deftest incomplete-band-data-is-rejected
  (let [d (w/band-data)]
    (is (not (w/finite-band-data? (update-in d [:reports 1] dissoc :chi))))
    (is (not (w/finite-band-data? (assoc-in d [:reports 1 :character] [1 0]))))
    (is (not (w/finite-band-data? (assoc-in d [:cuts 2] []))))))

(deftest content-rows
  (let [[cube flipped deca strip] (w/content-data)
        near? (fn [a b] (< (Math/abs (- a b)) 1e-9))]
    (is (:oriented? cube))
    (is (every? #(near? 1.0 (Math/abs %)) (:as-written cube)) "the cube: 1 from either apex")
    (is (not (:listed-consistent? flipped)))
    (is (not (near? (first (:as-written flipped)) (second (:as-written flipped)))) "wrong senses: the sum moves with the apex")
    (is (every? #(near? 1.0 (Math/abs %)) (:repaired flipped)) "reversing that face restores the content")
    (is (not (:oriented? deca)))
    (is (nil? (:repaired deca)))
    (is (not (:oriented? strip)))
    (is (= 0 (:chi strip)))))
