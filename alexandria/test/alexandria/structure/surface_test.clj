(ns alexandria.structure.surface-test
  "The strip passes the surface contract for every k and t; chi of the
   annulus and the Möbius band is 0 and the orientation character tells them
   apart; Möbius' signed content of the cube, of the cube with a face
   reversed, and of the twisted strip."
  (:require [alexandria.structure.ring-contract :refer [defcharacter-contract]]
            [alexandria.structure.surface :as s]
            [alexandria.structure.surface-contract :refer [defsurface-contract]]
            [alexandria.structure.surfaces :as ss]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]))

(defsurface-contract strips
  (gen/fmap (fn [[k t]] (ss/strip k t)) (gen/tuple (gen/choose 1 8) (gen/choose 0 5))))

(defsurface-contract polyhedra
  (gen/elements [ss/cube ss/cube-flipped]))

(defn- chi-of [k t] (:chi (ss/strip-report k t)))

(deftest chi-does-not-see-the-twist
  (testing "annulus and Möbius band both have chi = 0, for every cell count"
    (doseq [k (range 1 9) t (range 0 4)]
      (is (= 0 (chi-of k t)) (str "k=" k " t=" t))))
  (testing "V E F of the 4-cell Möbius band"
    (is (= {:V 8 :E 12 :F 4 :chi 0} (select-keys (ss/strip-report 4 1) [:V :E :F :chi])))))

(deftest orientation-sees-it
  (doseq [k (range 1 9) t (range 0 4)]
    (let [r (ss/strip-report k t)]
      (is (= (even? t) (:orientable? r)) (str "k=" k " t=" t))
      (is (= (if (even? t) 1 -1) (:holonomy r)))
      (is (= (if (even? t) [1 1] [1 -1]) (:character r)))))
  (testing "the boundary: two circles for the annulus, one for the Möbius band"
    (is (= 2 (:boundary (ss/strip-report 5 0))))
    (is (= 1 (:boundary (ss/strip-report 5 1))))
    (is (= 2 (:boundary (ss/strip-report 5 2))))))

(deftest the-orientation-character
  (let [c (fn [k t] (let [st (ss/strip k t)] (s/orientation-character (s/holonomy st (:loop st)))))]
    (is (s/trivial-character? (c 4 0)) "the annulus: trivial")
    (is (not (s/trivial-character? (c 4 1))) "the Möbius band: non-trivial")
    (is (s/trivial-character? (c 4 2)))
    (is (not (s/trivial-character? (c 7 3))))))

(def ^:private band-character
  (let [st (ss/strip 4 1)] (s/orientation-character (s/holonomy st (:loop st)))))

(defcharacter-contract moebius-band-orientation (:group band-character) (:chi band-character) (gen/elements [0 1]))

(deftest cutting-along-the-core
  (testing "t even: two bands, the same twists; t odd: one band, doubled, orientable"
    (let [[a b :as even] (ss/cut-along-core 4 2)
          [one :as odd] (ss/cut-along-core 4 1)]
      (is (= 2 (count even)))
      (is (= (select-keys a [:k :twists :orientable?]) (select-keys b [:k :twists :orientable?])))
      (is (= 1 (count odd)))
      (is (= [8 2 true 2] ((juxt :k :twists :orientable? :boundary) one)))
      (is (= 0 (:chi one))))))

(deftest moebius-content-of-polyhedra
  (testing "the cube: oriented, closed, content 1 from any apex"
    (let [c (ss/content ss/cube)]
      (is (:oriented? c)) (is (:closed? c))
      (is (< (Math/abs (- 1.0 (Math/abs (:volume c)))) 1e-9))
      (is (:listed-consistent? c)))
    (doseq [o [[0 0 0] [5 -3 2] [0.5 0.5 0.5]]]
      (is (< (Math/abs (- 1.0 (Math/abs (ss/signed-volume ss/cube o)))) 1e-9)
          "the sum of signed pyramids does not depend on the apex")))
  (testing "one face reversed: the listed faces are not consistent, and the sum depends on the apex"
    (let [c (ss/content ss/cube-flipped)]
      (is (not (:listed-consistent? c)))
      (is (:oriented? c) "it can be repaired by reversing that face")
      (is (not= (ss/signed-volume ss/cube-flipped [0 0 0]) (ss/signed-volume ss/cube-flipped [5 -3 2])))))
  (testing "a twisted strip: no consistent orientation, no content"
    (is (not (s/orientable? (ss/strip 4 1))))
    (is (= 0 (:chi (ss/strip-report 4 1))))
    (is (not (s/closed? (ss/strip 4 1))) "and it has a boundary")))

(deftest moebius-decahedron-of-1865
  (let [d ss/moebius-decahedron]
    (is (= {:V 6 :E 15 :F 10 :chi 1} (s/counts d)) "six corners, fifteen edges, ten faces")
    (is (not (s/orientable? d)) "the law of opposite edges fails: the zone of five triangles is one-sided")
    (is (not (s/closed? (assoc d :faces []))))
    (is (s/closed? d) "every edge is in exactly two triangles")
    (is (nil? (:volume (ss/content d))) "no content")))
