(ns alexandria.structure.invariant-test
  "Klein's chain and the space forms as geometries, and the contract of
   any geometry: a property its generators keep is kept by every word in
   them, and the unit keeps everything."
  (:require [alexandria.structure.curvature :as kc]
            [alexandria.structure.group :as g]
            [alexandria.structure.groups :as gs]
            [alexandria.structure.invariant :as inv]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [emmy.env :as e]))

(defmacro defgeometry-contract
  "The contract of geometry G over figures from figure-gen: a property
   kept by every generator on a family of figures (16 generated plus the
   one under test) is kept on the figure by every word in the generators;
   the unit keeps every defined property."
  [name G figure-gen & [{:keys [num-tests] :or {num-tests 30}}]]
  `(do
     (let [family# (gen/sample ~figure-gen 16)]
       (defspec ~(symbol (str name "-words-keep-invariants")) ~num-tests
         (prop/for-all [fig# ~figure-gen
                        w# (gen/vector (gen/choose 0 (dec (count (:generators ~G)))) 1 4)]
           (let [el# (reduce #(g/compose (:group ~G) %1 (nth (:generators ~G) %2)) (g/unit (:group ~G)) w#)
                 img# (inv/transport (:group ~G) el# fig#)
                 ks# (inv/invariants ~G (cons fig# family#))]
             (every? (fn [k#] (inv/kept? (get-in ~G [:properties k#]) fig# img#)) ks#)))))
     (defspec ~(symbol (str name "-unit-keeps-all")) ~num-tests
       (prop/for-all [fig# ~figure-gen]
         (let [img# (inv/transport (:group ~G) (g/unit (:group ~G)) fig#)]
           (every? (fn [k#] (let [p# (get-in ~G [:properties k#])]
                              (or (not ((or (:defined? p#) (constantly true)) fig#))
                                  (inv/kept? p# fig# img#))))
                   (keys (:properties ~G))))))))

;; ---------------------------------------------------------------------------
;; Figures: four points A B C D

(defn- v- [[a b] [c d]] [(- a c) (- b d)])
(defn- len [v] (Math/sqrt (reduce + (map * v v))))
(defn- cross [[a b] [c d]] (- (* a d) (* b c)))

(def plane-properties
  {:length {:measure (fn [[A B]] (len (v- B A))) :compare :equal}
   :angle {:measure (fn [[A B C]] (let [u (v- A C) w (v- B C)] (Math/atan2 (Math/abs (double (cross u w))) (double (reduce + (map * u w))))))
           :compare :equal
           :defined? (fn [[A B C]] (and (> (len (v- A C)) 1e-6) (> (len (v- B C)) 1e-6)))}
   :ratio {:measure (fn [[A B C]] (/ (len (v- B A)) (len (v- C B)))) :compare :equal
           :defined? (fn [[_ B C]] (> (len (v- C B)) 1e-6))}
   :midpoint {:measure (fn [[A B _ _ M]] (len (v- M (mapv #(/ (+ %1 %2) 2) A B)))) :compare :vanishes}
   :collinear {:measure (fn [[A B _ _ _ P]] (cross (v- B A) (v- P A))) :compare :vanishes}})

(defn figure
  "A B C D, M the midpoint of AB, P = A + 0.3 (B - A) on AB."
  [[A B C D]]
  [A B C D (mapv #(/ (+ %1 %2) 2) A B) (mapv #(+ %1 (* 0.3 (- %2 %1))) A B)])

(def figure-gen
  (gen/fmap figure (gen/vector (gen/tuple (gen/fmap #(/ % 100.0) (gen/choose -50 50))
                                          (gen/fmap #(/ % 100.0) (gen/choose -50 50))) 4)))

(def chain
  "Klein's chain of the plane, as subgroups of PGL(3) given by generators."
  {:euclid [(gs/family [:rotation 0.7]) (gs/family [:translation 0.3 -0.2]) (gs/family [:reflection 0.4])]
   :similarity [(gs/family [:rotation 0.7]) (gs/family [:translation 0.3 -0.2]) (gs/family [:scale 1.6])]
   :affine [(gs/family [:rotation 0.7]) (gs/family [:shear 0.8]) (gs/family [:scale 1.3 0.7])]
   :projective [(gs/family [:rotation 0.7]) (gs/family [:shear 0.8]) [[1 0 0] [0 1 0] [0.3 -0.2 1]]]})

(defn plane-geometry [k]
  (inv/geometry {:name k :group gs/projective :generators (chain k) :properties plane-properties}))

(defgeometry-contract euclid (plane-geometry :euclid) figure-gen)
(defgeometry-contract affine (plane-geometry :affine) figure-gen)
(defgeometry-contract projective (plane-geometry :projective) figure-gen)

(def sample-figures (gen/sample figure-gen 8))

(deftest klein-chain
  (let [inv-of #(inv/invariants (plane-geometry %) sample-figures)]
    (is (= #{:length :angle :ratio :midpoint :collinear} (inv-of :euclid)))
    (is (= #{:angle :ratio :midpoint :collinear} (inv-of :similarity)))
    (is (= #{:midpoint :collinear} (inv-of :affine)))
    (is (= #{:collinear} (inv-of :projective)))
    (testing "each larger group keeps a subset (Klein's ordering)"
      (is (every? true? (for [[a b] (partition 2 1 [:euclid :similarity :affine :projective])]
                          (inv/finer? (plane-geometry a) (plane-geometry b) sample-figures)))))))

;; ---------------------------------------------------------------------------
;; The space forms: their motions keep their own distance and angle

(defn space-form-geometry [K]
  (let [G (kc/space-form K) M (kc/motions G)]
    (inv/geometry
     {:name (keyword (str "K" K)) :group M
      :generators [(gs/moebius-rotation 0.9) (gs/moebius-translation [0.2 -0.1]) (gs/moebius-translation [-0.05 0.3])]
      :properties {:distance {:measure (fn [[A B]] (kc/distance G A B)) :compare :equal}
                   :angle {:measure (fn [[A B C]] (kc/angle G C A B)) :compare :equal}
                   :euclid-length {:measure (fn [[A B]] (len (v- B A))) :compare :equal}}})))

(def disk-figure-gen
  (gen/vector (gen/tuple (gen/fmap #(/ % 100.0) (gen/choose -40 40)) (gen/fmap #(/ % 100.0) (gen/choose -40 40))) 3))

(defgeometry-contract lobachevsky (space-form-geometry -1) disk-figure-gen)
(defgeometry-contract spherical (space-form-geometry 1) disk-figure-gen)

(deftest space-forms
  (let [figs (gen/sample disk-figure-gen 8)]
    (is (= #{:distance :angle} (inv/invariants (space-form-geometry -1) figs))
        "Lobachevsky's motions keep his length and the angle, not Euclid's length")
    (is (= #{:distance :angle} (inv/invariants (space-form-geometry 1) figs)))
    (is (= #{:distance :angle :euclid-length} (inv/invariants (space-form-geometry 0) figs))
        "at K = 0 the motions are Euclid's and keep his length too")))
