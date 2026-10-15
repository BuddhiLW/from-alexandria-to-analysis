(ns alexandria.boole.logic
  "Boole's logic of classes (Laws of Thought, 1854) on alexandria.structure.
   boolring: the Aristotelian syllogism as elimination of the middle term.

   A categorical proposition on terms a, b is an equation (Boole XV. 3):
     A  All a are b      a (1 - b) = 0
     E  No a are b       a b = 0
   The particulars (I: some a are b, O: some a are not b) are not equations
   in this algebra but denials of one: a b != 0, a (1 - b) != 0. Boole writes
   them with an indefinite class v (v a = v b); here they are tested against
   models.

   Terms are indices into a vector [s p m] (minor, major, middle). A MODEL
   is a set of populated regions, a region being a bit vector over [s p m]:
   the 2^3 = 8 constituents. A proposition is true in a model when no
   populated region contradicts it (A, E) or some populated region witnesses
   it (I, O).

   A mood is valid when its conclusion is true in every model of its
   premises. EXISTENTIAL IMPORT: Aristotle's universals are read as if their
   subject classes were not empty; Boole's equations carry no such claim
   (y(1-x) = 0 holds when y = 0). A mood valid only when every term is
   populated is flagged :import."
  (:require [alexandria.structure.boolring :as b]
            [alexandria.structure.ring :as ring]))

(def two "The two-element Boolean ring {0, 1}: the algebra of one object." (b/boolring #{:t}))

(def forms [:A :E :I :O])

(def terms {0 "S" 1 "P" 2 "M"})

(defn- el [bit] (if (= 1 bit) #{:t} #{}))

(defn equation-fn
  "The function of the vector v of ring elements whose zero is the universal
   proposition: A a (1 - b); E a b. Nil for I and O."
  [form a b]
  (case form
    :A (fn [v] (ring/mul two (v a) (b/complement-of two (v b))))
    :E (fn [v] (ring/mul two (v a) (v b)))
    nil))

(defn equation-text
  "The proposition as Boole writes it."
  [form a b]
  (let [x (terms a) y (terms b)
        lc #(.toLowerCase ^String %)]
    (case form
      :A (str (lc x) "(1 - " (lc y) ") = 0")
      :E (str (lc x) (lc y) " = 0")
      :I (str (lc x) (lc y) " \u2260 0")
      :O (str (lc x) "(1 - " (lc y) ") \u2260 0"))))

(defn words
  "The proposition in words."
  [form a b]
  (let [x (terms a) y (terms b)]
    (case form
      :A (str "All " x " are " y)
      :E (str "No " x " are " y)
      :I (str "Some " x " are " y)
      :O (str "Some " x " are not " y))))

(defn true-in?
  "The proposition holds in the model (a set of populated regions, each a
   [s p m] bit vector)."
  [model form a b]
  (let [hit? (fn [pa pb] (some (fn [r] (and (= pa (r a)) (= pb (r b)))) model))]
    (case form
      :A (not (hit? 1 0))
      :E (not (hit? 1 1))
      :I (boolean (hit? 1 1))
      :O (boolean (hit? 1 0)))))

(def regions "The 8 constituents of s, p, m in counting order." (b/bit-vectors 3))

(def models "All 2^8 sets of populated regions." (reduce (fn [acc r] (into acc (map #(conj % r) acc))) [#{}] regions))

(defn- populated? [model t] (boolean (some #(= 1 (% t)) model)))

(defn- all-populated? [model] (every? #(populated? model %) [0 1 2]))

(defn premise-terms
  "The terms (a b) of the major and minor premise in figure 1..4 (the
   middle term M is the subject of the major in 1 and 3, ...):
     1  M-P, S-M    2  P-M, S-M    3  M-P, M-S    4  P-M, M-S."
  [figure]
  (case figure
    1 [[2 1] [0 2]]
    2 [[1 2] [0 2]]
    3 [[2 1] [2 0]]
    4 [[1 2] [2 0]]))

(defn mood
  "Validity of premises (f1 f2) in the figure with conclusion form fc on S
   and P (subject S, predicate P): {:name \"AAA\" :figure :premises
   :conclusion :valid? :import?}. :valid? is true when the conclusion is
   true in all models of the premises; :import? when it holds only among
   models in which S, P and M are all populated."
  [figure f1 f2 fc]
  (let [[[a1 b1] [a2 b2]] (premise-terms figure)
        prem? (fn [m] (and (true-in? m f1 a1 b1) (true-in? m f2 a2 b2)))
        ms (filter prem? models)
        ms-imp (filter all-populated? ms)
        ok? (fn [coll] (every? #(true-in? % fc 0 1) coll))]
    {:name (str (name f1) (name f2) (name fc))
     :figure figure
     :premises [{:form f1 :a a1 :b b1 :words (words f1 a1 b1) :eq (equation-text f1 a1 b1)}
                {:form f2 :a a2 :b b2 :words (words f2 a2 b2) :eq (equation-text f2 a2 b2)}]
     :conclusion {:form fc :words (words fc 0 1)}
     :valid? (ok? ms)
     :import? (and (not (ok? ms)) (boolean (seq ms-imp)) (ok? ms-imp))}))

(defn all-moods
  "All 4 * 16 * 4 = 256 moods (figure, two premises, conclusion S-P)."
  []
  (vec (for [fig [1 2 3 4] f1 forms f2 forms fc forms] (mood fig f1 f2 fc))))

(defn tally
  "{:valid n :import n :invalid n} over moods."
  [moods]
  {:valid (count (filter :valid? moods))
   :import (count (filter :import? moods))
   :invalid (count (remove #(or (:valid? %) (:import? %)) moods))})

;; ---------------------------------------------------------------------------
;; Elimination of the middle term, for two universal premises

(defn empty-regions-words
  "Words for a constituent of (s, p) found empty by the residual equation."
  [[bs bp]]
  (case [bs bp]
    [1 0] "All S are P"
    [1 1] "No S are P"
    [0 1] "All P are S"
    [0 0] "Everything is S or P"))

(defn eliminate-middle
  "For two universal premises (A or E) in the figure: combine the equations
   v1 = 0, v2 = 0 into v1 + v2 + v1 v2 = 0 (the union: either vanishing
   part is a failure), eliminate m by f(1) f(0) = 0 (Boole VII. Prop. I),
   develop the residue in the constituents of s and p, and read the
   constituents whose coefficient is 1 as classes that do not exist.
   Returns nil when a premise is particular, else
   {:residual [{:bits :coef} ..] :empty [bits ..] :conclusions [words ..]}."
  [figure f1 f2]
  (let [[[a1 b1] [a2 b2]] (premise-terms figure)
        e1 (equation-fn f1 a1 b1) e2 (equation-fn f2 a2 b2)]
    (when (and e1 e2)
      (let [f (fn [v] (b/join-in two (e1 v) (e2 v)))
            g (b/eliminate two f 2)                         ; the third variable m
            dev (b/develop two (fn [bits] (g bits)) 2)
            one? #(ring/same? two (:coef %) (ring/one two))
            empty (mapv :bits (filter one? dev))]
        {:residual dev :empty empty :conclusions (mapv empty-regions-words empty)}))))

(defn truth-table
  "The 2^3 assignments of s p m (0/1) with the value of each premise's
   left-hand side (0 = the equation holds), and whether the conclusion
   equation holds; for two universal premises and a universal conclusion.
   Verification: on every row where both premises hold the conclusion holds."
  [figure f1 f2 fc]
  (let [[[a1 b1] [a2 b2]] (premise-terms figure)
        e1 (equation-fn f1 a1 b1) e2 (equation-fn f2 a2 b2) ec (equation-fn fc 0 1)]
    (when (and e1 e2 ec)
      (let [val (fn [e v] (if (b/holds? two e v) 0 1))]
        (mapv (fn [bits]
                (let [v (mapv el bits)
                      p1 (val e1 v) p2 (val e2 v) c (val ec v)]
                  {:bits bits :premise-1 p1 :premise-2 p2 :conclusion c
                   :premises-hold? (and (zero? p1) (zero? p2))
                   :ok? (or (not (and (zero? p1) (zero? p2))) (zero? c))}))
              regions)))))
