(ns alexandria.structure.graph-contract
  "The conformance suite of alexandria.structure.graph on generated
   multigraphs (2 to 5 vertices, 0 to 2 edges per pair): the handshake
   lemma, Kirchhoff's determinant against the trees listed by brute force,
   Euler's criterion against an exhaustive search for a walk, the witness
   walk, the raster readout against the functions, Kempe swaps, and the
   automorphisms obeying the group contract."
  (:require [alexandria.structure.graph :as gr]
            [alexandria.structure.group :as g]
            [alexandria.structure.group-contract :refer [defgroup-contract]]
            [alexandria.structure.tie :as tie]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(def multigraph
  "[n counts graph]: n vertices 0..n-1, counts the multiplicity per pair."
  (gen/bind (gen/choose 2 5)
            (fn [n]
              (gen/fmap (fn [cs] [n cs (gr/from-counts (range n) (zipmap (gr/pairs n) cs))])
                        (gen/vector (gen/frequency [[3 (gen/return 0)] [3 (gen/return 1)] [1 (gen/return 2)]])
                                    (count (gr/pairs n)))))))

(def small-multigraph
  "Few enough edges for the exhaustive walk search."
  (gen/such-that (fn [[_ _ G]] (<= (gr/edge-count G) 8)) multigraph 100))

(defspec handshake 80
  (prop/for-all [[_ _ G] multigraph]
    (and (= (reduce + (vals (gr/degrees G))) (* 2 (gr/edge-count G)))
         (even? (count (gr/odd-vertices G))))))

(defspec kirchhoff-against-brute-force 60
  (prop/for-all [[_ _ G] multigraph]
    (= (gr/tree-count G) (count (gr/spanning-trees G)))))

(defspec cycle-rank-is-edges-beyond-a-forest 60
  (prop/for-all [[_ _ G] multigraph]
    (= (gr/cycle-rank G)
       (- (gr/edge-count G) (- (gr/vertex-count G) (count (gr/components G)))))))

(defspec euler-criterion-against-search 60
  (prop/for-all [[_ _ G] small-multigraph]
    (let [{c :case} (gr/euler-case G)
          found (gr/trail-search G)]
      (and (= (boolean (#{:circuit :path :empty} c)) (some? found))
           (or (not (#{:circuit :path} c)) (gr/walk? G (gr/euler-walk G)))
           (or (not= :circuit c) (let [w (gr/euler-walk G)] (= (first (first w)) (peek (peek w)))))))))

(defspec readout-agrees-with-the-functions 60
  (prop/for-all [[n cs G] multigraph]
    (let [v (gr/pair-values n cs)
          near (fn [x y] (< (Math/abs (- (double x) (double y))) 1e-9))]
      (and (near (:odd v) (count (gr/odd-vertices G)))
           (near (:E v) (gr/edge-count G))
           (near (:pieces v) (count (gr/pieces G)))
           (near (:components v) (count (gr/components G)))
           (near (:rank v) (gr/cycle-rank G))
           (near (:trees v) (gr/tree-count G))
           (= (name (tie/case-of gr/euler-cases v)) (name (:case (gr/euler-case G))))))))

(defspec kempe-swap-keeps-a-proper-colouring 60
  (prop/for-all [[_ _ G] multigraph c1 (gen/choose 0 2) c2 (gen/choose 0 2)]
    (let [col (gr/greedy-colouring G) v (first (:vertices G))]
      (and (gr/proper? G col)
           (gr/proper? G (gr/kempe-swap G col v c1 c2))))))

(defspec automorphisms-closed 40
  (prop/for-all [[_ _ G] multigraph]
    (let [A (gr/automorphism-group G) els (gr/automorphisms G)]
      (and (every? #(gr/automorphism? G %) els)
           (every? (fn [s] (every? (fn [t] (some #(g/same? A % (g/compose A s t)) els)) els)) els)))))

(def petersen-like
  "The 5-cycle with one chord: Aut has order 2."
  (gr/from-pairs (range 5) [[0 1] [1 2] [2 3] [3 4] [4 0] [0 2]]))

(defgroup-contract cycle-automorphisms (gr/automorphism-group (gr/from-pairs (range 5) [[0 1] [1 2] [2 3] [3 4] [4 0]]))
  (gen/elements (gr/automorphisms (gr/from-pairs (range 5) [[0 1] [1 2] [2 3] [3 4] [4 0]])))
  (gen/choose 0 4) =)

(deftest plane-faces
  (testing "a fan in convex position: V - E + F = 2"
    (doseq [n (range 3 8)]
      (let [at (into {} (for [i (range n)] [i [(Math/cos (* 2 Math/PI (/ i n))) (Math/sin (* 2 Math/PI (/ i n)))]]))
            G (gr/embed (gr/from-pairs (range n) (concat (for [i (range n)] [i (mod (inc i) n)])
                                                         (for [j (range 2 (dec n))] [0 j])))
                        at)]
        (is (= (+ 1 (- n 2)) (count (gr/faces G))) "the fan cuts the polygon into n - 2 triangles, plus the outside")
        (is (= 2 (gr/euler-characteristic G))))))
  (testing "Koenigsberg as Euler drew it: 4 points, 7 lines, 5 faces"
    (let [K (gr/graph {:vertices [:A :B :C :D]
                       :edges [{:id :a :ends [:A :B]} {:id :b :ends [:A :B]} {:id :c :ends [:A :C]} {:id :d :ends [:A :C]}
                               {:id :e :ends [:A :D]} {:id :f :ends [:B :D]} {:id :g :ends [:C :D]}]
                       :at {:A [-1 0] :B [0 2] :C [0 -2] :D [2.4 0]}})]
      (is (= 5 (count (gr/faces K))))
      (is (= 2 (gr/euler-characteristic K))))))

(deftest cayley-and-symbols
  (is (= [1 1 3 16 125 1296] (map (comp gr/tree-count gr/complete) (range 1 7))))
  (is (= [1 3 16 125 1296]
         (map (fn [k] (Math/round (double (:trees (gr/pair-values 6 (vec (for [[i j] (gr/pairs 6)] (if (and (< i k) (< j k)) 1 0))) true)))))
              (range 2 7))))
  (is (= 2 (:ok (gr/automorphism-order petersen-like)))))

(deftest hamilton-dodecahedron
  (let [D (gr/dodecahedron)]
    (is (every? #{3} (vals (gr/degrees D))))
    (is (= 30 (gr/edge-count D)))
    (is (= 12 (count (gr/faces D))) "twelve pentagons, the outer one included")
    (is (= 2 (gr/euler-characteristic D)))
    (is (every? #(= 5 (count %)) (gr/faces D)))
    (is (gr/hamiltonian-path? D (gr/hamiltonian-cycle D) true))
    (is (= :none (:case (gr/euler-case D))) "twenty odd vertices: no walk over every edge once")
    (is (= 120 (count (gr/automorphisms D))) "the full symmetry group of the dodecahedron")))
