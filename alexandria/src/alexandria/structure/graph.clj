(ns alexandria.structure.graph
  "Domain stratum: a finite multigraph as a validated value, and the
   operations every graph gets.

   A graph is DATA, a closed shape (malli `Graph`):
     {:vertices [k ...]                    vertex keys, in order
      :edges    [{:id id :ends [u v]} ...] one map per edge, so parallel
                                           edges (Euler's two bridges a, b
                                           from A to B) are two maps
      :at       {k [x y]}}                 optional plane embedding
   Loops are refused: none of the eras draws one.

   The algorithms are functions over that value (closed set of shapes,
   open set of questions): degree, degree-sequence, components, the Euler
   case and its witness walk (Hierholzer), the Laplacian and Kirchhoff's
   spanning-tree count as an Emmy determinant (exact on numbers, a
   polynomial on symbols), the spanning trees listed, the cycle rank
   E - V + c, the faces of a plane embedding and V - E + F, proper and
   greedy colourings, Kempe chains and their swap, Hamiltonian paths and
   cycles, and the automorphism group as an instance of
   alexandria.structure.group/Group and Action.

   Renderers are open: `render` is a multimethod on a target keyword
   (media namespaces add :mafs, :svg ...).

   The raster face: `pair-readout` is the alexandria.structure.tie readout
   of a multigraph on n vertices whose edge multiplicities are the
   parameters (one per pair): degrees, odd vertices, edges, pieces, cycle
   rank and the tree count, branch-free Emmy arithmetic that raster
   compiles. `euler-cases` names the case as data.

   Conformance: test/alexandria/structure/graph_contract.clj (handshake
   lemma, Kirchhoff against brute force, Euler's criterion against search,
   automorphisms a group) on generated multigraphs."
  (:require [alexandria.structure.arith :as a]
            [alexandria.structure.group :as g]
            [emmy.env :as e]
            [hive-dsl.result :as r]
            [malli.core :as m]))

;; ---------------------------------------------------------------------------
;; The value

(def Graph
  [:and
   [:map
    [:vertices [:vector any?]]
    [:edges [:vector [:map [:id any?] [:ends [:tuple any? any?]]]]]
    [:at {:optional true} [:map-of any? [:tuple number? number?]]]]
   [:fn {:error/message "edge ends must be distinct vertices, edge ids unique"}
    (fn [{:keys [vertices edges]}]
      (let [vs (set vertices)]
        (and (= (count vs) (count vertices))
             (every? (fn [{[u v] :ends}] (and (vs u) (vs v) (not= u v))) edges)
             (= (count (set (map :id edges))) (count edges)))))]])

(defn graph
  "A graph value, validated (malli): {:vertices :edges [:at]}."
  [m]
  (m/assert Graph m)
  m)

(defn from-pairs
  "A graph from vertices and [u v] pairs, the edges numbered 0, 1, ...
   (or named by ids)."
  ([vertices pairs] (from-pairs vertices pairs (range)))
  ([vertices pairs ids]
   (graph {:vertices (vec vertices)
           :edges (mapv (fn [id [u v]] {:id id :ends [u v]}) ids pairs)})))

(defn from-counts
  "A multigraph from {[u v] multiplicity}; edge ids [u v j]."
  [vertices counts]
  (graph {:vertices (vec vertices)
          :edges (vec (for [[[u v] c] (sort-by (comp str key) counts) j (range c)]
                        {:id [u v j] :ends [u v]}))}))

(defn complete
  "K_n on the vertices 0..n-1."
  [n]
  (from-pairs (range n) (for [i (range n) j (range (inc i) n)] [i j])))

(defn vertex-count [G] (count (:vertices G)))
(defn edge-count [G] (count (:edges G)))
(defn embed "G with the plane embedding at {k [x y]}." [G at] (graph (assoc G :at at)))

(defn without-edge "G less the edge id." [G id] (update G :edges (fn [es] (filterv #(not= id (:id %)) es))))
(defn with-edge "G plus an edge id from u to v." [G id u v] (graph (update G :edges conj {:id id :ends [u v]})))

;; ---------------------------------------------------------------------------
;; Degrees

(defn degrees "{vertex degree}: the edges meeting it, each parallel edge counted." [G]
  (reduce (fn [m {[u v] :ends}] (-> m (update u inc) (update v inc)))
          (zipmap (:vertices G) (repeat 0)) (:edges G)))

(defn degree [G v] ((degrees G) v))
(defn degree-sequence "The degrees, largest first." [G] (vec (sort > (vals (degrees G)))))
(defn odd-vertices "The vertices of odd degree, in vertex order." [G]
  (let [ds (degrees G)] (filterv (comp odd? ds) (:vertices G))))

(defn neighbours "{vertex [[edge-id other] ...]}, edges in order." [G]
  (reduce (fn [m {:keys [id] [u v] :ends}] (-> m (update u conj [id v]) (update v conj [id u])))
          (zipmap (:vertices G) (repeat [])) (:edges G)))

(defn adjacent? [G u v] (boolean (some (fn [{[a b] :ends}] (= #{a b} #{u v})) (:edges G))))

;; ---------------------------------------------------------------------------
;; Connection

(defn components
  "The connected components, each a vector of vertices in vertex order,
   in the order of their first vertex."
  [G]
  (let [nb (neighbours G)]
    (loop [left (:vertices G) seen #{} out []]
      (if-let [s (first (remove seen left))]
        (let [comp (loop [stack [s] c #{s}]
                     (if-let [x (peek stack)]
                       (let [new (remove c (map second (nb x)))]
                         (recur (into (pop stack) new) (into c new)))
                       c))]
          (recur left (into seen comp) (conj out (filterv comp (:vertices G)))))
        out))))

(defn connected? [G] (<= (count (components G)) 1))

(defn pieces
  "The components that carry an edge (isolated vertices do not count for
   Euler: a region no bridge reaches is not walked)."
  [G]
  (let [ds (degrees G)] (filterv #(pos? (ds (first %))) (components G))))

(defn cycle-rank
  "E - V + c: the number of independent cycles (Kirchhoff's count of
   independent equations from his first law)."
  [G]
  (+ (- (edge-count G) (vertex-count G)) (count (components G))))

;; ---------------------------------------------------------------------------
;; Euler (E53 par. 20)

(defn euler-case
  "Euler's rule: {:case :empty | :disconnected | :circuit | :path | :none,
   :odd [vertices]}. :path names its two ends."
  [G]
  (let [odd (odd-vertices G)]
    {:case (cond (zero? (edge-count G)) :empty
                 (> (count (pieces G)) 1) :disconnected
                 (empty? odd) :circuit
                 (= 2 (count odd)) :path
                 :else :none)
     :odd odd}))

(defn euler-walk
  "The witness: a walk [[from edge-id to] ...] crossing every edge once
   (Hierholzer 1873), from an odd vertex when there are two, else from the
   first vertex with an edge; nil when Euler's rule says none."
  [G]
  (let [{c :case odd :odd} (euler-case G)]
    (when (#{:circuit :path} c)
      (let [nb (neighbours G)
            start (or (first odd) (first (first (pieces G))))]
        (loop [used #{} stack [[start nil nil]] out []]
          (if-let [[v :as top] (peek stack)]
            (if-let [[id w] (first (remove (comp used first) (nb v)))]
              (recur (conj used id) (conj stack [w id v]) out)
              (recur used (pop stack) (conj out top)))
            (->> (rseq out) rest (mapv (fn [[to id from]] [from id to])))))))))

(defn walk?
  "True when steps [[from id to] ...] is a walk of G crossing every edge
   exactly once."
  [G steps]
  (let [ends (into {} (map (juxt :id (comp set :ends))) (:edges G))]
    (and (= (count steps) (edge-count G))
         (or (empty? steps) (apply distinct? (map second steps)))
         (every? (fn [[u id v]] (= (ends id) #{u v})) steps)
         (every? (fn [[[_ _ v] [u]]] (= v u)) (partition 2 1 steps)))))

(defn trail-search
  "Euler's question answered the long way: some walk crossing every edge
   once, found by trying every walk; nil when there is none."
  [G]
  (let [nb (neighbours G) n (edge-count G)]
    (letfn [(go [v used path]
              (if (= n (count used))
                path
                (some (fn [[id w]] (when-not (used id) (go w (conj used id) (conj path [v id w]))))
                      (nb v))))]
      (if (zero? n) [] (some #(go % #{} []) (:vertices G))))))

;; ---------------------------------------------------------------------------
;; Kirchhoff (1847): the Laplacian and the tree count

(defn laplacian
  "The Laplacian D - A as rows of Emmy values; (weight edge) gives each
   edge its weight (default 1), so symbols give Kirchhoff's polynomial."
  ([G] (laplacian G (constantly 1)))
  ([G weight]
   (let [vs (:vertices G) ix (zipmap vs (range)) n (count vs)
         z (vec (repeat n (vec (repeat n 0))))]
     (reduce (fn [L {[u v] :ends :as edge}]
               (let [i (ix u) j (ix v) w (weight edge)]
                 (-> L (update-in [i i] e/+ w) (update-in [j j] e/+ w)
                     (update-in [i j] e/- w) (update-in [j i] e/- w))))
             z (:edges G)))))

(defn minor
  "Rows with row and column k struck out."
  [rows k]
  (let [drop-k (fn [v] (into (subvec v 0 k) (subvec v (inc k))))]
    (mapv drop-k (drop-k rows))))

(defn det
  "The determinant of rows (Emmy values), by cofactors along the first
   row: exact on integers, a polynomial on symbols, branch-free for
   raster. 1 for the empty matrix."
  [rows]
  (case (count rows)
    0 1
    1 (ffirst rows)
    (reduce e/+ (map-indexed (fn [j x] (e/* (if (even? j) 1 -1) x (det (mapv #(into (subvec % 0 j) (subvec % (inc j))) (rest rows)))))
                             (first rows)))))

(defn tree-count
  "Kirchhoff's matrix-tree theorem: the spanning trees of G (weighted:
   the sum over trees of the product of weights), the determinant of the
   Laplacian with the row and column of the first vertex struck out."
  ([G] (tree-count G (constantly 1)))
  ([G weight] (if (zero? (vertex-count G)) 0 (det (minor (laplacian G weight) 0)))))

(defn- forest? [vs edges]
  (let [parent (atom (zipmap vs vs))
        find (fn find [x] (let [p (@parent x)] (if (= p x) x (find p))))]
    (every? (fn [{[u v] :ends}]
              (let [a (find u) b (find v)]
                (when (not= a b) (swap! parent assoc a b) true)))
            edges)))

(defn- choose [k xs]
  (cond (zero? k) [[]]
        (empty? xs) []
        :else (concat (map #(cons (first xs) %) (choose (dec k) (rest xs))) (choose k (rest xs)))))

(defn spanning-trees
  "Every spanning tree, as a vector of edge ids (brute force: the V - 1
   edge sets without a cycle). Small graphs only."
  [G]
  (let [n (vertex-count G)]
    (if (zero? n) []
        (->> (choose (dec n) (:edges G))
             (filter #(forest? (:vertices G) %))
             (mapv #(mapv :id %))))))

;; ---------------------------------------------------------------------------
;; Plane embeddings: faces and V - E + F

(defn- angle [[x y] [p q]] (Math/atan2 (- q y) (- p x)))

(defn- darts
  "Each edge as two darts [from id to], with the angle it leaves its tail
   at; parallel edges bow apart (the j-th of c pushed (j - (c-1)/2) to
   the left of u -> v), so they leave u in that order and v reversed."
  [{:keys [edges at]}]
  (let [groups (group-by (fn [{[u v] :ends}] (set [u v])) edges)]
    (for [[_ es] groups
          [j {:keys [id] [u v] :ends}] (map-indexed vector es)
          :let [[u0 v0] (:ends (first es))
                off (* 1e-3 (- j (/ (dec (count es)) 2.0)))
                off (if (= [u v] [u0 v0]) off (- off))]
          d [[u id v (+ (angle (at u) (at v)) off)] [v id u (- (angle (at v) (at u)) off)]]]
      d)))

(defn faces
  "The faces of the plane embedding (G needs :at), each a vector of darts
   [from id to] going round it with the face on the left; the outer face
   included. Traced by the rotation at each vertex (the next dart
   clockwise after the reverse)."
  [G]
  (let [ds (darts G)
        out (group-by first ds)
        rot (into {} (for [[v xs] out] [v (vec (sort-by last xs))]))
        next-dart (fn [[u id v]]
                    ;; arriving at v along id: leave by the dart just clockwise of (v id u)
                    (let [xs (rot v) i (first (keep-indexed (fn [i [_ id' w]] (when (and (= id id') (= w u)) i)) xs))
                          [a b c] (nth xs (mod (dec i) (count xs)))]
                      [a b c]))]
    (loop [left (set (map #(vec (take 3 %)) ds)) out []]
      (if-let [d0 (first (sort-by str left))]
        (let [face (loop [d d0 acc []] (let [acc (conj acc d) n (next-dart d)] (if (= n d0) acc (recur n acc))))]
          (recur (reduce disj left face) (conj out face)))
        out))))

(defn euler-characteristic
  "V - E + F of a plane embedding; 1 + c for c pieces (2 when connected)."
  [G]
  (+ (- (vertex-count G) (edge-count G))
     (count (faces (update G :vertices (fn [vs] (filterv (set (map first (darts G))) vs)))))
     (count (filter #(zero? ((degrees G) %)) (:vertices G)))))

;; ---------------------------------------------------------------------------
;; Colourings and Kempe chains (1879)

(defn proper?
  "colouring {vertex colour} gives every vertex a colour and no edge two
   ends of one colour."
  [G colouring]
  (and (every? #(contains? colouring %) (:vertices G))
       (every? (fn [{[u v] :ends}] (not= (colouring u) (colouring v))) (:edges G))))

(defn clashes "The edges whose ends share a colour." [G colouring]
  (filterv (fn [{[u v] :ends}] (and (some? (colouring u)) (= (colouring u) (colouring v)))) (:edges G)))

(defn greedy-colouring
  "Colours 0, 1, ... given in vertex order (or `order`): each vertex the
   least colour no earlier neighbour has."
  ([G] (greedy-colouring G (:vertices G)))
  ([G order]
   (let [nb (neighbours G)]
     (reduce (fn [col v]
               (let [taken (set (keep (comp col second) (nb v)))]
                 (assoc col v (first (remove taken (range))))))
             {} order))))

(defn kempe-chain
  "Kempe's chain: the vertices reachable from v through vertices coloured
   c1 or c2 (v's own colour one of them), in vertex order; empty when
   c1 = c2 or v has neither colour."
  [G colouring v c1 c2]
  (let [nb (neighbours G) ok? #(contains? #{c1 c2} (colouring %))]
    (if (or (= c1 c2) (not (ok? v))) []
        (let [reach (loop [stack [v] seen #{v}]
                      (if-let [x (peek stack)]
                        (let [new (filter #(and (ok? %) (not (seen %))) (map second (nb x)))]
                          (recur (into (pop stack) new) (into seen new)))
                        seen))]
          (filterv reach (:vertices G))))))

(defn kempe-swap
  "The colouring with c1 and c2 exchanged on the Kempe chain of v; proper
   stays proper."
  [G colouring v c1 c2]
  (reduce (fn [col x] (update col x {c1 c2 c2 c1})) colouring (kempe-chain G colouring v c1 c2)))

;; ---------------------------------------------------------------------------
;; Hamilton (1857): every vertex once

(defn dodecahedron
  "The dodecahedron's graph, Hamilton's icosian board: vertices 0..19, an
   outer pentagon 0-4, a middle ring of ten 5-14, an inner pentagon 15-19;
   outer k joins middle 5 + 2k, middle 6 + 2k joins inner 15 + k. :at is
   the Schlegel diagram (the solid seen through one face)."
  []
  (let [ring (fn [o n k] (+ o (mod k n)))
        polar (fn [r t] [(* r (Math/sin t)) (* r (Math/cos t))])
        at (merge (into {} (for [k (range 5)] [k (polar 3.0 (* k 0.4 Math/PI))]))
                  (into {} (for [k (range 10)] [(+ 5 k) (polar (if (even? k) 2.0 1.6) (* k 0.2 Math/PI))]))
                  (into {} (for [k (range 5)] [(+ 15 k) (polar 0.8 (* (+ (* 2 k) 1) 0.2 Math/PI))])))]
    (embed (from-pairs (range 20)
                       (concat (for [k (range 5)] [(ring 0 5 k) (ring 0 5 (inc k))])
                               (for [k (range 5)] [k (+ 5 (* 2 k))])
                               (for [k (range 10)] [(+ 5 k) (ring 5 10 (inc k))])
                               (for [k (range 5)] [(+ 6 (* 2 k)) (+ 15 k)])
                               (for [k (range 5)] [(ring 15 5 k) (ring 15 5 (inc k))])))
           at)))

(defn hamiltonian-path?
  "path (vertices) visits every vertex once along edges; with cycle? its
   last vertex is joined to its first."
  ([G path] (hamiltonian-path? G path false))
  ([G path cycle?]
   (and (= (count path) (vertex-count G))
        (= (set path) (set (:vertices G)))
        (every? (fn [[u v]] (adjacent? G u v)) (partition 2 1 path))
        (or (not cycle?) (adjacent? G (peek (vec path)) (first path))))))

(defn hamiltonian-cycle
  "A closed path through every vertex once, from the first vertex (or
   start), or nil (backtracking)."
  ([G] (hamiltonian-cycle G (first (:vertices G))))
  ([G start]
   (let [nb (into {} (map (fn [[k xs]] [k (distinct (map second xs))])) (neighbours G))
         n (vertex-count G)]
     (letfn [(go [path seen]
               (if (= n (count path))
                 (when (adjacent? G (peek path) start) path)
                 (some #(when-not (seen %) (go (conj path %) (conj seen %))) (nb (peek path)))))]
       (go [start] #{start})))))

;; ---------------------------------------------------------------------------
;; Automorphisms: a group (alexandria.structure.group)

(defn- multiplicities [G]
  (let [ix (zipmap (:vertices G) (range))]
    (frequencies (map (fn [{[u v] :ends}] (set [(ix u) (ix v)])) (:edges G)))))

(defn automorphism?
  "p (a vector, vertex index i -> index (p i)) keeps every multiplicity."
  [G p]
  (let [mu (multiplicities G)]
    (every? (fn [[s c]] (= c (mu (set (map p s))))) mu)))

(defn automorphisms
  "Every automorphism of G as a permutation vector of vertex indices
   (backtracking, degrees and multiplicities checked as it goes)."
  [G]
  (let [n (vertex-count G) mu (multiplicities G)
        ds (mapv (degrees G) (:vertices G))
        mult (fn [i j] (get mu #{i j} 0))]
    (letfn [(go [p used]
              (let [i (count p)]
                (if (= i n) [p]
                    (mapcat (fn [j] (when (and (not (used j)) (= (ds i) (ds j))
                                               (every? (fn [k] (= (mult k i) (mult (p k) j))) (range i)))
                                      (go (conj p j) (conj used j))))
                            (range n)))))]
      (vec (go [] #{})))))

(defrecord Automorphisms [G]
  g/Group
  (unit [_] (vec (range (vertex-count G))))
  (compose [_ s t] (mapv #(nth s (nth t %)) (range (count t))))
  (inverse [_ s] (reduce (fn [acc i] (assoc acc (nth s i) i)) (vec (range (count s))) (range (count s))))
  (same? [_ s t] (= s t))
  g/Action
  (act [_ s v] (let [vs (:vertices G)] (nth vs (nth s (.indexOf ^java.util.List vs v))))))

(defn automorphism-group "Aut(G) acting on the vertices of G." [G] (->Automorphisms G))

(defn vertex-orbits
  "The orbits of Aut(G) on the vertices (structure.group/orbit), each in
   vertex order, in the order of their first vertex."
  [G]
  (let [A (automorphism-group G) els (automorphisms G)]
    (vec (distinct (map (fn [v] (filterv (set (g/orbit A els v =)) (:vertices G))) (:vertices G))))))

(defn automorphism-order
  "|Aut(G)|, the generated group closed up by structure.group/closure (a
   Result)."
  [G]
  (let [res (g/closure (automorphism-group G) (automorphisms G) 100000)]
    (if (r/ok? res) (r/ok (count (:ok res))) res)))

;; ---------------------------------------------------------------------------
;; The raster face: a multigraph on n vertices with its multiplicities as
;; parameters, and Euler's rule as named cases

(defn pairs "The pairs [i j], i < j, of n vertices, in the kernel's order." [n]
  (vec (for [i (range n) j (range (inc i) n)] [i j])))

(defn- pair-matrix [n cs]
  (let [ix (zipmap (pairs n) cs)]
    (vec (for [i (range n)] (vec (for [j (range n)] (if (= i j) 0 (ix (if (< i j) [i j] [j i])))))))))

(defn pair-values
  "{output value}: the readout of the multigraph on n vertices with
   multiplicity (nth cs p) on pair p (Emmy values: numbers, symbols or
   raster). :deg/i degrees, :odd/i 1 for an odd degree else 0 (as
   (1 - cos(pi d))/2), :odd the number of odd vertices, :E edges, :reach/i
   whether vertex i carries an edge, :pieces the components carrying an
   edge, :components all components, :rank E - V + components, :trees
   Kirchhoff's determinant (with pad? an edgeless vertex counts as a
   point on its own, so K_k inside n vertices gives k^(k-2)), :L/i/j the
   reduced Laplacian. Components: R = clamp((I + A)^(n-1)) is 1 where two
   vertices are joined by a chain; a component of s vertices contributes
   s times 1/s."
  ([n cs] (pair-values n cs false))
  ([n cs pad?]
   (let [C (pair-matrix n cs)
         deg (mapv #(reduce e/+ 0 %) C)
         par (mapv (fn [d] (e/* 0.5 (e/- 1 (e/cos (e/* Math/PI d))))) deg)
         on (mapv #(a/clamp % 0 1) deg)
         A1 (vec (for [i (range n)] (vec (for [j (range n)] (if (= i j) 1 (a/clamp ((C i) j) 0 1))))))
         mm (fn [X Y] (vec (for [i (range n)] (vec (for [j (range n)] (reduce e/+ (for [k (range n)] (e/* ((X i) k) ((Y k) j)))))))))
         R (mapv #(mapv (fn [x] (a/clamp x 0 1)) %) (reduce mm A1 (repeat (max 0 (- n 2)) A1)))
         size (mapv #(reduce e/+ 0 %) R)
         comps (reduce e/+ 0 (map #(e/divide 1 %) size))
         pieces (reduce e/+ 0 (map (fn [i] (e/divide (on i) (a/at-least (reduce e/+ 0 (map e/* (R i) on)) 1))) (range n)))
         E (reduce e/+ 0 cs)
         L (vec (for [i (range n)] (vec (for [j (range n)]
                                         (if (= i j)
                                           (if pad? (e/+ (deg i) (e/- 1 (on i))) (deg i))
                                           (e/- 0 ((C i) j)))))))
         Lr (minor L 0)]
     (merge {:odd (reduce e/+ 0 par) :E E :pieces pieces :components comps
             :rank (e/+ (e/- E n) comps) :trees (det Lr)}
            (into {} (map-indexed (fn [i d] [(keyword "deg" (str i)) d]) deg))
            (into {} (map-indexed (fn [i d] [(keyword "odd" (str i)) d]) par))
            (into {} (map-indexed (fn [i d] [(keyword "reach" (str i)) d]) on))
            (into {} (for [i (range (dec n)) j (range (dec n))] [(keyword "L" (str i "-" j)) ((Lr i) j)]))))))

(defn pair-trees
  "Kirchhoff's count alone for the multigraph on n vertices with
   multiplicities cs (cheaper than `pair-values` for larger n): with pad?
   an edgeless vertex counts as a point on its own."
  [n cs pad?]
  (let [C (pair-matrix n cs)
        deg (mapv #(reduce e/+ 0 %) C)
        on (mapv #(a/clamp % 0 1) deg)]
    (det (minor (vec (for [i (range n)]
                       (vec (for [j (range n)]
                              (if (= i j)
                                (if pad? (e/+ (deg i) (e/- 1 (on i))) (deg i))
                                (e/- 0 ((C i) j)))))))
                0))))

(defn pair-readout
  "The alexandria.structure.tie readout of `pair-values`: {:outputs :f},
   f taking the n(n-1)/2 multiplicities."
  ([n] (pair-readout n false))
  ([n pad?]
   {:outputs (vec (concat [:odd :E :pieces :components :rank :trees]
                          (map #(keyword "deg" (str %)) (range n))
                          (map #(keyword "odd" (str %)) (range n))
                          (map #(keyword "reach" (str %)) (range n))
                          (for [i (range (dec n)) j (range (dec n))] (keyword "L" (str i "-" j)))))
    :f (fn [& cs] (pair-values n (vec cs) pad?))}))

(def euler-cases
  "Euler's rule (E53 par. 20) as named cases on the readout: no edges, the
   edges in more than one piece, every degree even (a circuit), two odd
   (a path between them), more (none)."
  {:cases [[:empty [:< :E 0.5]]
           [:disconnected [:> :pieces 1.5]]
           [:circuit [:< :odd 0.5]]
           [:path [:< :odd 2.5]]
           [:none :else]]})

;; ---------------------------------------------------------------------------
;; Renderers: open

(defmulti render
  "Draw G for a target (:mafs, :svg, ...), opts per target. Open: media
   namespaces add methods."
  (fn [target _G _opts] target))
