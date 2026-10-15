(ns alexandria.graph-theory.bridges
  "The graph-theory strand, first notebook (Euler 1736 to Kempe 1879): the
   special cases of alexandria.structure.graph it applies.

     koenigsberg   E53's multigraph (Euler's letters A-D, bridges a-g)
                   with the embedding of the euler playground
     bridge-cases  structure.tie named cases of the readout, Euler's rule
                   (structure.graph/euler-cases) in his words
     walk-form     Hierholzer's walk as a quoted form the page runs; the
                   same form is evaluated on the JVM and checked against
                   structure.graph/walk?
     trees-form    the spanning trees listed, likewise checked against
                   structure.graph/spanning-trees
     prufer-form   a labelled tree from its Pruefer code (1918), the
                   bijection behind Cayley's n^(n-2)
     cayley-tie    n <-> E = n(n-1)/2 <-> cycle rank (n-1)(n-2)/2 of K_n,
                   a structure.tie :pivot
     icosian       the dodecahedron with Hamilton's twenty consonants
     kempe-map     a district of four neighbours, Kempe's chain swap as
                   steps (structure.graph/kempe-chain, kempe-swap)
     graded        the claims, each an exact difference that is 0"
  (:require [alexandria.grade :as grade]
            [alexandria.structure.graph :as gr]
            [alexandria.structure.tie :as tie]
            [emmy.env :as e]
            [hive-dsl.result :as r]
            [alexandria.structure.arith :as a]))

;; ---------------------------------------------------------------------------
;; 1. Euler 1736

(def regions ["A" "B" "C" "D"])

(def koenigsberg
  "E53 par. 2: A the Kneiphof, B and C the banks, D between the branches;
   A-B by a and b, A-C by c and d, A-D by e, B-D by f, C-D by g."
  (gr/graph {:vertices [:A :B :C :D]
             :edges [{:id :a :ends [:A :B]} {:id :b :ends [:A :B]} {:id :c :ends [:A :C]} {:id :d :ends [:A :C]}
                     {:id :e :ends [:A :D]} {:id :f :ends [:B :D]} {:id :g :ends [:C :D]}]
             :at {:A [-1.0 0.0] :B [0.0 2.0] :C [0.0 -2.0] :D [2.4 0.0]}}))

(def counts-init "Bridges per pair, in the order of (structure.graph/pairs 4)." [2 2 1 0 1 1])

(def readout "The raster readout of the four-region multigraph." (gr/pair-readout 4))

(def bridge-cases
  "Euler's rule as named cases (structure.graph/euler-cases)."
  gr/euler-cases)

(def case-sentences
  "What each case means, in E53's terms."
  {"empty" "no bridges: nothing to walk"
   "disconnected" "the bridges fall into separate pieces: no single walk reaches them all"
   "circuit" "every region even: a walk over each bridge once from any region, ending where it began (par. 20)"
   "path" "exactly two odd regions: the walk exists, from one of them to the other (par. 20)"
   "none" "more than two odd regions: no walk crosses every bridge once (par. 20)"})

(def walk-form
  "Hierholzer's walk (1873) over edges [[u v tag] ...] from vertex start:
   [[vertex edge-index] ...], the first edge nil. Runs in the page and on
   the JVM."
  '(fn [edges start]
     (let [used (atom #{}) stack (atom [[start nil]]) out (atom [])]
       (while (seq @stack)
         (let [[v _ :as top] (peek @stack)
               nxt (first (keep-indexed (fn [i [a b _]] (when (and (not (contains? @used i)) (or (= a v) (= b v))) [i (if (= a v) b a)]))
                                        edges))]
           (if nxt
             (do (swap! used conj (first nxt)) (swap! stack conj [(second nxt) (first nxt)]))
             (do (swap! out conj top) (swap! stack pop)))))
       (vec (reverse @out)))))

(defn edges-of-counts
  "[[u v [p j]] ...]: the edges of the multigraph with counts per pair, in
   Euler's lettering order (A-B, A-C, A-D, B-D, C-D, then B-C)."
  [counts]
  (vec (for [p [0 1 2 4 5 3] j (range (nth counts p))
             :let [[u v] (nth (gr/pairs 4) p)]]
         [u v [p j]])))

;; ---------------------------------------------------------------------------
;; 2. Kirchhoff 1847

(def trees-form
  "The spanning trees of edges [[u v tag] ...] on n vertices, as vectors of
   edge indices (the n - 1 edge sets without a cycle), at most limit of
   them. Runs in the page and on the JVM."
  '(fn [edges n limit]
     (let [m (count edges) out (atom [])
           forest? (fn [idx]
                     (let [parent (atom (vec (range n)))
                           root (fn [x] (loop [x x] (let [p (nth @parent x)] (if (= p x) x (recur p)))))]
                       (every? (fn [i] (let [[u v] (nth edges i) a (root u) b (root v)]
                                         (when (not= a b) (swap! parent assoc a b) true)))
                               idx)))
           go (fn go [start chosen]
                (when (< (count @out) limit)
                  (if (= (count chosen) (dec n))
                    (when (forest? chosen) (swap! out conj chosen))
                    (doseq [i (range start m)] (go (inc i) (conj chosen i))))))]
       (go 0 [])
       @out)))

(defn tree-polynomial
  "Kirchhoff's determinant for four regions with symbolic multiplicities
   c_AB ... c_CD: the number of spanning trees as a polynomial."
  []
  (e/simplify (:trees (gr/pair-values 4 '[c_AB c_AC c_AD c_BC c_BD c_CD]))))

;; ---------------------------------------------------------------------------
;; 3. Cayley 1857/1889

(def cayley-max 6)

(defn complete-counts "Counts of K_n inside six points." [n]
  (mapv (fn [[i j]] (if (and (< i n) (< j n)) 1 0)) (gr/pairs cayley-max)))

(def cayley-tie
  "n <-> E <-> rank for K_n: E = n(n-1)/2, rank = E - n + 1 = (n-1)(n-2)/2;
   n solved from E by n = (1 + sqrt(1 + 8E))/2, from rank by
   n = (3 + sqrt(1 + 8 rank))/2."
  (tie/tie {:tie/kind :pivot
            :keys [:n :E :rank]
            :ranges {:n [2 6 1] :E [1 15 1] :rank [0 10 1]}
            :to-pivot {:n identity
                       :E (fn [E] (e/* 0.5 (e/+ 1 (e/sqrt (e/+ 1 (e/* 8 E))))))
                       :rank (fn [k] (e/* 0.5 (e/+ 3 (e/sqrt (e/+ 1 (e/* 8 k))))))}
            :state (fn [n] [n (e/* 0.5 n (e/- n 1)) (e/* 0.5 (e/- n 1) (e/- n 2))])}))

(def cayley-readout
  "The readout of K_n inside six points, n a parameter (pair [i j] present
   with weight clamp(n - j, 0, 1)): :trees Kirchhoff's determinant (an
   edgeless point its own tree), :cayley n^(n-2) as exp((n-2) log n), :E,
   :rank, and the reduced Laplacian :L/i-j (5x5)."
  {:outputs (vec (concat [:trees :cayley :E :rank]
                         (for [i (range 5) j (range 5)] (keyword "L" (str i "-" j)))))
   :f (fn [n]
        (let [cs (mapv (fn [[_ j]] (a/clamp (e/- n j) 0 1)) (gr/pairs cayley-max))
              ix (zipmap (gr/pairs cayley-max) cs)
              w (fn [i j] (ix (if (< i j) [i j] [j i])))
              deg (vec (for [i (range cayley-max)] (reduce e/+ 0 (for [j (range cayley-max) :when (not= i j)] (w i j)))))
              L (vec (for [i (range 1 cayley-max)]
                       (vec (for [j (range 1 cayley-max)]
                              (if (= i j) (e/+ (deg i) (e/- 1 (a/clamp (deg i) 0 1))) (e/- 0 (w i j)))))))
              E (reduce e/+ 0 cs)]
          (merge {:trees (gr/det L) :cayley (e/exp (e/* (e/- n 2) (e/log n))) :E E :rank (e/+ (e/- E n) 1)}
                 (into {} (for [i (range 5) j (range 5)] [(keyword "L" (str i "-" j)) ((L i) j)])))))})

(def prufer-form
  "Pruefer's bijection (1918): the code [a1 ... a_{n-2}] (each in 0..n-1)
   gives the tree's n - 1 edges. Runs in the page and on the JVM."
  '(fn [code n]
     (loop [code code deg (reduce (fn [d a] (update d a inc)) (vec (repeat n 1)) code) out []]
       (if (empty? code)
         (let [[u v] (filter #(= 1 (nth deg %)) (range n))] (conj out [u v]))
         (let [a (first code) leaf (first (filter #(= 1 (nth deg %)) (range n)))]
           (recur (rest code) (-> deg (update leaf dec) (update a dec)) (conj out [leaf a])))))))

(defn code-of
  "The k-th Pruefer code on n points (k from 0), its digits base n."
  [k n]
  (vec (reverse (take (- n 2) (map #(mod % n) (iterate #(quot % n) k))))))

;; ---------------------------------------------------------------------------
;; 4. Hamilton 1857

(def consonants
  "Hamilton's lettering of the icosian board: twenty consonants."
  ["B" "C" "D" "F" "G" "H" "J" "K" "L" "M" "N" "P" "Q" "R" "S" "T" "V" "W" "X" "Z"])

(def icosian "The dodecahedron's graph, its Schlegel diagram." (gr/dodecahedron))

;; ---------------------------------------------------------------------------
;; 5. Kempe 1879

(def palette ["#C0392B" "#E6A700" "#2E8B57" "#1F6FB4"])
(def colour-names ["red" "yellow" "green" "blue"])

(def kempe-map
  "A district X (0) with four neighbours N E S W (1-4) in a ring, and two
   outer districts P (5, touching N and E) and Q (6, touching S and W):
   each district a point, each common border a line."
  (gr/embed (gr/from-pairs (range 7) [[0 1] [0 2] [0 3] [0 4] [1 2] [2 3] [3 4] [4 1] [5 1] [5 2] [6 3] [6 4]])
            {0 [0.0 0.0] 1 [0.0 1.6] 2 [1.6 0.0] 3 [0.0 -1.6] 4 [-1.6 0.0] 5 [1.5 1.5] 6 [-1.5 -1.5]}))

(def kempe-names ["X" "N" "E" "S" "W" "P" "Q"])

(def kempe-start "N red, E yellow, S green, W blue, P green, Q red; X waits." {1 0 2 1 3 2 4 3 5 2 6 0})

(defn kempe-steps
  "Kempe's argument for a district with four neighbours, one state per
   step: {:colouring :chain :text}."
  []
  (let [G kempe-map
        chain (gr/kempe-chain G kempe-start 1 0 2)
        swapped (gr/kempe-swap G kempe-start 1 0 2)
        done (assoc swapped 0 0)]
    [{:colouring kempe-start :chain [] :text "X touches four districts painted in all four colours: no colour is left for X."}
     {:colouring kempe-start :chain chain :text "Follow the red and green districts from N: they form a chain, N and P."}
     {:colouring kempe-start :chain chain :text "The chain does not reach S, the green district opposite N: it is cut off from the S and Q chain."}
     {:colouring swapped :chain chain :text "Interchange red and green along N's chain only: N turns green, P red, and the map stays properly coloured."}
     {:colouring done :chain [] :text "Red no longer touches X: paint X red. Four colours suffice here."}]))

;; ---------------------------------------------------------------------------
;; Graded

(defn- variant [diffs]
  (let [res (grade/grade :symbolic diffs)]
    (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails)))

(defn- run-walk [G]
  (let [vs (:vertices G) ix (zipmap vs (range))
        edges (mapv (fn [{:keys [id] [u v] :ends}] [(ix u) (ix v) id]) (:edges G))
        start (ix (or (first (gr/odd-vertices G)) (first vs)))
        w ((eval walk-form) edges start)]
    (mapv (fn [[_ ei] [v]] (let [[a b id] (nth edges ei)] [(vs (if (= a v) b a)) id (vs v)]))
          (rest w) (rest w))))

(defn graded
  "[{:label :grade}] for the five sections."
  []
  (let [K koenigsberg
        K-g (gr/without-edge K :g)
        cs '[c_AB c_AC c_AD c_BC c_BD c_CD]
        sym (gr/pair-values 4 cs)
        trees-of (fn [counts] (:trees (gr/pair-values 4 counts)))
        deg-sum (reduce e/+ (map #(sym (keyword "deg" (str %))) (range 4)))
        D icosian
        hk (gr/hamiltonian-cycle D)
        steps (kempe-steps)
        ix {:A 0 :B 1 :C 2 :D 3}
        browser-trees (count ((eval trees-form) (mapv (fn [{[u v] :ends}] [(ix u) (ix v) nil]) (:edges K)) 4 1000))
        prufer (fn [n] (count (distinct (for [k (range (long (Math/pow n (- n 2))))]
                                          (set (map set ((eval prufer-form) (code-of k n) n)))))))
        cayley-det (fn [m] (e/simplify (gr/det (vec (for [i (range m)] (vec (for [j (range m)] (if (= i j) 'd -1))))))))]
    [{:label "E53 par. 16 (handshake): the bridge counts at the four regions add up to twice the bridges, with the six multiplicities left as symbols"
      :grade (variant [(e/simplify (e/- deg-sum (e/* 2 (:E sym))))])}
     {:label "par. 20: Koenigsberg has four odd regions, so no walk; trying every walk finds none"
      :grade (variant [(- (count (gr/odd-vertices K)) 4) (if (nil? (gr/trail-search K)) 0 1)])}
     {:label "par. 20: without bridge g only A and B are odd; the page's Hierholzer walk crosses all six bridges once, from A to B"
      :grade (variant [(if (= :path (:case (gr/euler-case K-g))) 0 1) (if (gr/walk? K-g (run-walk K-g)) 0 1)])}
     {:label "Kirchhoff: Koenigsberg's tree count, the determinant 21, equals the trees listed by the page's search and by brute force"
      :grade (variant [(- (gr/tree-count K) 21) (- browser-trees 21) (- (count (gr/spanning-trees K)) 21)])}
     {:label "Kirchhoff: the determinant is a polynomial of degree V - 1 = 3 in the six multiplicities (doubling every bridge multiplies it by 8), and it is 21 at Euler's counts"
      :grade (variant [(e/simplify (e/- (trees-of (mapv #(e/* 2 %) cs)) (e/* 8 (trees-of cs))))
                       (- (trees-of counts-init) 21)])}
     {:label "Kirchhoff: independent equations from his first law = E - V + 1 = 7 - 4 + 1 = 4 for Koenigsberg"
      :grade (variant [(- (gr/cycle-rank K) 4)])}
     {:label "Cayley: Kirchhoff's determinant on K_n gives n^(n-2) for n = 2..6 (1, 3, 16, 125, 1296)"
      :grade (variant (for [n (range 2 7)] (- (gr/tree-count (gr/complete n)) (long (Math/pow n (- n 2))))))}
     {:label "Cayley: the (n-1)x(n-1) matrix with d on the diagonal and -1 elsewhere has determinant (d + 1)^(n-2) (d - n + 2); at d = n - 1 that is n^(n-2) (Emmy, n = 3..6)"
      :grade (variant (for [m (range 2 6)] (e/simplify (e/- (cayley-det m) (e/* (e/expt (e/+ 'd 1) (dec m)) (e/- 'd (dec m)))))))}
     {:label "Pruefer's codes give n^(n-2) distinct trees on n = 3, 4, 5 points"
      :grade (variant (for [n [3 4 5]] (- (prufer n) (long (Math/pow n (- n 2))))))}
     {:label "the tie n <-> E <-> rank of K_n solves back to itself from every key (raster round trip)"
      :grade (variant (map #(if (< (Math/abs (double %)) 1e-9) 0 %) (tie/round-trip-gaps cayley-tie :n [2 3 4 5 6])))}
     {:label "Hamilton: the dodecahedron has 20 vertices of degree 3, 30 edges, 12 faces (V - E + F = 2) and 120 symmetries"
      :grade (variant [(- (gr/vertex-count D) 20) (- (gr/edge-count D) 30) (- (count (gr/faces D)) 12)
                       (- (gr/euler-characteristic D) 2) (- (count (gr/automorphisms D)) 120)])}
     {:label "Hamilton against Euler: a closed path through all twenty vertices exists, but twenty odd vertices forbid a walk over all thirty edges"
      :grade (variant [(if (gr/hamiltonian-path? D hk true) 0 1) (- (count (gr/odd-vertices D)) 20)])}
     {:label "Kempe: every step of the swap is a proper colouring, and the last colours X too"
      :grade (variant (concat (for [{:keys [colouring]} steps] (if (gr/proper? (update kempe-map :vertices #(filterv colouring %)) colouring) 0 1))
                              [(if (gr/proper? kempe-map (:colouring (peek steps))) 0 1)]))}]))
