(ns alexandria.euler.polyhedra
  "Euler, Elementa doctrinae solidorum and Demonstratio nonnullarum
   insignium proprietatum quibus solida hedris planis inclusa sunt praedita
   (E230, E231; written 1750, printed 1758): S + H = A + 2, solid angles
   plus faces exceed edges by two; in modern letters V - E + F = 2.

     solids      values shaped like alexandria.solids ({:id :name :vertices
                 :faces :edges}): the five Platonic solids are taken from
                 alexandria.solids; prisms and pyramids on any n-gon are
                 built here, their faces and edges derived the same way
     counting    V, E, F are read from the derived data, never from the
                 formula: a face is a maximal set of vertices on a plane
                 that leaves every other vertex on one side; an edge is a
                 pair of vertices lying on two faces
     Cauchy      (1813, forward link) the cube's Schlegel diagram: remove
                 one face (V - E + F drops to 1), draw diagonals, then take
                 off boundary triangles one at a time; every step keeps
                 V - E + F, down to one triangle, 3 - 3 + 1 = 1
     figures     the cube flattening into its Schlegel diagram, an Emmy
                 function for media

   Reuse searched: alexandria.solids holds the regular solids (exact Emmy
   vertices); Emmy has no polytope or convex-hull module; desargues draws
   plane constructions."
  (:require [alexandria.grade :as grade]
            [alexandria.proofs :as proofs]
            [hive-dsl.result :as r]
            [clojure.set :as set]
            [emmy.env :as e]
            [alexandria.solids :as solids]))

(declare with-faces)

;; ---------------------------------------------------------------------------
;; Solids

(def platonic
  "The five Platonic solids from alexandria.solids, {:id :name :vertices
   :faces :edges}, in Euclid's order."
  (mapv solids/solid [:tetrahedron :cube :octahedron :dodecahedron :icosahedron]))

(defn- ngon [n z r] (mapv (fn [k] (let [a (/ (* 2 Math/PI k) n)] [(* r (Math/cos a)) (* r (Math/sin a)) z])) (range n)))

(defn prism [n] (with-faces {:id (keyword (str "prism-" n)) :name (str n "-gonal prism")
                             :vertices (into (ngon n -1 1) (ngon n 1 1))}))

(defn pyramid [n] (with-faces {:id (keyword (str "pyramid-" n)) :name (str n "-gonal pyramid")
                               :vertices (conj (ngon n 0 1) [0 0 1.5])}))

;; ---------------------------------------------------------------------------
;; Counting V, E, F from coordinates

(def ^:private eps 1e-9)

(defn- sub [[a b c] [x y z]] [(- a x) (- b y) (- c z)])
(defn- dot [[a b c] [x y z]] (+ (* a x) (* b y) (* c z)))
(defn- cross [[a b c] [x y z]] [(- (* b z) (* c y)) (- (* c x) (* a z)) (- (* a y) (* b x))])

(defn faces
  "The faces of the convex hull of vertices, each a set of vertex indices."
  [vertices]
  (let [vs (vec vertices) n (count vs)]
    (set (for [i (range n) j (range (inc i) n) k (range (inc j) n)
               :let [p (vs i) nrm (cross (sub (vs j) p) (sub (vs k) p))]
               :when (> (dot nrm nrm) eps)
               :let [side (map #(dot nrm (sub % p)) vs)]
               :when (or (every? #(<= % eps) side) (every? #(>= % (- eps)) side))]
           (set (keep-indexed (fn [m d] (when (< (Math/abs (double d)) eps) m)) side))))))

(defn edges
  "The edges: pairs of vertices that lie together on two faces."
  [fs]
  (set (for [f fs g fs :when (not= f g)
             :let [common (clojure.set/intersection f g)]
             :when (= 2 (count common))]
         common)))

(defn with-faces
  "solid {:vertices} completed to the alexandria.solids shape: :faces (index
   sets) and :edges (index pairs) derived from the coordinates."
  [{:keys [vertices] :as s}]
  (let [fs (faces vertices)]
    (assoc s :faces (vec fs) :edges (mapv vec (edges fs)))))

(defn counts
  "{:V :E :F} of a solid shaped like alexandria.solids values ({:vertices
   :faces :edges}); the same reading as alexandria.solids/counts."
  [s]
  (solids/counts s))

(defn characteristic [{:keys [V E F]}] (+ (- V E) F))

;; ---------------------------------------------------------------------------
;; Cauchy's reduction on the cube's Schlegel diagram

(def schlegel-cube
  "The cube's faces as vertex cycles. Vertices 0-3 the top face (z = 1),
   4-7 the bottom, k above k + 4. In the Schlegel diagram the top face is
   the outside."
  {:outer [0 1 2 3]
   :faces [[4 5 6 7] [0 1 5 4] [1 2 6 5] [2 3 7 6] [3 0 4 7]]})

(defn- cycle-edges [f] (map (fn [a b] #{a b}) f (concat (rest f) [(first f)])))

(defn complex-counts
  "{:V :E :F} of a plane complex given by its faces (vertex cycles)."
  [faces]
  (let [es (set (mapcat cycle-edges faces))]
    {:V (count (set (apply concat faces))) :E (count es) :F (count faces)}))

(defn- triangulate
  "A face cycle cut by diagonals from its first vertex."
  [[a & more]]
  (mapv (fn [[b c]] [a b c]) (partition 2 1 more)))

(defn- boundary-edges
  "Edges on exactly one face."
  [faces]
  (set (keep (fn [[e k]] (when (= 1 k) e)) (frequencies (mapcat cycle-edges faces)))))

(defn- removable?
  "Cauchy's two moves: a triangle with one boundary edge whose third vertex
   is inside, or with two boundary edges (a vertex goes with them)."
  [faces tri]
  (let [bd (boundary-edges faces)
        on (filter bd (cycle-edges tri))
        bd-vertices (set (apply concat bd))]
    (case (count on)
      1 (not (bd-vertices (first (remove (first on) tri))))
      2 true
      false)))

(defn cauchy-steps
  "The stages of the reduction: [{:move :faces :counts}], from the closed
   cube to one triangle."
  []
  (let [{:keys [outer faces]} schlegel-cube
        closed (conj faces outer)
        opened faces
        start [{:move :closed :faces closed :counts (complex-counts closed)}
               {:move :remove-face :faces opened :counts (complex-counts opened)}]
        tris (reduce (fn [acc f] (let [fs (into (vec (remove #{f} (:faces (peek acc)))) (triangulate f))]
                                   (conj acc {:move :diagonal :faces fs :counts (complex-counts fs)})))
                     [(peek start)] opened)
        reduce-step (fn [fs]
                      (when (> (count fs) 1)
                        (when-let [t (first (filter #(removable? fs %) fs))]
                          (let [fs' (vec (remove #{t} fs))]
                            {:move :remove-triangle :faces fs' :counts (complex-counts fs') :removed t}))))]
    (loop [acc (into start (rest tris))]
      (if-let [s (reduce-step (:faces (peek acc)))]
        (recur (conj acc s))
        acc))))

;; ---------------------------------------------------------------------------
;; Graded

(defn- variant [kind diffs]
  (let [res (grade/grade kind diffs)]
    (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails)))

(def solids
  "Every solid checked: the five Platonic solids, prisms and pyramids on
   3- to 8-gons."
  (concat platonic (map prism (range 3 9)) (map pyramid (range 3 9))))

(defn graded
  "[{:label :counts :grade}]: V - E + F - 2 = 0 for each solid, counted from
   its coordinates, and Cauchy's invariant at every step of the reduction."
  []
  (let [steps (cauchy-steps)]
    (conj (mapv (fn [s] (let [c (counts s)]
                          {:label (str (:name s) ": " (:V c) " - " (:E c) " + " (:F c) " = 2")
                           :counts c
                           :grade (variant :symbolic [(- (characteristic c) 2)])}))
                solids)
          {:label (str "Cauchy: after the first face is removed, every one of " (dec (count steps))
                       " steps keeps V - E + F = 1, down to one triangle")
           :grade (variant :symbolic (map #(- (characteristic (:counts %)) 1) (rest steps)))}
          {:label "Cauchy: the last stage is one triangle"
           :grade (variant :symbolic [(- (count (:faces (peek steps))) 1)])})))

;; ---------------------------------------------------------------------------
;; The cube flattening, for media

(def cube-vertices
  "The cube's vertices in the order of schlegel-cube: 0-3 top, 4-7 bottom."
  [[-1 -1 1] [1 -1 1] [1 1 1] [-1 1 1] [-1 -1 -1] [1 -1 -1] [1 1 -1] [-1 1 -1]])

(defn flatten-cube
  "A cube vertex [x y z] at time t: t = 0 the cube seen obliquely, t = 1 its
   Schlegel diagram, the cube seen from just above its top face (the top
   face becomes the outside square, the bottom face the inner one). Emmy
   arithmetic, so a medium can compile it."
  [t]
  (fn [[x y z]]
    (let [ox (e/+ (e/* 0.86 x) (e/* -0.5 y))
          oy (e/+ (e/* 0.26 x) (e/* 0.45 y) (e/* 0.85 z))
          k (e/divide 2.25 (e/- 2 z))
          sx (e/* k x) sy (e/* k y)]
      [(e/+ ox (e/* t (e/- sx ox))) (e/+ oy (e/* t (e/- sy oy)))])))

(def figures
  "{:f :params :state} of every moving figure."
  {:cube {:f flatten-cube :params [0] :state [1 1 1]}})

;; ---------------------------------------------------------------------------
;; The proofs as data

(def proofs-resource "alexandria/euler/opera.proofs.edn")

(defn proof [] (proofs/read-proofs proofs-resource))
