(ns alexandria.solids
  "The five regular solids of Euclid XIII (Heath 1908, vol. 3, pp. 467-511)
   as values: vertices exact in Emmy, faces and edges derived from them.

     solid        {:id :name :vertices :faces :edges}; vertices are Emmy
                  values, exact by default (sqrt 5 an Emmy literal), or
                  doubles with {:numeric? true}
     counts       {:V :E :F} of a solid, read from the derived data
     radii        squared circumradius (to a vertex), midradius (to the
                  middle of an edge) and inradius (to the centre of a face)
     ratio-sq     (inradius / circumradius)^2, exact

   Construction: the cube on (+-1, +-1, +-1); the tetrahedron on alternate
   corners of it; the octahedron on the unit axes; the icosahedron on the
   cyclic permutations of (0, +-1, +-phi); the dodecahedron on the cube's
   corners and the cyclic permutations of (0, +-1/phi, +-phi), phi the golden
   ratio (1 + sqrt 5)/2. The faces are not typed by hand: a face is the set
   of vertices on a plane through three vertices that leaves every vertex on
   one side (the solid is convex). Edges are the pairs of vertices at the
   least distance.

   Reuse searched: desargues (board.shapes, board.symmetry, geometry) has
   plane polygons and symmetry groups but no polyhedra; Emmy has no solids;
   raster none. Written here, on Emmy's generic arithmetic."
  (:require [emmy.env :as e]))

(def solid-ids
  "Kepler's order, from Saturn's sphere inward."
  [:cube :tetrahedron :dodecahedron :icosahedron :octahedron])

(def names
  {:tetrahedron "tetrahedron" :cube "cube" :octahedron "octahedron"
   :dodecahedron "dodecahedron" :icosahedron "icosahedron"})

(def duals
  {:tetrahedron :anti-tetrahedron :cube :octahedron :octahedron :cube
   :dodecahedron :icosahedron :icosahedron :dodecahedron})

(defn root-five
  "sqrt 5: an exact Emmy literal, or a double."
  [numeric?]
  (if numeric? (Math/sqrt 5) (e/sqrt (e/literal-number 5))))

(defn golden
  "phi = (1 + sqrt 5)/2 over the given sqrt 5."
  [r5]
  (e// (e/+ 1 r5) 2))

(defn- cyclic [[x y z]] [[x y z] [y z x] [z x y]])

(def ^:private signs [1 -1])

(defn- raw-vertices [id r5]
  (let [phi (golden r5)]
    (case id
      :cube (vec (for [a signs b signs c signs] [a b c]))
      :tetrahedron [[1 1 1] [1 -1 -1] [-1 1 -1] [-1 -1 1]]
      :anti-tetrahedron [[-1 -1 -1] [-1 1 1] [1 -1 1] [1 1 -1]]
      :octahedron (vec (for [s signs v (cyclic [s 0 0])] v))
      :icosahedron (vec (for [s1 signs s2 signs v (cyclic [0 s1 (e/* s2 phi)])] v))
      :dodecahedron (vec (concat (for [a signs b signs c signs] [a b c])
                                 (for [s1 signs s2 signs
                                       v (cyclic [0 (e/* s1 (e// 1 phi)) (e/* s2 phi)])]
                                   v))))))

(defn dot [u v] (reduce e/+ (map e/* u v)))

(defn norm-sq [v] (dot v v))

(defn- sub [u v] (mapv e/- u v))

(defn centroid
  "The centre of the points ps."
  [ps]
  (mapv (fn [i] (e// (reduce e/+ (map #(nth % i) ps)) (count ps))) [0 1 2]))

(defn- close? [a b] (< (Math/abs (- (double a) (double b))) 1e-9))

(defn- faces-of
  "Index sets of the faces of the convex solid on vertices vs (doubles):
   every plane through three vertices with all vertices on one side carries
   a face; its vertices are ordered round the face."
  [vs]
  (let [n (count vs)
        cross (fn [[a b c] [x y z]] [(- (* b z) (* c y)) (- (* c x) (* a z)) (- (* a y) (* b x))])
        planes (for [i (range n) j (range (inc i) n) k (range (inc j) n)
                     :let [nv (cross (sub (vs j) (vs i)) (sub (vs k) (vs i)))
                           nv (if (neg? (dot nv (vs i))) (mapv - nv) nv)
                           h (dot nv (vs i))]
                     :when (and (> (norm-sq nv) 1e-12)
                                (every? #(<= (dot nv %) (+ h 1e-9)) vs))]
                 [nv h])
        faces (distinct (for [[nv h] planes]
                          (vec (sort (filter #(close? h (dot nv (vs %))) (range n))))))]
    (vec (for [idx faces
               :let [c (centroid (map vs idx))
                     d c
                     u (sub (vs (first idx)) c)
                     w (cross d u)]]
           (vec (sort-by (fn [i] (let [q (sub (vs i) c)] (Math/atan2 (dot q w) (dot q u)))) idx))))))

(defn- edges-of [vs]
  (let [n (count vs)
        pairs (for [i (range n) j (range (inc i) n)] [i j])
        d (fn [[i j]] (norm-sq (sub (vs i) (vs j))))
        m (apply min (map d pairs))]
    (filterv #(close? m (d %)) pairs)))

(defn solid
  "The regular solid id: {:id :name :vertices :faces :edges}. Vertices are
   exact Emmy values unless opts has :numeric? true. :faces are vectors of
   vertex indices ordered round the face, :edges pairs of indices."
  ([id] (solid id {}))
  ([id {:keys [numeric?]}]
   (let [vs (raw-vertices id (root-five numeric?))
         ds (mapv #(mapv double %) (raw-vertices id (root-five true)))]
     {:id id :name (names id) :vertices vs
      :faces (faces-of ds) :edges (edges-of ds)})))

(def solids
  "All five, exact, by id."
  (into {} (map (juxt identity solid)) solid-ids))

(defn counts
  "Vertices, edges and faces of solid s, counted from its data."
  [{:keys [vertices edges faces]}]
  {:V (count vertices) :E (count edges) :F (count faces)})

(defn radii
  "Squared radii of solid s from its centre: :circum (a vertex), :mid (the
   middle of an edge), :in (the centre of a face)."
  [{:keys [vertices edges faces]}]
  (let [[i j] (first edges)]
    {:circum (norm-sq (first vertices))
     :mid (norm-sq (centroid [(vertices i) (vertices j)]))
     :in (norm-sq (centroid (map vertices (first faces))))}))

(defn ratio-sq
  "(inradius/circumradius)^2 of solid s, or with which = :mid the
   (midradius/circumradius)^2."
  ([s] (ratio-sq s :in))
  ([s which] (let [r (radii s)] (e/simplify (e// (which r) (:circum r))))))
