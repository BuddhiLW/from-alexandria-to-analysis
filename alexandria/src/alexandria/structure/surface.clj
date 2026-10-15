(ns alexandria.structure.surface
  "Domain stratum: a surface as a cell complex, its Euler characteristic,
   its orientation, and the signed volume of a closed polyhedron.

   A complex is {:edges {label [u v]} :faces [[[label sign] ...] ...]}: every
   face is a cycle of oriented edge uses (sign +1 runs u -> v, -1 runs v -> u).
   Gluing is sharing a label, so a face may use an edge twice and two edges
   may join the same vertices (a strip of one cell closed on itself); no
   simplicial assumption is made.

   Möbius, Über die Bestimmung des Inhaltes eines Polyëders (1865): the
   content of a polyhedron is the sum of the signed pyramids on its faces,
   and it is defined only when the faces can be oriented consistently (each
   shared edge run in opposite senses by its two faces). A strip closed with
   a half twist has no such orientation.

   Built here, from what the library already holds:
     euler-characteristic  V - E + F of any complex (the number structure.graph
                           computes over a rotation system; this ns is
                           self-contained so it does not need that branch)
     orientation           a consistent choice of face senses, or the edge
                           where it fails
     holonomy              the sign the orientation picks up going once round
                           a loop of faces
     orientation-character the Character (structure.ring: a homomorphism to
                           {+1, -1}) of the loop group Z/2 = structure.groups/
                           cyclic 2 sending a to holonomy^a; trivial iff the
                           loop does not reverse orientation
     signed-volume         Möbius' sum of signed pyramids from a point

   Instances: strip (k square cells closed with t half twists: the annulus
   for t even, the Möbius band for t odd), polyhedron (faces as vertex
   cycles)."
  (:require [alexandria.structure.groups :as gs]))

;; ---------------------------------------------------------------------------
;; The complex

(defn vertices "The vertex labels of the complex." [{:keys [edges]}]
  (into #{} (mapcat val) edges))

(defn euler-characteristic "V - E + F." [{:keys [edges faces] :as c}]
  (+ (- (count (vertices c)) (count edges)) (count faces)))

(defn counts "{:V :E :F :chi}." [{:keys [edges faces] :as c}]
  {:V (count (vertices c)) :E (count edges) :F (count faces) :chi (euler-characteristic c)})

(defn- uses
  "{label [[face sign] ...]} over every oriented edge use."
  [{:keys [faces]}]
  (reduce (fn [m [fi f]] (reduce (fn [m [e s]] (update m e (fnil conj []) [fi s])) m f))
          {} (map-indexed vector faces)))

(defn boundary-edges "Labels used by exactly one face use." [c]
  (into #{} (keep (fn [[e us]] (when (= 1 (count us)) e))) (uses c)))

(defn closed? "Every edge is used twice: no boundary." [c]
  (and (seq (:faces c)) (empty? (boundary-edges c))))

(defn boundary-components
  "The number of closed boundary curves: the edges used once, joined at
   their endpoints."
  [{:keys [edges] :as c}]
  (let [be (boundary-edges c)
        root (fn root [m x] (let [p (get m x x)] (if (= p x) x (recur m p))))
        m (reduce (fn [m e] (let [[u v] (edges e)] (assoc m (root m u) (root m v)))) {} be)]
    (count (into #{} (map (fn [e] (root m (first (edges e))))) be))))

;; ---------------------------------------------------------------------------
;; Orientation

(defn- constraints
  "For every edge used twice, [fa fb k]: the senses satisfy s_fb = k s_fa,
   k = -(sign in fa)(sign in fb), so the edge is run in opposite senses."
  [c]
  (for [[_ [[fa sa] [fb sb] :as u]] (uses c) :when (= 2 (count u))]
    [fa fb (- (* sa sb))]))

(defn orientation
  "Can every face be given a sense (+1 as listed, -1 reversed) so that each
   edge shared by two faces is run in opposite senses? Propagates the
   constraints from the first face of each component. {:orientable? true
   :senses {face s}} or {:orientable? false :conflict edge-label}. An edge
   used by more than two faces is a conflict (not a surface)."
  [c]
  (let [us (uses c)
        over (some (fn [[e u]] (when (> (count u) 2) e)) us)
        adj (reduce (fn [m [a b k]] (-> m (update a (fnil conj []) [b k]) (update b (fnil conj []) [a k])))
                    {} (constraints c))]
    (if over
      {:orientable? false :conflict over}
      (let [senses (loop [senses {} todo (range (count (:faces c)))]
                     (if-let [f0 (first (remove senses todo))]
                       (recur (loop [senses (assoc senses f0 1) queue [f0]]
                                (if-let [f (first queue)]
                                  (let [nxt (for [[g k] (adj f) :when (not (contains? senses g))] [g (* k (senses f))])
                                        senses' (reduce (fn [s [g v]] (assoc s g v)) senses nxt)]
                                    (recur senses' (into (subvec (vec queue) 1) (distinct (map first nxt)))))
                                  senses))
                              todo)
                       senses))
            bad (some (fn [[e [[fa sa] [fb sb] :as u]]]
                        (when (and (= 2 (count u)) (= (* (senses fa) sa) (* (senses fb) sb))) e))
                      us)]
        (if bad {:orientable? false :conflict bad} {:orientable? true :senses senses})))))

(defn orientable? [c] (:orientable? (orientation c)))

(defn holonomy
  "The sign the orientation picks up going once round a loop of face
   crossings, each [face-a pos-a face-b pos-b]: the crossing from face-a
   through its use at pos-a into face-b, whose use at pos-b is the same edge.
   Each crossing contributes -(sign at pos-a)(sign at pos-b); the product is
   +1 when the loop does not reverse orientation, -1 when it does."
  [{:keys [faces]} crossings]
  (reduce (fn [acc [fa pa fb pb]]
            (* acc (- (* (second (get-in faces [fa pa])) (second (get-in faces [fb pb]))))))
          1 crossings))

(defn orientation-character
  "The orientation character of a loop of holonomy h (+1 or -1): a Character
   of Z/2, the loop traversed a times: {:group :elements :chi}, chi(a) = h^a."
  [h]
  {:group (gs/cyclic 2) :elements [0 1] :chi (fn [a] (if (even? a) 1 h))})

(defn trivial-character? [{:keys [elements chi]}] (every? #(= 1 (chi %)) elements))
