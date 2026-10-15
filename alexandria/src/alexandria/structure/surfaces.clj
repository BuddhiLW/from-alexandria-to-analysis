(ns alexandria.structure.surfaces
  "Instances of alexandria.structure.surface, the way structure.groups holds
   the instances of structure.group:

     strip        k square cells in a row, the ends glued after t half
                  twists: the annulus (t even) and the Möbius band (t odd)
     polyhedron   a complex from coordinates and faces as vertex cycles, with
                  Möbius' signed content (1865)
     cube, cube-flipped, moebius-band-5   the cases the notebook shows."
  (:require [alexandria.structure.surface :as s]))

(defn strip
  "k >= 1 square cells in a row, the two ends glued after t half twists (t
   even the annulus, t odd the Möbius band; only the parity changes the
   gluing). Vertices [:b i] and [:t i], edges [:bot i] [:top i] [:rung i]; for
   t odd the end rung k is rung 0 run backwards. Face i is bottom, right rung,
   top reversed, left rung reversed. {:edges :faces :loop :k :twists}, :loop
   the crossings of the core line for `surface/holonomy`."
  [k t]
  (let [odd (odd? t)
        end-b (if odd [:t 0] [:b 0])
        end-t (if odd [:b 0] [:t 0])
        b (fn [i] (if (= i k) end-b [:b i]))
        tp (fn [i] (if (= i k) end-t [:t i]))
        edges (into {} (concat (for [i (range k)] [[:bot i] [(b i) (b (inc i))]])
                               (for [i (range k)] [[:top i] [(tp i) (tp (inc i))]])
                               (for [i (range k)] [[:rung i] [[:b i] [:t i]]])))
        faces (vec (for [i (range k)]
                     [[[:bot i] 1]
                      [[:rung (mod (inc i) k)] (if (and (= (inc i) k) odd) -1 1)]
                      [[:top i] -1]
                      [[:rung i] -1]]))]
    {:edges edges :faces faces :k k :twists t
     :loop (vec (for [i (range k)] [i 1 (mod (inc i) k) 3]))}))

(defn strip-report
  "Everything the band widget shows for the strip of k cells and t half
   twists, all computed from the cell structure: V E F chi, orientability,
   the holonomy of the core loop, its orientation character on {0, 1}, the
   number of boundary curves."
  [k t]
  (let [c (strip k t)
        h (s/holonomy c (:loop c))
        ch (s/orientation-character h)
        o (s/orientation c)]
    (merge (s/counts c)
           {:k k :twists t :orientable? (:orientable? o) :conflict (:conflict o)
            :holonomy h :character (mapv (:chi ch) (:elements ch))
            :boundary (s/boundary-components c)})))

(defn cut-along-core
  "Cut the strip along its core line. Two bands of k cells and t half twists
   when t is even; one band of 2k cells and 2t half twists (the core line
   traversed twice: an even number of half twists, two-sided) when t is odd.
   Returns the reports of the pieces."
  [k t]
  (if (even? t)
    [(strip-report k t) (strip-report k t)]
    [(strip-report (* 2 k) (* 2 t))]))

;; ---------------------------------------------------------------------------
;; Polyhedra and Möbius' content

(defn polyhedron
  "A complex from vertex coordinates (a vector of [x y z]) and faces as cycles
   of vertex indices. Edge label [lo hi], use sign +1 when the face runs
   lo -> hi."
  [coords faces]
  (let [lab (fn [u v] [(min u v) (max u v)])
        fs (vec (for [f faces]
                  (vec (for [[u v] (map vector f (concat (rest f) [(first f)]))]
                         [(lab u v) (if (< u v) 1 -1)]))))]
    {:coords coords :faces fs :vertex-cycles (vec faces)
     :edges (into {} (for [f fs [e _] f] [e e]))}))

(defn- det3 [[a b c] [d e f] [g h i]]
  (+ (* a (- (* e i) (* f h))) (- (* b (- (* d i) (* f g)))) (* c (- (* d h) (* e g)))))

(defn- minus [p q] (mapv - p q))

(defn signed-volume
  "Möbius' content: the sum, over the faces (fanned into triangles), of the
   signed volume of the pyramid on the face with apex `origin`, each face
   taken in the sense listed by `senses` (a map face -> +1/-1, default all as
   listed). For a closed polyhedron with consistently oriented faces it does
   not depend on the origin."
  ([p origin] (signed-volume p origin nil))
  ([{:keys [coords vertex-cycles]} origin senses]
   (/ (reduce + (for [[fi f] (map-indexed vector vertex-cycles)
                      :let [sn (if senses (get senses fi 1) 1)
                            p0 (minus (coords (first f)) origin)]
                      [u v] (map vector (rest f) (rest (rest f)))]
                  (* sn (det3 p0 (minus (coords u) origin) (minus (coords v) origin)))))
      6.0)))

(defn content
  "Möbius' verdict on a polyhedron as written (every face in the sense
   listed): {:oriented? :closed? :volume}. Oriented means the listed senses
   already run each shared edge in opposite directions; the volume is then
   the signed content from the first vertex, and nil when it is not defined."
  [p]
  (let [o (s/orientation p)
        listed? (and (:orientable? o) (every? #(= 1 (val %)) (:senses o)))]
    {:oriented? (boolean (:orientable? o)) :listed-consistent? (boolean listed?)
     :closed? (s/closed? p) :conflict (:conflict o)
     :volume (when (:orientable? o) (signed-volume p (first (:coords p)) (:senses o)))}))

(def cube
  "The unit cube, faces as vertex cycles seen from outside."
  (polyhedron [[0 0 0] [1 0 0] [1 1 0] [0 1 0] [0 0 1] [1 0 1] [1 1 1] [0 1 1]]
              [[0 3 2 1] [4 5 6 7] [0 1 5 4] [2 3 7 6] [1 2 6 5] [3 0 4 7]]))

(def cube-flipped
  "The same cube with the top face written the other way round."
  (polyhedron (:coords cube) (assoc (:vertex-cycles cube) 1 [7 6 5 4])))

(def moebius-decahedron
  "Möbius' decahedron of 1865, §8: vertices A..F = 0..5, the zone (R) of five
   triangles ABC BCD CDE DEA EAB and the five FAC FBD FCE FDA FEB. Six
   vertices, ten faces, fifteen edges (chi = 1), every edge in two faces, and
   no consistent orientation: 'kein Euler'sches Polyeder'. No coordinates:
   it cannot be built without self-intersection in space."
  (polyhedron nil [[0 1 2] [1 2 3] [2 3 4] [3 4 0] [4 0 1]
                   [5 0 2] [5 1 3] [5 2 4] [5 3 0] [5 4 1]]))
