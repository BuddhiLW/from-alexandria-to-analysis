(ns alexandria.structure.groups
  "Domain stratum: the groups the eras use, as instances of
   alexandria.structure.group/Group and Action. Each was extracted from
   era code (named per instance); none is new mathematics.

     rotations      SO(2): angles under +, acting on the plane by rotation
                    about the origin (non-euclidean.disk/rotate,
                    klein.erlangen theta, ptolemy chords, euler cis).
     cyclic         Z/n: residues under + mod n, acting on the n vertices of
                    the regular n-gon (euler.widgets, solids).
     units-mod      (Z/p)^*: residues under * mod p, acting on Z/p by
                    multiplication (gauss.heptadecagon: primitive roots,
                    Gauss' periods are its cosets).
     symmetric      S_n: permutations as vectors, acting on positions
                    (kepler.mysterium-playground/perms).
     projective     PGL(3) on the plane: 3x3 matrices up to scale, acting
                    by desargues.board.linalg/apply-mat; its subgroups
                    (motions, similarities, affine maps) are Klein's chain
                    (klein.erlangen), and desargues.board.groups/finite
                    lists finite subgroups (dihedral, cyclic) in it.
     moebius-disk   the motions of the curvature-K conformal disk
                    (Moebius maps z -> (alpha z + beta)/(-K conj(beta) z +
                    conj(alpha)), K = -1 Poincare's disk): elements [alpha
                    beta] of complex pairs, up to a real scale
                    (non-euclidean.disk T_p, klein.erlangen moebius-map).

   Equality of numeric elements is within `tolerance`; of symbolic ones,
   Emmy simplification to 0."
  (:require [alexandria.structure.arith :as a]
            [alexandria.structure.group :as g]
            [desargues.board.groups :as dg]
            [desargues.board.linalg :as la]
            [emmy.env :as e]))

(def tolerance "Numeric elements closer than this are the same." 1e-9)

(defn zero-ish?
  "x is 0: within tolerance for a number, by Emmy simplification otherwise."
  [x]
  (let [v (e/simplify x)]
    (if (number? v) (< (Math/abs (double v)) tolerance) (e/zero? v))))

(defn same-point?
  "Two points (vectors of Emmy values) are equal, coordinate by coordinate."
  [p q]
  (every? zero-ish? (map e/- p q)))

;; ---------------------------------------------------------------------------
;; SO(2)

(defn rotate "The point [x y] turned by angle t about the origin." [[x y] t]
  [(e/- (e/* (e/cos t) x) (e/* (e/sin t) y)) (e/+ (e/* (e/sin t) x) (e/* (e/cos t) y))])

(defrecord Rotations []
  g/Group
  (unit [_] 0)
  (compose [_ s t] (e/+ s t))
  (inverse [_ t] (e/- 0 t))
  (same? [_ s t] (and (zero-ish? (e/- (e/cos s) (e/cos t))) (zero-ish? (e/- (e/sin s) (e/sin t)))))
  g/Action
  (act [_ t p] (rotate p t)))

(def rotations "SO(2) acting on the plane." (->Rotations))

;; ---------------------------------------------------------------------------
;; Z/n, (Z/p)^*

(defrecord Cyclic [n]
  g/Group
  (unit [_] 0)
  (compose [_ a b] (mod (+ a b) n))
  (inverse [_ a] (mod (- a) n))
  (same? [_ a b] (= (mod a n) (mod b n)))
  g/Action
  (act [_ a k] (mod (+ a k) n)))

(defn cyclic "Z/n acting on the vertices 0..n-1 of the regular n-gon." [n] (->Cyclic n))

(defn- inv-mod [x p]
  (first (filter #(= 1 (mod (* x %) p)) (range 1 p))))

(defrecord UnitsMod [p]
  g/Group
  (unit [_] 1)
  (compose [_ a b] (mod (* a b) p))
  (inverse [_ a] (inv-mod a p))
  (same? [_ a b] (= (mod a p) (mod b p)))
  g/Action
  (act [_ a k] (mod (* a k) p)))

(defn units-mod "(Z/p)^*, p prime, acting on Z/p by multiplication." [p] (->UnitsMod p))

;; ---------------------------------------------------------------------------
;; S_n

(defrecord Symmetric [n]
  g/Group
  (unit [_] (vec (range n)))
  (compose [_ s t] (mapv #(nth s (nth t %)) (range n)))
  (inverse [_ s] (reduce (fn [acc i] (assoc acc (nth s i) i)) (vec (range n)) (range n)))
  (same? [_ s t] (= s t))
  g/Action
  (act [_ s i] (nth s i)))

(defn symmetric "S_n: permutations of 0..n-1 as vectors (s i = image of i)." [n] (->Symmetric n))

;; ---------------------------------------------------------------------------
;; PGL(3) on the plane

(defn- proportional? [A B]
  (let [xs (flatten (map seq A)) ys (flatten (map seq B))]
    (every? zero-ish? (for [i (range 9) j (range (inc i) 9)]
                        (e/- (e/* (nth xs i) (nth ys j)) (e/* (nth xs j) (nth ys i)))))))

(defn- adjugate [[[a b c] [d f g'] [h i j]]]
  [[(e/- (e/* f j) (e/* g' i)) (e/- (e/* c i) (e/* b j)) (e/- (e/* b g') (e/* c f))]
   [(e/- (e/* g' h) (e/* d j)) (e/- (e/* a j) (e/* c h)) (e/- (e/* c d) (e/* a g'))]
   [(e/- (e/* d i) (e/* f h)) (e/- (e/* b h) (e/* a i)) (e/- (e/* a f) (e/* b d))]])

(defrecord Projective []
  g/Group
  (unit [_] [[1 0 0] [0 1 0] [0 0 1]])
  (compose [_ A B] (mapv vec (la/mat* A B)))
  (inverse [_ A] (adjugate A))
  (same? [_ A B] (proportional? A B))
  g/Action
  (act [_ A p] (la/apply-mat A p)))

(def projective "PGL(3): 3x3 matrices up to scale acting on the plane." (->Projective))

(defn family
  "The matrix of a desargues map family, e.g. [:rotation t], [:translation
   dx dy], [:scale k], [:shear k], [:reflection t] (desargues.board.groups/
   family, open on the family keyword), as an element of `projective`."
  [f]
  (mapv vec (dg/family f)))

(defn finite-elements
  "The elements of a desargues finite group form ([:cyclic n], [:dihedral
   n], [:klein4], ...) as elements of `projective`."
  [form]
  (mapv #(mapv vec %) (:elements (dg/finite form))))

;; ---------------------------------------------------------------------------
;; Moebius motions of the conformal disk of curvature K

(defn moebius
  "The motion z -> (alpha z + beta) / (-K conj(beta) z + conj(alpha)) of
   the curvature-K disk, as an element [alpha beta]."
  [alpha beta]
  [alpha beta])

(defn moebius-rotation "e^{i t} z." [t] (moebius (a/cis (e/divide t 2)) [0 0]))

(defn moebius-translation
  "The motion taking 0 to p along the geodesic: z -> (z + p)/(1 - K conj(p) z)."
  [p]
  (moebius a/one p))

(defrecord MoebiusDisk [K]
  g/Group
  (unit [_] (moebius a/one [0 0]))
  (compose [_ [a1 b1] [a2 b2]]
    ;; [[a1 b1] [-K b1* a1*]] [[a2 b2] [-K b2* a2*]]
    (moebius (a/c+ (a/c* a1 a2) (a/cscale (e/- 0 K) (a/c* b1 (a/cconj b2))))
             (a/c+ (a/c* a1 b2) (a/c* b1 (a/cconj a2)))))
  (inverse [_ [al be]] (moebius (a/cconj al) (a/cscale -1 be)))
  (same? [_ [a1 b1] [a2 b2]]
    (let [xs (concat a1 b1) ys (concat a2 b2)]
      (every? zero-ish? (for [i (range 4) j (range (inc i) 4)]
                          (e/- (e/* (nth xs i) (nth ys j)) (e/* (nth xs j) (nth ys i)))))))
  g/Action
  (act [_ [al be] z]
    (a/c-div (a/c+ (a/c* al z) be)
             (a/c+ (a/cscale (e/- 0 K) (a/c* (a/cconj be) z)) (a/cconj al)))))

(defn moebius-disk "The motions of the conformal disk of curvature K." [K] (->MoebiusDisk K))
