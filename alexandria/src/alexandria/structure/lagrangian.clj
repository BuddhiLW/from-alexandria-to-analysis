(ns alexandria.structure.lagrangian
  "Structure stratum: analytical mechanics on a configuration space, the
   one vocabulary the Lanczos series (and any notebook on Euler, Lagrange,
   Hamilton or Jacobi) calls.

   Reuse searched before writing (no NIH): Emmy's emmy.mechanics.lagrange
   has the local tuple (->local, Gamma), Lagrange-equations,
   Euler-Lagrange-operator, Lagrangian->energy, F->C (point
   transformations), Lagrangian-action and the standard Lagrangians
   (L-free-particle, L-harmonic, L-pendulum, L-central-polar);
   emmy.mechanics.hamilton has Lagrangian->Hamiltonian (Legendre),
   Hamiltonian->Lagrangian, Hamilton-equations, Poisson-bracket,
   Hamiltonian->state-derivative; alexandria.lagrange.variations has
   Lagrange's delta. Nothing here re-derives them. This namespace names
   them in Lanczos' terms, adds the few compositions the book uses again
   and again, and holds the contract every Lagrangian of the series meets
   (test/alexandria/structure/lagrangian_contract.clj):

     euler-lagrange       the Euler-Lagrange expression along a path,
                          d/dt dL/dq' - dL/dq
     momentum, force      dL/dq' and dL/dq at a local tuple
     energy               the energy function q' dL/dq' - L
     energy-rate          dE/dt along a path + q' . (Euler-Lagrange): zero
                          for a time-independent L on every path, so E is
                          constant on every motion
     cyclic?              dL/dq_i = 0: the coordinate is ignorable and its
                          momentum is conserved
     legendre, inverse    L -> H and H -> L (Emmy's, guarded by its own
                          check that L is quadratic in q')
     hamilton-residual    Hamilton's equations along (q, p)
     canonical-agreement  Hamilton's equations along (q, dL/dq' of q)
                          minus (0, Euler-Lagrange): zero, the two forms
                          of the equations are one
     poisson              the Poisson bracket {f, g}
     transform            L seen through a point transformation q = F(t, q')
     action               the time integral of L along a path (numeric)
     varied-action        the action of q + a eta: stationary at a = 0 on
                          the actual motion

   States are Emmy's: a local tuple (up t q q'), a Hamiltonian state
   (up t q p) with p a down tuple. Everything is symbolic unless named
   numeric; the grades go through alexandria.grade."
  (:refer-clojure :exclude [force])
  (:require [alexandria.grade :as grade]
            [alexandria.lagrange.variations :as variations]
            [emmy.env :as e]
            [emmy.mechanics.hamilton :as H]
            [emmy.mechanics.lagrange :as L]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; Local tuples and paths

(def ->local "The local tuple (up t q q'), Emmy's." L/->local)
(def ->H-state "The Hamiltonian state (up t q p), Emmy's." H/->H-state)
(def Gamma "The local-tuple path of a configuration path q, Emmy's." L/Gamma)

(defn local
  "The general local tuple of n degrees of freedom on symbols t, q_i, v_i
   (one degree of freedom: scalars q, v)."
  [n]
  (if (= 1 n)
    (->local 't 'q 'v)
    (->local 't
             (apply e/up (map #(symbol (str "q_" %)) (range n)))
             (apply e/up (map #(symbol (str "v_" %)) (range n))))))

(defn H-state
  "The general Hamiltonian state of n degrees of freedom on symbols t, q_i,
   p_i."
  [n]
  (if (= 1 n)
    (->H-state 't 'q 'p)
    (->H-state 't
               (apply e/up (map #(symbol (str "q_" %)) (range n)))
               (apply e/down (map #(symbol (str "p_" %)) (range n))))))

;; ---------------------------------------------------------------------------
;; Standard Lagrangians (Emmy's, under the book's names)

(def free-particle "L = m v^2/2." L/L-free-particle)
(def harmonic "L = m v^2/2 - k q^2/2." L/L-harmonic)
(def pendulum "L of the plane pendulum, Emmy's L-pendulum g m l." L/L-pendulum)
(def central-polar "L of a particle in a central potential U(r), polar coordinates." L/L-central-polar)

(defn T-minus-V
  "L = T - V from a kinetic energy T and a potential V, each a function of
   the local tuple: the form every Lagrangian of the book takes."
  [T V]
  (fn [local] (e/- (T local) (V local))))

;; ---------------------------------------------------------------------------
;; Euler-Lagrange

(defn euler-lagrange
  "The Euler-Lagrange expression d/dt dL/dq' - dL/dq of L along path q, as
   a function of t (Emmy's Lagrange-equations)."
  [Lf q]
  ((L/Lagrange-equations Lf) q))

(defn momentum "dL/dq' at a local tuple." [Lf local] (((e/partial 2) Lf) local))
(defn force "dL/dq at a local tuple." [Lf local] (((e/partial 1) Lf) local))

(defn cyclic?
  "Is coordinate i (nil for one degree of freedom) ignorable in Lf: does
   dL/dq_i simplify to 0 at the general local tuple of n degrees of
   freedom?"
  [Lf n i]
  (let [f (force Lf (local n))]
    (e/zero? (e/simplify (if (nil? i) f (nth f i))))))

;; ---------------------------------------------------------------------------
;; Energy

(def energy "The energy function q' dL/dq' - L (Emmy's Lagrangian->energy)." L/Lagrangian->energy)

(defn energy-rate
  "dE/dt along path q minus q' . (Euler-Lagrange of q), a function of t.
   Differentiating E = q' p - L gives dE/dt = q' (p' - dL/dq) - dL/dt =
   q' . (Euler-Lagrange) - dL/dt, so this is -dL/dt: 0 on every path for a
   time-independent L, and on a motion (where the Euler-Lagrange expression
   vanishes) E is constant."
  [Lf q]
  (let [E (e/compose (energy Lf) (Gamma q))]
    (fn [t] (e/- ((e/D E) t)
                 (e/* ((e/D q) t) ((euler-lagrange Lf q) t))))))

;; ---------------------------------------------------------------------------
;; Legendre and Hamilton

(def legendre "L -> H (Emmy's Lagrangian->Hamiltonian)." H/Lagrangian->Hamiltonian)
(def inverse-legendre "H -> L (Emmy's Hamiltonian->Lagrangian)." H/Hamiltonian->Lagrangian)

(defn legendre-twice
  "L at the local tuple minus the Lagrangian of L's Hamiltonian there: 0
   when Legendre's transformation is its own inverse on L."
  [Lf local]
  (e/- (Lf local) ((inverse-legendre (legendre Lf)) local)))

(defn hamilton-residual
  "Hamilton's equations of H along (q, p): (up 0, q' - dH/dp, p' + dH/dq)
   as a function of t (Emmy's Hamilton-equations)."
  [Hf q p]
  ((H/Hamilton-equations Hf) q p))

(defn conjugate-momentum-path
  "The momentum path t -> dL/dq'(t, q(t), q'(t)) of path q."
  [Lf q]
  (fn [t] (momentum Lf ((Gamma q) t))))

(defn canonical-agreement
  "Hamilton's equations of L's Hamiltonian along (q, p = dL/dq'), minus
   (up 0 0 E) with E the Euler-Lagrange expression of q: (up 0 0 0) when the
   canonical equations and the Lagrangian equations are one."
  [Lf q]
  (let [p (conjugate-momentum-path Lf q)
        res (hamilton-residual (legendre Lf) q p)
        el (euler-lagrange Lf q)]
    (fn [t] (let [[a b c] (res t)]
              (e/up a b (e/- c (el t)))))))

(def poisson "The Poisson bracket {f, g} of two phase functions (Emmy's)." H/Poisson-bracket)

(defn coordinate "q of a Hamiltonian state." [state] (nth state 1))
(defn conjugate "p of a Hamiltonian state." [state] (nth state 2))

;; ---------------------------------------------------------------------------
;; Point transformations

(defn transform
  "Lf seen through the point transformation F: local tuple (t q' v') ->
   old coordinates q (Emmy's F->C)."
  [Lf F]
  (e/compose Lf (L/F->C F)))

;; ---------------------------------------------------------------------------
;; Action and its variation

(defn action
  "The time integral of Lf along path q from t1 to t2 (numeric, Emmy's
   Lagrangian-action)."
  [Lf q t1 t2]
  (L/Lagrangian-action Lf q t1 t2))

(defn varied-action
  "The action of the varied path q + a eta, as a function of a."
  [Lf q eta t1 t2]
  (fn [a] (action Lf (fn [t] (e/+ (q t) (e/* a (eta t)))) t1 t2)))

(defn first-variation-integrand
  "d/da of L(q + a eta) at a = 0, minus the total derivative
   d/dt (eta . dL/dq') and plus eta . (Euler-Lagrange of q): 0 for every L,
   q and eta (Lagrange's integration by parts, through
   alexandria.lagrange.variations/delta). Integrated between ends where
   eta vanishes, the first variation is - the integral of eta times the
   Euler-Lagrange expression: zero on a motion."
  [Lf q eta]
  (let [density (fn [q] (e/compose Lf (Gamma q)))
        lhs (((variations/delta eta) density) q)
        el (euler-lagrange Lf q)
        boundary (e/D (fn [t] (e/* (eta t) (momentum Lf ((Gamma q) t)))))]
    (fn [t] (e/+ (e/- (lhs t) (boundary t)) (e/* (eta t) (el t))))))

;; ---------------------------------------------------------------------------
;; Grading

(defn components
  "The simplified leaves of x (a structure or a scalar)."
  [x]
  (let [v (e/simplify x)] (if (e/structure? v) (flatten (seq v)) [v])))

(defn grade
  "The Grade variant keyword of diffs (each lhs - rhs) as alexandria.grade
   grades them, :grade/fails when grading itself fails."
  ([kind diffs] (grade kind diffs grade/default-tolerance))
  ([kind diffs tol]
   (let [res (grade/grade kind (mapcat components diffs) tol)]
     (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))))
