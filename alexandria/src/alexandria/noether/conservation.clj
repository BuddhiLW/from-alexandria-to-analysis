(ns alexandria.noether.conservation
  "Noether, Invariante Variationsprobleme (Goettinger Nachrichten 1918):
   Theorem I in the one-dimensional case, on Emmy.

   Reuse searched before writing: emmy.mechanics.lagrange has
   Lagrange-equations, Lagrangian->energy, Gamma and the partials; nothing
   here re-derives them. What this namespace adds is Noether's statement
   for two groups of the plane, each checked as an identity that holds on
   EVERY path (Noether's integral-free form): the time derivative of the
   charge plus a combination of the Lagrange expressions is zero, so on a
   motion (the Lagrange expressions zero) the charge is constant.

     time translation  L does not contain t: the charge is the energy
                       E = p . v - L, and dE/dt = - v . psi
     rotation          L depends on x, y only through r = |q|: the charge
                       is J = x p_y - y p_x, and dJ/dt = - (x psi_y - y psi_x)

   The perturbed Lagrangians break one group each: a time-dependent
   strength breaks time translation (energy is lost, J kept); a potential
   that depends on the angle breaks rotation (J is lost, E kept)."
  (:require [alexandria.grade :as grade]
            [emmy.env :as e]
            [emmy.mechanics.lagrange :as L]
            [hive-dsl.result :as r]))

(defn r-of [x y] (e/sqrt (e/+ (e/square x) (e/square y))))

(defn L-central
  "A point of mass m in the plane with potential energy V(r), V a function
   of the distance r to the centre only: invariant under time translation
   and under every rotation about the centre."
  [m V]
  (fn [[_ [x y] [vx vy]]]
    (e/- (e/* 1/2 m (e/+ (e/square vx) (e/square vy))) (V (r-of x y)))))

(defn L-time-broken
  "The Kepler Lagrangian with a strength k(t) that changes in time: still
   rotation invariant, no longer invariant under t -> t + epsilon."
  [m k]
  (fn [[t [x y] [vx vy]]]
    (e/+ (e/* 1/2 m (e/+ (e/square vx) (e/square vy))) (e// (k t) (r-of x y)))))

(defn L-rotation-broken
  "A central potential plus a term W(x) in x alone: still time invariant,
   no longer invariant under rotations."
  [m V W]
  (fn [[_ [x y] [vx vy]]]
    (e/- (e/* 1/2 m (e/+ (e/square vx) (e/square vy))) (V (r-of x y)) (W x))))

(defn energy
  "The charge of time translation along a path: Emmy's Lagrangian->energy,
   E = p . v - L."
  [Lf path]
  (e/compose (L/Lagrangian->energy Lf) (L/Gamma path)))

(defn angular-momentum
  "The charge of rotation along a path: the generator of rotations is
   delta q = (-y, x), so the charge is p . delta q = x p_y - y p_x, with
   p = dL/dv (Emmy's (partial 2))."
  [Lf path]
  (fn [t]
    (let [local ((L/Gamma path) t)
          [_ [x y] _] local
          p (((e/partial 2) Lf) local)]
      (e/- (e/* x (nth p 1)) (e/* y (nth p 0))))))

(defn- psi
  "The Lagrange expressions along path at 't, Noether's psi: Emmy's
   Lagrange-equations are d/dt dL/dv - dL/dq = - psi."
  [Lf path]
  (e/- (((L/Lagrange-equations Lf) path) 't)))

(defn energy-identity
  "dE/dt + v . psi along an arbitrary path: zero for a time invariant L."
  [Lf path]
  (let [ps (psi Lf path) v ((e/D path) 't)]
    (e/simplify (e/+ ((e/D (energy Lf path)) 't) (e/* (nth v 0) (nth ps 0)) (e/* (nth v 1) (nth ps 1))))))

(defn momentum-identity
  "dJ/dt + (x psi_y - y psi_x) along an arbitrary path: zero for a
   rotation invariant L."
  [Lf path]
  (let [ps (psi Lf path) q (path 't)]
    (e/simplify (e/+ ((e/D (angular-momentum Lf path)) 't)
                     (e/- (e/* (nth q 0) (nth ps 1)) (e/* (nth q 1) (nth ps 0)))))))

(defmacro ^:private on-path [[x y & fs] & body]
  `(e/with-literal-functions [~x ~y ~@fs] (let [~'path (e/up ~x ~y)] ~@body)))

(defn identities
  "Every identity of the page as {:id :label :expr}, :expr simplified."
  []
  [{:id :time :label "central force: dE/dt = - v . psi on every path, so E is constant on a motion"
    :expr (on-path [x y V] (energy-identity (L-central 'm V) path))}
   {:id :rotation :label "central force: dJ/dt = -(x psi_y - y psi_x), so J = x p_y - y p_x is constant on a motion"
    :expr (on-path [x y V] (momentum-identity (L-central 'm V) path))}
   {:id :time-broken :label "strength k(t): J is still conserved"
    :expr (on-path [x y k] (momentum-identity (L-time-broken 'm k) path))}
   {:id :rotation-broken :label "potential V(r) + W(x): E is still conserved"
    :expr (on-path [x y V W] (energy-identity (L-rotation-broken 'm V W) path))}])

(defn energy-leak
  "For strength k(t): dE/dt + v . psi is - (dk/dt) / r, not zero; the
   symmetry lost is exactly the energy lost."
  []
  (on-path [x y k] (energy-identity (L-time-broken 'm k) path)))

(defn torque-leak
  "For V(r) + W(x): dJ/dt + (x psi_y - y psi_x) is y W'(x), the torque of
   the asymmetric force."
  []
  (on-path [x y V W] (momentum-identity (L-rotation-broken 'm V W) path)))

(defn graded
  "[{:id :label :grade}]: every identity graded :symbolic by alexandria.grade."
  []
  (for [{:keys [id label expr]} (identities)
        :let [res (grade/grade :symbolic [expr] grade/default-tolerance)]]
    {:id id :label label :grade (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails)}))
