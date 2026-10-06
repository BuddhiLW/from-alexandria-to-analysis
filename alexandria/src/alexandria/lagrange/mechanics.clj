(ns alexandria.lagrange.mechanics
  "Lagrange, Mecanique analytique (Paris 1788): the equations of motion from
   the principle of virtual velocities, on Emmy.

   Reuse searched before writing: emmy.mechanics.lagrange has the whole
   machinery (Lagrange-equations, Euler-Lagrange-operator, L-pendulum,
   L-Kepler-polar, F->C, Lagrangian->energy, Lagrangian->state-derivative)
   and Lagrangian->state-derivative gives the equations of motion as a
   function; raster integrates them (alexandria.raster/ode: the state
   derivative lowered to a raster kernel, raster.ode's RK4). Nothing here
   re-derives them. What this namespace adds is the historical objects:

     double pendulum   Lagrange's coordinates (two angles) through Emmy's F->C
     virtual-work      the 1788 route: sum (F - m a) . dr/dq = - the Lagrange
                       expression, for any system of points and coordinates
     kepler            the inverse-square Lagrangian: phi is cyclic, so
                       r^2 phi' is constant (Kepler's second law), and every
                       conic r = p / (1 + e cos phi) solves the equations
     portraits         phase curves of the pendulum and a double-pendulum
                       trajectory, integrated by raster"
  (:require [alexandria.grade :as grade]
            [alexandria.proofs :as proofs]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [emmy.mechanics.lagrange :as L]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; Systems

(defn double-pendulum-coordinates
  "Lagrange's choice of coordinates: the two angles from the vertical, to the
   rectangular positions of the two bobs (y up, pivot at the origin)."
  [l1 l2]
  (fn [[_ [a b] _]]
    (let [x1 (e/* l1 (e/sin a)) y1 (e/- (e/* l1 (e/cos a)))]
      (e/up x1 y1 (e/+ x1 (e/* l2 (e/sin b))) (e/- y1 (e/* l2 (e/cos b)))))))

(defn L-two-bobs
  "Two free points of masses m1 m2 in uniform gravity, rectangular coordinates."
  [m1 m2 g]
  (fn [[_ [_ y1 _ y2] [vx1 vy1 vx2 vy2]]]
    (e/- (e/+ (e/* 1/2 m1 (e/+ (e/square vx1) (e/square vy1)))
              (e/* 1/2 m2 (e/+ (e/square vx2) (e/square vy2))))
         (e/+ (e/* m1 g y1) (e/* m2 g y2)))))

(defn L-double-pendulum
  "The double pendulum: the free Lagrangian seen through Lagrange's
   coordinates (Emmy's F->C)."
  [m1 m2 l1 l2 g]
  (e/compose (L-two-bobs m1 m2 g) (L/F->C (double-pendulum-coordinates l1 l2))))

;; ---------------------------------------------------------------------------
;; Virtual velocities -> Lagrange's equations

(defn virtual-work-residual
  "The principle of virtual velocities for points of masses ms pulled by
   gravity g, positioned by coordinates F (a local tuple -> up of x1 y1 x2 y2
   ...): for each coordinate q_k,

     sum_i (F_i - m_i a_i) . dr_i/dq_k  +  (Lagrange's expression)_k

   along the literal path. Zero means Lagrange's equations ARE the virtual
   work of the forces lost (d'Alembert), coordinate by coordinate."
  [ms g F L-fn path]
  (let [pos (fn [t] (F (L/->local t (path t) ((e/D path) t))))
        acc ((e/D (e/D pos)) 't)
        J ((e/D (fn [q] (F (L/->local 't q ((e/D path) 't))))) (path 't))
        n (count ms)
        lost (fn [k] (reduce e/+ (for [i (range n)]
                                   (let [m (nth ms i)
                                         ax (nth acc (* 2 i)) ay (nth acc (inc (* 2 i)))
                                         jx (nth (nth J k) (* 2 i)) jy (nth (nth J k) (inc (* 2 i)))]
                                     (e/+ (e/* (e/- (e/* m ax)) jx)
                                          (e/* (e/- (e/* -1 m g) (e/* m ay)) jy))))))
        LE (((L/Lagrange-equations L-fn) path) 't)]
    (e/up (e/+ (lost 0) (nth LE 0))
          (e/+ (lost 1) (nth LE 1)))))

(defn double-pendulum-virtual-work []
  (e/with-literal-functions [a b]
    (virtual-work-residual ['m_1 'm_2] 'g (double-pendulum-coordinates 'l_1 'l_2)
                           (L-double-pendulum 'm_1 'm_2 'l_1 'l_2 'g) (e/up a b))))

(defn pendulum-equation
  "Emmy's Lagrange equation of the pendulum (L-pendulum), along theta(t)."
  []
  (e/with-literal-functions [theta]
    (e/simplify (((L/Lagrange-equations (L/L-pendulum 'g 'm 'l)) theta) 't))))

(defn pendulum-check
  "The pendulum's Lagrange expression minus m l^2 (theta'' + (g/l) sin theta)."
  []
  (e/with-literal-functions [theta]
    (e/- (((L/Lagrange-equations (L/L-pendulum 'g 'm 'l)) theta) 't)
         (e/* 'm (e/square 'l) (e/+ (((e/expt e/D 2) theta) 't)
                                    (e/* (e// 'g 'l) (e/sin (theta 't))))))))

(defn energy-conserved
  "The time derivative of the pendulum's energy along the Lagrange flow:
   Emmy's Lagrangian->energy differentiated along a path, minus theta' times
   the Lagrange expression. Zero, so on a solution E is constant: the phase
   curves are the level sets of E."
  []
  (e/with-literal-functions [theta]
    (let [Lp (L/L-pendulum 'g 'm 'l)
          E (e/compose (L/Lagrangian->energy Lp) (L/Gamma theta))]
      (e/- ((e/D E) 't)
           (e/* ((e/D theta) 't) (((L/Lagrange-equations Lp) theta) 't))))))

;; ---------------------------------------------------------------------------
;; The Kepler problem

(defn kepler-equations
  "Emmy's Lagrange equations of L-Kepler-polar along (r(t), phi(t))."
  []
  (e/with-literal-functions [r phi]
    (e/simplify (((L/Lagrange-equations (L/L-Kepler-polar 'GM 'm)) (e/up r phi)) 't))))

(defn phi-is-cyclic
  "dL/dphi of the Kepler Lagrangian at a general local tuple: zero."
  []
  (nth (((e/partial 1) (L/L-Kepler-polar 'GM 'm))
        (L/->local 't (e/up 'r 'phi) (e/up 'rdot 'phidot)))
       1))

(defn angular-momentum
  "The momentum conjugate to phi, dL/dphi' = m r^2 phi', as an expression."
  []
  (nth (((e/partial 2) (L/L-Kepler-polar 'GM 'm))
        (L/->local 't (e/up 'r 'phi) (e/up 'rdot 'phidot)))
       1))

(defn second-law
  "The phi-equation minus d/dt (m r^2 phi'): zero, so the phi-equation says
   exactly that m r^2 phi' is constant; the areal velocity r^2 phi' / 2 is
   the same at every moment (Kepler's second law)."
  []
  (e/with-literal-functions [r phi]
    (e/- (nth (((L/Lagrange-equations (L/L-Kepler-polar 'GM 'm)) (e/up r phi)) 't) 1)
         ((e/D (fn [t] (e/* 'm (e/square (r t)) ((e/D phi) t)))) 't))))

(defn conic-residual
  "The Kepler equations at the state of the conic r = p / (1 + e cos phi)
   with p = h^2/GM, moved with angular momentum per mass h (phi' = h / r^2):
   (down 0 0) for every eccentricity e, so the orbit is a conic section."
  []
  (let [GM 'GM h 'h ecc 'e ph 'phi
        p (e// (e/square h) GM)
        R (fn [f] (e// p (e/+ 1 (e/* ecc (e/cos f)))))
        w (fn [f] (e// h (e/square (R f))))
        rdot (fn [f] (e/* ((e/D R) f) (w f)))
        rddot (e/* ((e/D rdot) ph) (w ph))
        phddot (e/* ((e/D w) ph) (w ph))
        local (e/up 't (e/up (R ph) ph) (e/up (rdot ph) (w ph)) (e/up rddot phddot))]
    ((L/Euler-Lagrange-operator (L/L-Kepler-polar GM 'm)) local)))

;; ---------------------------------------------------------------------------
;; Integration (Emmy's state derivative, raster's RK4)

(defn- pendulum-sd [g l] (L/Lagrangian->state-derivative (L/L-pendulum g 1 l)))
(defn- double-sd [m1 m2 l1 l2 g] (L/Lagrangian->state-derivative (L-double-pendulum m1 m2 l1 l2 g)))

(defn- flat [s] (if (or (e/structure? s) (sequential? s)) (mapcat flat s) [s]))

(defn- deriv
  "Emmy's state derivative sd at params as the (fn [t & u]) alexandria.raster/ode
   takes: u the flat leaves of state0 = (up t q v) after the time, the result
   the flat leaves of the derivative after dt/dt. Pure algebra: raster lowers
   it."
  [sd params state0]
  (let [sdp (apply sd params)
        [_ q v] state0
        nq (count (flat q))
        like (fn [proto xs] (if (or (e/structure? proto) (sequential? proto)) (apply e/up xs) (first xs)))]
    (fn [t & us]
      (let [[qs vs] (split-at nq us)]
        (vec (rest (flat (sdp (e/up t (like q qs) (like v vs))))))))))

(defn- integrate
  "[[t q... v...] ...] of sd at params from state0 over t1, every dt, by
   raster's RK4 (alexandria.raster/ode) at dt / substeps."
  [sd params state0 t1 dt substeps]
  (let [{:keys [ts us]} (raster/ode (deriv sd params state0) (vec (rest (flat state0)))
                                    t1 (/ (double dt) substeps))
        rows (map (fn [t u] (into [t] u)) ts us)
        want (map #(* % (double dt)) (range (inc (long (Math/floor (+ (/ t1 dt) 1e-9))))))]
    ;; raster's fixed-step loop may end on a sliver step; keep the sample
    ;; nearest each multiple of dt
    (mapv (fn [w] (apply min-key #(Math/abs (- (double (first %)) w)) rows)) want)))

(defn pendulum-orbit
  "[[theta theta'] ...] of the pendulum (g = 9.81, l = 1) from theta0, theta'0
   over t1 seconds, every dt: Emmy's state derivative, integrated by raster's
   RK4 (alexandria.raster/ode)."
  [theta0 omega0 t1 dt]
  (mapv (fn [[_ th om]] [th om])
        (integrate pendulum-sd [9.81 1.0] (e/up 0 theta0 omega0) t1 dt 1)))

(def portrait-starts
  "Initial states of the phase portrait: librations of growing amplitude, the
   near-separatrix, and two rotations."
  [[0.5 0] [1.2 0] [2.0 0] [2.7 0] [3.1 0] [-3.14159 7.0] [3.14159 -7.0]])

(defn pendulum-portrait
  "The phase curves of portrait-starts, each over one period or 3 s."
  []
  (mapv (fn [[th om]] (pendulum-orbit th om 3.0 0.02)) portrait-starts))

(def ^:private double-substeps
  "RK4 steps per sample of the double pendulum: at dt/10 raster's RK4 holds
   the energy to 2e-8 over 4 s at the samples' dt = 0.05."
  10)

(defn double-pendulum-run
  "[[t a b a' b'] ...] of the double pendulum (unit masses and lengths,
   g = 9.81) from angles a0, b0 at rest: Emmy's state derivative of
   L-double-pendulum, integrated by raster's RK4 at dt/10, sampled every dt."
  [a0 b0 t1 dt]
  (integrate double-sd [1 1 1 1 9.81] (e/up 0 (e/up a0 b0) (e/up 0 0)) t1 dt double-substeps))
(defn double-pendulum-energy
  "Total energy of a double-pendulum sample [t a b a' b']."
  [[t a b ad bd]]
  ((L/Lagrangian->energy (L-double-pendulum 1 1 1 1 9.81)) (L/->local t (e/up a b) (e/up ad bd))))

;; ---------------------------------------------------------------------------
;; Moving figures

(defn conic-figure
  "Figure for media: the orbit r = p / (1 + e cos phi) with p = 1, the
   attracting centre at the origin; params [e], state [phi]."
  [ecc]
  (fn [[phi]]
    (let [r (e// 1 (e/+ 1 (e/* ecc (e/cos phi))))]
      [(e/* r (e/cos phi)) (e/* r (e/sin phi))])))

(defn bobs-figure
  "Figure for media: the double pendulum's two bobs (unit rods, pivot at the
   origin) through Lagrange's coordinates; state [a b end], end 0 the first
   bob, end 1 the second (end is 0 or 1)."
  []
  (fn [[a b end]]
    (let [x1 (e/sin a) y1 (e/- (e/cos a))]
      [(e/+ x1 (e/* end (e/sin b))) (e/- y1 (e/* end (e/cos b)))])))

(def figures
  {:conic {:f conic-figure :params [0.5] :state [0]}
   :bobs {:f bobs-figure :params [] :state [0 0 0]}})

(defn conic-anomaly
  "The angle phi on the conic r = 1/(1 + e cos phi) after fraction t of a
   period, equal areas in equal times: Kepler's equation E - e sin E =
   2 pi t by raster's Brent, then phi from E by a raster kernel."
  [ecc t]
  (let [ecc (double ecc) M (* 2 Math/PI (double t))
        E (if (zero? ecc) M
              (:value (raster/root (fn [E] (e/- E (e/* ecc (e/sin E)) M)) (- M ecc) (+ M ecc))))]
    (raster/value (fn [E] (e/* 2 (e/atan (e/* (e/sqrt (+ 1 ecc)) (e/sin (e// E 2)))
                                         (e/* (e/sqrt (- 1 ecc)) (e/cos (e// E 2))))))
                  E)))

(defn pendulum-swing
  "One swing of the pendulum (g = 9.81, l = 1) from theta0 at rest, the
   angles over one period-ish 2.05 s, integrated by raster: [theta ...]."
  [theta0]
  (mapv first (pendulum-orbit theta0 0.0 2.05 0.025)))

;; ---------------------------------------------------------------------------
;; Graded

(defn- components [x]
  (let [v (e/simplify x)] (if (e/structure? v) (flatten (seq v)) [v])))

(defn- g
  ([kind diffs] (g kind diffs grade/default-tolerance))
  ([kind diffs tol]
   (let [res (grade/grade kind (mapcat components diffs) tol)]
     (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))))

(defn graded
  "[{:id :label :grade}] for the Mecanique analytique; the numeric grade
   carries :engine :raster, the solver that produced its number."
  []
  (let [run (double-pendulum-run 1.2 0.4 4.0 0.05)
        es (map double-pendulum-energy run)
        ;; relative drift of the energy over the raster RK4 run
        drift (/ (- (apply max es) (apply min es)) (Math/abs (double (first es))))]
    [{:id :virtual-velocities :label "virtual work of the lost forces = Lagrange's expression (double pendulum)"
      :grade (g :symbolic [(double-pendulum-virtual-work)])}
     {:id :pendulum :label "Lagrange's equation of the pendulum: m l^2 (theta'' + (g/l) sin theta) = 0"
      :grade (g :symbolic [(pendulum-check)])}
     {:id :pendulum :label "energy is constant along the motion"
      :grade (g :symbolic [(energy-conserved)])}
     {:id :double-pendulum :label "the double pendulum integrated by raster (RK4) keeps its energy to 1e-6 (4 s)"
      :grade (g :numeric [drift] 1e-6) :engine :raster}
     {:id :kepler :label "phi is cyclic: dL/dphi = 0"
      :grade (g :symbolic [(phi-is-cyclic)])}
     {:id :kepler :label "the phi-equation is d/dt (m r^2 phi') = 0: equal areas in equal times"
      :grade (g :symbolic [(second-law)])}
     {:id :kepler :label "every conic r = p/(1 + e cos phi), p = h^2/GM, solves the equations"
      :grade (g :symbolic [(conic-residual)])}]))

;; ---------------------------------------------------------------------------
;; The proofs as data

(def proofs-resource "alexandria/lagrange/mecanique_analytique.proofs.edn")

(defn proof [] (proofs/read-proofs proofs-resource))
