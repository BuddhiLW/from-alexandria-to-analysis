(ns alexandria.noether.orbit
  "Orbits of the three Lagrangians of alexandria.noether.conservation,
   integrated by raster, with their energy and angular momentum.

   Emmy's Lagrangian->state-derivative gives the equations of motion as an
   Emmy function; alexandria.raster/ode lowers it to a raster kernel and runs
   raster's RK4 (the pattern of alexandria.lagrange.mechanics). E and J are
   Emmy functions of the state (t x y vx vy), sampled by raster kernels
   (alexandria.raster/sample). Unit mass, inverse-square strength 1.

     :symmetric  V = -1/r                   E and J constant
     :time       strength k(t) = 1 + eps t  J constant, E drifts
     :rotation   V = -1/r + eps x^2 / 2     E constant, J oscillates"
  (:require [alexandria.noether.conservation :as c]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [emmy.mechanics.lagrange :as L]))

(defn lagrangian
  "The Lagrangian of case kind at perturbation strength eps."
  [kind eps]
  (case kind
    :symmetric (c/L-central 1 (fn [r] (e/- (e// 1 r))))
    :time (c/L-time-broken 1 (fn [t] (e/+ 1 (e/* eps t))))
    :rotation (c/L-rotation-broken 1 (fn [r] (e/- (e// 1 r))) (fn [x] (e/* 1/2 eps (e/square x))))))

(defn- local [t x y vx vy] (L/->local t (e/up x y) (e/up vx vy)))

(defn- deriv
  "(fn [t x y vx vy] -> [x' y' vx' vy']) from Emmy's state derivative."
  [Lf]
  (let [sd (L/Lagrangian->state-derivative Lf)]
    (fn [t x y vx vy]
      (let [[_ [dx dy] [ax ay]] (sd (local t x y vx vy))]
        [dx dy ax ay]))))

(defn charges
  "{:E f :J f}, each an Emmy function of (t x y vx vy): the energy p . v - L
   and the angular momentum x p_y - y p_x of Lf."
  [Lf]
  {:E (fn [t x y vx vy] ((L/Lagrangian->energy Lf) (local t x y vx vy)))
   :J (fn [t x y vx vy]
        (let [p (((e/partial 2) Lf) (local t x y vx vy))]
          (e/- (e/* x (nth p 1)) (e/* y (nth p 0)))))})

(def start
  "The initial state [x y vx vy]: an ellipse of eccentricity 0.5 at
   perihelion. Semi-latus rectum p = 1, so r = p / (1 + e) = 2/3, and the
   speed is h / r = 1.5 with h = sqrt(p) = 1 (strength 1)."
  [(/ 2.0 3) 0.0 0.0 (Math/sqrt 2.25)])

(defn run
  "{:kind :eps :samples [{:t :x :y :E :J} ...]} of case kind over t1, every
   dt, RK4 at dt / substeps."
  ([kind eps] (run kind eps 24.0 0.06 8))
  ([kind eps t1 dt substeps]
   (let [Lf (lagrangian kind eps)
         {:keys [ts us]} (raster/ode (deriv Lf) start t1 (/ dt substeps))
         rows (->> (map (fn [t u] (into [t] u)) ts us) (take-nth substeps) vec)
         {:keys [E J]} (charges Lf)
         Es (raster/sample E rows)
         Js (raster/sample J rows)]
     {:kind kind :eps eps
      :samples (mapv (fn [[t x y] en j] {:t t :x x :y y :E en :J j}) rows Es Js)})))

(defn spread
  "max - min of key k over a run's samples."
  [{:keys [samples]} k]
  (let [vs (map k samples)] (- (apply max vs) (apply min vs))))

(defn graded
  "The numeric claims of the widget: [{:id :label :grade :engine}]. The
   rotation-breaking strength is kept at 0.01: from about 0.05 on, J swings
   through 0, the orbit falls into the 1/r singularity, and no fixed-step
   integrator keeps anything."
  []
  (let [sym (run :symmetric 0.0) tim (run :time 0.05) rot (run :rotation 0.01)
        num (fn [ok?] (if ok? :grade/numeric :grade/fails))]
    [{:id :orbit :label "central force: E and J constant to 1e-6 over 24 time units (raster RK4)"
      :grade (num (and (< (spread sym :E) 1e-6) (< (spread sym :J) 1e-6))) :engine :raster}
     {:id :orbit :label "strength k(t) = 1 + 0.05 t: J constant to 1e-6, E changes by more than 0.01"
      :grade (num (and (< (spread tim :J) 1e-6) (> (spread tim :E) 0.01))) :engine :raster}
     {:id :orbit :label "potential -1/r + 0.01 x^2/2: E constant to 1e-6, J changes by more than 0.01"
      :grade (num (and (< (spread rot :E) 1e-6) (> (spread rot :J) 0.01))) :engine :raster}]))
