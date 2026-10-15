(ns alexandria.ctx.surfaces
  "Named numeric surfaces as data over alexandria.ctx.riemann. Each defmethod
  registers an open Surface kind with alexandria.ctx/make."
  (:require [alexandria.ctx :as ctx]
            [alexandria.ctx.riemann :as r]))

(def pi Math/PI)
(def tau (* 2.0 Math/PI))

(defn wrap-angle [x]
  (let [y (mod (+ x Math/PI) tau)]
    (- y Math/PI)))

(defn cylinder
  [{:keys [radius] :or {radius 1.0} :as opts}]
  (r/surface
   (merge opts
          {:kind :surface/cylinder
           :embedding (fn [[u v]] [(* radius (Math/cos u)) (* radius (Math/sin u)) v])
           :metric-components (fn [_] [[(* radius radius) 0.0] [0.0 1.0]])
           :curvature-fn (constantly 0.0)
           :geodesic-fn (fn [_ p q]
                          (let [[u0 v0] p
                                [u1 v1] q
                                du (wrap-angle (- u1 u0))
                                dv (- v1 v0)]
                            (fn [t] [(+ u0 (* t du)) (+ v0 (* t dv))])))
           :distance-fn (fn [_ [u0 v0] [u1 v1]]
                          (Math/sqrt (+ (r/sqr (* radius (wrap-angle (- u1 u0))))
                                        (r/sqr (- v1 v0)))))})))

(defn cone
  [{:keys [slope] :or {slope 1.0} :as opts}]
  (let [a (Math/sqrt (+ 1.0 (* slope slope)))]
    (r/surface
     (merge opts
            {:kind :surface/cone
             :embedding (fn [[u v]] [(* u (Math/cos v)) (* u (Math/sin v)) (* slope u)])
             :metric-components (fn [[u _]] [[(* a a) 0.0] [0.0 (* u u)]])
             :curvature-fn (constantly 0.0)}))))

(defn paraboloid
  [{:keys [a] :or {a 1.0} :as opts}]
  (r/surface
   (merge opts
          {:kind :surface/paraboloid
           :embedding (fn [[u v]] [u v (* a (+ (* u u) (* v v)))])
           :curvature-fn (fn [[u v]]
                           (let [rho2 (+ (* u u) (* v v))]
                             (/ (* 4.0 a a)
                                (Math/pow (+ 1.0 (* 4.0 a a rho2)) 2.0))))})))

(defn torus
  [{:keys [major minor] :or {major 2.0 minor 0.5} :as opts}]
  (r/surface
   (merge opts
          {:kind :surface/torus
           :embedding (fn [[u v]]
                        (let [x (+ major (* minor (Math/cos v)))]
                          [(* x (Math/cos u)) (* x (Math/sin u)) (* minor (Math/sin v))]))
           :metric-components (fn [[_ v]] [[(r/sqr (+ major (* minor (Math/cos v)))) 0.0]
                                           [0.0 (* minor minor)]])
           :curvature-fn (fn [[_ v]]
                           (/ (Math/cos v) (* minor (+ major (* minor (Math/cos v))))))})))

(defn catenoid
  [{:keys [a] :or {a 1.0} :as opts}]
  (r/surface
   (merge opts
          {:kind :surface/catenoid
           :embedding (fn [[u v]]
                        [(* a (Math/cosh v) (Math/cos u))
                         (* a (Math/cosh v) (Math/sin u))
                         (* a v)])
           :metric-components (fn [[_ v]]
                                (let [c (Math/cosh v) m (* a a c c)]
                                  [[m 0.0] [0.0 m]]))
           :curvature-fn (fn [[_ v]] (/ -1.0 (* a a (Math/pow (Math/cosh v) 4.0))))})))

(defn pseudosphere
  [{:keys [a] :or {a 1.0} :as opts}]
  (r/surface
   (merge opts
          {:kind :surface/pseudosphere
           :embedding (fn [[u v]]
                        ;; tractricoid, v > 0
                        [(* a (Math/cosh v) (Math/cos u))
                         (* a (Math/cosh v) (Math/sin u))
                         (* a (- v (Math/tanh v)))])
           :metric-components (fn [[_ v]]
                                (let [ch (Math/cosh v)]
                                  [[(* a a ch ch) 0.0]
                                   [0.0 (* a a (Math/pow (Math/tanh v) 2.0))]]))
           :curvature-fn (constantly (/ -1.0 (* a a)))})))

(defmethod ctx/make :surface/cylinder [_ opts] (cylinder opts))
(defmethod ctx/make :surface/cone [_ opts] (cone opts))
(defmethod ctx/make :surface/paraboloid [_ opts] (paraboloid opts))
(defmethod ctx/make :surface/torus [_ opts] (torus opts))
(defmethod ctx/make :surface/catenoid [_ opts] (catenoid opts))
(defmethod ctx/make :surface/pseudosphere [_ opts] (pseudosphere opts))
