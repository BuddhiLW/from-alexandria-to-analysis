(ns alexandria.ctx.riemann
  "Generic Surface implementation for two-dimensional Riemannian coordinate
  charts. Emmy supplies the differential geometry: induced metrics from
  embeddings, Levi-Civita curvature, exact Lagrange equations for geodesic
  flow, ODE integration, and multidimensional minimization for shooting and
  intersections."
  (:refer-clojure :exclude [partial])
  (:require [alexandria.ctx :as ctx]
            [emmy.calculus.basis :as basis]
            [emmy.calculus.connection :as connection]
            [emmy.calculus.covariant :as covariant]
            [emmy.calculus.curvature :as curvature]
            [emmy.calculus.manifold :as manifold]
            [emmy.calculus.metric :as metric]
            [emmy.env :as e]
            [emmy.generic :as g]
            [emmy.numerical.minimize :as minimize]
            [emmy.structure :as s]))

(set! *warn-on-reflection* true)

(def ^:const default-step 0.02)
(def ^:const default-epsilon 1.0e-8)
(def ^:const meet-tolerance 1.0e-6)

(defn sqr [x] (g/* x x))
(defn clamp [lo hi x] (max lo (min hi x)))
(defn up2 [p] (if (vector? p) (s/up (p 0) (p 1)) p))
(defn vec2 [p]
  (let [p (if (and (map? p) (contains? p :spec)) (:spec p) p)]
    [(nth p 0) (nth p 1)]))
(defn v- [p q] (vec2 (g/- (up2 p) (up2 q))))
(defn v+ [p q] (vec2 (g/+ (up2 p) (up2 q))))
(defn v* [a p] (vec2 (g/* a (up2 p))))
(defn norm2 [p] (Math/sqrt (double (g/dot-product (up2 p) (up2 p)))))

(def R2-basis (basis/coordinate-system->basis manifold/R2-rect))
(def R2-point (manifold/point manifold/R2-rect))

(defn emmy-embedding
  "Coerce a chart embedding to an Emmy map R2 -> R3 using up structures."
  [embedding]
  (fn [p]
    (let [r (embedding (up2 p))]
      (if (vector? r) (apply s/up r) r))))

(defn induced-metric-components
  "First fundamental form induced by r(u,v), computed by Emmy's exact
  `embedding-map->metric-components` (which calls `D`) and evaluated at p on
  demand."
  [embedding]
  (let [components (metric/embedding-map->metric-components 2 (emmy-embedding embedding))]
    (fn [p]
      (s/mapr #(% (up2 p)) components))))

(defn components->emmy
  "Coerce numeric component function p -> [[E F] [F G]] to Emmy's component
  structure of scalar fields."
  [metric-components]
  (s/down (s/down (fn [p] (get-in (metric-components (vec2 p)) [0 0]))
                  (fn [p] (get-in (metric-components (vec2 p)) [0 1])))
          (s/down (fn [p] (get-in (metric-components (vec2 p)) [1 0]))
                  (fn [p] (get-in (metric-components (vec2 p)) [1 1])))))

(defn emmy-metric [metric-components]
  (metric/components->metric (components->emmy metric-components) R2-basis))

(defn det [[[E F] [_ G]]]
  (g/- (g/* E G) (g/* F F)))

(defn inverse2 [[[E F] [_ G] :as m]]
  (let [d (det m)]
    [[(g// G d) (g// (g/- F) d)]
     [(g// (g/- F) d) (g// E d)]]))

(defn metric-inner [metric-components p v w]
  (let [[[E F] [_ G]] (metric-components (vec2 p))
        [a b] (vec2 v)
        [c d] (vec2 w)]
    (g/+ (g/* E a c) (g/* F (g/+ (g/* a d) (g/* b c))) (g/* G b d))))

(defn metric-norm [metric-components p v]
  (Math/sqrt (max 0.0 (double (metric-inner metric-components p v v)))))

(defn scalar-curvature
  "Scalar curvature R from Emmy's Ricci tensor and inverse metric; in 2D the
  Gaussian curvature is R/2."
  [metric-components p]
  (let [em (emmy-metric metric-components)
        christoffel (connection/metric->Christoffel-2 em R2-basis)
        nabla (covariant/covariant-derivative (covariant/Christoffel->Cartan christoffel))
        ricci (curvature/Ricci nabla R2-basis)
        vectors (basis/basis->vector-basis R2-basis)
        ginv (inverse2 (metric-components p))
        m (R2-point (up2 p))]
    (reduce g/+
            (for [i (range 2) j (range 2)]
              (g/* (get-in ginv [i j]) ((ricci (nth vectors i) (nth vectors j)) m))))))

(defn gaussian-curvature [metric-components p]
  (g/* 1/2 (scalar-curvature metric-components p)))

(defn kinetic-lagrangian
  "Free-motion Lagrangian L(t,q,v)=1/2 g_q(v,v). Emmy differentiates this exact
  expression in `Lagrangian->state-derivative`; no Christoffel symbols are
  expanded by Alexandria."
  [metric-components]
  (fn [local]
    (let [q (e/coordinate local)
          v (e/velocity local)]
      (g/* 1/2 (metric-inner metric-components q v v)))))

(defn geodesic-state-derivative [metric-components]
  (e/Lagrangian->state-derivative (kinetic-lagrangian metric-components)))

(defn state-advancer [metric-components]
  (e/state-advancer (fn [] (geodesic-state-derivative metric-components))))

(defn local-state [p velocity]
  (e/->L-state 0.0 (up2 p) (up2 velocity)))

(defn integrate-geodesic
  ([metric-components p velocity t] (integrate-geodesic metric-components p velocity t default-step))
  ([metric-components p velocity t _step]
   (if (zero? (double t))
     [(double (nth p 0)) (double (nth p 1))]
     (let [end ((state-advancer metric-components)
                (local-state p velocity)
                (double t)
                {:compile? true :epsilon default-epsilon})]
       (vec2 (e/coordinate end))))))

(defn- objective [metric-components p q velocity]
  (let [end (integrate-geodesic metric-components p velocity 1.0)
        e (norm2 (v- end q))]
    (* e e)))

(defn shoot-velocity
  "Coordinate shooting by Emmy's multidimensional Nelder-Mead minimizer."
  [metric-components p q]
  (let [guess (v- q p)]
    (vec (minimize/multidimensional-minimize
          #(objective metric-components p q %)
          guess
          :simplex-tolerance 1.0e-8))))

(defn geodesic-length [metric-components p velocity]
  (metric-norm metric-components p velocity))

(defn polar-circle [metric-components center r]
  (fn [theta]
    (let [[[E F] [_ G]] (metric-components center)
          a (/ (Math/cos theta) (Math/sqrt (double E)))
          b (/ (- (Math/sin theta) (* (double F) a (/ 1.0 (Math/sqrt (double G)))))
               (Math/sqrt (double G)))]
      (integrate-geodesic metric-components center (v* r [a b]) 1.0))))

(defn curve->fn [ctx curve]
  (cond
    (:geodesic curve) (apply ctx/geodesic ctx (:geodesic curve))
    (:circle curve) (apply ctx/circle ctx (:circle curve))
    :else (throw (ex-info "unknown curve" {:curve curve}))))

(defn- meet-seeds [curve-a curve-b]
  (let [base [[0.0 0.0] [0.25 0.25] [0.5 0.5] [0.75 0.75] [1.0 1.0]
              [0.25 0.75] [0.75 0.25]]
        circle? #(contains? % :circle)]
    (if (or (circle? curve-a) (circle? curve-b))
      (concat base
              (for [a [0.0 (/ Math/PI 2.0) Math/PI (* 3.0 (/ Math/PI 2.0))]
                    b [0.0 0.25 0.5 0.75 1.0]]
                (if (circle? curve-a) [a b] [b a])))
      base)))

(defn- distinct-solution? [solutions point]
  (every? #(> (norm2 (v- (:point %) point)) 1.0e-4) solutions))

(defn meet-curves [ctx curve-a curve-b]
  (let [fa (curve->fn ctx curve-a)
        fb (curve->fn ctx curve-b)
        residual (fn [[ta tb]] (let [d (v- (fa ta) (fb tb))] (sqr (norm2 d))))]
    (reduce
     (fn [solutions seed]
       (try
         (let [[ta tb :as ts] (vec (minimize/multidimensional-minimize
                                    residual seed :simplex-tolerance 1.0e-9))
               ra (residual ts)]
           (if (< ra (* meet-tolerance meet-tolerance))
             (let [pa (fa ta)
                   pb (fb tb)
                   p (v* 0.5 (v+ pa pb))]
               (if (distinct-solution? solutions p)
                 (conj solutions {:point p :at [ta tb]})
                 solutions))
             solutions))
         (catch Throwable _ solutions)))
     []
     (meet-seeds curve-a curve-b))))

(defrecord RiemannSurface [kind embedding metric-components curvature-fn distance-fn geodesic-fn point-fn meet-fn opts]
  ctx/Surface
  (metric [_]
    (emmy-metric metric-components))
  (point [_ tangent]
    (if point-fn
      (point-fn tangent)
      (integrate-geodesic metric-components (:origin opts [0.0 0.0]) tangent 1.0)))
  (geodesic [this p q]
    (if geodesic-fn
      (geodesic-fn this p q)
      (let [v (shoot-velocity metric-components p q)]
        (fn [t] (integrate-geodesic metric-components p v t)))))
  (distance [this p q]
    (if distance-fn
      (distance-fn this p q)
      (geodesic-length metric-components p (shoot-velocity metric-components p q))))
  (circle [_ center r]
    (polar-circle metric-components center r))
  (-angle [_ v p q]
    (let [vp (shoot-velocity metric-components v p)
          vq (shoot-velocity metric-components v q)
          ip (metric-inner metric-components v vp vq)
          np (metric-norm metric-components v vp)
          nq (metric-norm metric-components v vq)]
      (Math/acos (clamp -1.0 1.0 (/ (double ip) (* np nq))))))
  (meet [this curve-a curve-b]
    (if meet-fn
      (meet-fn this curve-a curve-b)
      (meet-curves this curve-a curve-b)))
  (curvature [_ p]
    (if curvature-fn
      (curvature-fn p)
      (gaussian-curvature metric-components p)))
  (embed [_ p]
    (if embedding (embedding (up2 p)) p)))

(defn surface
  "Build a Riemannian Surface from either `:embedding` r(u,v)->R3 or direct
  `:metric-components` p -> [[E F] [F G]]. Optional closed-form hooks may
  override generic shooting for named cases; the metric/connection/curvature
  data are still built through Emmy."
  [{:keys [kind embedding metric-components curvature-fn distance-fn geodesic-fn point-fn meet-fn]
    :as opts}]
  (let [metric-components (or metric-components
                              (when embedding (induced-metric-components embedding)))]
    (when-not metric-components
      (throw (ex-info "surface needs :embedding or :metric-components" {:opts opts})))
    (map->RiemannSurface {:kind (or kind :riemann/parametric)
                          :embedding embedding
                          :metric-components metric-components
                          :curvature-fn curvature-fn
                          :distance-fn distance-fn
                          :geodesic-fn geodesic-fn
                          :point-fn point-fn
                          :meet-fn meet-fn
                          :opts (assoc opts :fidelity :numeric)})))

(defmethod ctx/make :riemann/parametric [_ opts]
  (surface (assoc opts :kind :riemann/parametric)))

(defmethod ctx/make :riemann/metric [_ opts]
  (surface (assoc opts :kind :riemann/metric)))
