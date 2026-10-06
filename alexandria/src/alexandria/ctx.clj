(ns alexandria.ctx
  "The CONTEXT port: the surface a construction is carried out on.

   Euclid's postulates read on any surface: a line is a geodesic (Post. 1),
   producing it extends the geodesic (Post. 2), a circle is the locus at
   geodesic distance r (Post. 3). desargues builds constructions in the flat
   plane (and projectively); a context lets the same construction data run on
   a sphere, a hyperbolic plane, a cylinder, any parametric surface.

   A context is, in Emmy's terms, a coordinate system on a 2-manifold with a
   metric (emmy.calculus.manifold, emmy.calculus.metric, FDG). Everything
   below is DERIVABLE from that metric: geodesics from its Christoffel symbols
   (emmy.calculus.connection) and an ODE (emmy.numerical.ode), curvature from
   emmy.calculus.curvature. alexandria.ctx.riemann is that generic
   implementation; a context with closed forms (the plane, the sphere, the
   hyperbolic plane) overrides it for exactness and speed. Values are Emmy
   values, so a closed-form context computes symbolically when handed
   symbols, and numerically (compiled through raster) when handed numbers.

   Points are coordinates in the context's own chart (an Emmy up structure
   or vector). `point` places a point from a tangent vector at the origin, so
   construction data written once means the same on every surface.

   Open by registration: a new context is a record extending Surface plus a
   `make` defmethod. The contract every context passes is
   test/alexandria/ctx_contract.clj."
  (:require [hive-dsl.result :as r]))

(defprotocol Surface
  (metric [ctx]
    "The Emmy metric of the context's chart, (fn [v w] ...) on vector
     fields: the single source the rest derives from.")
  (point [ctx tangent]
    "The point reached by the geodesic from the origin with initial velocity
     tangent [u v] for unit time (the exponential map).")
  (geodesic [ctx p q]
    "The shortest geodesic from p to q, as (fn [t] point), arc-length
     proportional, t = 0 at p and 1 at q; t outside [0 1] produces it.")
  (distance [ctx p q]
    "The geodesic distance from p to q.")
  (circle [ctx center r]
    "The geodesic circle of radius r about center, as (fn [theta] point).")
  (-angle [ctx v p q]
    "SPI: the angle at v between the geodesics to p and to q, in [0 pi],
     for p and q distinct from v. Callers use `angle`, the guarded port
     function, which answers a Result.")
  (meet [ctx curve-a curve-b]
    "Where two curves cross: curves are {:geodesic [p q]} or {:circle [c r]}.
     A vector of {:point x :at [ta tb]}, ta and tb the parameters on each
     curve; [] when they do not cross. Two crossings of circles are ordered
     with the one left of the line of centres first.")
  (curvature [ctx p]
    "The Gaussian curvature at p.")
  (embed [ctx p]
    "p in R^3 (or R^2 for a planar model) for drawing: the coordinates a
     desargues 3D :view or a MathBox / Mafs viewer receives."))

(def degenerate-distance
  "Below this geodesic distance two points count as coincident: no
   geodesic direction leaves one toward the other. 1e-6, not machine
   epsilon: the sphere's acos(u.u) of one point with itself is ~1e-8."
  1.0e-6)

(defn coincident?
  "True when p and q are numerically the same point of ctx. A symbolic
   distance is never judged coincident."
  [ctx p q]
  (let [d (distance ctx p q)]
    (and (number? d) (< (Math/abs (double d)) degenerate-distance))))

(defn angle
  "The angle at v between the geodesics to p and to q, in [0 pi], as a
   hive-dsl Result. Err {:alexandria/error :ctx/degenerate-angle} when p or
   q coincides with v, where no angle exists (Railway: never a silent
   divide by zero, never nil)."
  [ctx v p q]
  (if-let [end (cond (coincident? ctx v p) :p
                     (coincident? ctx v q) :q)]
    (r/err {:alexandria/error :ctx/degenerate-angle :vertex v :coincides end})
    (r/ok (-angle ctx v p q))))

(defmulti make
  "The context named kind, configured by opts. Open: a new context is a
   defmethod in its own namespace (alexandria.ctx.*)."
  (fn [kind _opts] kind))
