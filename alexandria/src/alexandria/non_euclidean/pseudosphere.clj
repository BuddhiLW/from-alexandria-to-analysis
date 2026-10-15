(ns alexandria.non-euclidean.pseudosphere
  "Beltrami's pseudosphere (Saggio di interpretazione della geometria
   non-euclidea, 1868): the surface of revolution of the tractrix, on which
   the measure of curvature is the same negative constant everywhere, so
   that a piece of it carries Lobachevsky's plane geometry the way a piece
   of the sphere carries spherical geometry.

     r(u, v) = (R sech v cos u, R sech v sin u, R (v - tanh v)),  v > 0

   The curvature comes from alexandria.gauss.egregium, the Gauss shelf:
   Brioschi's formula from E, F, G alone, and the shape operator from the
   embedding. With radius R the curvature is -1/R^2; R = 1 gives K = -1.

   Each tractrix (u = const) is a geodesic; the circles v = const are not.
   The kernel also returns the curvature of a sphere of the same radius
   (+1/R^2) and the plane's 0, so the page can name the three geometries."
  (:require [alexandria.gauss.egregium :as g]
            [alexandria.grade :as grade]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

(defn- sech-exp [v] (e/divide 2 (e/+ (e/exp v) (e/exp (e/- 0 v)))))
(defn- tanh-exp [v] (e/divide (e/- (e/exp v) (e/exp (e/- 0 v))) (e/+ (e/exp v) (e/exp (e/- 0 v)))))

(defn pseudosphere
  "The pseudosphere of radius R: (u, v) -> xyz, cosh/tanh written by exp so
   Emmy's simplifier and raster both read it."
  [R]
  (fn [u v]
    [(e/* R (sech-exp v) (e/cos u))
     (e/* R (sech-exp v) (e/sin u))
     (e/* R (e/- v (tanh-exp v)))]))

(defn sphere
  "The sphere of radius R in the same parameters (u longitude, v latitude),
   for comparison."
  [R]
  (fn [u v]
    [(e/* R (e/cos v) (e/cos u)) (e/* R (e/cos v) (e/sin u)) (e/* R (e/sin v))]))

(defn probe
  "Kernel at (R, u, v): [x y z E F G K-brioschi K-shape -1/R^2]."
  [R u v]
  (fn [_]
    (let [r (pseudosphere R)
          [E F G] ((g/first-form r) u v)]
      (conj (vec (r u v)) E F G
            ((g/brioschi (g/first-form r)) u v)
            ((g/shape-K r) u v)
            (e/divide -1 (e/square R))))))

(def probe-index {:x 0 :y 1 :z 2 :E 3 :F 4 :G 5 :K 6 :Ks 7 :K-expected 8})

(defn sphere-patch
  "The sphere of radius R in the pseudosphere's parameter box: longitude u,
   latitude v - 3/2 (v in [0.05 3])."
  [R]
  (fn [u v] ((sphere R) u (e/- v 1.5))))

(defn surface-of
  "Surface k (0 the pseudosphere, 1 the sphere) of radius R at (u, v): the
   two blended by k, which is exactly 0 or 1."
  [k R u v]
  (mapv (fn [a b] (e/+ (e/* (e/- 1 k) a) (e/* k b)))
        ((pseudosphere R) u v) ((sphere-patch R) u v)))

(defn probe-both
  "Kernel at (k, R, u, v): the probe of surface k, [x y z E F G K-brioschi
   K-shape K-expected] with K-expected -1/R^2 or +1/R^2; every curvature by
   alexandria.gauss.egregium."
  [k R u v]
  (fn [_]
    (let [one (fn [r expected]
                (let [[E F G] ((g/first-form r) u v)]
                  (conj (vec (r u v)) E F G ((g/brioschi (g/first-form r)) u v) ((g/shape-K r) u v) expected)))
          a (one (pseudosphere R) (e/divide -1 (e/square R)))
          b (one (sphere-patch R) (e/divide 1 (e/square R)))]
      (mapv (fn [x y] (e/+ (e/* (e/- 1 k) x) (e/* k y))) a b))))

(defn tractrix "The meridian u = const as a curve in v." [R u] (fn [v] ((pseudosphere R) u v)))
(defn parallel-circle "The circle v = const." [R v] (fn [u] ((pseudosphere R) u v)))

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn metric
  "Emmy's E, F, G of the unit pseudosphere, simplified."
  []
  (mapv e/simplify ((g/first-form (pseudosphere 1)) 'u 'v)))

(defn graded
  "K = -1/R^2 from the metric alone (Brioschi, the Gauss shelf), checked
   against the embedding, at sample points and radii."
  []
  (let [pts (for [R [0.5 1.0 2.0] u [0.0 1.3 4.0] v [0.3 1.0 2.2]]
              (raster/value (fn [_] ((probe R u v) nil)) 0))
        kb (for [o pts] (- (nth o 6) (nth o 8)))
        ks (for [o pts] (- (nth o 6) (nth o 7)))]
    [{:label "Brioschi's K from E, F, G alone (alexandria.gauss.egregium) is −1/R² at every sampled point"
      :grade (variant (grade/grade :numeric kb 1e-8))}
     {:label "the same K from the embedding, (LN − M²)/(EG − F²)²"
      :grade (variant (grade/grade :numeric ks 1e-8))}
     {:label "the meridians meet every circle v = const at right angles: F = 0"
      :grade (variant (grade/grade :symbolic [(second (metric))]))}]))
