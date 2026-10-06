(ns alexandria.kepler.mysterium
  "Kepler, Mysterium Cosmographicum (Tuebingen, 1596): the five regular
   solids nested between the six planetary spheres.

     Saturn | cube | Jupiter | tetrahedron | Mars | dodecahedron | Earth |
     icosahedron | Venus | octahedron | Mercury

   Each solid is inscribed in the sphere outside it and circumscribed about
   the sphere inside it, so the ratio of two neighbouring planetary radii is
   the solid's inradius over its circumradius:

     cube, octahedron            1/sqrt 3
     tetrahedron                 1/3
     dodecahedron, icosahedron   sqrt((5 + 2 sqrt 5)/15)
                                 = phi^2 / (sqrt 3 sqrt(phi^2 + 1))

   The solids come from alexandria.solids (vertices exact in Emmy); the
   ratios are proved by Emmy simplification to 0 and graded by
   alexandria.grade. The comparison is Kepler's own table (Myst. Cosmogr.
   cap. XIV), as given by Dreyer 1906, p. 375: the semidiameter of each
   inner sphere when the outer one is 1000, computed from the solid and
   according to Copernicus (De revolutionibus, 1543).

   The shelf speaks vocabulary: `(radius cube-insphere)` is realized here
   by registration on alexandria.vocab/realize.

   Reuse searched: Emmy has no polyhedra; desargues none in 3D. The solids
   live in alexandria.solids for every era that needs them."
  (:require [alexandria.grade :as grade]
            [alexandria.proofs :as proofs]
            [alexandria.solids :as solids]
            [alexandria.vocab :as vocab]
            [desargues.board.construction :as c]
            [emmy.env :as e]
            [hive-dsl.result :as r]
            [alexandria.raster :as raster]))

;; ---------------------------------------------------------------------------
;; The model

(def planets
  "From the outside in."
  [:saturn :jupiter :mars :earth :venus :mercury])

(def shells
  "[outer-planet solid inner-planet], Kepler's order."
  (mapv vector planets solids/solid-ids (rest planets)))

(defn- exact-sqrt
  "sqrt of an exact value, never a double. A rational p/q is written
   sqrt(p q)/q, a square root of an integer, which Emmy's simplifier
   cancels."
  [x]
  (let [v (e/simplify x)]
    (cond
      (ratio? v) (e// (e/sqrt (e/literal-number (* (numerator v) (denominator v)))) (denominator v))
      (number? v) (e/sqrt (e/literal-number v))
      :else (e/sqrt v))))

(defn radius
  "Radius of the sphere `which` (:in, :mid or :circum) of solid id, the
   solid's vertices on (+-1, +-1, +-1) or the like (alexandria.solids)."
  [id which]
  (exact-sqrt (which (solids/radii (solids/solids id)))))

(defn ratio
  "Inner radius over outer radius of the shell of solid id; which is :in
   (Kepler's rule) or :mid (the sphere touching the edges)."
  ([id] (ratio id :in))
  ([id which] (e/simplify (e// (radius id which) (radius id :circum)))))

(def ratio-value
  "The ratio as a double: the exact Emmy ratio evaluated by its raster
   kernel (alexandria.raster/value). Memoized, one kernel per ratio."
  (memoize
   (fn
     ([id] (ratio-value id :in))
     ([id which] (raster/value (fn [] (ratio id which)))))))

(defn nested-radii
  "Planet -> radius, Saturn's sphere 1, every inner sphere the inradius of
   the solid outside it; Mercury on the octahedron's insphere, or on its
   midsphere with mercury = :mid. Doubles."
  ([] (nested-radii :in))
  ([mercury]
   (reductions (fn [rr [_ id _]] (* rr (ratio-value id (if (= id :octahedron) mercury :in))))
               1.0 shells)))

;; ---------------------------------------------------------------------------
;; The vocabulary: (radius cube-insphere), (radius octahedron-midsphere) ...

(def ^:private sphere-kinds {"insphere" :in "midsphere" :mid "circumsphere" :circum})

(defmethod vocab/realize 'radius [_ [sphere] _env]
  (let [[_ solid kind] (re-matches #"(\w+)-(insphere|midsphere|circumsphere)" (name sphere))]
    (radius (keyword solid) (sphere-kinds kind))))

;; ---------------------------------------------------------------------------
;; Kepler's table (Dreyer 1906, p. 375): inner semidiameter, outer = 1000

(def kepler-table
  "Rows of Kepler's comparison, outer planet first. :computed from the solid
   and :copernicus from De revolutionibus, both as Kepler gives them;
   Mercury also with the octahedron's midsphere (707)."
  [{:outer :saturn :inner :jupiter :solid :cube :computed 577 :copernicus 635}
   {:outer :jupiter :inner :mars :solid :tetrahedron :computed 333 :copernicus 333}
   {:outer :mars :inner :earth :solid :dodecahedron :computed 795 :copernicus 757}
   {:outer :earth :inner :venus :solid :icosahedron :computed 795 :copernicus 794}
   {:outer :venus :inner :mercury :solid :octahedron :computed 577 :copernicus 723
    :midsphere 707}])

(defn comparison
  "Kepler's table with the exact ratios: per row the exact ratio, its value
   x 1000, Kepler's computed value, Copernicus' value and the miss
   (computed - copernicus)/copernicus."
  []
  (mapv (fn [{:keys [solid copernicus midsphere] :as row}]
          (let [v (* 1000 (ratio-value solid))]
            (cond-> (assoc row :exact (ratio solid) :value v
                           :miss (/ (- v copernicus) copernicus))
              midsphere (assoc :midsphere-exact (ratio solid :mid)
                               :midsphere-value (* 1000 (ratio-value solid :mid))
                               :midsphere-miss (/ (- (* 1000 (ratio-value solid :mid)) copernicus)
                                                  copernicus)))))
        kepler-table))

;; ---------------------------------------------------------------------------
;; Graded

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(def identities
  "[label difference] pairs, each exactly 0: the five ratios in closed form."
  (let [r5 (solids/root-five false)
        phi (solids/golden r5)
        golden-form (e// (e/expt phi 4) (e/* 3 (e/+ (e/square phi) 1)))]
    [["tetrahedron: inradius / circumradius = 1/3" (e/- (ratio :tetrahedron) 1/3)]
     ["cube: (inradius / circumradius)^2 = 1/3" (e/- (e/square (ratio :cube)) 1/3)]
     ["octahedron: (inradius / circumradius)^2 = 1/3" (e/- (e/square (ratio :octahedron)) 1/3)]
     ["dodecahedron: (inradius / circumradius)^2 = phi^4 / (3 (phi^2 + 1))"
      (e/- (e/square (ratio :dodecahedron)) golden-form)]
     ["icosahedron: (inradius / circumradius)^2 = phi^4 / (3 (phi^2 + 1))"
      (e/- (e/square (ratio :icosahedron)) golden-form)]
     ["phi^4 / (3 (phi^2 + 1)) = (5 + 2 sqrt 5) / 15"
      (e/- golden-form (e// (e/+ 5 (e/* 2 r5)) 15))]
     ["octahedron: (midradius / circumradius)^2 = 1/2" (e/- (e/square (ratio :octahedron :mid)) 1/2)]]))

(defn graded
  "[{:label :grade}] of the identities (proved), and of Kepler's computed
   column against the exact ratios: |1000 x ratio - Kepler's number| within
   half a unit, the precision he wrote (numeric, the ratio's value from its
   raster kernel: :source :raster)."
  []
  (into (mapv (fn [[label diff]] {:label label :grade (variant (grade/grade :symbolic [diff]))})
              identities)
        (for [{:keys [solid computed value]} (comparison)]
          {:label (str (solids/names solid) ": Kepler's " computed " = 1000 x ratio, to the unit")
           :source raster/source
           :grade (variant (grade/grade :numeric [(- value computed)] 0.5))})))

(defn claim-grade
  "The grade of a shelf check {:claim vocabulary-expression}."
  [check]
  (variant (grade/grade :symbolic [(c/check check {})])))

;; ---------------------------------------------------------------------------
;; The proposition as data

(def proofs-resource "alexandria/kepler/mysterium_cosmographicum.proofs.edn")

(defn proof [] (proofs/read-proofs proofs-resource))
