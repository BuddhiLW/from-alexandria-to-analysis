(ns alexandria.structure.invariant
  "Domain stratum: Klein's link between a group and a geometry (Erlangen
   1872): a geometry is a set, a group acting on it, and the properties of
   figures the group keeps.

   Property  {:measure (fn [figure] value) :compare kind :label string}
             kind :equal     the value is unchanged (relative tolerance)
                  :vanishes  a defect that is 0 before stays 0 after
             optional :defined? (fn [figure] bool): outside it the property
             does not exist (e.g. a hyperbolic length off the disk) and is
             not kept.
   Geometry  {:name :group (Group and Action) :generators [element ...]
              :properties {key Property}}

   A figure is a vector (or map) of points; the group moves it point by
   point (`transport`). `invariant?` is Klein's test: the property has the
   same value on the figure and on its image under every generator (a
   property every generator keeps is kept by the group they generate).
   `invariants` is the set a geometry keeps on a family of figures, and
   `finer?` compares two geometries (a subgroup keeps a superset).

   The comparison is `holds?`, also usable on before/after values a raster
   kernel computed."
  (:require [alexandria.structure.group :as g]
            [malli.core :as m]))

(def tolerance "Default relative tolerance for an unchanged value." 1e-6)

(def Property
  [:map
   [:measure fn?]
   [:compare [:enum :equal :vanishes]]
   [:label {:optional true} string?]
   [:defined? {:optional true} fn?]
   [:tolerance {:optional true} number?]])

(def Geometry
  [:map
   [:name keyword?]
   [:group some?]
   [:generators [:sequential some?]]
   [:properties [:map-of keyword? Property]]])

(defn geometry
  "A geometry value (validated with malli)."
  [m]
  (m/assert Geometry m)
  m)

(defn holds?
  "Does the value before -> after count as kept, for a property of kind
   compare (:equal or :vanishes) at relative tolerance tol?"
  ([compare before after] (holds? compare before after tolerance))
  ([compare before after tol]
   (let [b (double before) a (double after)]
     (case compare
       :vanishes (< (Math/abs a) tol)
       :equal (< (Math/abs (- a b)) (* tol (max 1.0 (Math/abs b))))))))

(defn transport
  "The image of figure (a vector or a map of points) under element el."
  [group el figure]
  (if (map? figure)
    (update-vals figure #(g/act group el %))
    (mapv #(g/act group el %) figure)))

(defn kept?
  "Property p kept from figure to image (both defined, value held)."
  [p figure image]
  (let [ok? (or (:defined? p) (constantly true))]
    (and (ok? figure) (ok? image)
         (holds? (:compare p) ((:measure p) figure) ((:measure p) image)
                 (or (:tolerance p) tolerance)))))

(defn invariant?
  "Klein's test: property k of geometry G is kept on figure by every
   generator of G's group."
  [G k figure]
  (let [p (get-in G [:properties k])]
    (every? #(kept? p figure (transport (:group G) % figure)) (:generators G))))

(defn invariants
  "The keys of G's properties kept on every figure of figures."
  [G figures]
  (into #{} (filter (fn [k] (every? #(invariant? G k %) figures))) (keys (:properties G))))

(defn finer?
  "True when geometry H keeps at least what G keeps on figures (H's group
   is, on these figures, no larger than G's): Klein's ordering."
  [H G figures]
  (every? (invariants H figures) (invariants G figures)))
