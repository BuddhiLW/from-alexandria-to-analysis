(ns alexandria.eudoxus.exhaustion-scenes
  "Elements XII.2 (scene :eudoxus/XII.2): polygons inscribed in a circle,
   their sides doubling until what is left of the circle is less than any
   given area. Vertices come from the :doubling figure of
   alexandria.eudoxus.proportion (a raster kernel in the browser); the
   area shown is the polygon on those vertices."
  (:require [alexandria.medium.anim :as a]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.math :as m]
            [alexandria.medium.plane :as plane]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            #?(:cljs [alexandria.medium.player :as player])))

#?(:cljs (def render "Clerk's render-fn for these scenes." player/render))

(def ^:private scene-id :eudoxus/XII.2)

(def ^:private vertex-states
  (memoize (fn [k] (mapv vector (range (* 2 (bit-shift-left 1 k)))))))

(defn- polygon [figures k t r]
  (figure/points (:doubling figures) [k t r] (vertex-states k)))

(defn- fmt [x digits]
  #?(:clj (format (str "%." digits "f") (double x))
     :cljs (.toFixed (js/Number x) digits)))

(defn- circle-with
  "The circle about [cx cy] of radius r with the polygon of the :doubling
   kernel, and the polygon's share of the circle from ctx :data :shares
   (alexandria.raster on the JVM)."
  [palette figures shares k t [cx cy] r]
  (let [ps (map #(plane/translate % [cx cy]) (polygon figures k t r))
        share (get shares (if (< t 1) k (inc k)))]
    [:g (svg/circle [cx cy] r {:stroke (:ink palette) :width 0.015})
     (svg/polygon ps {:stroke (:found palette) :fill (:found palette) :opacity 0.3 :width 0.012})
     (svg/text [cx (- cy r 0.25)]
               (str (bit-shift-left 1 (if (< t 1) k (inc k))) "-gon: "
                    (if share (fmt share 4) "") " of the circle")
               {:size 0.12 :colour (:ink palette) :mono? true})]))

(defmethod scene/draw [scene-id :polygon] [_ [_ k] p {:keys [palette figures data]}]
  (circle-with palette figures (:shares data) (dec (or k 2)) (a/play p 0.1 0.8) [0 0] 1))

(defmethod scene/draw [scene-id :ratio] [_ _ p {:keys [palette figures data]}]
  (let [k (long (+ 1 (* 4 (a/play p 0 0.8 a/linear))))]
    [:g (circle-with palette figures (:shares data) k 1 [0 0] 1)
     (circle-with palette figures (:shares data) k 1 [2.6 0] 0.6)
     (svg/layer (a/fade (a/play p 0.5 0.8))
                (svg/text [1.3 1.3] "circle : circle = d^2 : d'^2" {:size 0.13 :colour (:found palette)}))]))
