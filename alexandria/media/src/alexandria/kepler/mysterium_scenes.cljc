(ns alexandria.kepler.mysterium-scenes
  "The drawings of Kepler's Mysterium Cosmographicum
   (alexandria.kepler.mysterium): the six planetary spheres and the five
   solids between them, turning in space and drawn by orthographic
   projection, one shell at a time. Manim's idiom (alexandria.medium.anim):
   edges are created stroke by stroke, the inner sphere grows until it
   touches the faces, the camera zooms in shell by shell.

   ctx :figures :project is the projection kernel (alexandria.kepler.
   mysterium-view/project): params [angle tilt scale], state a vertex
   [x y z], out the point in the plane. ctx :data carries the solids
   (doubles), the sphere radii and Kepler's table."
  (:require [alexandria.medium.anim :as a]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.math :as m]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            #?(:cljs [alexandria.medium.player :as player])))

#?(:cljs (def render "Clerk's render-fn for these scenes." player/render))

(def ^:private scene-id :kepler/mysterium)

(def ^:private order
  [[:saturn :cube :jupiter] [:jupiter :tetrahedron :mars] [:mars :dodecahedron :earth]
   [:earth :icosahedron :venus] [:venus :octahedron :mercury]])

(def ^:private shell-of (into {} (map-indexed (fn [i [_ s _]] [s i]) order)))

(def ^:private planet-names
  {:saturn "Saturn" :jupiter "Jupiter" :mars "Mars" :earth "Earth" :venus "Venus" :mercury "Mercury"})

(defn- fmt [x digits]
  #?(:clj (format (str "%." digits "f") (double x))
     :cljs (.toFixed (js/Number x) digits)))

;; ---------------------------------------------------------------------------
;; Camera: turning angle and zoom

(def ^:private stage-offset
  {:spheres 0 :five 1 :cube 2 :tetrahedron 3 :dodecahedron 4 :icosahedron 5 :octahedron 6
   :table 7 :mercury 8 :verdict 9})

(defn- angle [stage p] (* 0.55 (+ (get stage-offset stage 0) p)))

(def ^:private tilt 0.42)

(defn- zoom-to
  "The zoom that makes the sphere of radius r fill the frame."
  [r] (/ 0.95 r))

(defn- lerp [a b t] (+ a (* t (- b a))))

;; ---------------------------------------------------------------------------
;; Drawing

(defn- write [palette at s opts]
  (svg/text at s (merge {:colour (:ink palette) :size 0.085 :anchor "start"} opts)))

(defn- readout
  "Lines of mono text at the right of the figure, faded in together."
  [palette f lines]
  (svg/layer (a/fade f [0.15 0])
             (into [:g] (map-indexed (fn [i s] (write palette [1.12 (- 0.95 (* 0.15 i))] s {:mono? true :size 0.072}))
                                     lines))))

(defn- sphere
  "The sphere of planet at zoom z, outline created to f, its name faded in."
  [palette radii planet z f & [{:keys [colour fill]}]]
  (let [r (* z (radii planet))]
    (when (and (pos? f) (< r 6))
      [:g
       (svg/circle [0 0] r {:stroke (or colour (:muted palette)) :width 0.008
                            :fill (or fill "none") :opacity 0.06 :attrs (a/create f)})
       (when (< 0.08 r 1.3)
         (svg/layer (a/fade f)
                    (write palette [(* r 0.72) (* r 0.72)] (planet-names planet)
                           {:size (min 0.08 (* 0.25 r)) :colour (:muted palette) :italic? true})))])))

(defn- solid-drawing
  "Solid id inscribed in the sphere of radius R (plane units), turned by
   angle, edges created to f, faces filled to fill."
  [palette figures solids id R ang f fill colour]
  (let [{:keys [vertices edges faces circum]} (solids id)
        pts (figure/points (:project figures) [ang tilt (/ R circum)] vertices)]
    (into [:g]
          (concat
           (when (pos? fill)
             (for [face faces]
               (svg/polygon (map pts face) {:fill colour :opacity (* 0.07 fill)})))
           (when (pos? f)
             (for [[i j] edges]
               (svg/segment (pts i) (pts j) {:stroke colour :width 0.009 :attrs (a/create f)})))))))

(defn- nest
  "Every sphere and every solid up to shell k (exclusive), fully drawn, at zoom z."
  [{:keys [palette figures data]} k z ang]
  (let [{:keys [radii solids]} data]
    (into [:g]
          (concat
           (for [[outer _ _] (take (inc k) order)] (sphere palette radii outer z 1))
           (for [[outer s inner] (take k order)]
             [:g (solid-drawing palette figures solids s (* z (radii outer)) ang 0.6 0.5 (:construction palette))
              (sphere palette radii inner z 1)])))))

(defn- shell-stage
  "Shell k: the camera zooms from shell k - 1 to k, the solid is created in
   the outer sphere, then the inner sphere grows to touch its faces."
  [{:keys [palette figures data] :as ctx} k stage p]
  (let [{:keys [radii solids ratios]} data
        [outer s inner] (order k)
        prev (if (zero? k) :saturn (first (order (dec k))))
        z (lerp (zoom-to (radii prev)) (zoom-to (radii outer)) (a/play p 0 0.25))
        ang (angle stage p)
        grow (a/play p 0.5 0.85)]
    [:g
     (nest ctx k z ang)
     (sphere palette radii outer z 1)
     (solid-drawing palette figures solids s (* z (radii outer)) ang (a/play p 0.15 0.5) (a/play p 0.3 0.6)
                    (:found palette))
     (when (pos? grow)
       (svg/circle [0 0] (* z (radii inner) grow) {:stroke (:ink palette) :width 0.012
                                                  :fill (:ink palette) :opacity 0.05}))
     (sphere palette radii inner z (a/play p 0.82 0.95))
     (readout palette (a/play p 0.55 0.8)
              [(str (planet-names outer) " | " (name s) " | " (planet-names inner))
               (str "inradius / circumradius = " (fmt (ratios s) 4))
               (str "Kepler's table: " (get-in data [:table s :computed]) " / 1000")
               (str "Copernicus:     " (get-in data [:table s :copernicus]) " / 1000")])]))

(doseq [[k [_ s _]] (map-indexed vector order)]
  (defmethod scene/draw [scene-id s] [_ stage p ctx] (shell-stage ctx k stage p)))

(defmethod scene/draw [scene-id :spheres] [_ stage p {:keys [palette data]}]
  (let [{:keys [radii]} data
        fs (a/lagged p 6 0.5 0 0.8)
        z (lerp (zoom-to 1) (zoom-to (radii :mars)) (a/play p 0.55 1))]
    (into [:g (svg/circle [0 0] (* 0.035 (a/play p 0 0.2)) {:fill (:found palette)})]
          (concat (map (fn [pl f] (sphere palette radii pl z f)) (map first (conj order [:mercury])) fs)
                  [(readout palette (a/play p 0.1 0.3) ["six spheres, by Copernicus' distances"
                                                       "Saturn = 1"])]))))

(defmethod scene/draw [scene-id :five] [_ stage p {:keys [palette figures data]}]
  (let [{:keys [solids]} data
        ang (angle stage p)
        fs (a/lagged p 5 0.4 0 0.7)
        spots [[-0.62 0.45] [0.62 0.45] [-0.62 -0.5] [0.62 -0.5] [0 -0.02]]]
    (into [:g (readout palette (a/play p 0.5 0.75) ["Euclid XIII.18:" "exactly five regular solids,"
                                                    "five gaps between six spheres"])]
          (map (fn [[_ s _] [cx cy] f]
                 [:g {:transform (str "translate(" cx "," (- cy) ")")}
                  (solid-drawing palette {:project (:project figures)} solids s 0.36 ang f f (:found palette))
                  (svg/layer (a/fade f) (write palette [-0.18 -0.47] (name s) {:size 0.07 :colour (:muted palette)}))])
               order spots fs))))

(defn- bars
  "Kepler's table as bars: the solid's value against Copernicus', per shell."
  [palette rows f mercury-f]
  (into [:g]
        (map-indexed
         (fn [i {:keys [solid computed copernicus midsphere]}]
           (let [y (- 0.75 (* 0.36 i))
                 w (fn [v] (* 1.6 (/ v 1000) f))
                 kv (if (and midsphere (pos? mercury-f)) (lerp computed midsphere mercury-f) computed)]
             [:g
              (write palette [-1.05 (+ y 0.04)] (name solid) {:size 0.07 :colour (:muted palette)})
              (svg/polygon [[-1.05 (- y 0.02)] [(+ -1.05 (w kv)) (- y 0.02)] [(+ -1.05 (w kv)) (- y 0.1)] [-1.05 (- y 0.1)]]
                           {:fill (:found palette) :opacity 0.8})
              (svg/polygon [[-1.05 (- y 0.12)] [(+ -1.05 (w copernicus)) (- y 0.12)] [(+ -1.05 (w copernicus)) (- y 0.2)] [-1.05 (- y 0.2)]]
                           {:fill (:construction palette) :opacity 0.8})
              (svg/layer (a/fade f)
                         (write palette [(+ -1.0 (w (max kv copernicus))) (- y 0.12)]
                                (str (m/floor (+ kv 0.5)) " : " copernicus) {:mono? true :size 0.065}))]))
         rows)))

(defmethod scene/draw [scene-id :table] [_ _ p {:keys [palette data]}]
  [:g (bars palette (:rows data) (a/play p 0 0.5) 0)
   (readout palette (a/play p 0.4 0.7) ["gold: the solid (Kepler)" "blue: Copernicus" ""
                                        "tetrahedron, icosahedron: fit" "dodecahedron: 5% wide"
                                        "cube: Jupiter 9% short" "octahedron: Mercury 20% short"])])

(defmethod scene/draw [scene-id :mercury] [_ stage p {:keys [palette figures data]}]
  (let [{:keys [radii solids ratios mid-ratio]} data
        R (* 0.95 1)
        ang (angle stage p)
        to-mid (a/play p 0.35 0.7)
        r-in (* R (lerp (ratios :octahedron) mid-ratio to-mid))]
    [:g
     (svg/circle [0 0] R {:stroke (:muted palette) :width 0.008})
     (solid-drawing palette figures solids :octahedron R ang 1 1 (:found palette))
     (svg/circle [0 0] (* R (/ 723 1000)) {:stroke (:construction palette) :width 0.01 :dash "0.03 0.02"})
     (svg/circle [0 0] r-in {:stroke (:ink palette) :width 0.014 :fill (:ink palette) :opacity 0.05})
     (readout palette (a/play p 0 0.3)
              ["Venus | octahedron | Mercury" (str "insphere:  " (fmt (* 1000 (ratios :octahedron)) 0))
               (str "midsphere: " (fmt (* 1000 mid-ratio) 0)) "Copernicus: 723 (dashed)"
               "the midsphere touches the edges:" "the circle in the middle square"])]))

(defmethod scene/draw [scene-id :verdict] [_ stage p {:keys [palette data] :as ctx}]
  (let [{:keys [radii]} data
        z (lerp (zoom-to (radii :venus)) (zoom-to 1) (a/play p 0 0.6 a/double-smooth))]
    [:g (nest ctx 5 z (angle stage p))
     (svg/circle [0 0] (* 0.02 (max 1 (* z 0.05))) {:fill (:found palette)})
     (readout palette (a/play p 0.6 0.9) ["the whole nest, Saturn outside" "Mercury at the centre"])]))
