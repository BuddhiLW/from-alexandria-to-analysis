(ns alexandria.euler.polyhedra-scenes
  "S + H = A + 2 (E230), drawn (alexandria.medium.scene/draw): the cube,
   the count on the regular solids, the cube flattened into its Schlegel
   diagram (the :cube figure of alexandria.euler.polyhedra), and Cauchy's
   1813 reduction, face by face and triangle by triangle, with V - E + F
   read off at every step.

   ctx :data: :cube the vertices, :faces the six faces (the outer last),
   :steps Cauchy's stages {:move :faces :counts}, :solids the five counts."
  (:require [alexandria.euler.draw :as d]
            [alexandria.medium.anim :as a]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.plane :as plane]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            #?(:cljs [alexandria.medium.player :as player])))

#?(:cljs (def render
           "Clerk's render-fn for these scenes."
           player/render))

(def ^:private scene-id :euler/polyhedra)

(defn- projected
  "The cube's vertices at flattening t, from the figure."
  [figures cube t]
  (figure/points (:cube figures) [t] cube))

(defn- cycle-edges [f] (map vector f (concat (rest f) [(first f)])))

(defn- edges-of [faces] (distinct (map (comp vec sort) (mapcat cycle-edges faces))))

(defn- vef [{:keys [V E F]}] (str "V - E + F = " V " - " E " + " F " = " (+ (- V E) F)))

(defn- draw-complex
  "Faces filled faintly, edges, vertices, all at the plane points pts."
  [palette pts faces {:keys [fill-f edge-f highlight] :or {fill-f 1 edge-f 1}}]
  (into [:g]
        (concat
         (map (fn [f] (svg/polygon (mapv pts f) {:fill (if (= f highlight) (:found palette) (:construction palette))
                                                 :opacity (* (if (= f highlight) 0.55 0.12) fill-f)}))
              faces)
         (map (fn [[u v]] (d/stroke [(pts u) (pts v)] (:ink palette) 0.025 edge-f)) (edges-of faces))
         (map (fn [p] (d/dot p (:ink palette) 0.05 edge-f)) (distinct (map pts (apply concat faces)))))))

(defmethod scene/draw [scene-id :cube] [_ _ p {:keys [palette data figures]}]
  (let [pts (projected figures (:cube data) 0)]
    [:g (draw-complex palette pts (:faces data) {:edge-f (a/play p 0 0.6) :fill-f (a/play p 0.3 0.7)})
     (d/readout palette (a/play p 0.5 0.8) [1.6 1.6]
                ["S = 8 solid angles" "A = 12 edges" "H = 6 faces" "8 + 6 = 12 + 2"])]))

(defmethod scene/draw [scene-id :solids] [_ _ p {:keys [palette data figures]}]
  (let [fs (a/lagged p (count (:solids data)) 0.5 0.05 0.8)
        pts (projected figures (:cube data) 0)]
    [:g (svg/layer {:opacity 0.4} (draw-complex palette pts (:faces data) {}))
     (into [:g] (map-indexed (fn [i [{:keys [name V E F]} f]]
                               (d/readout palette f [1.1 (- 1.7 (* 0.36 i))]
                                          [(str name ": " V " - " E " + " F " = " (+ (- V E) F))]))
                             (map vector (:solids data) fs)))]))

(defmethod scene/draw [scene-id :flatten] [_ _ p {:keys [palette data figures]}]
  (let [t (a/play p 0.05 0.8)
        pts (projected figures (:cube data) t)]
    [:g (draw-complex palette pts (:faces data) {:highlight (peek (:faces data))})
     (d/readout palette (a/play p 0.7 0.95) [1.6 1.9]
                ["seen from just above the top face:" "top = the outside, bottom = the inner square"
                 (vef (:counts (first (:steps data))))] {:size 0.07 :step 0.19})]))

(defn- flat [figures data] (projected figures (:cube data) 1))

(defmethod scene/draw [scene-id :remove-face] [_ _ p {:keys [palette data figures]}]
  (let [pts (flat figures data)
        [closed opened] (:steps data)
        f (a/play p 0.1 0.6)]
    [:g (svg/layer {:opacity (- 1 f)}
                   (svg/polygon (mapv pts (peek (:faces data))) {:fill (:found palette) :opacity 0.3}))
     (draw-complex palette pts (:faces opened) {})
     (d/readout palette 1 [1.6 1.9] [(vef (:counts (if (< f 0.5) closed opened)))] {:size 0.07})]))

(defmethod scene/draw [scene-id :triangulate] [_ _ p {:keys [palette data figures]}]
  (let [pts (flat figures data)
        diag (filter #(= :diagonal (:move %)) (:steps data))
        n (count diag)
        k (min (dec n) (long (* n (a/play p 0.05 0.9 a/linear))))
        s (nth diag k)]
    [:g (draw-complex palette pts (:faces s) {})
     (d/readout palette 1 [1.6 1.9] [(str "diagonals: " (inc k)) (vef (:counts s))] {:size 0.07})]))

(defmethod scene/draw [scene-id :peel] [_ _ p {:keys [palette data figures]}]
  (let [pts (flat figures data)
        peel (vec (cons (last (filter #(= :diagonal (:move %)) (:steps data)))
                        (filter #(= :remove-triangle (:move %)) (:steps data))))
        n (count peel)
        u (* (dec n) (a/play p 0.03 0.92 a/linear))
        k (long (min (dec n) u))
        s (nth peel k)
        going (when (< k (dec n)) (:removed (nth peel (inc k))))
        f (- u k)]
    [:g (draw-complex palette pts (:faces s) {:highlight going})
     (when going
       (svg/layer {:opacity f}
                  (svg/polygon (mapv #(plane/translate (pts %) [0 (* -0.3 f)]) going)
                               {:stroke (:found palette) :width 0.02})))
     (d/readout palette 1 [1.6 1.9] [(str "faces left: " (:F (:counts s))) (vef (:counts s))] {:size 0.07})]))
