(ns alexandria.euclid.elements-scenes
  "Elements I.1 (scene :euclid/I.1) and I.47 (scene :euclid/I.47).

   Every moving point comes from the :motion figure of
   alexandria.euclid.elements through ctx :figures, which is a raster
   kernel in the browser. The given points come from ctx :data, which the
   board's raster frame computed on the JVM. A scene does no coordinate
   arithmetic of its own.

     I.1   Manim's Create: each circle of Post. 3 is traced from its
           radius (the kernel turns B about A by s 2 pi, s up to the
           step's progress), then the joins are drawn
     I.47  the windmill: each square is sheared along a side of the
           triangle (I.41), turned a right angle about B or C (I.4), and
           sheared again into its rectangle under the big square"
  (:require [alexandria.medium.anim :as a]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            #?(:cljs [alexandria.medium.player :as player])))

#?(:cljs (def render "Clerk's render-fn for these scenes." player/render))

(def ^:private two-pi (* 2 3.141592653589793))
(def ^:private arc-steps 72)

(defn- traced
  "The circle about centre through point pt, traced up to fraction f: the
   points of the :motion kernel at s = 0, 1/72, .., f."
  [figures [cx cy] [x y] f]
  (let [n (max 1 (long (* f arc-steps)))
        states (mapv (fn [i] [x y 0 0 0 0 0 0 0 0 (* f (/ i n))]) (range (inc n)))]
    (figure/points (:motion figures) [cx cy two-pi 0 0] states)))

(defn- label [palette [x y] s]
  (svg/text [x (+ y 0.08)] s {:size 0.1 :italic? true :colour (:ink palette)}))

(defn- line [palette p q f colour]
  (svg/polyline [p q] {:stroke (get palette colour) :width 0.014 :attrs (a/create f)}))

(defn- i-1 [palette figures {{:keys [A B C]} :points} {:keys [ca cb join radii]}]
  (let [circle (fn [centre through f]
                 (when (pos? f)
                   (svg/polyline (traced figures centre through f)
                                 {:stroke (:construction palette) :width 0.01})))]
    [:g (circle A B ca) (circle B A cb)
     (svg/segment A B {:stroke (:ink palette) :width 0.016})
     (line palette C A join :found) (line palette C B join :found)
     (svg/layer (a/fade radii)
                (svg/text [0 -0.35] "AC = AB = BC" {:size 0.1 :colour (:found palette)}))
     (svg/dot A (:ink palette)) (svg/dot B (:ink palette))
     (when (pos? ca) (svg/dot C (:found palette)))
     (label palette A "A") (label palette B "B")
     (when (pos? ca) (label palette C "C"))]))

(def ^:private i-1-at
  "Each stage's progress map at step progress p, earlier stages complete."
  {:given (fn [_] {})
   :circle-a (fn [p] {:ca (a/play p 0 0.85)})
   :circle-b (fn [p] {:ca 1 :cb (a/play p 0 0.85)})
   :join (fn [p] {:ca 1 :cb 1 :join (a/play p 0 0.7)})
   :radii (fn [p] {:ca 1 :cb 1 :join 1 :radii (a/play p 0 0.5)})
   :equal (fn [_] {:ca 1 :cb 1 :join 1 :radii 1})
   :done (fn [_] {:ca 1 :cb 1 :join 1 :radii 1})})

(doseq [stage (keys i-1-at)]
  (defmethod scene/draw [:euclid/I.1 stage] [_ st p {:keys [palette figures data]}]
    (i-1 palette figures data
         (merge {:ca 0 :cb 0 :join 0 :radii 0} ((i-1-at (scene/stage-kind st)) p)))))

;; ---------------------------------------------------------------------------
;; I.47

(defn- moved
  "The corners of one half of the windmill at slides wa, wb and turn s."
  [figures {:keys [centre theta states]} wa s wb]
  (figure/points (:motion figures) [(first centre) (second centre) theta wa wb]
                 (mapv #(assoc % 10 s) states)))

(defn- phases
  "Shear, turn, shear over a step: [wa s wb] at progress p."
  [p]
  [(a/play p 0 0.3) (a/play p 0.35 0.65) (a/play p 0.7 1)])

(defn- i-47 [palette figures {:keys [left right points]} {:keys [squares parallel tri l r]}]
  (let [{:keys [A B C D E F G H K L M]} points
        sq (fn [ps colour f]
             (svg/layer (a/fade f) (svg/polygon ps {:stroke (get palette colour) :fill (get palette colour)
                                                    :opacity 0.25 :width 0.012})))
        half (fn [h [wa s wb] colour]
               (svg/polygon (moved figures h wa s wb)
                            {:stroke (get palette colour) :fill (get palette colour)
                             :opacity 0.45 :width 0.014}))]
    [:g (svg/polygon [A B C] {:stroke (:ink palette) :width 0.018})
     (sq [B D E C] :muted squares)
     (sq [A B F G] :construction squares) (sq [A C K H] :construction squares)
     (line palette A L parallel :ink)
     (svg/layer (a/fade tri)
                (svg/polygon [A B D] {:stroke (:found palette) :width 0.014})
                (svg/polygon [F B C] {:stroke (:found palette) :width 0.014}))
     (when l (half left l :found))
     (when r (half right r :construction))
     (into [:g] (map (fn [[k pt]] (label palette pt (name k))))
           {:A A :B B :C C :D D :E E :F F :G G :H H :K K :L L})
     (svg/layer (a/fade (if (= [1 1 1] r) 1 0))
                (svg/text [(first M) 2.65] "BL + CL = BDEC" {:size 0.13 :colour (:found palette)}))]))

(def ^:private i-47-at
  {:given (fn [_] {})
   :squares (fn [p] {:squares (a/play p 0 0.7)})
   :parallel (fn [p] {:squares 1 :parallel (a/play p 0 0.7)})
   :triangles (fn [p] {:squares 1 :parallel 1 :tri (a/play p 0 0.5)})
   :shear-left (fn [p] {:squares 1 :parallel 1 :l (phases p)})
   :shear-right (fn [p] {:squares 1 :parallel 1 :l [1 1 1] :r (phases p)})
   :done (fn [_] {:squares 1 :parallel 1 :l [1 1 1] :r [1 1 1]})})

(doseq [stage (keys i-47-at)]
  (defmethod scene/draw [:euclid/I.47 stage] [_ st p {:keys [palette figures data]}]
    (i-47 palette figures data
          (merge {:squares 0 :parallel 0 :tri 0} ((i-47-at (scene/stage-kind st)) p)))))
