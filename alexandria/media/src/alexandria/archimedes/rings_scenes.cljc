(ns alexandria.archimedes.rings-scenes
  "The circle unrolled ring by ring into the triangle K (scene
   :archimedes/circle-rings), drawn from the :rings figure of
   alexandria.archimedes.circle. ctx :controls :n is the number of rings,
   set with the player's slider (e = r/n); the :limit stage lets n grow by
   itself."
  (:require [alexandria.medium.anim :as a]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.math :as m]
            [alexandria.medium.plane :as plane]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            #?(:cljs [alexandria.medium.player :as player])))

#?(:cljs (def render
           "Clerk's render-fn for these scenes."
           player/render))

(def ^:private scene-id :archimedes/circle-rings)
(def ^:private r 1)
(def ^:private arc-points 48)

;; ---------------------------------------------------------------------------
;; Which points of the :rings figure a frame asks for

(def ^:private outline-states
  "Every ring's outline: its outer edge left to right, then its inner edge
   back."
  (memoize
   (fn [n]
     (vec (for [k (range n)
                [b as] [[1 (range (inc arc-points))] [0 (reverse (range (inc arc-points)))]]
                i as]
            [k (- (/ i arc-points) 1/2) b])))))

(def ^:private line-states
  "Line b (0 inner edge, 1 outer, 1/2 the middle) of ring k, along its length."
  (memoize (fn [k b] (mapv (fn [i] [k (- (/ i arc-points) 1/2) b]) (range (inc arc-points))))))

(def ^:private cut-states
  "The two sides of the cut of ring k, from its inner to its outer edge."
  (memoize (fn [k] [[[k -1/2 0] [k -1/2 1]] [[k 1/2 0] [k 1/2 1]]])))

(defn- rings
  "The outlines of the n rings: ring 0 opened by s0, the others by s, the
   whole leaned by lean."
  [figures n s0 s lean]
  (partition (* 2 (inc arc-points))
             (figure/points (:rings figures) [n s0 s r lean] (outline-states n))))

(defn- length [ps] (reduce + (map (fn [[x1 y1] [x2 y2]] (m/hypot (- x2 x1) (- y2 y1))) ps (rest ps))))

(defn- fmt [x digits]
  #?(:clj (format (str "%." digits "f") (double x))
     :cljs (.toFixed (js/Number x) digits)))

;; ---------------------------------------------------------------------------
;; Drawing

(defn- write
  ([palette at s] (write palette at s {}))
  ([palette at s opts]
   (svg/text at s (-> (merge {:colour (:ink palette) :size 0.12} opts) (update :size * 1.45)))))

(defn- readout [palette f lines]
  (svg/layer (a/fade f [0.2 0])
             (into [:g] (map-indexed (fn [i s] (write palette [1.25 (- 1.2 (* 0.2 i))] s
                                                      {:anchor "start" :size 0.085 :mono? true}))
                                     lines))))

(defn- ring-shapes
  "The ring outlines as filled polygons, alternating gold and blue, each
   faded in by its own f."
  [palette outlines fs]
  (map (fn [ps k f]
         (let [c (if (even? k) (:found palette) (:construction palette))]
           (svg/layer {:opacity f} (svg/polygon ps {:fill c :opacity 0.45 :stroke c :width 0.006}))))
       outlines (range) fs))

(defn- n-of [ctx] (long (get-in ctx [:controls :n] 4)))

(defn- frame [palette & children]
  (into [:g (svg/segment [-3.5 -1] [3.5 -1] {:stroke (:muted palette) :width 0.006 :dash "0.04 0.04"})]
        children))

;; ---------------------------------------------------------------------------
;; Stages

(defmethod scene/draw [scene-id :rings] [_ _ p {:keys [palette figures] :as ctx}]
  (let [n (n-of ctx)]
    (into (frame palette)
          (concat (ring-shapes palette (rings figures n 0 0 0) (a/lagged p n 0.4 0 0.6))
                  [(svg/polyline [[0 0] [0 r]] {:stroke (:ink palette) :width 0.02 :attrs (a/create (a/play p 0.6 0.9))})
                   (readout palette (a/play p 0.3 0.6) [(str "n = " n " rings") (str "e = r/n = " (fmt (/ r n) 3))])]))))

(defmethod scene/draw [scene-id :one-ring] [_ _ p {:keys [palette figures] :as ctx}]
  (let [n (n-of ctx) e (/ r n)
        s0 (a/play p 0.05 0.75)]
    (into (frame palette)
          (concat (ring-shapes palette (rings figures n s0 0 0) (repeat 1))
                  [(readout palette (a/play p 0.7 0.9)
                            [(str "outer edge 2πr = " (fmt (* 2 m/pi r) 3))
                             (str "inner edge 2π(r−e) = " (fmt (* 2 m/pi (- r e)) 3))
                             (str "width e = " (fmt e 3))])]))))

(defmethod scene/draw [scene-id :stretch] [_ _ p {:keys [palette figures] :as ctx}]
  (let [n (n-of ctx) e (/ r n)
        s0 (a/there-and-back-with-pause p)
        pts (fn [states] (figure/points (:rings figures) [n s0 0 r 0] states))
        middle (pts (line-states 0 1/2))
        [cut-l cut-r] (map pts (cut-states 0))]
    (into (frame palette)
          (concat (ring-shapes palette (rings figures n s0 0 0) (cons 1 (repeat 0.35)))
                  [(svg/polyline middle {:stroke (:ink palette) :width 0.014 :dash "0.05 0.03"})
                   (svg/polyline cut-l {:stroke "#E8735A" :width 0.03})
                   (svg/polyline cut-r {:stroke "#E8735A" :width 0.03})
                   (readout palette 1
                            [(str "middle circle " (fmt (length middle) 3) " = 2π(r−e/2)")
                             (str "cut side " (fmt (length cut-l) 3) ": e → e√(1+π²)")
                             "along: kept; across: stretched"])]))))

(defmethod scene/draw [scene-id :stack] [_ _ p {:keys [palette figures] :as ctx}]
  (let [n (n-of ctx)]
    (into (frame palette)
          (concat (ring-shapes palette (rings figures n 1 (a/play p 0 0.9 a/linear) 0) (repeat 1))
                  [(readout palette (a/play p 0.85 1)
                            ["base: C = 2πr" "height: r" "no gaps: each top edge is the next bottom edge"])]))))

(defmethod scene/draw [scene-id :lean] [_ _ p {:keys [palette figures] :as ctx}]
  (let [n (n-of ctx)
        lean (a/play p 0.1 0.8)]
    (into (frame palette)
          (concat (ring-shapes palette (rings figures n 1 1 lean) (repeat 1))
                  [(svg/layer (a/fade (a/play p 0.75 1))
                              (svg/polygon [[(- (* m/pi r)) -1] [(* m/pi r) -1] [(- (* m/pi r)) 0]]
                                           {:stroke (:ink palette) :width 0.016}))
                   (readout palette (a/play p 0.75 1) ["same base, same height:" "the triangle K (Elements I.38)"])]))))

(defn- slab-rects
  "For each of n slabs of K: the inner rectangle (inside the slab) and the
   outer one (containing it), each [x0 x1 y0 y1]."
  [n]
  (let [e (/ r n) left (- (* m/pi r))]
    (for [k (range n)
          :let [outer (- r (* k e)) inner (- outer e)
                y0 (- outer) y1 (- inner)]]
      [[left (+ left (* 2 m/pi inner)) y0 y1]
       [left (+ left (* 2 m/pi outer)) y0 y1]])))

(defn- squeeze-figure [palette figures n f]
  (let [e (/ r n)
        c (* 2 m/pi r)
        inner-sum (reduce + (map (fn [[[x0 x1]]] (* (- x1 x0) e)) (slab-rects n)))]
    (into (frame palette)
          (concat (ring-shapes palette (rings figures n 1 1 1) (repeat 0.5))
                  (mapcat (fn [[[ix0 ix1 y0 y1] [ox0 ox1]] g]
                            [(svg/layer {:opacity g}
                                        (svg/polygon (plane/rect ix0 ix1 y0 y1) {:fill (:found palette) :opacity 0.55})
                                        (svg/polygon (plane/rect ox0 ox1 y0 y1) {:stroke (:construction palette) :width 0.012}))])
                          (slab-rects n) (a/lagged f n 0.3 0 1))
                  [(readout palette 1
                            [(str "n = " n ", e = " (fmt e 4))
                             (str "in " (fmt inner-sum 3) " ≤ circle, K ≤ out " (fmt (+ inner-sum (* c e)) 3))
                             (str "gap = C · e = " (fmt (* c e) 4))
                             (str "πr² = " (fmt (* m/pi r r) 4))])]))))

(defmethod scene/draw [scene-id :squeeze] [_ _ p {:keys [palette figures] :as ctx}]
  (squeeze-figure palette figures (n-of ctx) (a/play p 0 0.8)))

(defmethod scene/draw [scene-id :limit] [_ _ p {:keys [palette figures]}]
  (squeeze-figure palette figures (long (m/floor (+ 2 (* 46 (a/play p 0 0.85 a/rush-from))))) 1))

(defmethod scene/draw [scene-id :lines] [_ _ p {:keys [palette figures]}]
  (let [n 24
        s (a/play p 0 0.75 a/linear)
        lean (a/play p 0.8 1)
        circles (map (fn [k] (figure/points (:rings figures) [n s s r lean] (line-states k 1))) (range n))]
    (into (frame palette)
          (concat (map-indexed (fn [k ps] (svg/polyline ps {:stroke (if (even? k) (:found palette) (:construction palette))
                                                             :width 0.012}))
                               circles)
                  [(readout palette (a/play p 0 0.2)
                            ["a circle of radius ρ = a line of length 2πρ"
                             "the circle, made of its circles;"
                             "K, made of its lines"])]))))
