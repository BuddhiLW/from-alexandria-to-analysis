(ns alexandria.medium.plane
  "Points of the plane as [x y] vectors, and the arithmetic a moving figure
   needs. Pure."
  (:require [alexandria.medium.math :as m]))

(defn lerp
  "a at t = 0, b at t = 1."
  [a b t]
  (+ a (* t (- b a))))

(defn lerp-point [[ax ay] [bx by] t] [(lerp ax bx t) (lerp ay by t)])

(defn translate [[x y] [dx dy]] [(+ x dx) (+ y dy)])

(defn rotate-about
  "Point p turned by angle a (radians, counterclockwise) about centre c."
  [[px py] [cx cy] a]
  (let [dx (- px cx) dy (- py cy) co (m/cos a) si (m/sin a)]
    [(+ cx (- (* co dx) (* si dy))) (+ cy (* si dx) (* co dy))]))

(defn area
  "Area of the simple polygon with vertices ps, in order."
  [ps]
  (let [ps (vec ps) n (count ps)]
    (m/abs (/ (reduce + (for [i (range n)
                        :let [[x1 y1] (ps i) [x2 y2] (ps (mod (inc i) n))]]
                    (- (* x1 y2) (* x2 y1))))
        2))))

(defn rect
  "The corners of the rectangle [x0 x1] by [y0 y1], counterclockwise."
  [x0 x1 y0 y1]
  [[x0 y0] [x1 y0] [x1 y1] [x0 y1]])
