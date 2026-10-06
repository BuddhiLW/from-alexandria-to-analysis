(ns alexandria.ctx.plane
  "Flat K=0 Surface adapter for Euclid I.1 notebooks."
  (:require [alexandria.ctx :as ctx]
            [alexandria.ops.euclid :as eu]
            [desargues.board.linalg :as la]
            [emmy.calculus.form-field :as ff]
            [emmy.calculus.manifold :as m]
            [emmy.calculus.metric :as metric]
            [emmy.env :as e]
            [emmy.structure :as s]))

(defn- clamp [x lo hi] (max lo (min hi (double x))))
(defn- point [x y] (s/up x y))

(def flat-metric
  "Emmy flat metric on R2-rect coordinates."
  (let [coordsys (m/with-coordinate-prototype m/R2-rect '[x y])
        [dx dy] (ff/coordinate-system->oneform-basis coordsys)]
    (metric/literal-metric (fn [v w]
                             (e/+ (e/* (dx v) (dx w))
                                  (e/* (dy v) (dy w))))
                           coordsys)))

(defrecord Plane []
  ctx/Surface
  (metric [_] flat-metric)
  (point [_ tangent] (point (nth tangent 0) (nth tangent 1)))
  (geodesic [_ p q] (fn [t] (la/lerp p q t)))
  (distance [_ p q] (la/norm (la/sub q p)))
  (circle [_ center r]
    (fn [alpha]
      (let [[x y] (eu/circle-xy center r alpha)]
        (point x y))))
  (-angle [_ v p q]
    (let [a (la/sub p v)
          b (la/sub q v)]
      (e/acos (clamp (e// (la/dot a b) (e/* (la/norm a) (la/norm b))) -1.0 1.0))))
  (meet [_ curve-a curve-b]
    (let [{ga :geodesic ca :circle} curve-a
          {gb :geodesic cb :circle} curve-b]
      (cond
        (and ga gb) (apply eu/line-line-xy (concat ga gb))
        (and ga cb) (apply eu/line-circle-xy (concat ga cb))
        (and ca gb) (mapv #(update % :at (fn [[a b]] [b a]))
                          (apply eu/line-circle-xy (concat gb ca)))
        (and ca cb) (apply eu/circle-circle-results (concat ca cb))
        :else [])))
  (curvature [_ _] 0.0)
  (embed [_ p] p))

(defmethod ctx/make :plane [_ _] (->Plane))
