(ns alexandria.euclid.application-playground
  "The application of areas (I.44, VI.27-29) as a playground: Euclid's
   setup with nothing held.

   The given straight line AB = a lies on the x axis from A = (0, 0). On it
   a parallelogram is applied in the given angle theta: its sides are AX
   along AB and AA' = y at the angle theta. It falls short of B by the
   parallelogram XB (k < 0, VI.28), lands on B (k = 0, I.44) or runs past
   it (k > 0, VI.29); the defect or excess has sides |k| y and y in the
   same angle, so it is similar to a fixed figure, as Euclid requires.

     AX = a + k y,   applied area S = (a + k y) y sin theta

   The reader is also given an area S0 (I.44's 'given triangle', VI.28's
   'given rectilineal figure') and asks for the y that makes S = S0. The
   inverse (alexandria.euclid.application/height) solves it; falling short
   it exists only while S0 <= a^2 sin theta / 4|k|, VI.27's greatest,
   reached at y = a / 2|k| (for k = -1: on half the line, the defect a
   square). That bound is Euclid's diorismos; the playground shows the
   greatest parallelogram dashed and turns its badge red when S0 passes it.

   `figure` is an Emmy function compiled by raster for the browser; its
   numbers are every number on the panel."
  (:require [alexandria.euclid.application :as ap]
            [emmy.env :as e]))

(def tiny 1e-9)

(defn- rad [d] (e/* d (/ Math/PI 180)))

(def points
  "Point outputs (plane, y up): the applied parallelogram A X X' A', the
   leftover X B B' X' (excess or defect), the greatest applied
   parallelogram of VI.27 (its far corners G G'), and the solved one for
   S0 (its corners Y Y')."
  [:A :X :X' :A' :B :B' :G :G' :Gt :Y :Y' :Yt])

(def scalars
  "Then: AX, the applied area S, the leftover area k y^2 sin theta, VI.27's
   greatest area and its height, the solved height y0 for the given area
   S0 (held at the greatest when S0 passes it), S - S0, sin theta."
  [:base :S :left :S-max :y-max :y0 :residual :sin])

(defn figure
  "Every point and number for a = AB, y = AA', k (shape of the leftover),
   theta (the angle, degrees), S0 (the given area)."
  [a y k theta S0]
  (fn [_]
    (let [s (e/sin (rad theta)) c (e/cos (rad theta))
          up (fn [[x0 y0] h] [(e/+ x0 (e/* h c)) (e/+ y0 (e/* h s))])
          base (e/+ a (e/* k y))
          neg (ap/at-least (e/negate k) tiny)
          y-max (e// a (e/* 2 neg))
          S-max (e// (e/* (e/square a) s) (e/* 4 neg))
          ;; the given area held to the greatest (k < 0 only: S-max is huge else)
          S0' (ap/at-most S0 S-max)
          y0 (ap/height a k (e// S0' (e/+ s tiny)))
          gx (e/+ a (e/* k y-max)) yx (e/+ a (e/* k y0))]
      (vec (concat
            [0 0 base 0] (up [base 0] y) (up [0 0] y)
            [a 0] (up [a 0] y)
            [gx 0] (up [gx 0] y-max) (up [0 0] y-max)
            [yx 0] (up [yx 0] y0) (up [0 0] y0)
            [base (e/* base y s) (e/* k y y s) S-max y-max y0
             (e/- (e/* base y s) S0) s])))))

(defn named
  "{name value} of one output row of `figure`."
  [row]
  (let [n (count points)]
    (merge (zipmap points (map vec (partition 2 (take (* 2 n) row))))
           (zipmap scalars (drop (* 2 n) row)))))

(def band "|k| below this names the application exact." 0.005)

(defn kind
  "The Pythagorean name of the case (the keys of
   alexandria.apollonius.conics/names)."
  [k]
  (cond (< (Math/abs (double k)) band) :parabola
        (pos? k) :hyperbola
        :else :ellipse))

(def start
  "Euclid's VI.28 setup: AB = 10, the defect a square (k = -1), the angle
   right, y = 3, the given area 21 (so y = 3 already solves it)."
  {:a 10 :y 3 :k -1 :theta 90 :S0 21})

(def order [:a :y :k :theta :S0])

(def ranges
  "Slider ranges [min max step]."
  {:a [1 14 0.01] :y [0 9 0.01] :k [-1.5 1.5 0.01] :theta [15 165 0.5] :S0 [0 60 0.1]})

(def presets
  "The 'try this' states."
  {:euclid start
   :i44 {:a 10 :y 3 :k 0 :theta 60 :S0 21}
   :greatest {:a 10 :y 5 :k -1 :theta 90 :S0 25}
   :impossible {:a 10 :y 3 :k -1 :theta 90 :S0 32}
   :exceed {:a 10 :y 2 :k 0.5 :theta 90 :S0 21}})

(def kernel
  "The figure for alexandria.medium.kernel."
  {:f figure :params (mapv start order) :state [0] :opts {:simplify? false}})
