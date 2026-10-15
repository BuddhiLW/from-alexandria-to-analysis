(ns alexandria.riemann.playground
  "Raster kernels and mount points for the four Riemann/Weierstrass experiments."
  (:require [alexandria.medium.clerk :as medium]
            [alexandria.medium.kernel :as kernel]
            [emmy.env :as e]
            [nextjournal.clerk :as clerk]))

(defn integral-curve [kind n]
  (fn [[x]]
    [x (e/+ (e/* (e/- 1 kind) (e/square x))
            (e/* kind (reduce e/+ (for [k (range 1 13)]
                                    (e// (e/atan (e/tan (e/* Math/PI k x)))
                                         (e/* Math/PI k k))))))]))

(defn uniform-curve [family n]
  (fn [[x]]
    [x (e/+ (e/* (e/- 1 family)
                  (reduce e/+ (for [k (range 16)]
                                (e/* (e// (e/+ 1 (e/- (e/abs (e/- n k)) (e/abs (e/- (e/- n k) 1)))) 2)
                                     (e/expt x k)))))
            (e/* family
                 (reduce e/+ (for [k (range 1 17)]
                               (e/* (if (odd? k) 1 -1)
                                    (e// (e/sin (e/* k x)) k)
                                    (e// (e/+ 1 (e/- (e/abs (e/- n k)) (e/abs (e/- (e/- n k) 1)))) 2))))))]))

(defn monster-curve [a b n]
  (fn [[x]]
    [x (reduce e/+ (for [k (range 9)]
                     (e/* (e/expt b k) (e/cos (e/* Math/PI (e/expt a k) x))
                          (e// (e/+ 1 (e/- (e/abs (e/- n k)) (e/abs (e/- (e/- n k) 1)))) 2))))]))

(defn mount [id initial]
  (clerk/with-viewer (medium/viewer 'alexandria.riemann.playground-scenes/render)
    {:scene id :initial initial
     :kernels (kernel/kernels
               (case id
                 :integral {:curve {:f integral-curve :params [0 8] :state [0]}}
                 :jumps {:curve {:f integral-curve :params [1 8] :state [0]}}
                 :uniform {:curve {:f uniform-curve :params [0 6] :state [0]}}
                 :monster {:curve {:f monster-curve :params [13 0.5 7] :state [0]}}))}))

(defn integral [] (mount :integral {:kind 0 :n 8 :tag 0.5}))
(defn jumps [] (mount :jumps {:kind 1 :n 8 :tag 0.5}))
(defn uniform [] (mount :uniform {:family 0 :n 6 :eps 0.2 :point 0.8}))
(defn monster [] (mount :monster {:a 13 :b 0.5 :n 7 :x 0.25 :zoom 0.4}))
