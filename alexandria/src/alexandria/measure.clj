(ns alexandria.measure
  "Surface measures used by contracts and notebooks."
  (:require [alexandria.ctx :as ctx]))

(def gauss-bonnet-subdivisions 24)

(defn heron-area [ab bc ca]
  (let [s (/ (+ (double ab) (double bc) (double ca)) 2.0)]
    (Math/sqrt (max 0.0 (* s (- s (double ab)) (- s (double bc)) (- s (double ca)))))))

(defn geodesic-triangle-integral
  "Numerically integrate Gaussian curvature over the geodesic triangle abc."
  ([ctx a b c] (geodesic-triangle-integral ctx a b c gauss-bonnet-subdivisions))
  ([ctx a b c n]
   (letfn [(tri-point [i j]
             (if (zero? i)
               a
               (let [base ((ctx/geodesic ctx b c) (/ j (double i)))]
                 ((ctx/geodesic ctx a base) (/ i (double n))))))
           (small-triangle [p q r]
             (let [pq (ctx/distance ctx p q)
                   qr (ctx/distance ctx q r)
                   rp (ctx/distance ctx r p)
                   area (heron-area pq qr rp)
                   centroid ((ctx/geodesic ctx p ((ctx/geodesic ctx q r) 0.5)) (/ 2.0 3.0))]
               (* (double (ctx/curvature ctx centroid)) area)))]
     (reduce
      +
      (for [i (range n)
            j (range (inc i))
            :let [p00 (tri-point i j)
                  p10 (tri-point (inc i) j)
                  p11 (tri-point (inc i) (inc j))]]
        (+ (small-triangle p00 p10 p11)
           (if (< j i)
             (small-triangle p00 p11 (tri-point i (inc j)))
             0.0)))))))
