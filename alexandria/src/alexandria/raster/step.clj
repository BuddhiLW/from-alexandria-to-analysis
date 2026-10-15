(ns alexandria.raster.step
  "One classical Runge-Kutta (RK4) step of an Emmy flow, written with Emmy
   arithmetic so a playground can compile it to a raster kernel and the
   browser can iterate it (playground brief: motions are integrated, never
   replaced by their closed form).

   Reuse searched: alexandria.kepler.focal/rk4-step is the same scheme fixed
   to Newton's flow; alexandria.raster/ode integrates on the JVM only. This
   is the flow-generic step both can share."
  (:require [emmy.env :as e]))

(defn rk4
  "One RK4 step over dt of du/dt = (deriv u), u a vector of Emmy values."
  [deriv dt s]
  (let [ad (fn [s k h] (mapv (fn [a b] (e/+ a (e/* h b))) s k))
        k1 (deriv s)
        k2 (deriv (ad s k1 (e/* dt 1/2)))
        k3 (deriv (ad s k2 (e/* dt 1/2)))
        k4 (deriv (ad s k3 dt))]
    (mapv (fn [s a b c d] (e/+ s (e/* (e// dt 6) (e/+ a (e/* 2 b) (e/* 2 c) d))))
          s k1 k2 k3 k4)))

(defn run
  "Iterate (rk4 deriv dt) n times from s0 on the JVM with plain doubles:
   [s0 s1 ... sn]. For tests; the browser iterates the compiled kernel."
  [deriv dt s0 n]
  (vec (take (inc n) (iterate #(mapv double (rk4 deriv dt %)) (mapv double s0)))))
