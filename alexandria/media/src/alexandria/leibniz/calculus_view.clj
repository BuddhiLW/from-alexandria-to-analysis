(ns alexandria.leibniz.calculus-view
  "Leibniz's calculus as proof players for a Clerk notebook, and the rules
   of 1684 as data a page can typeset: each rule's frozen formula in modern
   TeX, in Leibniz's d and in Newton's fluxions, with its grade."
  (:require [alexandria.leibniz.calculus :as calc]
            [alexandria.medium.clerk :as medium]
            [alexandria.proofs :as proofs]
            [hive-dsl.result :as r]))

(def ^:private render-fn 'alexandria.leibniz.calculus-scenes/render)

(defn proposition
  "The data of proposition id in the proofs resource (:steps :passages
   :source ...)."
  [id]
  (let [res (calc/proof)]
    (when (r/ok? res) (get-in res [:ok id]))))

(defn rules
  "[{:name :quote :locus :tex :leibniz :newton :grade}] of Nova Methodus."
  []
  (mapv (fn [rule]
          (merge {:name (:name rule)
                  :quote (get-in rule [:passage :quote])
                  :locus (get-in rule [:passage :locus])
                  :grade (calc/rule-grade rule)}
                 (calc/renderings rule)))
        calc/rules))

(defn- steps [id] (proofs/steps calc/proofs-resource id))

(defn series-data
  "What the series scenes show, computed here: the exact partial sums
   S_1 .. S_40 (as doubles) and pi/4 by raster's quadrature of
   1/(1 + z^2) (alexandria.leibniz.calculus/quarter-pi)."
  []
  {:sums (mapv #(double (calc/partial-sum %)) (range 1 41))
   :quarter-pi (calc/quarter-pi)})

(defn transmutation
  "The characteristic triangle and the transmutation, played step by step."
  []
  (medium/player render-fn :leibniz/transmutation (steps :leibniz/transmutation)
                 calc/figures (series-data)
                 {:window [[-0.35 3.0] [-0.3 2.45]] :height 420
                  :durations {:triangle 6000 :shrink 7000 :sector 6000 :z 7000
                              :strips 8000 :rational 6000 :series 8000}}))

(defn quadrature
  "The partial sums of 1 - 1/3 + 1/5 - ... closing on pi/4."
  []
  (medium/player render-fn :leibniz/arithmetical-quadrature (steps :leibniz/arithmetical-quadrature)
                 {} (series-data)
                 {:window [[-0.1 3.0] [-0.3 2.7]] :height 340
                  :durations {:sums 8000 :bound 8000}}))

(defn nova-methodus
  "The rules of 1684 on their figures."
  []
  (medium/player render-fn :leibniz/nova-methodus (steps :leibniz/nova-methodus)
                 {} {}
                 {:window [[-0.3 3.2] [-0.3 2.4]] :height 340
                  :durations {:definition 6000 :product 7000 :maximum 7000}}))

(defn series-table [ns] (calc/series-table ns))

(defn graded [] (calc/graded))
