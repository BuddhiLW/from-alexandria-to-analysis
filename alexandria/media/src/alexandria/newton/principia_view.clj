(ns alexandria.newton.principia-view
  "Principia, Book I, as proof players for a Clerk notebook: Lemma I,
   Proposition I (the polygon of impulses) and Proposition XI (the ellipse
   about a focus)."
  (:require [alexandria.medium.clerk :as medium]
            [alexandria.newton.principia :as principia]
            [alexandria.proofs :as proofs]
            [hive-dsl.result :as r]))

(def ^:private render-fn 'alexandria.newton.principia-scenes/render)

(defn proposition
  "The data of proposition id (:statement :steps :source ...)."
  [id]
  (let [res (principia/proof)]
    (when (r/ok? res) (get-in res [:ok id]))))

(defn- steps [id] (proofs/steps principia/proofs-resource id))

(defn lemma-1 []
  (medium/player render-fn :newton/lemma-1 (steps :newton/lemma-1) {} {}
                 {:window [[-0.2 4.4] [-0.25 2.2]] :height 320
                  :durations {:limit 7000}}))

(defn proposition-1 []
  (medium/player render-fn :newton/prop-1 (steps :newton/prop-1) {} (principia/polygon-data)
                 {:window [[-1.6 3.4] [-1.4 2.0]] :height 420
                  :durations {:first-moment 4000 :unhindered 6000 :impulse 6000 :parallels 7000
                              :polygon 8000 :limit 8000}}))

(def ^:private prop-11-data (delay (principia/prop-11-data)))

(defn proposition-11 []
  (medium/player render-fn :newton/prop-11 (steps :newton/prop-11)
                 (select-keys principia/figures [:orbit :focal]) @prop-11-data
                 {:window [[-4.4 2.4] [-2.0 2.0]] :height 420
                  :durations {:ellipse 4000 :focus 6000 :qr 7000 :force 9000 :around 10000}}))

(defn graded [] (principia/graded))
