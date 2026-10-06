(ns alexandria.archimedes.method-view
  "The Method, Proposition 1, as a proof player for a Clerk notebook, and
   the letter to Eratosthenes as data a page can quote."
  (:require [alexandria.archimedes.method :as method]
            [alexandria.medium.clerk :as medium]
            [alexandria.proofs :as proofs]
            [hive-dsl.result :as r]))

(def ^:private render-fn 'alexandria.archimedes.method-scenes/render)

(defn proposition
  "The proposition's data: :letter, :statement, :disclaimer, :source."
  []
  (let [res (method/proof)]
    (when (r/ok? res) (get-in res [:ok :archimedes/method-1]))))

(defn- plain-points [] (update-vals method/points (fn [p] (mapv double p))))

(defn proposition-1
  "The balance, played step by step."
  []
  (medium/player render-fn :archimedes/method-1
                 (proofs/steps method/proofs-resource :archimedes/method-1)
                 method/figures
                 {:points (plain-points)}
                 {:window [[-3.8 4.6] [-0.55 4.95]] :height 440
                  :durations {:segment 4500 :construct 6000 :slice 7000 :weigh 6000
                              :all-slices 9000 :centres 6000 :ratio 6000}}))
