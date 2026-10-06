(ns alexandria.dedekind.cuts-view
  "Dedekind's cut of sqrt 2 as a proof player for a Clerk notebook, and the
   preface and the Eudoxus passage as data a page can quote."
  (:require [alexandria.dedekind.cuts :as cuts]
            [alexandria.medium.clerk :as medium]
            [alexandria.proofs :as proofs]
            [hive-dsl.result :as r]))

(def ^:private render-fn 'alexandria.dedekind.cuts-scenes/render)

(defn passage
  "The proof file's entry id (:dedekind/preface, :dedekind/eudoxus), or nil."
  [id]
  (let [res (cuts/proof)]
    (when (r/ok? res) (get-in res [:ok id]))))

(defn scene-data
  "The numbers the scenes show: the rationals m/n (n <= 24, 0 < m/n < 2.2)
   as [m n] states, the y-map climbs from 1 and from 3/2, the descent from
   99/70, and a few Eudoxus verdicts."
  []
  {:states (vec (for [n (range 1 25) m (range 1 (inc (long (* 2.2 n))))
                      :when (= 1 (.gcd (biginteger m) (biginteger n)))]
                  [m n]))
   :climb (mapv double (take 3 (iterate (partial cuts/y-map 2) 1)))
   :fall (mapv double (take 3 (iterate (partial cuts/y-map 2) 3/2)))
   :descent (cuts/descent 2 [99 70])
   :eudoxus (mapv (fn [[m n]] {:m m :n n :verdict (cuts/eudoxus-verdict m n)})
                  [[1 1] [3 2] [4 3] [7 5] [10 7] [17 12] [24 17] [41 29]])})

(defn cut-sqrt-2
  "The cut of sqrt 2, played step by step."
  []
  (medium/player render-fn :dedekind/cut-sqrt-2
                 (proofs/steps cuts/proofs-resource :dedekind/cut-sqrt-2)
                 cuts/figures
                 (scene-data)
                 {:window [[-3.3 3.3] [-1.0 1.35]] :height 340
                  :durations {:line 4500 :gap 8000 :cut 5000 :no-square 6000
                              :no-greatest 8000 :eudoxus 7000 :complete 6000}}))
