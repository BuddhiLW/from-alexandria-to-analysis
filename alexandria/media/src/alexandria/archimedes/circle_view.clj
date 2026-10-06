(ns alexandria.archimedes.circle-view
  "Measurement of a Circle as proof players for a Clerk notebook: each
   function returns a Clerk value, the proposition's steps
   (alexandria.archimedes.circle/proofs) played over its scene
   (alexandria.archimedes.circle-scenes)."
  (:require [alexandria.archimedes.circle :as circle]
            [alexandria.medium.clerk :as medium]
            [hive-dsl.result :as r]))

(def ^:private render-fn 'alexandria.archimedes.circle-scenes/render)

(defn- steps
  "The steps of proposition id, or a one-step explanation of the failure."
  [id]
  (let [res (circle/proofs)]
    (if (r/ok? res)
      (get-in res [:ok id :steps])
      [{:stage :error :why "unavailable" :claim (pr-str (:error res))}])))

(defn- excess [id] (get-in (circle/proofs) [:ok id :excess]))

(defn mixed
  "A rational as Archimedes writes it: 4673 1/2, 1838 9/11, 265."
  [q]
  (let [q (rationalize q)
        whole (long (Math/floor (double q)))
        part (- q whole)]
    (if (zero? part) (str whole) (str whole " " part))))

(defn- bound-str [x] (format "%.5f" (double x)))

(defn prop-3-data
  "Archimedes' tables as the Proposition 3 scene reads them."
  []
  {:upper (mapv (fn [row] {:sides (:sides row) :a-str (mixed (:a row))
                           :bound (double (circle/upper-ratio row))
                           :bound-str (bound-str (circle/upper-ratio row))})
                circle/upper-table)
   :lower (mapv (fn [row] {:sides (:sides row) :h-str (mixed (:h row)) :b (:b row)
                           :bound (double (circle/lower-ratio row))
                           :bound-str (bound-str (circle/lower-ratio row))})
                circle/lower-table)
   :lo (double circle/lower-bound)
   :hi (double circle/upper-bound)})

(defn proposition-1 []
  (medium/player render-fn :archimedes/circle-1 (steps :archimedes/circle-1) circle/figures
                 {:excess (excess :archimedes/circle-1)}
                 {:window [[-1.35 8.25] [-1.45 1.4]] :height 330
                  :durations {:unroll 5000 :bisect-in 7000 :row-in 7000 :bisect-out 6000 :row-out 7000}}))

(defn proposition-2 []
  (medium/player render-fn :archimedes/circle-2 (steps :archimedes/circle-2) {} {}
                 {:window [[-0.3 6.7] [-0.4 2.45]] :height 300}))

(defn proposition-3 []
  (medium/player render-fn :archimedes/circle-3 (steps :archimedes/circle-3)
                 (select-keys circle/figures [:inscribed :circumscribed]) (prop-3-data)
                 {:window [[-0.3 8.6] [-1.45 2.2]] :height 360 :durations {:squeeze 3000}}))

(defn rings
  "The circle unrolled ring by ring into K, with a slider for the number of
   rings n (e = r/n)."
  []
  (medium/player 'alexandria.archimedes.rings-scenes/render :archimedes/circle-rings
                 (steps :archimedes/circle-rings)
                 (select-keys circle/figures [:rings]) {}
                 {:window [[-3.6 3.6] [-1.6 1.35]] :height 340
                  :controls [{:id :n :label "rings n" :min 1 :max 32 :step 1 :init 4}]
                  :durations {:one-ring 5000 :stretch 7000 :stack 7000 :squeeze 6000 :limit 7000 :lines 7000}}))
