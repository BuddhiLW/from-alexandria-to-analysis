(ns alexandria.cantor.sets-view
  "Cantor's countings as proof players for a Clerk notebook: the zig-zag,
   the heights of 1874, the diagonal of 1891; and the German passages with
   the series' rendering as data a page can quote."
  (:require [alexandria.cantor.sets :as sets]
            [alexandria.medium.clerk :as medium]
            [alexandria.proofs :as proofs]
            [hive-dsl.result :as r]))

(def ^:private render-fn 'alexandria.cantor.sets-scenes/render)

(defn entry
  "The proof file's entry id (:cantor/heights, :cantor/diagonal, ...), or nil."
  [id]
  (let [res (sets/proof)]
    (when (r/ok? res) (get-in res [:ok id]))))

(defn- label [x] (if (ratio? x) (str (numerator x) "/" (denominator x)) (str x)))

(defn- root-label [{:keys [equation root]}]
  (if (= 1 (sets/degree equation))
    (label root)
    (format "%.3f" (double root))))

(defn heights-data
  "The real algebraic numbers of heights 1 to 4: [[{:N :x :label}]]."
  []
  (mapv (fn [N] (mapv (fn [n] {:N N :x (double (:root n)) :label (root-label n)})
                      (sets/numbers-of-height N)))
        (range 1 5)))

(defn diagonal-data
  "The 8-row binary table of the first rationals below 1, and their values."
  []
  (let [rows (sets/binary-rows 8)]
    {:rows (mapv :row rows) :xs (mapv (comp label :x) rows)}))

(defn zigzag
  "The zig-zag, played step by step."
  []
  (medium/player render-fn :cantor/zigzag
                 (proofs/steps sets/proofs-resource :cantor/zigzag)
                 sets/figures
                 {:walk sets/walk-cells :list (mapv label (take 24 (sets/zig-zag)))}
                 {:window [[-0.4 5.7] [-2.8 0.4]] :height 340
                  :durations {:walk 12000 :count 7000}}))

(defn heights
  "Cantor's heights, played step by step."
  []
  (medium/player render-fn :cantor/heights
                 (proofs/steps sets/proofs-resource :cantor/heights)
                 {} {:heights (heights-data)}
                 {:window [[-3.3 3.3] [-1.3 1.2]] :height 320
                  :durations {:finite 6000 :list 7000}}))

(defn diagonal
  "The diagonal argument, played step by step."
  []
  (medium/player render-fn :cantor/diagonal
                 (proofs/steps sets/proofs-resource :cantor/diagonal)
                 {} (diagonal-data)
                 {:window [[-0.8 4.4] [-3.4 0.35]] :height 360
                  :durations {:table 5000 :diagonal 5000 :flip 6000 :differs 8000 :reals 6000}}))
