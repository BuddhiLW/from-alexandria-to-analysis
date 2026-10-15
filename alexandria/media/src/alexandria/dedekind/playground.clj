(ns alexandria.dedekind.playground
  "Six free experiments for Dedekind's cuts, Cantor's lists and Lebesgue's cover."
  (:require [alexandria.medium.clerk :as medium]
            [alexandria.medium.kernel :as kernel]
            [alexandria.cantor.sets :as sets]
            [alexandria.cantor.sets-view :as sets-view]
            [alexandria.lebesgue.measure :as measure]
            [emmy.env :as e]
            [nextjournal.clerk :as clerk]))

(defn line [cut d]
  (fn [[x]] [x (e/- (e/square x) d)]))
(defn location [x]
  (fn [[t]] [t (e/- t x)]))
(defn cover [eps depth]
  (fn [[t]] [t (e/* eps (e/expt 0.5 depth) t)]))

(defn mount [scene initial figures data]
  (clerk/with-viewer (medium/viewer 'alexandria.dedekind.playground-scenes/render)
    {:scene scene :initial initial :kernels (kernel/kernels figures) :data data}))

(defn cut []
  (mount :cut {:d 2 :bound 16 :cut 1.414}
         {:curve {:f line :params [1.414 2] :state [0]}}
         {}))
(defn zigzag []
  (mount :zigzag {:index 8}
         {:position {:f location :params [1] :state [0]}}
         {:rationals (mapv str (take 80 (sets/zig-zag)))}))
(defn heights []
  (mount :heights {:height 2}
         {:position {:f location :params [1] :state [0]}}
         {:rows (sets-view/heights-data)}))
(defn diagonal []
  (mount :diagonal {:row 0 :col 0 :edits {}}
         {:position {:f location :params [1] :state [0]}}
         {:rows (mapv (fn [row] (mapv #(if (= % :w) 1 0) row))
                      (:rows (sets-view/diagonal-data)))}))
(defn measure-zero []
  (mount :measure {:eps 0.5 :depth 8}
         {:cover {:f cover :params [0.5 8] :state [0]}}
         {:rationals (mapv double (take 40 (measure/rationals-01)))}))
(defn dirichlet []
  (mount :dirichlet {:n 8 :tag 0.5}
         {:position {:f location :params [0.5] :state [0]}}
         {}))
