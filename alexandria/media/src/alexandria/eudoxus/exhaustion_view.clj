(ns alexandria.eudoxus.exhaustion-view
  "Elements XII.2 as a proof player for a Clerk notebook: Heath's steps
   (elements_5.proofs.edn) over inscribed polygons doubling their sides."
  (:require [alexandria.eudoxus.proportion :as ep]
            [alexandria.medium.clerk :as medium]
            [alexandria.proofs :as proofs]))

(defn xii-2
  "The exhaustion of the circle, played step by step."
  []
  (medium/player 'alexandria.eudoxus.exhaustion-scenes/render :eudoxus/XII.2
                 (proofs/steps ep/proofs-resource :eudoxus/XII.2)
                 ep/figures {:shares (ep/circle-shares 8)}
                 {:window [[-1.4 3.5] [-1.55 1.5]] :height 340
                  :durations {:polygon 4000 :ratio 7000}}))
