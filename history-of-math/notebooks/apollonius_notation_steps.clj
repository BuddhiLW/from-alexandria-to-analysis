;; # Apollonius: how a plane cuts a cone, step by step
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns apollonius-notation-steps
  {:history/year -200
   :history/title "Apollonius: how a plane cuts a cone, step by step"
   :history/era "Greek geometry"
   :nextjournal.clerk/visibility {:code :hide :result :show}}
  (:require [alexandria.apollonius.notation-steps-view :as notation]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:result :hide}}
(hom/install!)

;; The notation of *Conics* I.11-13, one sentence at a time. Each sentence
;; is a step: the figure adds what the sentence names and nothing else.
;; Play runs through all eight; ◀ and ▶▶ (or a click on a sentence) play one.
;; Drag the figure to turn it. Every point is computed by a raster kernel of
;; the Emmy construction in `alexandria.apollonius.conics`.

(notation/figure)
