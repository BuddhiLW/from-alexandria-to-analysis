(ns alexandria.ptolemy.chords-view
  "Ptolemy's table of chords for a Clerk notebook: the half-arc and the
   sum constructions as proof players (alexandria.ptolemy.chords-scenes),
   one of Ptolemy's sentences per step, and the table beside raster.

   The figures are alexandria.ptolemy.chords/half-figure and sum-figure
   compiled to raster kernels; the reader's arcs are player controls."
  (:require [alexandria.medium.clerk :as medium]
            [alexandria.proofs :as proofs]
            [alexandria.ptolemy.chords :as chords]
            [nextjournal.clerk :as clerk]))

(def ^:private render-fn 'alexandria.ptolemy.chords-scenes/render)

(def proofs-resource "alexandria/ptolemy/almagest.proofs.edn")

(def ^:private window [[-80 80] [-75 70]])

(defn half
  "The chord of half an arc, stepped (Halma pp. 31-32); the arc GB is a control."
  []
  (medium/player render-fn :ptolemy/half
                 (proofs/steps proofs-resource :ptolemy/half)
                 chords/figures {}
                 {:window [[-80 80] [-40 70]] :height 360
                  :durations {:setup 4000 :ae 5000 :dz 5000 :result 6000}
                  :controls [{:id :beta :label "arc GB (°)" :min 2 :max 178 :step 0.5 :init 72}]}))

(defn sum
  "The chord of the sum of two arcs, stepped (Halma pp. 32-33); both arcs are controls."
  []
  (medium/player render-fn :ptolemy/sum
                 (proofs/steps proofs-resource :ptolemy/sum)
                 chords/figures {}
                 {:window window :height 420
                  :durations {:setup 4000 :diameter 5000 :quad 6000 :result 6000}
                  :controls [{:id :alpha :label "arc AB (°)" :min 2 :max 170 :step 0.5 :init 72}
                             {:id :beta2 :label "arc BG (°)" :min 2 :max 170 :step 0.5 :init 60}]}))

(defn table
  "Ptolemy's chords of I.10 beside 120 sin(a/2) by raster, with the error."
  []
  (clerk/html
   [:table {:data-chord-table true :style {:font-size "0.9em"}}
    [:thead [:tr [:th "arc"] [:th "what"] [:th "Ptolemy (Halma)"] [:th "120 sin(a/2), raster"] [:th "error"]]]
    (into [:tbody]
          (for [{:keys [arc what page ptolemy-str raster-str error-seconds]} (chords/table)]
            [:tr [:td (str (if (ratio? arc) (format "%.2f" (double arc)) arc) "°")]
             [:td what " (p. " page ")"] [:td ptolemy-str] [:td raster-str]
             [:td {:data-error-seconds (str error-seconds)} (format "%+.2f″" (double error-seconds))]]))]))

(defn grades [] (chords/graded))
