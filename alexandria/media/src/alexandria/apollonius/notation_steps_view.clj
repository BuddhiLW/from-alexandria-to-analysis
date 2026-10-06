(ns alexandria.apollonius.notation-steps-view
  "The notation passage of Apollonius I.11-13 as a step-by-step 3D figure
   for a Clerk notebook: (figure) returns the Clerk value. Each sentence of
   the passage is a step beside the MathBox scene; the scene is
   alexandria.apollonius.notation-steps/render, its kernels compiled here
   on the raster backend (alexandria.medium.kernel).

   Needs a page that loads the series' bundle (history-of-math.build), whose
   SCI context already carries emmy-viewers' MathBox namespaces."
  (:require [alexandria.apollonius.notation-steps :as steps]
            [alexandria.medium.kernel :as kernel]
            [nextjournal.clerk :as clerk]))

(def bundled-cljs
  "The MathBox namespaces the render-fn requires, compiled into the bundle
   (mathbox.sci through emmy.viewer.sci/install!). Clerk must not ship their
   sources: they require npm modules SCI cannot load."
  '#{mathbox.core mathbox.primitives})

(defn- register-bundle! []
  (swap! @(requiring-resolve 'nextjournal.clerk.cljs-libs/already-loaded-sci-namespaces)
         into bundled-cljs))

(def viewer
  {:name `figure
   :require-cljs true
   :transform-fn clerk/mark-presented
   :render-fn 'alexandria.apollonius.notation-steps/render})

(defn value
  "The plain value the render-fn reads: the sentences, the kernels, the
   constants."
  []
  {:steps (mapv #(select-keys % [:id :text]) (:steps (steps/passage)))
   :kernels (kernel/kernels steps/figures)
   :data (steps/data)})

(defn figure
  "The step-by-step figure of the notation passage, as a Clerk value."
  []
  (register-bundle!)
  (clerk/with-viewer viewer (value)))
