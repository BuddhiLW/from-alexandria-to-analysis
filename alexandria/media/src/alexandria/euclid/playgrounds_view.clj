(ns alexandria.euclid.playgrounds-view
  "The Euclid notebook's playgrounds as Clerk values, their kernels compiled
   on the raster backend (alexandria.medium.kernel).

     (i1)           I.1, A and B free
     (i47)          I.47 with II.12-13, A, B, C free (alexandria.euclid.play-i47)
     (application)  I.44 / VI.27-29, the line, the height, the angle, the
                    defect's shape and the given area free
                    (alexandria.euclid.play-application)"
  (:require [alexandria.apollonius.conics :as conics]
            [alexandria.euclid.application-playground :as app]
            [alexandria.euclid.i47-playground :as ip]
            [alexandria.euclid.playgrounds :as pg]
            [alexandria.medium.kernel :as kernel]
            [nextjournal.clerk :as clerk]))

(def bundled-cljs
  "Namespaces the render-fns require that the bundle already compiles."
  '#{reagent.core leva.core})

(defn- mount [render value]
  (swap! @(requiring-resolve 'nextjournal.clerk.cljs-libs/already-loaded-sci-namespaces) into bundled-cljs)
  (clerk/with-viewer {:name render :require-cljs true
                      :transform-fn clerk/mark-presented :render-fn render} value))

(defn i1 [] (mount 'alexandria.euclid.playgrounds/i1 {:kernels @pg/kernels}))

(defn i47-value
  "What the I.47 playground's render-fn reads: the raster kernel of the
   free-vertex figure, Euclid's start and the 'try this' presets."
  []
  {:kernel (kernel/kernel ip/kernel) :start ip/start :presets ip/presets})

(defn i47 [] (mount 'alexandria.euclid.play-i47/render (i47-value)))

(defn application-value
  "What the application playground's render-fn reads."
  []
  {:kernel (kernel/kernel app/kernel) :order app/order :ranges app/ranges
   :start app/start :presets app/presets :names conics/names})

(defn application [] (mount 'alexandria.euclid.play-application/render (application-value)))
