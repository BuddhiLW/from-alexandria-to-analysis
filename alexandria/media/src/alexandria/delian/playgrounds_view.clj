(ns alexandria.delian.playgrounds-view
  "The Delian notebook's playgrounds as Clerk values, their kernels compiled
   on the raster backend (alexandria.medium.kernel); render-fns in
   alexandria.delian.play.

     (means)     Hippocrates / Menaechmus: a, b and P free
     (conchoid)  Nicomedes: pole, interval, line and the given line free
     (cissoid)   Diocles: the radius and the abscissa free
     (archytas)  Archytas: AD, the chord AB and P on the curve free, MathBox
                 on raster (alexandria.delian.archytas-playground)"
  (:require [alexandria.delian.archytas-playground :as archytas-pg]
            [alexandria.delian.playgrounds :as pg]
            [alexandria.medium.kernel :as kernel]
            [nextjournal.clerk :as clerk]))

(def bundled-cljs '#{reagent.core leva.core})

(defn- mount [render value]
  (swap! @(requiring-resolve 'nextjournal.clerk.cljs-libs/already-loaded-sci-namespaces) into bundled-cljs)
  (clerk/with-viewer {:name render :require-cljs true
                      :transform-fn clerk/mark-presented :render-fn render} value))

(defn- kernels [ks] (select-keys (kernel/kernels pg/kernels) ks))

(defn means-value []
  {:kernels (kernels [:curves :fig]) :scalars pg/means-scalars
   :start pg/means-start :presets pg/means-presets
   :ranges {:a [0.3 4 0.01] :b [0.3 4.6 0.01] :px [0 4.6 0.01] :py [0 4.4 0.01]}})

(defn conchoid-value []
  {:kernels (kernels [:conchoid :cfig]) :scalars pg/conchoid-scalars
   :start pg/conchoid-start :presets pg/conchoid-presets
   :ranges {:d [0.2 2.5 0.01] :l [0.1 3 0.01] :phi [5 175 0.1] :c [-3.5 3.5 0.01]}})

(defn cissoid-value []
  {:kernels (kernels [:cissoid :sfig]) :scalars pg/cissoid-scalars
   :start pg/cissoid-start :presets pg/cissoid-presets
   :ranges {:r [0.5 1.6 0.01] :u [0.02 3.1 0.0001]}})

(defn means [] (mount 'alexandria.delian.play/means (means-value)))
(defn conchoid [] (mount 'alexandria.delian.play/conchoid (conchoid-value)))
(defn cissoid [] (mount 'alexandria.delian.play/cissoid (cissoid-value)))

(defn archytas
  "Archytas' setup free: AD, the chord AB (drag B) and P on the curve (drag
   its foot); an emmy-viewers form, MathBox and Mafs on the raster backend."
  []
  (archytas-pg/playground))
