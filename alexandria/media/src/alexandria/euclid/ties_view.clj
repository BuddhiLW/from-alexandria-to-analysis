(ns alexandria.euclid.ties-view
  "The Euclid notebook's tied widgets as Clerk values, their kernels
   compiled on the raster backend (alexandria.medium.kernel), and Emmy's
   derivations and grades for the page.

     (pythagoras)   I.47 with II.12-13, a draggable triangle
     (application)  I.44 and VI.28-29, the three names
     (derivation k) k = :pythagoras or :application: Emmy's equations as
                    TeX (emmy.clerk/->TeX) and the grades

   Needs a page that loads the series' bundle (history-of-math.build)."
  (:require [alexandria.apollonius.conics :as conics]
            [alexandria.euclid.application :as ap]
            [alexandria.euclid.pythagoras :as py]
            [alexandria.euclid.ties :as ties]
            [alexandria.medium.kernel :as kernel]
            [emmy.clerk :as ec]
            [nextjournal.clerk :as clerk]))

(def bundled-cljs
  "Namespaces the render-fns require that the bundle already compiles."
  '#{leva.core})

(defn- register-bundle! []
  (swap! @(requiring-resolve 'nextjournal.clerk.cljs-libs/already-loaded-sci-namespaces)
         into bundled-cljs))

(defn- viewer [render]
  {:name render :require-cljs true :transform-fn clerk/mark-presented :render-fn render})

(defn- kernels [ks] (select-keys (kernel/kernels ties/figures) ks))

(defn pythagoras-value []
  {:kernels (kernels [:tie :fig])
   :points py/points :scalars py/scalars
   :tied py/tied :offsets py/source-block :ranges py/ranges
   :start py/start :names py/names})

(defn application-value []
  {:kernels (kernels [:atie :afig :rect])
   :outputs ap/outputs :tied ap/tied :offsets ap/source-block :ranges ap/ranges
   :start ap/start :names conics/names :props ap/propositions})

(defn pythagoras
  "The tied I.47 / II.12-13 widget."
  []
  (register-bundle!)
  (clerk/with-viewer (viewer 'alexandria.euclid.ties/pythagoras) (pythagoras-value)))

(defn application
  "The tied application-of-areas widget."
  []
  (register-bundle!)
  (clerk/with-viewer (viewer 'alexandria.euclid.ties/application) (application-value)))

(defn derivation
  "Emmy's derivation of the tie (equations, as ->TeX) and its grades."
  [which]
  (let [[eqs grades] (case which
                       :pythagoras [(py/equations) (py/grades)]
                       :application [(ap/equations) (ap/grades)])]
    (apply clerk/col
           (concat
            (for [{:keys [label eqs]} eqs]
              (apply clerk/col
                     (clerk/html [:div {:style {:font-size "0.85em" :opacity 0.7 :margin-top "0.5em"}} label])
                     (for [q eqs] (ec/->TeX q :simplify? false))))
            [(clerk/html
              (into [:div {:style {:display "flex" :flex-wrap "wrap" :gap "0.4em" :margin "0.6em 0"}}]
                    (for [{:keys [id label grade]} grades
                          :let [ok? (= :grade/proved grade)
                                tone (if ok? "#2f8a3e" "#c0392b")]]
                      [:div {:data-grade (name grade) :data-identity (name id)
                             :style {:padding "0.15em 0.8em" :border-radius "1em" :font-size "0.82em"
                                     :font-family "ui-monospace, monospace"
                                     :border (str "1px solid " tone) :color tone}}
                       (str (if ok? "proved by Emmy: " "FAILS: ") label)])))]))))
