(ns alexandria.apollonius.areas-steps-view
  "The application of areas, Euclid I.44 and VI.27-29 to Apollonius' three
   names, as a step-by-step plane figure for a Clerk notebook: (figure)
   returns the stepped figure, (identities) the Emmy proofs of each
   algebraic step as TeX with their grades. The scene is
   alexandria.apollonius.areas-steps/render, its kernels compiled here on
   the raster backend (alexandria.medium.kernel).

   Needs a page that loads the series' bundle (history-of-math.build),
   whose SCI context carries emmy-viewers' Leva namespace."
  (:require [alexandria.apollonius.areas-steps :as steps]
            [alexandria.medium.kernel :as kernel]
            [emmy.env :as e]
            [nextjournal.clerk :as clerk]))

(def bundled-cljs
  "The namespaces the render-fn requires that the bundle already compiles
   (leva.sci through emmy.viewer.sci/install!). Clerk must not ship their
   sources: they require npm modules SCI cannot load."
  '#{leva.core})

(defn- register-bundle! []
  (swap! @(requiring-resolve 'nextjournal.clerk.cljs-libs/already-loaded-sci-namespaces)
         into bundled-cljs))

(def viewer
  {:name `figure
   :require-cljs true
   :transform-fn clerk/mark-presented
   :render-fn 'alexandria.apollonius.areas-steps/render})

(defn value
  "The plain value the render-fn reads: the sentences with their quotes,
   the kernels, the numbers."
  []
  {:steps (mapv #(select-keys % [:id :text :quote :cite]) (:steps (steps/passage)))
   :kernels (kernel/kernels steps/figures)
   :data (steps/data)})

(defn figure
  "The step-by-step figure of the application of areas, as a Clerk value."
  []
  (register-bundle!)
  (clerk/with-viewer viewer (value)))

(defn identities
  "Each algebraic step as one TeX line, lhs = Emmy's expansion of it, with
   Emmy's grade (lhs - rhs simplified to 0)."
  []
  (clerk/html
   (into [:div {:style {:margin "0.6em 0"}}]
         (for [{:keys [id label lhs rhs grade]} (steps/identities)
               :let [ok? (= :grade/proved grade)
                     tone (if ok? "#2f8a3e" "#c0392b")]]
           [:div {:data-identity (name id) :style {:display "flex" :flex-wrap "wrap" :align-items "center"
                                                   :gap "0.8em" :margin "0.25em 0"}}
            [:div {:style {:padding "0.1em 0.7em" :border-radius "1em" :font-size "0.78em"
                           :font-family "ui-monospace, monospace" :white-space "nowrap"
                           :border (str "1px solid " tone) :color tone}}
             (if ok? "proved by Emmy" "FAILS")]
            [:div {:style {:font-size "0.9em"}} label]
            (clerk/tex (str (e/->TeX lhs) " = " (e/->TeX (e/simplify lhs))
                            (when-not (= (e/simplify lhs) (e/simplify rhs))
                              (str " = " (e/->TeX rhs)))))]))))
