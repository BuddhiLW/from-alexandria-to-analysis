(ns alexandria.apollonius.book-one-view
  "Conics I.11-14 for a Clerk notebook, one proposition at a time:

     (proposition \"I.12\")  Heath's figure stepped through his sentences,
                            with the proposition's live widget (the scene
                            alexandria.apollonius.book-one-scene/render)
     (greek \"I.12\")        Heiberg's naming sentence, quoted
     (steps \"I.12-13\")     Heath's steps as Emmy proves them, TeX-free
                            labels with their grades (alexandria.apollonius.
                            book-one/graded)

   Kernels: the :heath, :section and :circle figures of
   alexandria.apollonius.book-one and the symptoma widget's :symptom
   (alexandria.apollonius.symptom), all compiled on the raster backend by
   alexandria.medium.kernel. Needs a page that loads the series' bundle
   (history-of-math.build)."
  (:require [alexandria.apollonius.book-one :as book]
            [alexandria.apollonius.book-one-scene :as scene]
            [alexandria.apollonius.symptom :as symptom]
            [alexandria.medium.kernel :as kernel]
            [nextjournal.clerk :as clerk]))

(def figures
  "Every kernel a proposition's figure runs."
  (assoc book/figures :symptom (:symptom symptom/figures)))

(def viewer
  {:name `proposition
   :require-cljs true
   :transform-fn clerk/mark-presented
   :render-fn 'alexandria.apollonius.book-one-scene/render})

(defn prop-of
  "The edn's proposition by id."
  [id]
  (first (filter #(= id (:id %)) (:props (book/walk)))))

(defn data
  "The numbers the render-fn starts from: the view, the symptoma kernel's
   outputs, the parabola's tilt (raster)."
  []
  (merge scene/view {:outputs symptom/outputs :parabola (book/parabola-tilt) :source :raster}))

(defn value
  "The plain value the render-fn reads for proposition id."
  [id]
  {:prop (prop-of id) :kernels (kernel/kernels figures) :data (data)})

(defn proposition
  "Proposition id's stepped figure and widget, as a Clerk value."
  [id]
  (clerk/with-viewer viewer (value id)))

(defn greek
  "Heiberg's naming sentence of proposition id, quoted with its page."
  [id]
  (let [{:keys [greek heiberg]} (prop-of id)]
    (clerk/md (str "> " greek "\n\n*" heiberg "*"))))

(defn steps
  "Heath's steps of key (:I.11 :I.12-13 :I.14) with Emmy's grades, as
   badges."
  [k]
  (clerk/html
   (into [:div {:style {:display "flex" :flex-wrap "wrap" :gap "0.4em" :margin "0.6em 0"}}]
         (for [{:keys [label grade]} (get (book/graded) k)
               :let [ok? (= :grade/proved grade)
                     tone (if ok? "#2f8a3e" "#c0392b")]]
           [:div {:data-grade (name grade)
                  :style {:padding "0.15em 0.8em" :border-radius "1em" :font-size "0.82em"
                          :font-family "ui-monospace, monospace"
                          :border (str "1px solid " tone) :color tone}}
            (str (if ok? "proved by Emmy: " "FAILS: ") label)]))))
