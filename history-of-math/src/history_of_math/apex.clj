(ns history-of-math.apex
  "The series as pages: the cards of the apex, a series page, the series
   navigation at a notebook's foot. Presentation only: what a series holds
   and in what order is history-of-math.series (pure), the files are
   written by history-of-math.build.

   Every href is relative, so the site works as plain files behind nginx:

     apex          <out>/                 series/<slug>/, <id>/?series=<slug>
     series page   <out>/series/<slug>/   ../../<id>/?series=<slug>, ../<slug>/
     notebook      <out>/<id>/            ../<id>/?series=<slug>, ../series/<slug>/

   Which series a notebook was entered from is the ?series query parameter,
   read in the browser (history-of-math.series-view); the build ships the
   navigation of every series the notebook is on."
  (:require [clojure.string :as str]
            [history-of-math.links :as links]
            [history-of-math.series :as series]
            [history-of-math.timeline :as timeline]))

(def card-style
  {:border "1px solid #71819566" :border-radius "12px" :padding "1em 1.2em"
   :background "#182b4133" :margin "0 0 1em" :display "block" :width "100%"
   :box-sizing "border-box"})

(defn span-label
  "300 BC to 1902; one year alone when the span is a point."
  [[a b]]
  (let [y (fn [y] (timeline/year-label {:year y}))]
    (cond (nil? a) "" (= a b) (y a) :else (str (y a) " to " (y b)))))

(defn count-label [n one many] (str n " " (if (= 1 n) one many)))

(defn notebook-href
  "href of notebook id entered through series slug, from a page `depth`
   directories below the apex (0 apex, 1 notebook, 2 series page)."
  [depth id slug]
  (if (links/serve?)
    (str "/notebooks/" (links/file-stem id) "?series=" slug)
    (str (apply str (repeat depth "../")) id "/?series=" slug)))

(defn series-href
  "href of the timeline page of topic slug from a page `depth` below the
   apex, in the mode of the links (history-of-math.links/topic-href)."
  [depth slug]
  (links/topic-href depth slug))

(defn anchor [href] (links/anchor href))

(defn series-card
  "One topic on the apex: title (the link to its timeline), scope, span of
   years, built and planned counts."
  [{:keys [slug title scope]} {:keys [span built planned]}]
  [:article {:class "series-card" :data-series slug :style card-style}
   [:h3 {:style {:margin "0 0 0.3em"}} [:a (assoc (anchor (series-href 0 slug)) :class "series-link") title]]
   [:p {:class "series-scope" :style {:margin "0 0 0.5em"}} scope]
   [:p {:class "series-facts" :style {:margin 0 :opacity 0.85 :font-size "0.92em"}}
    [:span {:class "series-span"} (span-label span)] " · "
    [:span {:class "series-built" :data-n built} (count-label built "notebook" "notebooks")] " · "
    [:span {:class "series-planned" :data-n planned} (str planned " planned")]]])

(defn cards
  "The apex's series cards, full width, one per row."
  [reg notebooks joined]
  (into [:section {:class "series-index" :style {:display "block" :max-width "100%"}}]
        (for [s (series/notebook-series reg)]
          (series-card s (series/summary s notebooks joined)))))

(defn decks
  "The series published elsewhere: a link when live, planned otherwise."
  [reg]
  (into [:ul {:class "series-decks"}]
        (for [{:keys [id title status url]} (series/decks reg)]
          [:li {:data-deck (name id)}
           (if (= :live status)
             [:a {:href url :data-ignore-anchor-click "true"} title]
             [:span title [:em {:style {:opacity 0.7}} " (planned)"]])])))

(defn notebook-nav
  "The data of a notebook's series navigation: every notebook series it is
   on (registry order, the first is the default), with the previous and
   next notebook in each, as hrefs from the notebook page."
  [reg notebooks id]
  {:hub (links/hub-href)
   :series (vec (for [s (get (series/memberships reg notebooks) id)
                      :let [{:keys [prev next]} (series/neighbours (series/members s notebooks) id)
                            link (fn [nb] (when nb {:title (:title nb) :href (notebook-href 1 (:id nb) (:slug s))}))]]
                  {:slug (:slug s) :title (:title s)
                   :href (series-href 1 (:slug s))
                   :prev (link prev) :next (link next)}))})
