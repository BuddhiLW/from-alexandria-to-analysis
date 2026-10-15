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
   years, `N of M entries built`, the planned count and the first notebook
   to start with."
  [{:keys [slug title scope]} {:keys [span planned entries entries-built first]}]
  [:article {:class "series-card" :data-series slug :style card-style}
   [:h3 {:style {:margin "0 0 0.3em"}} [:a (assoc (anchor (series-href 0 slug)) :class "series-link") title]]
   [:p {:class "series-scope" :style {:margin "0 0 0.5em"}} scope]
   [:p {:class "series-facts" :style {:margin 0 :opacity 0.85 :font-size "0.92em"}}
    [:span {:class "series-span"} (span-label span)] " · "
    [:span {:class "series-built" :data-n entries-built :data-of entries}
     (str entries-built " of " (count-label entries "entry" "entries") " built")] " · "
    [:span {:class "series-planned" :data-n planned} (str planned " planned")]]
   [:p {:style {:margin "0.5em 0 0"}}
    (if first
      [:a (assoc (anchor (notebook-href 0 (:id first) slug))
                 :class "series-first" :data-first (:id first))
       (str "Start with " (:title first))]
      [:span {:class "series-first-planned" :style {:opacity 0.7}} "No notebook yet"])]])

(defn cards
  "The apex's series cards, full width, one per row."
  [reg notebooks joined]
  (into [:section {:class "series-index" :style {:display "block" :max-width "100%"}}]
        (for [s (series/notebook-series reg)]
          (series-card s (series/summary s notebooks joined)))))

(defn entry-label
  "Year, figure and work of a timeline entry: 300 BC, Euclid, Elements; a
   notebook without a timeline entry of its own: its year and title."
  [{:keys [figure work title] :as e}]
  (if figure
    (str (timeline/year-label e) ", " figure ", " (first (str/split work #";")))
    (str (timeline/year-label e) ", " title)))

(defn index-entry
  "One line of a series' index: a link to the notebook (carrying ?series)
   when the entry is built, plain text marked planned when it is not."
  [slug {:keys [notebook built? year] :as e}]
  (if (and built? notebook)
    [:li {:class "index-entry" :data-notebook notebook :data-year year :data-built "true"}
     [:a (assoc (anchor (notebook-href 0 notebook slug)) :class "index-notebook") (entry-label e)]]
    [:li {:class "index-entry" :data-year year :data-built "false" :style {:opacity 0.7}}
     (entry-label e) [:em " (planned)"]]))

(defn index-rows
  "The rows of a series' index in reading order: its timeline entries, plus
   the member notebooks that have no timeline entry of their own (as
   {:notebook id :built? true :year :title}), merged by year."
  [s notebooks joined]
  (let [es (series/entries s joined)
        listed (set (keep :notebook es))
        extra (for [m (series/members s notebooks) :when (not (listed (:id m)))]
                {:notebook (:id m) :built? true :year (:year m) :title (:title m)})]
    (vec (sort-by :year (concat es extra)))))

(defn index-series
  "One series in the index: its title a plain link to its timeline page,
   then its whole timeline in reading order."
  [{:keys [slug title] :as s} notebooks joined]
  [:section {:class "index-series" :data-series slug :id (str "index-" slug)}
   [:h3 {:style {:margin "1.2em 0 0.3em"}}
    [:a (assoc (anchor (series-href 0 slug)) :class "index-series-link") title]]
   (into [:ol {:class "index-entries" :style {:margin 0 :padding-left "1.6em"}}]
         (map #(index-entry slug %))
         (index-rows s notebooks joined))])

(defn decks
  "The series published elsewhere: a link when live, planned otherwise."
  [reg]
  (into [:ul {:class "series-decks"}]
        (for [{:keys [id title status url]} (series/decks reg)]
          [:li {:data-deck (name id)}
           (if (= :live status)
             [:a {:href url :data-ignore-anchor-click "true"} title]
             [:span title [:em {:style {:opacity 0.7}} " (planned)"]])])))

(defn index
  "The index of the apex: every notebook series in registry order with its
   whole timeline, then the decks published elsewhere (absolute links).
   Static HTML with literal hrefs."
  [reg notebooks joined]
  (into [:section {:class "apex-index" :style {:display "block" :max-width "100%"}}]
        (concat (for [s (series/notebook-series reg)]
                  (index-series s notebooks joined))
                [[:h3 {:style {:margin "1.2em 0 0.3em"}} "Slides and courses"]
                 (decks reg)])))

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
