(ns history-of-math.series-view
  "A notebook's series navigation, rendered in the browser: the line of
   every series the notebook is on, and previous / next within the one the
   reader came from (the ?series query parameter, else the first). The
   hrefs come from the JVM (history-of-math.apex/notebook-nav); the view
   only chooses which series is current.")

(defn current-slug
  "The ?series of the page's URL when it names one of slugs, else the
   first of slugs."
  [slugs]
  (let [q (.get (js/URLSearchParams. (.. js/window -location -search)) "series")]
    (if (some #{q} slugs) q (first slugs))))

(defn render [{:keys [hub series]}]
  (let [slug (current-slug (map :slug series))
        {:keys [prev next] :as cur} (first (filter #(= slug (:slug %)) series))]
    [:nav {:class "series-nav" :data-series (or slug "")
           :style {:margin "3em 0 1em" :padding-top "1em" :border-top "1px solid #8884"}}
     (when (seq series)
       [:p {:class "series-breadcrumb" :style {:margin "0 0 0.8em" :font-size "0.92em"}}
        "In the series "
        (interpose ", "
                   (for [{s :slug :keys [title href]} series]
                     ^{:key s}
                     [:a {:href href :data-ignore-anchor-click "true" :data-series s
                          :aria-current (when (= s slug) "true")
                          :style {:font-weight (if (= s slug) "bold" "normal")}}
                      title]))])
     [:div {:style {:display "flex" :flex-wrap "wrap" :justify-content "space-between" :gap "1em"}}
      [:span (when prev [:a {:href (:href prev) :rel "prev" :data-ignore-anchor-click "true"}
                         (str "← previous: " (:title prev))])]
      [:a {:href hub :data-ignore-anchor-click "true" :class "series-home"} "all series"]
      [:span {:style {:text-align "right"}}
       (when next [:a {:href (:href next) :rel "next" :data-ignore-anchor-click "true"}
                   (str "next: " (:title next) " →")])]]
     (when cur
       [:p {:class "series-current" :style {:opacity 0.75 :font-size "0.85em" :margin "0.6em 0 0"}}
        "Previous and next follow " (:title cur) "."])]))
