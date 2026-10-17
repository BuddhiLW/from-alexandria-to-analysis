(ns history-of-math.page
  "Page furniture the notebooks share: a proof as a table of steps, a graded
   check as a badge, the previous/next links at a notebook's foot.
   Presentation only; the grades come from alexandria.grade through the
   shelf namespaces, the order of the series from history-of-math.build."
  (:require [clojure.string :as str]
            [hive-dsl.result :as r]
            [history-of-math.build :as build]
            [nextjournal.clerk :as clerk]
            [history-of-math.links :as links]
            [history-of-math.apex :as apex]
            [history-of-math.series :as series]))

(defn steps
  "A proof as a table: step number, claim, reason. rows are [claim why]
   pairs or {:claim :why} maps. A reason keeps to one line."
  [rows]
  (clerk/md
   (str "| | | |\n|---:|:---|:---|\n"
        (str/join "\n" (map-indexed (fn [i r]
                                      (let [[claim why] (if (map? r) [(:claim r) (:why r)] r)]
                                        (str "| " (inc i) " | " claim " | *" (str/replace (str why) " " " ") "* |")))
                                    rows)))))

(defn blockquote
  "A passage from a source, set as a block quote in italics with its
   attribution: who wrote or translated it, and where (edition, page)."
  [text attribution]
  (clerk/md (str "> *" text "*\n>\n> — " attribution)))

(defn- badge-hiccup [ok? text]
  (let [tone (if ok? "#2f8a3e" "#c0392b")]
    [:div {:style {:display "inline-block" :padding "0.15em 0.8em" :border-radius "1em"
                   :font-family "ui-monospace, monospace" :font-size "0.85em" :margin "0.2em 0"
                   :border (str "1px solid " tone) :color tone}}
     (str (if ok? "checked: " "FAILS: ") text)]))

(defn badge
  "A one-line verdict, green or red on Clerk's light and dark pages alike."
  [ok? text]
  (clerk/html (badge-hiccup ok? text)))

(defn grade-badges
  "Badges of [{:label :grade}] as alexandria.grade graded them; ok are the
   grades that pass (default proved only); by names the checker (default
   \"Emmy\"; a series whose checks run elsewhere names its own engine)."
  ([graded] (grade-badges graded #{:grade/proved}))
  ([graded ok] (grade-badges graded ok "Emmy"))
  ([graded ok by]
   (clerk/html
    (into [:div]
          (for [{:keys [label grade]} graded]
            [:div {:data-grade (name grade)}
             (badge-hiccup (contains? ok grade) (str label " (" (name grade) " by " by ")"))])))))

(defn- page-link
  "Anchor attributes for notebook id (the hub for nil) from a notebook page,
   as a real page load: <out>/<id>/ in the static build, Clerk's route in
   serve mode (history-of-math.links)."
  [id]
  (links/anchor (if id (links/notebook-href :notebook id) (links/hub-href))))

(defn prev-next-hiccup
  "Hiccup of the links to {:prev :next} (notebooks as history-of-math.build
   discovers them; either may be nil) and to the hub, from a notebook page."
  [{:keys [prev next]}]
  [:nav {:style {:display "flex" :justify-content "space-between" :gap "1em"
                 :margin "3em 0 1em" :padding-top "1em" :border-top "1px solid #8884"}}
   [:span (when prev [:a (assoc (page-link (:id prev)) :rel "prev")
                      (str "← previous: " (:title prev))])]
   [:a (page-link nil) "all works"]
   [:span {:style {:text-align "right"}}
    (when next [:a (assoc (page-link (:id next)) :rel "next")
                (str "next: " (:title next) " →")])]])

(defn prev-next
  "The foot of the notebook at path: a line naming every series it is on
   (history-of-math.series, series.edn) and the previous and next notebook
   within the series it was entered from (?series=<slug>, else its first
   series), plus the way back to the apex. The navigation of each series is
   built here; the browser (history-of-math.series-view) picks the current
   one, so the static pages need no server. Call it uncached so a notebook
   added to a series shows up:

     ^{::clerk/no-cache true} (page/prev-next \"notebooks/archimedes.clj\")"
  [path]
  (let [res (build/discover build/notebooks-dir)]
    (when-not (r/ok? res)
      (throw (ex-info (build/error-message (:error res)) (:error res))))
    (clerk/with-viewer
      {:name 'history-of-math.series-view/render
       :require-cljs true :transform-fn clerk/mark-presented
       :render-fn 'history-of-math.series-view/render}
      (apex/notebook-nav (series/registry) (:ok res) (build/notebook-id path)))))
