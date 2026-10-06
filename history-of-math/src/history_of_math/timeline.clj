(ns history-of-math.timeline
  "The series' timeline: the planned works (resources/history_of_math/
   timeline.edn) joined with the notebooks that exist, drawn as an SVG strip.

   Collect   `timeline`: the EDN resource, validated with malli.
   Promote   `join`: each entry gets :notebook (an id) and :built? from the
             discovered notebooks (history-of-math.build/discover).
   Boundary  `strip`, `table`: hiccup; the hub notebook hands them to Clerk.

   Colours are the :series palette of alexandria.palette, one per era.
   Reuse searched: alexandria.medium.svg draws in a y-up world for proof
   figures; a timeline is a page-space strip with links, so plain hiccup."
  (:require [alexandria.palette :as palette]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-dsl.result :as r]
            [malli.core :as m]
            [malli.error :as me]
            [history-of-math.links :as links]))

(def resource "history_of_math/timeline.edn")

(def Era [:map [:id keyword?] [:title string?] [:role qualified-keyword?]])

(def Entry
  [:map
   [:year int?]
   [:circa {:optional true} boolean?]
   [:figure [:string {:min 1}]]
   [:work [:string {:min 1}]]
   [:significance [:string {:min 1}]]
   [:era keyword?]
   [:notebook {:optional true} [:re #"^[a-z0-9][a-z0-9-]*$"]]])

(def Timeline
  [:and
   [:map [:title string?] [:eras [:vector Era]] [:entries [:vector Entry]]]
   [:fn {:error/message "every entry's :era is one of :eras"}
    (fn [{:keys [eras entries]}] (every? (set (map :id eras)) (map :era entries)))]])

(defn validate
  "Result of timeline value t, or an Err with malli's explanation."
  [t]
  (if (m/validate Timeline t)
    (r/ok t)
    (r/err {:history/error :timeline/invalid :explain (me/humanize (m/explain Timeline t))})))

(defn timeline
  "Result of the timeline read from resource (default the series')."
  ([] (timeline resource))
  ([res]
   (if-let [url (io/resource res)]
     (r/let-ok [t (r/try-effect* :timeline/unreadable (edn/read-string (slurp url)))]
       (validate t))
     (r/err {:history/error :resource/not-found :resource res}))))

(defn- names-figure? [{:keys [title]} figure]
  (and title (str/includes? (str/lower-case title) (str/lower-case figure))))

(defn join
  "The entries with :notebook and :built?: an entry naming a notebook id is
   built when that notebook was discovered; an entry without one takes a
   discovered notebook of the same year whose title names its figure."
  [{:keys [entries]} notebooks]
  (let [by-id (into {} (map (juxt :id identity)) notebooks)]
    (mapv (fn [{:keys [notebook year figure] :as e}]
            (let [nb (if notebook
                       (by-id notebook)
                       (first (filter #(and (= year (:year %)) (names-figure? % figure)) notebooks)))]
              (assoc e :notebook (or (:id nb) notebook) :built? (some? nb))))
          entries)))

(defn year-label
  "300 BC, c. 250 BC, 1543, c. 1350."
  [{:keys [year circa]}]
  (str (when circa "c. ") (if (neg? year) (str (- year) " BC") (str year))))

(defn href
  "Anchor attributes for notebook id from the hub, opened as its own page:
   its directory in the static build, its Clerk route in serve mode
   (history-of-math.links)."
  [id]
  (links/anchor (links/notebook-href :hub id)))

;; ---------------------------------------------------------------------------
;; The strip

(defn- era-colour [pal eras era]
  (get pal (:role (first (filter #(= era (:id %)) eras))) (:ink pal)))

(defn- x-scale
  "Page x of a year: the ancient world (to 200 AD) and the modern (from
   1300) each get a linear stretch, the gap between them a short break."
  [year {:keys [x0 x-break x1]}]
  (if (<= year 400)
    (+ x0 (* (- x-break 20 x0) (/ (- year -320) (- 400 -320))))
    (+ x-break 20 (* (- x1 x-break 20) (/ (- year 1300) (- 1920 1300))))))

(defn strip
  "Hiccup SVG of the joined entries on a time axis, one row of labels per
   entry, coloured by era; built notebooks are links, planned ones muted."
  ([t joined] (strip t joined {}))
  ([{:keys [eras]} joined {:keys [palette width] :or {palette :series width 960}}]
   (let [pal (palette/resolve-palette palette)
         dims {:x0 40 :x-break 300 :x1 (- width 40)}
         row 22
         top 70
         height (+ top (* row (count joined)) 30)
         axis-y 40]
     (into
      [:svg {:viewBox (str "0 0 " width " " height) :width width
             :style {:max-width "100%" :height "auto" :background (:background pal)
                     :font-family "ui-serif, Georgia, serif"}}
       [:line {:x1 (:x0 dims) :y1 axis-y :x2 (:x1 dims) :y2 axis-y :stroke (:muted pal) :stroke-width 1.5}]
       [:text {:x (:x-break dims) :y (- axis-y 8) :fill (:muted pal) :font-size 12 :text-anchor "middle"} "⋯"]
       [:g (for [[y lbl] [[-300 "300 BC"] [0 "0"] [300 "300"] [1400 "1400"] [1600 "1600"] [1800 "1800"] [1900 "1900"]]]
             [:text {:x (x-scale y dims) :y (- axis-y 10) :fill (:muted pal) :font-size 11 :text-anchor "middle"} lbl])]
       [:g (for [[i {:keys [id title]}] (map-indexed vector eras)]
             [:g [:rect {:x (+ 40 (* i 180)) :y 6 :width 12 :height 12 :fill (era-colour pal eras id)}]
              [:text {:x (+ 58 (* i 180)) :y 16 :fill (:ink pal) :font-size 11} title]])]]
      (map-indexed
       (fn [i {:keys [year figure work era built? notebook] :as e}]
         (let [c (era-colour pal eras era)
               x (x-scale year dims)
               y (+ top (* i row))
               right? (> x (/ width 2))
               label [:text {:x (if right? (- x 10) (+ x 10)) :y (+ y 4) :font-size 13
                             :text-anchor (if right? "end" "start")
                             :fill (if built? (:ink pal) (:muted pal))
                             :font-style (if built? "normal" "italic")}
                      (str (year-label e) "  " figure ", " (first (str/split work #";")))
                      (when-not built? " (planned)")]]
           [:g
            [:line {:x1 x :y1 axis-y :x2 x :y2 y :stroke c :stroke-width 1 :stroke-opacity 0.35}]
            [:circle {:cx x :cy axis-y :r 4 :fill c}]
            [:circle {:cx x :cy y :r 5 :fill (if built? c "none") :stroke c :stroke-width 1.5}]
            (if built? [:a (href notebook) label] label)]))
       joined)))))

(defn table
  "Hiccup table of the joined entries: date, who, work, why it matters."
  [joined]
  [:table {:style {:font-size "0.9em"}}
   [:thead [:tr [:th "date"] [:th "who"] [:th "work"] [:th "what it did"]]]
   (into [:tbody]
         (for [{:keys [figure work significance built? notebook] :as e} joined]
           [:tr
            [:td {:style {:white-space "nowrap"}} (year-label e)]
            [:td figure]
            [:td (if built? [:a (href notebook) [:em work]] [:em work])
             (when-not built? [:span {:style {:opacity 0.6}} " (planned)"])]
            [:td significance]]))])
