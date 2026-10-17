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
            [alexandria.raster :as raster]
            [history-of-math.lens :as lens]
            [nextjournal.clerk :as clerk]
            [emmy.env :as e]
            [history-of-math.links :as links]
            [history-of-math.strands :as strands]))

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
   ;; the strands (history-of-math.strands) whose timelines the work is on
   [:fields {:optional true} strands/Fields]
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
  "The entries with :notebook and :built?, in order of year: an entry naming
   a notebook id is built when that notebook was discovered; an entry without
   one takes a discovered notebook of the same year whose title names its
   figure, unless another entry names that notebook by id (so Euler's 1736
   Mechanica does not take the notebook his 1736 Solutio names)."
  [{:keys [entries]} notebooks]
  (let [by-id (into {} (map (juxt :id identity)) notebooks)
        claimed (set (keep :notebook entries))
        free (remove #(contains? claimed (:id %)) notebooks)]
    (->> entries
         (mapv (fn [{:keys [notebook year figure] :as e}]
                 (let [nb (if notebook
                            (by-id notebook)
                            (first (filter #(and (= year (:year %)) (names-figure? % figure)) free)))]
                   (assoc e :notebook (or (:id nb) notebook) :built? (some? nb)))))
         (sort-by :year)
         vec)))

(defn strand
  "The joined entries on strand id, in order: one field's timeline."
  [joined id]
  (filterv #(contains? (:fields %) id) joined))

(defn ordered-fields
  "A set of strand ids in registry order."
  [fields]
  (filterv (set fields) strands/ids))

(defn bridges
  "Strand-to-strand bridges carried by influence edges: for each edge, every
   pair [a b] of a strand of its source and a strand of its target that the
   other end is not on. Returns [{:from a :to b :count n :notes [..]}], pairs
   unordered (a before b in registry order)."
  [joined edges]
  (let [fields-of (into {} (keep (fn [{:keys [notebook fields]}] (when notebook [notebook fields]))) joined)
        rank (zipmap strands/ids (range))
        pairs (for [{:keys [from to note]} edges
                    :let [ff (fields-of from) tf (fields-of to)]
                    a ff b tf
                    :when (and (not= a b) (not (contains? tf a)) (not (contains? ff b)))]
                [(sort-by rank [a b]) note])]
    (->> (group-by first pairs)
         (map (fn [[[a b] xs]] {:from a :to b :count (count xs) :notes (vec (distinct (map second xs)))}))
         (sort-by (juxt (comp rank :from) (comp rank :to)))
         vec)))

(defn year-label
  "300 BC, c. 250 BC, 1543, c. 1350."
  [{:keys [year circa]}]
  (str (when circa "c. ") (if (neg? year) (str (- year) " BC") (str year))))

(defn href
  "Anchor attributes for notebook id from a page depth directories below
   the apex (default 0, the apex), opened as its own page: its directory in
   the static build, its Clerk route in serve mode (history-of-math.links)."
  ([id] (href id 0))
  ([id depth]
   (links/anchor (if (links/serve?)
                   (links/notebook-href :hub id)
                   (str (links/up depth) id "/")))))

;; ---------------------------------------------------------------------------
;; The strip

(defn- era-colour [pal eras era]
  (get pal (:role (first (filter #(= era (:id %)) eras))) (:ink pal)))

(def default-scale
  "The time axis of the series: the ancient world (-320 to 400) and the
   modern (1300 to 1920) each a linear stretch, a short break between, and
   the years labelled on the axis. A series with other dates passes its own
   to `strip` as :scale."
  {:ancient [-320 400] :modern [1300 1920]
   :ticks [[-300 "300 BC"] [0 "0"] [300 "300"] [1400 "1400"] [1600 "1600"] [1800 "1800"] [1900 "1900"]]})

(defn- x-scale
  "Page x of a year: the ancient stretch and the modern stretch of scale
   each get a linear part of the strip, the gap between them a short break."
  [year {:keys [x0 x-break x1]} {[a0 a1] :ancient [m0 m1] :modern}]
  (if (<= year a1)
    (+ x0 (* (- x-break 20 x0) (/ (- year a0) (- a1 a0))))
    (+ x-break 20 (* (- x1 x-break 20) (/ (- year m0) (- m1 m0))))))

(defn strip
  "Hiccup SVG of the joined entries on a time axis, one row of labels per
   entry, coloured by era; built notebooks are links, planned ones muted.
   opts: :palette (default :series), :width, :scale (default
   `default-scale`), :legend-step (px per era in the legend; the legend
   wraps to more rows when the eras do not fit), and :decorate, a fn of
   (entry x y) to hiccup drawn after the entry's row mark (a series marks
   more than the era there, e.g. which arts a work belongs to)."
  ([t joined] (strip t joined {}))
  ([{:keys [eras]} joined {:keys [palette width scale legend-step decorate]
                           :or {palette :series width 960 scale default-scale legend-step 180}}]
   (let [pal (palette/resolve-palette palette)
         dims {:x0 40 :x-break 300 :x1 (- width 40)}
         per-row (max 1 (quot (- width 40) legend-step))
         legend-rows (quot (+ (count eras) per-row -1) per-row)
         row 22
         axis-y (+ 40 (* 18 (dec legend-rows)))
         top (+ axis-y 30)
         height (+ top (* row (count joined)) 30)]
     (into
      [:svg {:viewBox (str "0 0 " width " " height) :width width
             :style {:max-width "100%" :height "auto" :background (:background pal)
                     :font-family "ui-serif, Georgia, serif"}}
       [:line {:x1 (:x0 dims) :y1 axis-y :x2 (:x1 dims) :y2 axis-y :stroke (:muted pal) :stroke-width 1.5}]
       [:text {:x (:x-break dims) :y (- axis-y 8) :fill (:muted pal) :font-size 12 :text-anchor "middle"} "⋯"]
       [:g (for [[y lbl] (:ticks scale)]
             [:text {:x (x-scale y dims scale) :y (- axis-y 10) :fill (:muted pal) :font-size 11 :text-anchor "middle"} lbl])]
       [:g (for [[i {:keys [id title]}] (map-indexed vector eras)
                 :let [lx (+ 40 (* (mod i per-row) legend-step))
                       ly (+ 6 (* 18 (quot i per-row)))]]
             [:g [:rect {:x lx :y ly :width 12 :height 12 :fill (era-colour pal eras id)}]
              [:text {:x (+ lx 18) :y (+ ly 10) :fill (:ink pal) :font-size 11} title]])]]
      (map-indexed
       (fn [i {:keys [year figure work era built? notebook] :as e}]
         (let [c (era-colour pal eras era)
               x (x-scale year dims scale)
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
            (when decorate (decorate e x y))
            (if built? [:a (href notebook) label] label)]))
       joined)))))

(defn table
  "Hiccup table of the joined entries: date, who, work, its strands, why it
   matters. depth: the page's depth below the apex (default 0). extra,
   optional, is [[header (fn [entry] hiccup)] ...]: columns put after the
   strands (a series lists more than the era there). The two-argument form
   takes either: a number is the depth, anything else the extra columns."
  ([joined] (table joined 0 []))
  ([joined depth-or-extra]
   (if (number? depth-or-extra)
     (table joined depth-or-extra [])
     (table joined 0 depth-or-extra)))
  ([joined depth extra]
   [:table {:style {:font-size "0.9em"}}
    [:thead (into [:tr [:th "date"] [:th "who"] [:th "work"] [:th "strands"]]
                  (concat (for [[h _] extra] [:th h]) [[:th "what it did"]]))]
    (into [:tbody]
          (for [{:keys [figure work significance built? notebook fields] :as e} joined]
            (into [:tr
                   [:td {:style {:white-space "nowrap"}} (year-label e)]
                   [:td figure]
                   [:td (if built? [:a (href notebook depth) [:em work]] [:em work])
                    (when-not built? [:span {:style {:opacity 0.6}} " (planned)"])]
                   [:td {:class "strand-tags"} (str/join ", " (map strands/title (ordered-fields fields)))]]
                  (concat (for [[_ f] extra] [:td (f e)]) [[:td significance]]))))]))

(defn influence-edges
  "Resolved arcs for all installed lenses, from their :links to timeline nodes."
  [joined]
  (let [ids (set (keep :notebook joined))]
    (vec (for [id ids
               {:keys [to relation note]} (:links (lens/read-lens id))
               :when (contains? ids to)]
           {:from id :to to :relation relation :note note}))))

(def strand-ring
  "The bridge map's frame: the strands sit on a circle of radius :r about
   (:cx, :cy) in a :width x :height view box, labels outside it in two
   lines. Square enough to read at phone width (390 px) without scrolling."
  {:width 560 :height 410 :cx 280 :cy 205 :r 128})

(defn interactive-data
  "Raster-projected node and arc coordinates for the interactive hub.

   Every node carries :fields (its strands, registry order) and :rows, its
   y on the union timeline (:all) and on each strand's own timeline, so the
   view only chooses which layout to show. :strands are the strands with
   their colour, entry count and x on the bridge row; :bridges are lens
   :links between works whose ends lie on different strands, folded to
   strand pairs."
  ([t joined] (interactive-data t joined 960))
  ([{:keys [eras]} joined width]
   (let [pal (palette/resolve-palette :series)
         row 32 top 88
         ancient (fn [y] (e/+ 40 (e/* (/ 240 720) (e/+ y 320))))
         modern (fn [y] (e/+ 320 (e/* (/ (- width 360) 620) (e/- y 1300))))
         vertical (fn [n] (e/+ top (e/* row n)))
         strand-rows (into {}
                           (for [id strands/ids]
                             [id (into {} (map-indexed (fn [i e] [e i])) (strand joined id))]))
         positions (mapv (fn [i {:keys [year era notebook work fields] :as entry}]
                           (let [rows (into {:all (raster/value vertical i)}
                                            (for [f fields] [f (raster/value vertical (get-in strand-rows [f entry]))]))]
                             (assoc entry :id notebook
                                    :x (raster/value (if (<= year 400) ancient modern) year)
                                    :y (:all rows)
                                    :rows rows
                                    :year (year-label entry)
                                    :work (first (str/split work #";"))
                                    :colour (era-colour pal eras era)
                                    :fields (ordered-fields fields))))
                         (range) joined)
         by-id (into {} (keep (fn [n] (when (and (:id n) (:built? n)) [(:id n) n]))) positions)
         edges (influence-edges joined)
         arcs (for [{:keys [from to] :as edge} edges
                    :let [a (by-id from) b (by-id to)] :when (and a b)]
                (assoc edge :x (:x a) :y (:y a) :target-x (:x b) :target-y (:y b)))
         ;; the strands on a circle, the bridges as chords across it: with
         ;; fourteen strands a row is too narrow on a phone, a ring is not
         n (count strands/ids)
         {:keys [cx cy r]} strand-ring
         ring-x (fn [k] (e/+ cx (e/* r (e/cos (e/- (e/* (/ (* 2 Math/PI) n) k) (/ Math/PI 2))))))
         ring-y (fn [k] (e/+ cy (e/* r (e/sin (e/- (e/* (/ (* 2 Math/PI) n) k) (/ Math/PI 2))))))
         strand-x (zipmap strands/ids (map #(raster/value ring-x %) (range)))
         strand-y (zipmap strands/ids (map #(raster/value ring-y %) (range)))
         counts (frequencies (mapcat :fields joined))]
     {:nodes positions :edges (vec arcs) :width width :ring strand-ring
      :fields (filterv (set (keys counts)) strands/ids)
      :strands (mapv (fn [{:keys [id title role scope]}]
                       {:id id :title title :scope scope :colour (get pal role (:ink pal))
                        :count (get counts id 0) :x (strand-x id) :y (strand-y id)})
                     strands/strands)
      :bridges (mapv (fn [{:keys [from to] :as b}]
                       (assoc b :x (strand-x from) :y (strand-y from)
                              :target-x (strand-x to) :target-y (strand-y to)))
                     (bridges joined edges))
      :height (+ top (* row (count joined)) 30)
      :heights (into {} (for [id strands/ids] [id (+ top (* row (count (strand-rows id))) 30)]))})))

(defn interactive
  "The interactive timeline of joined entries with its strand selector and
   ring map, the page of every work at once (<out>/all/). depth is the
   page's depth below the apex: each built node gets its :href in the mode
   of the links (history-of-math.links)."
  ([t joined] (interactive t joined 0))
  ([t joined depth]
   (clerk/with-viewer
     {:name 'history-of-math.timeline-view/render
      :require-cljs true :transform-fn clerk/mark-presented
      :render-fn 'history-of-math.timeline-view/render}
     (update (interactive-data t joined) :nodes
             (fn [ns] (mapv (fn [{:keys [id built?] :as n}]
                              (cond-> n
                                (and id built?)
                                (assoc :href (if (links/serve?)
                                               (links/notebook-href :hub id)
                                               (str (links/up depth) id "/")))))
                            ns))))))
