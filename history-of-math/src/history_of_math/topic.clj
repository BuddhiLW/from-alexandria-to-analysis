(ns history-of-math.topic
  "The page of one topic (a notebook series of series.edn): its timeline
   alone, in the look of the original hub (one line, era bands, a figure,
   its work and one sentence per entry), then the topics it connects to.

   Collect   none: the registry, the joined timeline and the discovered
             notebooks come from the caller.
   Promote   `layout` (pure): rows, x positions on the topic's own span,
             era bands, the sentence wrapped to lines; `connections`
             (pure): one sentence per other topic, through a work both
             timelines hold or a lens :link between their works.
   Boundary  `page` hiccup, `viewer` the Clerk viewer that picks the topic
             of ?series in the browser, so the same notebook serves every
             topic under Clerk serve and one topic in the static build."
  (:require [alexandria.palette :as palette]
            [clojure.string :as str]
            [history-of-math.apex :as apex]
            [history-of-math.links :as links]
            [history-of-math.series :as series]
            [history-of-math.timeline :as timeline]
            [nextjournal.clerk :as clerk]))

(def width 960)
(def axis-y 64)
(def top 104)

(defn first-sentence
  "The first sentence of s (to its first full stop followed by a space)."
  [s]
  (let [i (str/index-of s ". ")]
    (if i (subs s 0 (inc i)) s)))

(defn wrap
  "s split at spaces into lines of at most n characters."
  [s n]
  (reduce (fn [lines w]
            (let [l (peek lines)]
              (if (and l (<= (+ (count l) 1 (count w)) n))
                (conj (pop lines) (str l " " w))
                (conj lines w))))
          [] (str/split s #"\s+")))

(defn x-scale
  "Year -> page x over the topic's own years: one linear stretch, or, when
   the topic reaches from antiquity (before 600) into the modern world
   (after 1200), the ancient and the modern each a stretch with a short
   break between them."
  [years]
  (let [lo (apply min years) hi (apply max years)
        x0 48 x1 (- width 48)
        lin (fn [a b u v] (fn [y] (if (= a b) (/ (+ u v) 2.0) (+ u (* (- v u) (/ (- y a) (double (- b a))))))))]
    (if (and (< lo 600) (> hi 1200))
      (let [ancient (filter #(< % 600) years) modern (filter #(>= % 600) years)
            brk 300
            fa (lin (apply min ancient) (apply max ancient) x0 (- brk 24))
            fm (lin (apply min modern) (apply max modern) (+ brk 24) x1)]
        {:f (fn [y] (if (< y 600) (fa y) (fm y))) :break brk})
      {:f (lin lo hi x0 x1)})))

(defn layout
  "The positions of a topic's entries (joined timeline entries, in year
   order): each with :x :y :lines (its sentence) :colour :label, the era
   :bands (only when the entries span more than one era), the :height."
  [{:keys [eras]} entries]
  (let [pal (palette/resolve-palette :series)
        era-by (into {} (map (juxt :id identity)) eras)
        colour (fn [era] (get pal (:role (era-by era)) (:ink pal)))
        {:keys [f break]} (x-scale (map :year entries))
        rows (loop [[e & more] entries y top acc []]
               (if-not e
                 acc
                 (let [lines (wrap (first-sentence (:significance e)) 78)]
                   (recur more (+ y 24 (* 15 (count lines)))
                          (conj acc (assoc e :x (f (:year e)) :y y :lines lines
                                           :colour (colour (:era e))
                                           :label (str (timeline/year-label e) "  " (:figure e) ", "
                                                       (first (str/split (:work e) #";")))))))))
        bands (for [[era es] (group-by :era rows)]
                (let [xs (map :x es)]
                  {:era era :title (:title (era-by era)) :colour (colour era)
                   :x (- (apply min xs) 8) :w (+ 16 (- (apply max xs) (apply min xs)))}))]
    {:rows rows :break break
     :bands (when (< 1 (count bands)) (vec (sort-by :x bands)))
     :height (+ (if (seq rows) (+ (:y (peek rows)) (* 15 (count (:lines (peek rows))))) top) 30)}))

(defn timeline-svg
  "The topic's timeline as SVG: the line, the era bands with their names,
   and per entry a stem, a circle (filled when built), the label (a link
   when built, muted and italic when planned) and its sentence."
  [{:keys [rows bands break height]} slug]
  (let [ink "#e9f1f5" muted "#8fa0b0"]
    (into
     [:svg {:class "timeline-svg topic-timeline" :data-series slug :role "img"
            :view-box (str "0 0 " width " " height) :aria-label "Timeline of the topic's works"
            :style {:display "block" :width "100%" :max-width (str width "px") :height "auto"
                    :background "#101e30" :font-family "ui-serif, Georgia, serif"}}
      [:line {:x1 40 :x2 (- width 40) :y1 axis-y :y2 axis-y :stroke "#b3c8d9"}]]
     (concat
      (when break [[:text {:x break :y (- axis-y 8) :fill "#b3c8d9" :text-anchor "middle"} "⋯"]])
      (for [{:keys [era title colour x w]} bands]
        [:g {:class "era-band" :data-era (name era)}
         [:rect {:x x :y (- axis-y 10) :width w :height 20 :rx 4 :fill colour :fill-opacity 0.28}]
         [:text {:x (+ x (/ w 2)) :y (- axis-y 18) :fill colour :font-size 11 :text-anchor "middle"} title]])
      (for [{:keys [x y lines colour label built? notebook fields] :as e} rows
            :let [right? (> x (* 0.55 width))
                  tx (if right? (- x 10) (+ x 10))
                  anchor (if right? "end" "start")
                  text [:text {:x tx :y (+ y 4) :font-size 13 :text-anchor anchor
                               :fill (if built? ink muted) :font-style (if built? "normal" "italic")}
                        (str label (when-not built? " (planned)"))]]]
        [:g {:class "timeline-entry" :data-notebook (or notebook "") :data-year (:year e)
             :data-built (str (boolean built?)) :opacity (if built? 1 0.62)}
         [:line {:x1 x :x2 x :y1 axis-y :y2 y :stroke colour :stroke-opacity 0.3}]
         [:circle {:cx x :cy axis-y :r 3 :fill colour}]
         [:circle {:cx x :cy y :r 5 :fill (if built? colour "none") :stroke colour}]
         (if built? [:a (assoc (apex/anchor (apex/notebook-href 2 notebook slug)) :class "entry-link") text] text)
         (into [:text {:class "entry-sentence" :x tx :y (+ y 19) :font-size 11 :text-anchor anchor :fill muted}]
               (map-indexed (fn [i l] [:tspan {:x tx :dy (if (zero? i) 0 15)} l]) lines))])))))

;; ---------------------------------------------------------------------------
;; Connections

(defn- work-name [e] (str (:figure e) ", " (first (str/split (:work e) #";"))))

(defn connections
  "The other topics topic s connects to, in registry order, each
   {:series t :sentence s}: through a work on both timelines (a built one
   first), else through a lens :link between a work of one and a work of
   the other, told by the link's own note (edges:
   history-of-math.timeline/influence-edges)."
  [reg s joined edges]
  (let [mine (series/entries s joined)
        mine-set (set mine)
        by-nb (into {} (keep (fn [e] (when (:notebook e) [(:notebook e) e]))) joined)
        nb-of (fn [es] (set (keep :notebook es)))]
    (vec
     (for [t (series/notebook-series reg)
           :when (not= (:id t) (:id s))
           :let [theirs (series/entries t joined)
                 shared (sort-by (complement :built?) (filter mine-set theirs))
                 a (nb-of mine) b (nb-of theirs)
                 link (first (for [{:keys [from to] :as edge} edges
                                   :when (or (and (a from) (b to)) (and (b from) (a to)))]
                               edge))
                 sentence (cond
                            (seq shared)
                            (let [n (dec (count shared))]
                              (str "Through " (work-name (first shared)) " (" (timeline/year-label (first shared)) ")"
                                   (when (pos? n) (str " and " n " other " (if (= 1 n) "work" "works")))
                                   ", on both timelines."))
                            link
                            (let [{:keys [from to note]} link
                                  [p q] (sort-by :year [(by-nb from) (by-nb to)])]
                              (str "From " (work-name p) " (" (timeline/year-label p) ") to "
                                   (work-name q) " (" (timeline/year-label q) "): " note)))]
           :when sentence]
       {:series t :sentence sentence}))))

;; ---------------------------------------------------------------------------
;; Boundary

(defn page
  "The hiccup of topic s: the way back to the apex, its title and scope,
   its timeline, the topics it connects to."
  [t s entries cs]
  [:article {:class "topic-page" :data-series (:slug s)}
   [:p [:a (assoc (apex/anchor (links/apex-href 2)) :class "topic-apex") "All topics"]]
   [:h1 (:title s)]
   [:p {:class "series-scope"} (:scope s)]
   [:p {:style {:opacity 0.8 :font-size "0.92em"}}
    "Filled circles are notebooks you can open; open circles are planned."]
   (timeline-svg (layout t entries) (:slug s))
   (when (seq cs)
     [:section {:class "topic-connections"}
      [:h2 "Connected topics"]
      (into [:ul {:class "series-connections"}]
            (for [{t' :series :keys [sentence]} cs]
              [:li {:data-series (:slug t')}
               [:a (apex/anchor (apex/series-href 2 (:slug t'))) (:title t')] ": " sentence]))])])

(defn pages
  "slug -> page hiccup for the topics of slugs."
  [t reg joined slugs]
  (let [edges (timeline/influence-edges joined)
        by (series/by-slug reg)]
    (into {} (for [slug slugs :let [s (by slug)]]
               [slug (page t s (series/entries s joined) (connections reg s joined edges))]))))

(def viewer
  "Shows the page of the topic named by the URL's ?series among the pages
   shipped, else the default one."
  {:name `viewer
   :transform-fn clerk/mark-presented
   :render-fn '(fn [{:keys [pages default]} _]
                 (let [q (.get (js/URLSearchParams. (.. js/window -location -search)) "series")]
                   (or (get pages q) (get pages default))))})

(defn view
  "The topic pages of slugs with default shown when ?series names none."
  [t reg joined slugs default]
  (clerk/with-viewer viewer {:pages (pages t reg joined slugs) :default default}))
