(ns history-of-math.topics
  "The page that connects every topic (<out>/series/, notebooks/series/
   topics.clj): the topics as a graph, a node per notebook series of
   series.edn and an edge between two topics for the works both hold, and
   under it a plain list of every topic and its works, which reads without
   JavaScript.

   Collect   `site`: the timeline, the discovered notebooks and the series
             registry, as the apex reads them.
   Promote   `works` (a topic's works, the rows of its index on the apex,
             history-of-math.apex/index-rows), `graph` (pure): the topics on
             a ring with their counts, the edges weighted by the works two
             topics share, work -> the topics it is on.
   Boundary  `topic-list` hiccup, `noscript-html` the same list as HTML
             (history-of-math.build puts it in the page's <noscript>),
             `view` the Clerk viewer (history-of-math.topics-view)."
  (:require [clojure.string :as str]
            [history-of-math.apex :as apex]
            [history-of-math.build :as build]
            [history-of-math.links :as links]
            [history-of-math.series :as series]
            [history-of-math.timeline :as timeline]
            [hive-dsl.result :as r]
            [nextjournal.clerk :as clerk]
            [hiccup2.core :as hiccup]))

(def depth
  "The page's depth below the apex: <out>/series/."
  1)

(def ring
  "The frame of the graph: the topics on a circle of radius :r about
   (:cx, :cy) in a :width x :height view box, labels outside it."
  {:width 680 :height 560 :cx 340 :cy 280 :r 178})

;; ---------------------------------------------------------------------------
;; Collect

(defn site
  "{:t :reg :notebooks :joined} of the site as the apex reads it; throws
   when the timeline or a notebook's metadata is invalid."
  []
  (let [t (timeline/timeline)
        nbs (build/discover build/notebooks-dir)]
    (when-not (r/ok? t) (throw (ex-info "timeline.edn is invalid" (:error t))))
    (when-not (r/ok? nbs) (throw (ex-info (build/error-message (:error nbs)) (:error nbs))))
    {:t (:ok t) :reg (series/registry) :notebooks (:ok nbs)
     :joined (timeline/join (:ok t) (:ok nbs))}))

;; ---------------------------------------------------------------------------
;; Promote

(defn work-key
  "The identity of a work across topics: its notebook id, else its year
   and figure (a planned entry with no notebook named)."
  [{:keys [notebook year figure title]}]
  (or notebook (str year ":" (or figure title))))

(defn works
  "The works of topic s in reading order, each once: {:key :label :year
   :built? :notebook :href}, the rows of its index on the apex. A built
   work's :href opens its notebook with ?series=<slug>."
  [{:keys [slug] :as s} notebooks joined]
  (->> (apex/index-rows s notebooks joined)
       (map (fn [{:keys [notebook built?] :as e}]
              (let [b (boolean (and built? notebook))]
                (cond-> {:key (work-key e) :label (apex/entry-label e) :year (:year e)
                         :built? b :notebook notebook}
                  b (assoc :href (apex/notebook-href depth notebook slug))))))
       (reduce (fn [[seen acc] w] (if (seen (:key w)) [seen acc] [(conj seen (:key w)) (conj acc w)]))
               [#{} []])
       second))

(defn two-lines
  "A title split at the space nearest its middle, so a label outside the
   ring stays narrow."
  [title]
  (let [mid (/ (count title) 2)
        spaces (keep-indexed (fn [i c] (when (= c \space) i)) title)]
    (if (and (< 12 (count title)) (seq spaces))
      (let [i (apply min-key #(Math/abs (double (- % mid))) spaces)]
        [(subs title 0 i) (subs title (inc i))])
      [title])))

(defn place
  "Position k of n on the ring (the first at the top, clockwise), with the
   radius of a node that holds `built` notebooks and its label's anchor,
   x and first-line y."
  [k n built title]
  (let [{:keys [cx cy r]} ring
        a (- (* 2 Math/PI (/ k (double n))) (/ Math/PI 2))
        x (+ cx (* r (Math/cos a)))
        y (+ cy (* r (Math/sin a)))
        radius (+ 7 (* 2.4 (Math/sqrt built)))
        dx (- x cx) dy (- y cy)
        d (max 1.0 (Math/hypot dx dy))
        lx (+ x (* (+ radius 8) (/ dx d)))
        ly (+ y (* (+ radius 8) (/ dy d)))
        lines (two-lines title)
        n-lines (count lines)]
    {:x x :y y :radius radius :lines lines
     :anchor (cond (< (Math/abs dx) 24) "middle" (pos? dx) "start" :else "end")
     :lx lx
     :ly (cond (< dy -24) (- ly 14 (* 15 (dec n-lines)))
               (> dy 24) (+ ly 12)
               :else (+ ly 4 (* -7 n-lines)))}))

(defn graph
  "The topics as a graph:
     :topics       per notebook series, registry order: :slug :title :scope
                   :href (its timeline page), :works, :built :planned, its
                   place on the ring (`place`)
     :edges        {:from :to :weight :built :works} for every two topics
                   that share a work (:weight works, :built of them built,
                   :works their keys), from before to in registry order
     :work-topics  work key -> the slugs of the topics it is on
     :ring         the frame."
  [reg notebooks joined]
  (let [ss (series/notebook-series reg)
        n (count ss)
        ws (into {} (for [s ss] [(:slug s) (works s notebooks joined)]))
        keys-of (into {} (for [[slug w] ws] [slug (set (map :key w))]))
        topics (vec (map-indexed
                     (fn [k {:keys [slug title scope]}]
                       (let [w (ws slug)
                             built (count (filter :built? w))]
                         (merge {:slug slug :title title :scope scope
                                 :href (links/topic-href depth slug)
                                 :works w :built built :planned (- (count w) built)}
                                (place k n built title))))
                     ss))
        edges (vec (for [[i a] (map-indexed vector ss)
                         b (drop (inc i) ss)
                         :let [shared (filterv #(contains? (keys-of (:slug b)) (:key %)) (ws (:slug a)))]
                         :when (seq shared)]
                     {:from (:slug a) :to (:slug b) :weight (count shared)
                      :built (count (filter :built? shared)) :works (mapv :key shared)}))
        work-topics (reduce (fn [m {:keys [slug]}]
                              (reduce #(update %1 (:key %2) (fnil conj []) slug) m (ws slug)))
                            {} ss)]
    {:topics topics :edges edges :work-topics work-topics :ring ring}))

(defn neighbours
  "slug -> [[other-slug weight] ...] from the edges, heaviest first."
  [edges]
  (reduce (fn [m {:keys [from to weight]}]
            (-> m (update from (fnil conj []) [to weight]) (update to (fnil conj []) [from weight])))
          {} (sort-by (comp - :weight) edges)))

;; ---------------------------------------------------------------------------
;; Boundary

(defn topic-list
  "Every topic with its works, as plain links: the topic's title opens its
   timeline, a built work its notebook (?series=<slug>), a planned work is
   text marked planned; a work on other topics names them, each a link to
   that topic in the list. Classes only, no inline styles, so it renders
   the same in Clerk and in the page's <noscript> (site.css)."
  [{:keys [topics edges work-topics]}]
  (let [title-of (into {} (map (juxt :slug :title)) topics)
        nbs (neighbours edges)
        to-topic (fn [slug] [:a (assoc (links/anchor (str "#topic-" slug)) :class "topics-jump") (title-of slug)])]
    (into [:section {:class "topics-list"}]
          (for [{:keys [slug title href built planned] ws :works} topics]
            [:section {:class "topics-topic" :id (str "topic-" slug) :data-series slug}
             [:h3 [:a (assoc (links/anchor href) :class "topics-topic-link") title]]
             [:p {:class "topics-facts"}
              (str built " built, " planned " planned")
              (when-let [ns (seq (nbs slug))]
                (into [:span {:class "topics-connected"} ". Shares works with "]
                      (interpose ", " (for [[o w] ns] [:span (to-topic o) (str " (" w ")")]))))]
             (into [:ol {:class "topics-works"}]
                   (for [{:keys [key label built? href]} (concat (filter :built? ws) (remove :built? ws))
                         :let [others (remove #{slug} (work-topics key))]]
                     [:li {:class (if built? "topics-work" "topics-work topics-planned")
                           :data-work key :data-built (str built?)}
                      (if built?
                        [:a (assoc (links/anchor href) :class "topics-notebook") label]
                        [:span label [:em " (planned)"]])
                      (when (seq others)
                        (into [:span {:class "topics-also"} ", also in "]
                              (interpose ", " (map to-topic others))))]))]))))

(defn noscript-html
  "`topic-list` of the site as an HTML string, for the page's <noscript>."
  []
  (let [{:keys [reg notebooks joined]} (site)]
    (str (hiccup/html (topic-list (graph reg notebooks joined))))))

(def viewer
  {:name 'history-of-math.topics-view/render
   :require-cljs true
   :transform-fn clerk/mark-presented
   :render-fn 'history-of-math.topics-view/render})

(defn view
  "The graph with its topic picker and the panel of the selected topic,
   edge or work (history-of-math.topics-view)."
  [g]
  (clerk/with-viewer viewer g))

(defn page-link
  "The link to this page from a page `depth` below the apex, with the topic
   slug preselected when given."
  [depth slug text]
  [:a (assoc (links/anchor (str (links/topics-href depth) (when slug (str "?series=" slug))))
             :class "topics-hub-link")
   text])

(defn summary-line
  "How many topics, works and shared works the graph holds."
  [{:keys [topics edges work-topics]}]
  (let [bridging (count (filter #(< 1 (count (val %))) work-topics))]
    (str (count topics) " topics, " (count work-topics) " works, "
         bridging " of them on more than one topic, " (count edges) " pairs of topics that share a work.")))

(comment
  (let [{:keys [reg notebooks joined]} (site)]
    (str/join "\n" (map (juxt :from :to :weight) (:edges (graph reg notebooks joined))))))
