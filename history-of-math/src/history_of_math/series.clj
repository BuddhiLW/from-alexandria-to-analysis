(ns history-of-math.series
  "The series a reader enters from the apex (resources/history_of_math/
   series.edn): named, ordered paths through the notebooks, and the decks
   published elsewhere. A series is data, not a protocol.

   Collect   `registry`: the EDN resource, validated with malli. One
             vocabulary: a lens :tangents {:series id} names any series by
             its :id, a notebook series or a deck.
   Promote   pure functions of the registry, the discovered notebooks
             (history-of-math.build/discover) and the joined timeline
             entries (history-of-math.timeline/join): `members` (the
             reading order), `entries` (the series' timeline), `summary`,
             `memberships`, `neighbours`, `connections`.
   Boundary  none here: history-of-math.build writes the series pages,
             history-of-math.apex renders them."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [malli.core :as m]
            [malli.error :as me]
            [history-of-math.strands :as strands]))

(def resource "history_of_math/series.edn")

(def Slug [:re #"^[a-z0-9][a-z0-9-]*$"])

(def NotebookSeries
  "A path through the notebooks of the site, with its page at
   <out>/series/<slug>/."
  [:map {:closed true}
   [:id keyword?]
   [:kind [:= :notebooks]]
   [:slug Slug]
   [:title [:string {:min 1}]]
   [:scope [:string {:min 1}]]
   [:strands [:vector {:min 1} strands/Field]]
   [:order {:optional true} [:vector {:min 1} Slug]]])

(def Deck
  "A series published elsewhere: live at an absolute https :url, or planned."
  [:and
   [:map {:closed true}
    [:id keyword?]
    [:kind [:= :deck]]
    [:title [:string {:min 1}]]
    [:status [:enum :live :planned]]
    [:url {:optional true} [:re #"^https://[^ ]+$"]]]
   [:fn {:error/message "a live deck has a :url"}
    (fn [{:keys [status url]}] (or (not= :live status) (some? url)))]])

(def Series [:multi {:dispatch :kind} [:notebooks NotebookSeries] [:deck Deck]])

(def Registry
  [:and
   [:map {:closed true} [:series [:vector {:min 1} Series]]]
   [:fn {:error/message "series ids are unique"}
    (fn [{:keys [series]}] (apply distinct? (map :id series)))]
   [:fn {:error/message "series slugs are unique"}
    (fn [{:keys [series]}] (let [slugs (keep :slug series)] (or (empty? slugs) (apply distinct? slugs))))]])

(defn validate
  "v when it is a valid registry; throws with malli's explanation otherwise."
  [v]
  (when-not (m/validate Registry v)
    (throw (ex-info "series.edn is invalid" {:explain (me/humanize (m/explain Registry v))})))
  v)

(defn registry
  "The series registry read from res (default the site's), validated."
  ([] (registry resource))
  ([res]
   (let [url (or (io/resource res) (throw (ex-info (str "Missing " res) {:resource res})))]
     (validate (edn/read-string (slurp url))))))

;; ---------------------------------------------------------------------------
;; Promote: pure functions of the registry and the notebooks

(defn notebook-series "The notebook series, in registry order." [reg]
  (filterv #(= :notebooks (:kind %)) (:series reg)))

(defn decks "The decks, in registry order." [reg]
  (filterv #(= :deck (:kind %)) (:series reg)))

(defn by-id "Series id -> series." [reg] (into {} (map (juxt :id identity)) (:series reg)))

(defn by-slug "Slug -> notebook series." [reg] (into {} (map (juxt :slug identity)) (notebook-series reg)))

(defn on-strands?
  "Whether x (a notebook or a timeline entry, by its :fields) is on one of
   the series' strands."
  [{:keys [strands]} x]
  (boolean (some (set strands) (:fields x))))

(defn members
  "The notebooks of series s in reading order: its explicit :order (ids
   that name no notebook are dropped; the tests forbid them), else every
   notebook on its strands by year, then id."
  [{:keys [order] :as s} notebooks]
  (if order
    (let [by (into {} (map (juxt :id identity)) notebooks)]
      (vec (keep by order)))
    (->> notebooks (filter #(on-strands? s %)) (sort-by (juxt :year :id)) vec)))

(defn entries
  "The series' timeline: the joined timeline entries on its strands, plus
   the entries of notebooks its explicit :order names, in order of year."
  [{:keys [order] :as s} joined]
  (let [named (set order)]
    (filterv #(or (on-strands? s %) (contains? named (:notebook %))) joined)))

(defn summary
  "{:span [first-year last-year] :built n :planned n :entries m
   :entries-built k :first notebook} of series s: built are its members,
   planned its timeline entries without a notebook, :entries the size of
   its timeline and :entries-built how many of those have a notebook (the
   honest `k of m entries built`; :built counts members and may differ,
   since a notebook can belong through its :order alone)."
  [s notebooks joined]
  (let [ms (members s notebooks)
        es (entries s joined)
        years (concat (map :year es) (map :year ms))]
    {:span (when (seq years) [(apply min years) (apply max years)])
     :built (count ms)
     :planned (count (remove :built? es))
     :entries (count es)
     :entries-built (count (filter :built? es))
     :first (first ms)}))

(defn memberships
  "Notebook id -> the notebook series it is a member of, in registry order."
  [reg notebooks]
  (let [paths (for [s (notebook-series reg)] [s (set (map :id (members s notebooks)))])]
    (into {}
          (for [{:keys [id]} notebooks]
            [id (vec (for [[s ids] paths :when (contains? ids id)] s))]))))

(defn neighbours
  "{:prev :next} of notebook id within the ordered members ms (either nil
   at an end); nil when id is not a member."
  [ms id]
  (let [v (vec ms)]
    (when-let [i (first (keep-indexed (fn [i nb] (when (= id (:id nb)) i)) v))]
      {:prev (get v (dec i)) :next (get v (inc i))})))

(defn connections
  "The other notebook series that series s connects to, in registry order:
   {:series t :shared [notebook ids on both] :bridges [[a b] ...]}, where a
   bridge is a strand bridge (history-of-math.timeline/bridges: {:from :to})
   from a strand of s to a strand of t that s itself is not on."
  [reg s notebooks strand-bridges]
  (let [mine (set (map :id (members s notebooks)))
        own (set (:strands s))]
    (vec
     (for [t (notebook-series reg)
           :when (not= (:id t) (:id s))
           :let [theirs (set (:strands t))
                 shared (filterv #(contains? mine (:id %)) (members t notebooks))
                 bridges (vec (distinct
                               (for [{:keys [from to]} strand-bridges
                                     [a b] [[from to] [to from]]
                                     :when (and (own a) (theirs b) (not (own b)))]
                                 [a b])))]
           :when (or (seq shared) (seq bridges))]
       {:series t :shared (mapv :id shared) :bridges bridges}))))
