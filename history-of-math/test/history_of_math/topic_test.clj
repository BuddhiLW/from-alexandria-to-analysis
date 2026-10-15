(ns history-of-math.topic-test
  "One plain timeline per topic: the apex lists topics only, a topic page
   holds its own entries in year order, era bands only across eras, the
   connections a sentence each, the everything view on its own page."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [history-of-math.apex :as apex]
            [history-of-math.build :as build]
            [history-of-math.links :as links]
            [history-of-math.series :as series]
            [history-of-math.timeline :as timeline]
            [history-of-math.topic :as topic]))

(defn- site []
  (let [nbs (:ok (build/discover build/notebooks-dir))
        t (:ok (timeline/timeline))]
    {:t t :reg (series/registry) :nbs nbs :joined (timeline/join t nbs)}))

(defn- nodes [h] (filter vector? (tree-seq coll? seq h)))
(defn- with-class [h c] (filter #(= c (:class (when (map? (second %)) (second %)))) (nodes h)))

(deftest the-apex-lists-topics-only
  (let [{:keys [reg nbs joined]} (site)
        index (slurp "notebooks/index.clj")
        cards (apex/cards reg nbs joined)]
    (is (= (count (series/notebook-series reg)) (count (with-class cards "series-card"))))
    (is (not (str/includes? index "timeline/interactive")) "no union timeline")
    (is (not (str/includes? index "timeline/table")))
    (is (not (re-find #":svg|strand-map|influence-arc" (pr-str cards))) "no svg, no map, no arcs")
    (is (str/includes? index "links/all-href") "one line to the page of every work")
    (is (str/includes? index "Free to read") "the licence stays")))

(defn- hrefs [h] (set (keep #(:href (when (map? (second %)) (second %))) (nodes h))))

(deftest the-apex-index-links-every-series-and-every-built-notebook
  (let [{:keys [reg nbs joined]} (site)
        ss (series/notebook-series reg)
        index (apex/index reg nbs joined)
        hs (hrefs index)
        ;; the universe from the files, as completeness_test
        files (->> (.listFiles (java.io.File. build/notebooks-dir))
                   (filter #(and (.isFile %) (str/ends-with? (.getName %) ".clj")
                                 (not= "index.clj" (.getName %))))
                   (map #(build/notebook-id (.getName %))) set)
        sections (with-class index "index-series")]
    (testing "non-vacuity"
      (is (<= 12 (count ss)))
      (is (<= 30 (count files)))
      (is (= (count ss) (count sections))))
    (testing "an h3 per series, a plain link to its page, in registry order"
      (is (= (map #(str "series/" (:slug %) "/") ss)
             (for [sec sections
                   :let [h3 (first (filter #(= :h3 (first %)) (nodes sec)))]]
               (:href (second (first (filter #(= :a (first %)) (nodes h3)))))))))
    (doseq [s ss]
      (testing (:slug s)
        (is (contains? hs (str "series/" (:slug s) "/")))
        (doseq [e (series/entries s joined) :when (:built? e)]
          (is (contains? hs (str (:notebook e) "/?series=" (:slug s))) (:notebook e)))
        (doseq [m (series/members s nbs)]
          (is (contains? files (:id m)))
          (is (contains? hs (str (:id m) "/?series=" (:slug s))) (:id m)))
        (let [sec (first (filter #(= (:slug s) (:data-series (second %))) sections))
              lis (with-class sec "index-entry")
              planned (filter #(= "false" (:data-built (second %))) lis)]
          (is (<= (count (series/entries s joined)) (count lis)) "the whole timeline")
          (is (= (:planned (series/summary s nbs joined)) (count planned)) "planned entries counted")
          (is (every? #(str/includes? (pr-str %) "(planned)") planned))
          (is (every? (fn [li] (not-any? #(= :a (first %)) (nodes li))) planned) "planned entries are plain text"))))
    (testing "the decks close the list as absolute https links"
      (doseq [d (series/decks reg) :when (= :live (:status d))]
        (is (contains? hs (:url d)))))))

(deftest a-series-card-starts-with-its-first-notebook-and-counts-honestly
  (let [{:keys [reg nbs joined]} (site)]
    (doseq [s (series/notebook-series reg)
            :let [sm (series/summary s nbs joined)
                  card (apex/series-card s sm)
                  first-link (first (with-class card "series-first"))
                  built (second (first (with-class card "series-built")))]]
      (testing (:slug s)
        (is (= (:entries sm) (count (series/entries s joined))))
        (is (<= (:entries-built sm) (:entries sm)))
        (is (= (:entries sm) (+ (:entries-built sm) (:planned sm))))
        (is (= (:entries-built sm) (:data-n built)))
        (is (str/includes? (pr-str card) (str (:entries-built sm) " of ")))
        (if (:first sm)
          (do (is (= (:id (:first sm)) (:data-first (second first-link))))
              (is (= (str (:id (:first sm)) "/?series=" (:slug s)) (:href (second first-link)))))
          (is (nil? first-link)))))))

(deftest a-topic-page-holds-its-own-entries-in-year-order
  (let [{:keys [t reg joined]} (site)
        edges (timeline/influence-edges joined)]
    (doseq [s (series/notebook-series reg)
            :let [es (series/entries s joined)
                  h (topic/page t s es (topic/connections reg s joined edges))
                  entries (with-class h "timeline-entry")
                  years (map #(:data-year (second %)) entries)]]
      (testing (:slug s)
        (is (= (count es) (count entries)) "exactly its own entries")
        (is (= years (sort years)) "in order of year")
        (is (every? #(= (:slug s) (:data-series (second %))) (with-class h "topic-timeline")))
        (is (not (re-find #"strand-chip|influence-arc|strand-map|timeline-fields" (pr-str h))))
        (let [eras (set (map :era es))
              bands (count (filter #(= "era-band" (:class (second %))) (nodes h)))]
          (if (< 1 (count eras))
            (is (= (count eras) bands))
            (is (zero? bands) "no era band for a topic in one era")))
        (testing "built works are links, planned ones are not"
          (let [links (count (with-class h "entry-link"))]
            (is (= (count (filter :built? es)) links))))
        (testing "a connection is a link and one sentence"
          (doseq [li (drop 2 (first (with-class h "series-connections")))]
            (is (str/ends-with? (str (last li)) "."))))))))

(deftest topic-layout-rows-descend-and-wrap
  (let [{:keys [t reg joined]} (site)
        s (get (series/by-slug reg) "curved-space")
        {:keys [rows height]} (topic/layout t (series/entries s joined))]
    (is (apply < (map :y rows)))
    (is (every? #(<= 30 (:x %) 930) rows))
    (is (every? (fn [r] (every? #(<= (count %) 78) (:lines r))) rows))
    (is (< (:y (last rows)) height))))

(deftest links-follow-the-mode
  (is (= "series/curved-space/" (links/topic-href 0 "curved-space")))
  (is (= "../../series/x/" (links/topic-href 2 "x")))
  (is (= "all/" (links/all-href 0)))
  (is (= "../../" (links/apex-href 2)))
  (try (links/set-mode! :serve)
       (is (= "/notebooks/series/series_page?series=x" (links/topic-href 2 "x")))
       (is (= "/notebooks/series/all" (links/all-href 0)))
       (is (= "/" (links/apex-href 2)))
       (finally (links/set-mode! :static))))

(deftest reader-strings-keep-the-language-rules
  (doseq [f ["notebooks/index.clj" "notebooks/series/series_page.clj" "notebooks/series/all.clj"
             "src/history_of_math/topic.clj"]
          :let [t (slurp f)]]
    (is (not (str/includes? t "—")) f)
    (is (not (re-find #"(?i)this (page|notebook|topic) (shows|lists)" t)) f)))
