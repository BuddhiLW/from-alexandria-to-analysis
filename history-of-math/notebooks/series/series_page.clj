;; The timeline page of one topic, built once per notebook series of
;; series.edn into <out>/series/<slug>/ (history-of-math.build/build-series!
;; sets the slug). Under Clerk serve every topic is shipped and ?series
;; picks one. Not a history notebook: it sits under notebooks/series/,
;; which discovery never reads.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns series-page
  {:nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/no-cache true}
  (:require [history-of-math.build :as build]
            [history-of-math.links :as links]
            [history-of-math.series :as series]
            [history-of-math.timeline :as timeline]
            [history-of-math.topic :as topic]
            [hive-dsl.result :as r]))

^{:nextjournal.clerk/visibility {:result :hide}}
(def site
  (let [t (timeline/timeline)
        nbs (build/discover build/notebooks-dir)
        reg (series/registry)
        all (map :slug (series/notebook-series reg))
        slug (or @build/!series-slug (first all))]
    (when-not (r/ok? t) (throw (ex-info "timeline.edn is invalid" (:error t))))
    (when-not (r/ok? nbs) (throw (ex-info (build/error-message (:error nbs)) (:error nbs))))
    (when-not (some #{slug} all) (throw (ex-info (str "No series " slug) {:slug slug})))
    {:t (:ok t) :reg reg :joined (timeline/join (:ok t) (:ok nbs)) :default slug
     :slugs (if (links/serve?) all [slug])}))

(topic/view (:t site) (:reg site) (:joined site) (:slugs site) (:default site))
