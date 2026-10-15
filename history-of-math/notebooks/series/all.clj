;; # Every work on one timeline
;;
;; All the works of every topic, in order of year. Pick a strand to see
;; only its works; the chips beside a work name the other strands it is
;; on. The ring is the fourteen strands; a chord across it is a bridge, one
;; work leading to another on a different strand, as Apollonius' conics
;; lead to Kepler's orbits.
;;
;; Filled circles are notebooks you can open; open circles are planned.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns all
  {:nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/no-cache true}
  (:require [history-of-math.build :as build]
            [history-of-math.links :as links]
            [history-of-math.timeline :as timeline]
            [hive-dsl.result :as r]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:result :hide}}
(def site
  (let [t (timeline/timeline)
        nbs (build/discover build/notebooks-dir)]
    (when-not (r/ok? t) (throw (ex-info "timeline.edn is invalid" (:error t))))
    (when-not (r/ok? nbs) (throw (ex-info (build/error-message (:error nbs)) (:error nbs))))
    {:timeline (:ok t) :joined (timeline/join (:ok t) (:ok nbs))}))

(clerk/html [:p [:a (links/anchor (links/apex-href 1)) "All topics"]])

(timeline/interactive (:timeline site) (:joined site) 1)

;; ## The works
;;
;; Dates are those of the work's publication or, for the ancient authors,
;; of its composition; "c." marks an approximate date.

(clerk/html (timeline/table (:joined site) 1))
