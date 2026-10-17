;; # How the topics connect
;;
;; Every topic of the site on one ring. A circle is a topic, larger the
;; more of its notebooks are written; an open circle has none yet. A chord
;; joins two topics that hold the same work, thicker the more works they
;; share: Gauss's Disquisitiones is number theory and algebra, his surfaces
;; are geometry and analysis.
;;
;; Pick a topic to light it and its neighbours and to list its works, the
;; notebooks you can open first. Pick a chord, or "also in" beside a work,
;; to see which topics it bridges. The list of every topic follows the
;; ring.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns topics
  {:nextjournal.clerk/visibility {:code :hide :result :show}
   ;; reads series.edn, timeline.edn and notebooks/ at build time
   :nextjournal.clerk/no-cache true}
  (:require [history-of-math.links :as links]
            [history-of-math.topics :as topics]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:result :hide}}
(def graph
  (let [{:keys [reg notebooks joined]} (topics/site)]
    (topics/graph reg notebooks joined)))

(clerk/html [:p [:a (links/anchor (links/apex-href topics/depth)) "All topics"]
             " · "
             [:a (links/anchor (links/all-href topics/depth)) "Every work on one timeline"]])

(topics/view graph)

;; ## Every topic and its works
;;
;; A topic's title opens its timeline. A work you can read is a link to its
;; notebook, followed through that topic; a planned work is not a link yet.

(clerk/html [:p {:class "topics-summary"} (topics/summary-line graph)])

(clerk/html (topics/topic-list graph))
