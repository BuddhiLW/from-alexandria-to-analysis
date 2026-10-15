;; # Mathematics from its sources
;;
;; A history of mathematics told one work at a time. Each notebook takes one
;; book: who wrote it, where, what problem it answered, what came before and
;; after. It states the propositions, follows the author's argument step by
;; step, moves the figure, and checks the claims with Emmy. Every figure,
;; proof and check lives in Alexandria, the library the first series is
;; named after; the notebooks only tell the story and mount them.
;;
;; The thread is one question asked again and again: how do you measure
;; what is curved, or changing, or infinite? The Greeks trapped curves
;; between polygons. Oresme drew motion as a figure. Kepler and Galileo
;; found the laws of the planets and of falling bodies. Fermat and Descartes
;; turned curves into equations. Newton and Leibniz made the calculus.
;; Euler made the function its object. Cauchy, Riemann, Weierstrass,
;; Dedekind and Cantor rebuilt it on limits and on the real numbers, and
;; Lebesgue rebuilt the integral on measure. The last era, structures,
;; foundations and computation, turns to the frameworks themselves: Riemann
;; asks what a geometry must be given, and finds a rule for measuring an
;; infinitely small step; Klein answers that a geometry is the group of
;; transformations that leaves its properties unchanged; Hilbert answers
;; that a geometry is whatever satisfies its axioms, and tests each axiom
;; with a model in which it fails.
;;
;; ## Topics
;;
;; Each topic has its own timeline, from its first work to its last, with
;; the notebooks you can open and the works still planned.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns index
  {:nextjournal.clerk/visibility {:code :hide :result :show}
   ;; the apex reads series.edn, timeline.edn and notebooks/ at build time;
   ;; Clerk's result cache does not see those files change, so never cache it
   :nextjournal.clerk/no-cache true}
  (:require [history-of-math.apex :as apex]
            [history-of-math.build :as build]
            [history-of-math.links :as links]
            [history-of-math.series :as series]
            [history-of-math.timeline :as timeline]
            [hive-dsl.result :as r]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:result :hide}}
(def site
  (let [t (timeline/timeline)
        nbs (build/discover build/notebooks-dir)]
    (when-not (r/ok? t) (throw (ex-info "timeline.edn is invalid" (:error t))))
    (when-not (r/ok? nbs) (throw (ex-info (build/error-message (:error nbs)) (:error nbs))))
    {:notebooks (:ok nbs) :registry (series/registry)
     :joined (timeline/join (:ok t) (:ok nbs))}))

(clerk/html (apex/cards (:registry site) (:notebooks site) (:joined site)))

;; ## Index
;;
;; Every topic with its whole timeline in reading order. A work with a
;; notebook is a link to it; a work without one is marked planned. The
;; slides and courses published beside the notebooks close the list.


(clerk/html (apex/index (:registry site) (:notebooks site) (:joined site)))

;; ## Closing
;;
;; The closing page asks three questions of the Greek notebooks: why the
;; Greeks valued geometry, how they worked without algebra, and whether they
;; could have gone on to algebra, the calculus and an industrial revolution.

(clerk/html [:p [:a (timeline/href "greek-closing")
                 "Closing: why the Greeks valued geometry, and what came after"]])

;; ## Free to read, copy and improve
;;
;; An open wiki for learning mathematics from its sources. The prose,
;; figures and notebooks are licensed under
;; [CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/). The code
;; (Alexandria, its players and kernels, and the build) is licensed under the
;; [GNU GPL, version 3 or later](https://www.gnu.org/licenses/gpl-3.0.html),
;; the licence of Emmy, which every notebook runs. Ancient and early-modern
;; texts are quoted from public-domain editions. A modern translation is
;; quoted with its translator and edition named beside the quote.
;;
;; The source is at
;; [github.com/BuddhiLW/from-alexandria-to-analysis](https://github.com/BuddhiLW/from-alexandria-to-analysis).
;; To fix a reading, add a proposition or write a new era, open an issue or a
;; pull request there. A notebook is one file in `history-of-math/notebooks/`.
;; Its figures, proofs and graded checks go into `alexandria/`, and Emmy checks
;; every claim when the site is built.

(clerk/html [:p {:class "all-works-link" :style {:margin "2em 0 0" :font-size "0.92em"}}
             [:a (links/anchor (links/all-href 0)) "Every work of every topic on one timeline"]])
