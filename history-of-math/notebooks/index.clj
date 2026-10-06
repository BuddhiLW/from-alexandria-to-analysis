;; # From Alexandria to Analysis
;;
;; A history of mathematics told one work at a time, from Euclid's
;; *Elements* to Lebesgue's integral. Each notebook takes one book: who wrote
;; it, where, what problem it answered, what came before and after. It
;; states the propositions, follows the author's argument step by step,
;; moves the figure, and checks the claims with Emmy. Every figure, proof and
;; check lives in Alexandria, the library the series is named after; the
;; notebooks only tell the story and mount them.
;;
;; The thread is one question asked again and again: how do you measure
;; what is curved, or changing, or infinite? The Greeks trapped curves
;; between polygons. Oresme drew motion as a figure. Kepler and Galileo
;; found the laws of the planets and of falling bodies. Fermat and Descartes
;; turned curves into equations. Newton and Leibniz made the calculus.
;; Euler made the function its object. Cauchy, Riemann, Weierstrass,
;; Dedekind and Cantor rebuilt it on limits and on the real numbers, and
;; Lebesgue rebuilt the integral on measure.
;;
;; Filled circles are notebooks you can open; open circles are planned.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns index
  {:nextjournal.clerk/visibility {:code :hide :result :show}
   ;; the hub reads timeline.edn and notebooks/ at build time; Clerk's
   ;; result cache does not see those files change, so never cache it
   :nextjournal.clerk/no-cache true}
  (:require [history-of-math.build :as build]
            [history-of-math.timeline :as timeline]
            [hive-dsl.result :as r]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:result :hide}}
(def series
  (let [t (timeline/timeline)
        nbs (build/discover build/notebooks-dir)]
    (when-not (r/ok? t) (throw (ex-info "timeline.edn is invalid" (:error t))))
    (when-not (r/ok? nbs) (throw (ex-info (build/error-message (:error nbs)) (:error nbs))))
    {:timeline (:ok t) :joined (timeline/join (:ok t) (:ok nbs))}))

(clerk/html (timeline/strip (:timeline series) (:joined series)))

;; ## The works
;;
;; Dates are those of the work's publication or, for the ancient authors,
;; of its composition; "c." marks an approximate date.

(clerk/html (timeline/table (:joined series)))

;; ## Free to read, copy and improve
;;
;; This is an open wiki for learning mathematics from its sources. The
;; prose, figures and notebooks are licensed under
;; [CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/). The code
;; (Alexandria, its players and kernels, and the build) is licensed under the
;; [GNU GPL, version 3 or later](https://www.gnu.org/licenses/gpl-3.0.html),
;; the licence of Emmy, which every page runs. Ancient and early-modern texts
;; are quoted from public-domain editions. A modern translation is quoted with
;; its translator and edition named beside the quote.
;;
;; The source of every page is at
;; [github.com/BuddhiLW/from-alexandria-to-analysis](https://github.com/BuddhiLW/from-alexandria-to-analysis).
;; To fix a reading, add a proposition or write a new era, open an issue or a
;; pull request there. A notebook is one file in `history-of-math/notebooks/`.
;; Its figures, proofs and graded checks go into `alexandria/`, and Emmy checks
;; every claim a page makes when the site is built.
