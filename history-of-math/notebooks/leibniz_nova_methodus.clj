;; # Leibniz, *Nova Methodus pro Maximis et Minimis* (1684)
;;
;; In October 1684 the Leipzig journal *Acta Eruditorum* printed six pages by
;; Gottfried Wilhelm Leibniz. The title promises "a new method for maxima and
;; minima, and for tangents, which is hindered neither by fractional nor by
;; irrational quantities, and a singular kind of calculus for them." It is the
;; first published account of the differential calculus.
;;
;; Leibniz was a lawyer and diplomat from Leipzig. He came to Paris in 1672 on
;; a diplomatic mission and stayed four years. Christiaan Huygens taught him
;; the geometry of the day: Descartes' algebra of curves (1637), Cavalieri's
;; indivisibles (1635), Pascal's treatise on the sines of a quarter circle
;; (1659). Within a year Leibniz had a method of his own. This page follows him
;; from that first discovery, in 1673, to the rules he printed in 1684.
;;
;; The previous notebooks of the series end with Kepler's ellipses and
;; Descartes' coordinates. The next one is Newton's *Principia*, printed three
;; years after this paper. Newton had his own calculus, the method of
;; fluxions, from 1665, and published it later. The dispute over who came first
;; is at the end of this page.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns leibniz-nova-methodus
  {:history/year 1684
   :history/title "Leibniz, Nova Methodus pro Maximis et Minimis"
   :history/era "Calculus"
   :nextjournal.clerk/toc true
   :nextjournal.clerk/visibility {:code :hide :result :show}}
  (:require [alexandria.leibniz.calculus-view :as view]
            [clojure.string :as str]
            [history-of-math.page :as page]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:result :hide}}
(defn badges
  "One pill per graded check: proved by Emmy, or checked numerically."
  [rows]
  (clerk/html
   (into [:div {:style {:display "flex" :flex-direction "column" :gap "0.3em"}}]
         (for [{:keys [label grade]} rows
               :let [tone (case grade :grade/proved "#2f8a3e" :grade/numeric "#b7791f" "#c0392b")]]
           [:div {:style {:display "inline-block" :padding "0.15em 0.8em" :border-radius "1em"
                          :font-family "ui-monospace, monospace" :font-size "0.85em"
                          :border (str "1px solid " tone) :color tone :width "fit-content"}}
            (str (name grade) " by Emmy: " label)]))))

^{::clerk/visibility {:result :hide}}
(defn quote-block [passages source]
  (clerk/md (str (str/join "\n>\n" (map #(str "> *" % "*") passages)) "\n\n— " source)))

^{::clerk/visibility {:result :hide}}
(defn graded-where [pred] (filter (comp pred :label) (view/graded)))

;; ## The characteristic triangle (1673)
;;
;; Take a curve. Mark a point $P$ on it, with abscissa $x$ (the distance along
;; the horizontal axis) and ordinate $y$ (the height). Move along the axis by a
;; small step, which Leibniz calls $dx$, "the difference of $x$". The height
;; changes by $dy$, and the point reaches $Q$.
;;
;; The little triangle with sides $dx$, $dy$ and the chord $PQ$ is what Leibniz
;; named the *characteristic triangle*. As $dx$ shrinks, the chord falls onto
;; the tangent at $P$, and the triangle keeps the shape of the triangle the
;; tangent cuts off. Pascal had used such a triangle for the circle only.
;; Leibniz saw that it works for every curve. Here is how he told it, forty
;; years later:

(let [{:keys [passages source]} (view/proposition :leibniz/transmutation)]
  (quote-block passages (str "Leibniz, *Historia et origo calculi differentialis*, tr. J. M. Child, *The Early Mathematical Manuscripts of Leibniz* (1920), " source)))

;; ## The transmutation theorem
;;
;; Cavalieri cut a figure into parallel strips. Leibniz cut it into thin
;; triangles $OPQ$ that all meet at the origin $O$. Let the tangent at $P$
;; meet the vertical axis at height
;;
;; $$z = y - x\,\frac{dy}{dx}.$$
;;
;; By similar triangles, the triangle $OPQ$ has area $\tfrac12 z\,dx$. So every
;; thin triangle is half of a strip of width $dx$ and height $z$. Add them up:
;; the area swept from $O$ is half the area under the curve of the $z$'s. One
;; figure has been *transmuted* into another of the same area. In the notation
;; he invented two years later this is
;;
;; $$\int y\,dx = \tfrac12\,[xy] + \tfrac12 \int z\,dx .$$
;;
;; For his circle of diameter 2, $y = \sqrt{2x - x^2}$, the new curve is
;; rational: $z^2 = x/(2-x)$, so $x = 2z^2/(1+z^2)$. Dividing as Nicholas
;; Mercator had divided for the hyperbola (1668),
;; $1/(1+z^2) = 1 - z^2 + z^4 - \dots$, and adding the pieces gives the series
;; below. Play the steps. The figure is computed in the browser by compiled
;; kernels of the same Emmy functions the checks use.

(view/transmutation)

(badges (graded-where #(or (str/includes? % "triangle") (str/includes? % "transmutation")
                           (str/includes? % "z^2") (str/includes? % "rational") (str/includes? % "1/(1+z^2)"))))

;; The badges say what Emmy verified. The triangle and transmutation
;; identities are proved for an arbitrary curve $y$, written as an Emmy literal
;; function. The circle's rational figure and Mercator's division (with its
;; exact remainder, for eight terms) are proved symbolically.

;; ## The arithmetical quadrature of the circle
;;
;; > *From which, by the help of ordinary geometry, it can be easily deduced
;; > that the square on the diameter is to the area of the circle as 1 is to
;; > 1/1 − 1/3 + 1/5 − 1/7 + etc.*
;;
;; — Leibniz, account written in London (1676), tr. Child (1920), pp. 189–190
;;
;; So $\pi/4 = 1 - \tfrac13 + \tfrac15 - \tfrac17 + \cdots$. James Gregory had
;; found the arctangent series in Scotland in 1671, and Madhava's school in
;; Kerala two centuries earlier, but neither was known in Paris. The partial
;; sums $S_n$ are exact fractions; they fall alternately above and below
;; $\pi/4$, and each misses it by less than the next term, $1/(2n+1)$.

(view/quadrature)

(clerk/table
 {:head ["n" "S_n (exact)" "|pi/4 - S_n|" "bound 1/(2n+1)"]
  :rows (for [{:keys [n sum error bound]} (view/series-table [1 2 3 4 5 10 50 100])]
          [n (if (> n 10) (format "%.10f" (double sum)) (str sum)) (format "%.2e" error) (str bound)])})

(badges (graded-where #(str/includes? % "pi/4")))

;; These two checks are *numeric*. $\pi$ has no closed form to compare an exact
;; fraction with, so Emmy checks the error against the remainder integral
;; $\int_0^1 z^{2n}/(1+z^2)\,dz$ and against Leibniz's bound. The convergence
;; is slow: a hundred terms give two decimals.

;; ## *Nova Methodus*: the rules
;;
;; In 1675 Leibniz wrote $\int$ (a long *s*, for *summa*) and $d$ (for
;; *differentia*). Nine years later he published the rules for $d$. He first
;; defines $dx$ as a line "taken at will", and $dv$ as the line that is to
;; $dx$ as the ordinate $v$ is to the subtangent. Then the rules follow, in one
;; paragraph. Each is shown below in Leibniz's Latin; the English in brackets
;; is the series' own rendering.

(let [{:keys [title-latin title-english steps]} (view/proposition :leibniz/nova-methodus)]
  (clerk/md (str "> *" title-latin "*\n>\n> " title-english "\n\n"
                 (str/join "\n\n" (map-indexed (fn [i {:keys [claim why]}] (str (inc i) ". " claim " *(" why ")*")) steps)))))

(view/nova-methodus)

;; ## One value, two notations
;;
;; Each rule below is one *frozen formula*: an Emmy expression with its
;; passage attached. Emmy proves the formula. Then the same value is
;; typeset three ways. *Modern* uses $\mathrm{d}(\ldots)$. *Leibniz* writes
;; $dx$ and puts a bar over a compound quantity, $d\overline{xv}$. *Newton*
;; writes the fluxion of $x$ as $\dot x$, a "pricked letter"; he printed this
;; notation in 1704, in the *Quadrature of Curves*. In *Principia* (Book II,
;; Lemma II) he states the same product rule in words: "the moment of the
;; generated rectangle AB will be aB + bA".

(clerk/html
 (into [:table {:style {:border-collapse "collapse" :width "100%"}}
        [:tr (for [h ["rule" "Leibniz (1684)" "Newton's fluxions" "graded"]]
               [:th {:style {:text-align "left" :padding "0.3em" :border-bottom "1px solid #888"}} h])]]
       (for [{:keys [name leibniz newton grade]} (view/rules)]
         [:tr
          [:td {:style {:padding "0.3em"}} (str name)]
          [:td {:style {:padding "0.3em"}} (clerk/tex leibniz)]
          [:td {:style {:padding "0.3em"}} (clerk/tex newton)]
          [:td {:style {:padding "0.3em" :font-family "monospace" :color (if (= grade :grade/proved) "#2f8a3e" "#c0392b")}}
           (clojure.core/name grade)]])))

(badges (graded-where #(str/starts-with? % "d rule")))

;; The priority dispute is visible in this table. The mathematics in the two
;; columns is one value. Newton had it first, in manuscripts of 1665–1666 and
;; in *De analysi* (1669), circulated in London but not printed until 1711.
;; Leibniz found it independently in Paris in 1675 and printed it first, here.
;; In 1712 a committee of the Royal Society, whose president was Newton,
;; declared Newton the first inventor and Leibniz a plagiarist; Newton wrote
;; its report himself. Mathematicians on the Continent kept Leibniz's $d$ and
;; $\int$. With them the Bernoullis and Euler built the analysis of the
;; eighteenth century. English mathematicians kept the dots, and fell behind
;; until Cambridge adopted $d$ in the 1810s.
;;
;; The paper's last rule, $dv = 0$ at a maximum, is the "method for maxima and
;; minima" of the title. The next notebook, Newton's *Principia*, uses the
;; same ideas on the motions of the planets, though Newton wrote its proofs
;; in the language of Greek geometry.

^{::clerk/no-cache true}
(page/prev-next "notebooks/leibniz_nova_methodus.clj")
