;; # Closing: why the Greeks valued geometry, and what came after
;;
;; The Greek notebooks of this series end with three questions that a reader
;; asked on seeing them. Why did the Greeks value geometry so much? How did
;; they measure, add and compare without algebra? And could they have gone
;; on to algebra, to the calculus, and to an industrial revolution? This page
;; answers each in turn and links every claim to the notebook where the
;; series follows it in the sources.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns greek-closing
  {:history/year 1920
   :history/title "Closing: why the Greeks valued geometry"
   :history/era "Closing"
   :history/fields #{:geometry :algebra :analysis}
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.apollonius.areas-steps-view :as areas-steps]
            [alexandria.archimedes.quadrature-view :as quadrature-view]
            [alexandria.euclid.ties-view :as euclid-ties]
            [alexandria.eudoxus.widgets :as eudoxus]
            [history-of-math.build :as build]
            [history-of-math.links :as links]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:result :hide}}
(hom/install!)

;; ## 1. Why geometry? The incentives
;;
;; Seven reasons run through the sources, and each one strengthens the
;; others.
;;
;; **1. A practical origin, then a Greek turn to proof.** Herodotus tells
;; that the Egyptian king divided the land into equal plots and taxed them,
;; and that when the Nile carried part of a plot away, surveyors measured
;; what remained; "from this, to my thinking, the Greeks learnt the art of
;; measuring land" (*Histories* II.109). Babylonian tablets give recipes
;; for areas and for the diagonal of a square. The Greeks, from Thales on,
;; asked a new question of these recipes: *why* they hold. Geometry became
;; the art of proof, which the [Euclid](../euclid-elements/) notebook
;; follows from the postulates to I.47.
;;
;; **2. Proof wins arguments.** G. E. R. Lloyd (*Magic, Reason and
;; Experience*, 1979; *Demystifying Mentalities*, 1990) ties the Greek
;; demand for demonstration to the city's habits of debate. Citizens argued
;; in assemblies and law courts, sophists sold the skill of persuasion, and
;; rival schools attacked each other's first principles. In that market an
;; argument nobody could answer was the strongest one, and a deduction from
;; agreed premises was exactly such an argument.
;;
;; **3. A path to the eternal.** The Pythagoreans held that number orders
;; the cosmos. Plato's *Republic* VII sets geometry in the education of the
;; rulers because it draws the soul towards truth: it is knowledge "of what
;; always is, not of what comes into being and passes away" (527b). Late
;; writers such as Philoponus and Tzetzes report the motto over the
;; Academy's door, "let no one ignorant of geometry enter". The report is
;; late, and it shows what later readers believed the Academy stood for.
;;
;; **4. Numbers had failed.** The side and the diagonal of a square have no
;; common measure, so no ratio of whole numbers gives their ratio. After
;; this discovery number could not carry every magnitude, and geometry,
;; whose lines need no unit, became the rigorous ground for continuous
;; quantity. The [Eudoxus](../eudoxus/) notebook gives the parity proof and
;; the theory of ratio built to repair the loss.
;;
;; **5. Patronage and prestige.** The Ptolemies founded the Museum and
;; Library of Alexandria and paid scholars to work there; Euclid taught in
;; it, and Eratosthenes ran the Library. Mathematicians competed by letter.
;; Archimedes sent his results to Conon, Dositheus and Eratosthenes, and in
;; the preface to *On Spirals* he says he once sent two false theorems on
;; purpose, so that those who claimed to have found everything would be
;; caught claiming the impossible. The [Archimedes](../archimedes/)
;; notebook reads those letters.
;;
;; **6. Uses.** Geometry served astronomy and the calendar, navigation and
;; maps, architecture, optics and mechanics, and also war. Philo of
;; Byzantium's rule for a stone-thrower (*Belopoeica* 51) sets the calibre
;; to the cube side of the shot's weight plus a tenth, so a shot $k$ times
;; heavier needs a calibre $\sqrt[3]{k}$ times wider. Building that engine
;; is the Delian problem, and Eratosthenes' mesolabe was one instrument for
;; it; see [the doubling of the cube](../doubling-the-cube/).
;;
;; **7. Leisure and the contempt for craft.** Aristotle says the
;; mathematical arts arose in Egypt "because the priestly caste was allowed
;; leisure" (*Metaphysics* A.1, 981b). *Schole*, leisure, belonged to free
;; men, and *banausic* manual work ranked below it. Plutarch writes that
;; Archimedes thought mechanics ignoble and left no treatise on his
;; machines (*Marcellus* 17). Theory therefore outranked application, and
;; that ranking is part of the answer in section 3 to why the applications
;; did not grow into an industry.

;; ## 2. How they worked without algebra
;;
;; ### Two worlds: number and magnitude
;;
;; A Greek *arithmos* was a count of units, a whole number. It was written
;; with letters of the alphabet, worked on a pebble board, and its parts were
;; unit fractions or Babylonian sixtieths. A *megethos* was a magnitude: a
;; line, an area, a volume. Because the side and the diagonal have no common
;; measure, magnitudes could not be reduced to numbers, so the two worlds
;; stayed apart and the rigorous work was done on magnitudes.
;;
;; ### Geometric algebra
;;
;; With no letters for unknowns, the Greeks calculated with figures. Each
;; step below is one sentence, in the order a reader of *Elements* II and VI
;; meets it.

(page/steps
 [["A product $a b$ is the rectangle *contained by* the lines $a$ and $b$." "II, Def. 1"]
  ["A square $a^2$ is the square *on* the line $a$." "I, Def. 22"]
  ["Three factors make a solid, and a fourth factor has no figure, so no term may pass the third degree." "XI"]
  ["A sum is two lines laid end to end, or two figures joined." "Common notion 2"]
  ["An identity is a cut figure: II.4 cuts the square on $a+b$ into $a^2$, $b^2$ and two rectangles $ab$." "II.4"]
  ["An equation is a figure to build: a rectangle of given area applied to a given line." "I.44"]
  ["A quadratic is that rectangle made to fall short or run past by a square, and its side is the root." "VI.28-29"]])

;; The figure below is the one the [Euclid](../euclid-elements/) and
;; [Apollonius](../apollonius-conics/) notebooks share. Step through it one
;; sentence at a time, from I.44 through VI.27-29 to the three conics.

(areas-steps/figure)

;; Every quantity of the application is a control: move the area $S$ and the
;; height $y$ is solved back, as a quadratic would be.

(euclid-ties/application)

;; ### Ratio and proportion
;;
;; Without division of magnitudes, the Greeks compared them. Euclid V
;; Def. 3 says that "a ratio is a sort of relation in respect of size between
;; two magnitudes of the same kind" (Heath 1908, vol. 2, p. 114). Def. 5,
;; which is Eudoxus', says when two ratios are the same: $A : B = C : D$
;; when, for all whole numbers $m$ and $n$, $mA$ exceeds, equals or falls
;; short of $nB$ exactly as $mC$ does of $nD$. Move the bars and the
;; equimultiples below, and the two comparisons always come out alike.

(eudoxus/def-5)

;; Definition 5 sorts every fraction $n/m$ into those below the ratio and
;; those above it. In 1872 Dedekind took that split of the fractions, the
;; *cut*, as the definition of a real number and named Book V as his source:
;; see [Dedekind and Cantor](../dedekind-cantor/).
;;
;; ### Sums
;;
;; A sum of many terms was a figure built piece by piece. *Elements* IX.35
;; sums a geometric progression by comparing differences of its terms.
;; Archimedes' *Quadrature of the Parabola* adds triangles, each stage a
;; quarter of the last, and shows the segment is $\tfrac43$ of the first
;; triangle; he never speaks of an infinite sum, and he rules out every
;; other value by exhaustion. *On Spirals* 10 sums the squares
;; $1^2 + 2^2 + \dots + n^2$ with rectangles of lines. Here is the
;; quadrature, stage by stage:

(quadrature-view/player :archimedes/parabola-24)

;; ### Diophantus' syncopated algebra
;;
;; Around AD 250 Diophantus of Alexandria wrote the *Arithmetica* with an
;; abbreviation for the unknown, signs for its powers up to the sixth, and a
;; sign for subtraction. Nesselmann (1842) called this stage *syncopated*:
;; between the rhetorical algebra of words and the symbolic algebra of
;; Viète. Diophantus worked on numbers, not on magnitudes, and he looked for
;; one rational answer to each problem, so his notation stayed outside the
;; geometry of the schools.

;; ## 3. Could they have gone further?
;;
;; Algebra was close, the calculus was very close, and an industrial
;; revolution was the least likely of the three. The line of descent below
;; runs from Archimedes to Newton. Each step names what it added, and each
;; step the series has written is a link to its notebook.

^{::clerk/visibility {:result :hide}}
(def descent
  "The line from Greek geometry to the calculus: [year label what notebook-id]."
  [[-250 "Archimedes, The Method" "Weighs slices of a figure against slices of a known one: integration in all but name. The letter was lost in a palimpsest until Heiberg read it in 1906, so it had no influence." "archimedes"]
   [250 "Diophantus, Arithmetica" "Writes the unknown and its powers with abbreviations: syncopated algebra on numbers." nil]
   [628 "Brahmagupta and the Indian numerals" "Place-value numerals with a zero, and rules for computing with zero and negatives." nil]
   [820 "al-Khwarizmi, al-jabr wa'l-muqabala" "Solves every quadratic by rules, and proves each rule with a completed square." "algebra-cubic"]
   [1545 "Cardano, Ars magna" "Solves the cubic, still with geometric proofs of cubes." "algebra-cubic"]
   [1591 "Viète, In artem analyticem isagoge" "Letters for the known quantities as well as the unknown: an equation becomes a general form." nil]
   [1637 "Descartes, La Géométrie" "A unit segment makes the product of two lines a line, which ends the dimension barrier, and every curve gets an equation." "descartes-fermat"]
   [1684 "Leibniz, Nova methodus" "The differential and its rules, in a notation that calculates by itself." "leibniz-nova-methodus"]
   [1687 "Newton, Principia" "Fluxions and first and last ratios, set to work on the system of the world." "newton-principia"]])

^{::clerk/visibility {:result :hide}}
(defn descent-timeline
  "Hiccup of the line of descent: one row per step, a dot on a vertical
   line, the work as a link when its notebook is built."
  [steps built]
  (into [:div {:style {:border-left "2px solid #8886" :margin "1em 0 1em 0.6em" :padding-left "1.2em"}}]
        (for [[year label what id] steps
              :let [link? (contains? built id)]]
          [:div {:data-descent (or id "planned") :style {:position "relative" :margin "0 0 1em"}}
           [:span {:style {:position "absolute" :left "-1.75em" :top "0.35em" :width "0.8em" :height "0.8em"
                           :border-radius "50%" :border "2px solid #1f3a5f"
                           :background (if link? "#1f3a5f" "transparent")}}]
           [:div [:strong (if (neg? year) (str "c. " (- year) " BC") (str year))] " "
            (if link? [:a (links/anchor (links/notebook-href :notebook id)) [:em label]] [:em label])]
           [:div {:style {:font-size "0.9em"}} what]])))

^{::clerk/no-cache true}
(clerk/html (descent-timeline descent
                              (set (map :id (:ok (build/discover build/notebooks-dir))))))

;; Filled dots are notebooks you can open; open dots are steps the series
;; has not written yet. Two things were missing from Greek geometry: a
;; number system that holds every magnitude, and symbols for a general
;; quantity. Place-value numerals and Viète's letters supplied them, and
;; Descartes' unit segment joined number and magnitude again. The calculus
;; then needed the function and a notation for the infinitely small, which
;; Fermat, Descartes, Newton and Leibniz supplied between 1630 and 1690.
;;
;; ### Why no industrial revolution
;;
;; Alexandria had steam and gears. Hero describes the aeolipile, a sphere
;; spun by jets of steam, and Ctesibius built force pumps and a water
;; organ; the Antikythera mechanism, from the first century BC, computes
;; the motions of the sun and moon with bronze gear trains. Nobody turned
;; these into engines that work, for reasons that reinforce each other.
;; Slavery made labour cheap, so a machine that saved labour saved little
;; money. There was no cheap coal or iron near the cities, and no
;; precision machining to bore a cylinder that holds pressure. There were
;; no patents to reward an inventor and no capital markets to fund one.
;; The contempt for banausic work kept the best minds on theory, and under
;; Rome the tradition of the Museum thinned and then broke. Britain in 1760
;; had what Alexandria lacked: dear labour, cheap coal, iron, machinists,
;; patents and banks, and a geometry that had become analysis.

^{::clerk/no-cache true}
(page/prev-next "notebooks/greek_closing.clj")
