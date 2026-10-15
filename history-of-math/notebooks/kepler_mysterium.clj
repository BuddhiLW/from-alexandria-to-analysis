;; # Kepler, *Mysterium Cosmographicum*: five solids between six spheres
;;
;; Johannes Kepler was twenty-four and taught mathematics at the Protestant
;; school in Graz. He had learned the Copernican system at Tübingen from his
;; teacher Michael Maestlin. Copernicus, in *De revolutionibus* (1543), had put
;; the Sun at the centre and fixed, for the first time, the relative sizes of
;; the planetary orbits. Ptolemy's system could not do that: in it each
;; planet's distance was free. Kepler asked the question that the new system
;; made possible: *why six planets, and why these distances?*
;;
;; His answer is the book he published at Tübingen in 1596, the
;; *Mysterium Cosmographicum*, the cosmographic mystery. Between six spheres
;; there are five gaps, and Euclid had proved that there are exactly five
;; regular solids. Kepler fitted one solid into each gap.
;;
;; This page follows J. L. E. Dreyer's account, *History of the Planetary
;; Systems from Thales to Kepler* (Cambridge, 1906, ch. XV), which reports
;; Kepler's order and his own comparison table. The solids, the ratios and the
;; checks come from Alexandria's shelf (`alexandria.solids`,
;; `alexandria.kepler.mysterium`).
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns kepler-mysterium
  {:history/year 1596
   :history/title "Kepler, Mysterium Cosmographicum"
   :history/era "Scientific Revolution"
   :history/fields #{:mechanics-physics :geometry}
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.kepler.mysterium :as my]
            [alexandria.kepler.mysterium-view :as view]
            [alexandria.kepler.mysterium-widget :as nest]
            [alexandria.kepler.mysterium-playground :as nest-play]
            [alexandria.solids :as solids]
            [clojure.string :as str]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:code :hide :result :hide}}
(hom/install!)

;; ## Where the idea came from
;;
;; Kepler tells the story in his preface. He was drawing, for a class, the
;; great conjunctions of Jupiter and Saturn on the zodiac:

^{::clerk/visibility {:result :hide}}
(def passages (:passages (view/proposition)))

^{::clerk/visibility {:result :hide}}
(defn quote-md [{:keys [text source]}]
  (clerk/md (str "> " text "\n>\n> — " source)))

(quote-md (first passages))

;; A triangle drawn inside a circle has an inscribed circle half as large.
;; Kepler saw in it nearly the ratio of the orbits of Saturn and Jupiter.
;; Plane figures could not finish the job: there are infinitely many regular
;; polygons, so they cannot fix the *number* of planets. Solids can. There are
;; only five, and Plato had already given them a cosmic role:

(quote-md (second passages))

;; ## The five regular solids
;;
;; A *regular solid* has faces that are all the same regular polygon, with
;; the same number meeting at each corner. Euclid ends the *Elements* (Book
;; XIII, Proposition 18) by building all five inside one sphere and proving
;; that no sixth exists. Alexandria keeps them as data: vertices exact in Emmy
;; (with $\varphi = (1+\sqrt5)/2$, the golden ratio), faces and edges derived
;; from the vertices.

(clerk/table
 {:head ["solid" "vertices V" "edges E" "faces F" "face"]
  :rows (for [id solids/solid-ids
              :let [s (solids/solids id) {:keys [V E F]} (solids/counts s)]]
          [(solids/names id) V E F (case (count (first (:faces s))) 3 "triangle" 4 "square" 5 "pentagon")])})

;; ## The nest
;;
;; Every regular solid has two spheres about its centre: the *circumsphere*
;; through its corners, radius $R$, and the *insphere* touching its faces,
;; radius $r$. Kepler's rule: each planet's sphere is the insphere of the
;; solid outside it and the circumsphere of the solid inside it. From the
;; outside in:
;;
;; Saturn | cube | Jupiter | tetrahedron | Mars | dodecahedron | Earth |
;; icosahedron | Venus | octahedron | Mercury.
;;
;; So the ratio of two neighbouring orbits is fixed by geometry alone: it is
;; $r/R$ of the solid between them.
;;
;; Press ▶ to watch the nest built one shell at a time, the whole figure
;; turning in space and drawn in orthographic projection.

(view/player)

;; Now try it yourself: reorder the five solids, scale the nest, and see
;; which order fits Copernicus' distances best.

(nest-play/playground)

;; ## The nest in space, every radius tied
;;
;; Kepler wrote the rule down the moment he found it, in Latin, in the
;; preface. Starting from the Earth:
;;
;; > *Terra est Circulus mensor omnium. Illi circumscribe Dodecaedron:
;; > Circulus hoc comprehendens erit Mars.*
;; > The Earth is the circle that measures all. Circumscribe a dodecahedron
;; > about it: the circle containing that will be Mars.
;; >
;; > — Kepler, *Mysterium Cosmographicum* (1596), preface
;;
;; Below, the six spheres and the five solids in MathBox. Every radius is a
;; slider, and the sliders are tied. The geometry fixes each ratio
;; $r_{\text{inner}} / r_{\text{outer}}$, so moving one radius solves Saturn's
;; back and carries the other five with it. The *shell* slider picks one
;; solid. That solid is drawn bold and named, and its relation is shown with
;; today's numbers. Copernicus' value is beside it, together with the miss.
;; The orange rings are Copernicus' radii, set beside Kepler's spheres as
;; Kepler set them in his table.

(nest/widget)

;; Emmy writes each radius as Saturn's radius times the chain of ratios
;; outside it. It grades the tie both ways for every planet. Solving
;; Saturn's radius back from a planet's and going forward again gives the
;; identity:

(nest/derivation)


;; ## The ratios, exactly
;;
;; Emmy computes each $r/R$ from the vertices and proves the closed forms:
;;
;; - tetrahedron: $r/R = 1/3$;
;; - cube and octahedron: $r/R = 1/\sqrt3$;
;; - dodecahedron and icosahedron, a dual pair with the same ratio:
;;   $(r/R)^2 = \dfrac{\varphi^4}{3(\varphi^2+1)} = \dfrac{5+2\sqrt5}{15}$,
;;   so $r/R \approx 0.7947$.
;;
;; The badges show each identity reduced by Emmy's simplifier to exactly 0
;; (*proved*), and Kepler's printed numbers checked against the exact ratios
;; to the unit he wrote (*numeric*).

(page/grade-badges (my/graded) #{:grade/proved :grade/numeric})

;; ## Kepler's table against Copernicus
;;
;; Kepler computed the radius of each inner sphere with the outer one set to
;; 1000, and set it beside the value from Copernicus' distances and
;; eccentricities (Dreyer, p. 375):

(clerk/table
 {:head ["outer | inner" "solid" "exact r/R × 1000" "Kepler" "Copernicus" "miss"]
  :rows (for [{:keys [outer inner solid value computed copernicus miss]} (my/comparison)]
          [(str (str/capitalize (name outer)) " | " (str/capitalize (name inner)))
           (solids/names solid) (format "%.1f" value) computed copernicus
           (format "%+.1f%%" (* 100 miss))])})

;; Read the last column plainly. The tetrahedron (Jupiter–Mars) and the
;; icosahedron (Earth–Venus) fit to within one part in a thousand. The
;; dodecahedron (Mars–Earth) is 5% wide. The cube puts Jupiter 9% too close to
;; Saturn. The octahedron misses Mercury by a fifth.
;;
;; Kepler rescued Mercury by changing the rule for that shell alone. Instead of
;; the sphere touching the octahedron's faces he took the circle inscribed in
;; the square formed by its four middle edges. That is the *midsphere*,
;; through the midpoints of the edges. Emmy proves
;; $(r_{\text{mid}}/R)^2 = 1/2$, so it gives 707 against Copernicus' 723,
;; within 3%. For Jupiter he blamed the distance:

(quote-md (nth passages 2))

;; ## The argument, step by step

(page/steps (:steps (view/proposition)))

;; ## What came after
;;
;; Tycho Brahe answered Kepler's book: the idea was interesting, but only his
;; own thirty years of observations could test it. In 1600 Kepler went to
;; Tycho in Prague to sharpen the distances. Tycho gave him the orbit of Mars.
;; That work became *Astronomia Nova* (1609) and the first two laws of
;; planetary motion. In *Harmonices Mundi* (1619) came the third law, and
;; Kepler set the solids beside it once more. The solids did not survive. The
;; laws found on the way did, and Newton built the *Principia* on them.
;;
;; The vertex, edge and face counts in the table above are the data of
;; Euler's later observation about polyhedra, $V - E + F = 2$, told on its own
;; page.

^{::clerk/no-cache true}
(page/prev-next "notebooks/kepler_mysterium.clj")
