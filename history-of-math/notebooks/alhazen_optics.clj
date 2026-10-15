;; # Ibn al-Haytham, the Optics: sight by rays, the equal angles, Alhazen's problem
;;
;; Abu Ali al-Hasan ibn al-Haytham (c. 965 to c. 1040), called Alhazen in
;; Latin, wrote the *Kitab al-Manazir*, the Book of Optics, in Cairo between
;; about 1011 and 1021. Its seven books treat light, the eye and vision
;; (I to III), reflection (IV to VI) and refraction (VII). A Latin version,
;; *De aspectibus*, circulated from about 1200; Friedrich Risner printed it
;; at Basel in 1572 with the *Perspectiva* of Witelo as the *Opticae
;; thesaurus*. That printed Latin is what Kepler, Barrow and their readers
;; quote.
;;
;; The Latin is quoted from Risner 1572 (pages are those of the e-rara scan,
;; with the printed page where it differs) and from J. L. Heiberg and
;; E. Wiedemann, *Ibn al Haitams Schrift über parabolische Hohlspiegel*,
;; Bibliotheca mathematica, 3. Folge, 10 (1910), pp. 201-237. The English
;; beside the Latin and German is this series' own translation. Spelling is
;; modernised where the print uses the long s, u for v and contractions.
;;
;; The figures and checks come from Alexandria's shelf
;; `alexandria.alhazen.mirror`: *proved* means Emmy simplified a difference
;; to exactly 0, *numeric* that numbers from raster were within the bound.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns alhazen-optics
  {:history/year 1016
   :history/title "Ibn al-Haytham (Alhazen), Kitab al-Manazir (c. 1011-1021), in Risner's Latin (1572)"
   :history/era "Medieval"
   :history/fields #{:mechanics-physics :geometry :analysis}
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.alhazen.mirror :as mirror]
            [alexandria.alhazen.widgets :as w]
            [history-of-math.lens :as lens]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:code :hide :result :hide}}
(hom/install!)

;; ## 1. Sight by rays
;;
;; Book I settles what earlier optics had left open: whether the eye sends
;; something out or receives something. Ibn al-Haytham argues from the
;; experience of looking at the Sun and at bright light, which hurts the eye,
;; that light and colour come to the eye from the thing seen. From every
;; point of a visible surface a form goes out in every direction; the eye
;; receives all of them on its surface, and still sees each point once,
;; because it senses only the rays that fall on it perpendicularly, the
;; ones aimed at its centre.

(page/blockquote
 "Visus ergo nihil potest comprehendere ex formis rerum visarum, nisi ex verticationibus linearum rectarum, quarum extremitates concurrunt apud hoc centrum tantum."
 "Alhazen, *Opticae thesaurus* I, cap. 5, Risner 1572, p. 27. Translation: *So sight can grasp nothing of the forms of things seen except along the straight lines whose ends meet at this centre alone.*")

;; The lines from the points of the object to the centre of the eye make
;; the visual pyramid: the cone of Euclid's optics, now run in the other
;; direction. Each point of the object is seen along one line, and that
;; line is what the later books reflect and refract.

;; ## 2. The law of equal angles
;;
;; Book IV is on reflection from polished bodies. Its law: the ray comes
;; back from the mirror on a line of the same position as the line it came
;; in on, so that the angle of incidence equals the angle of reflection.
;; Ibn al-Haytham argues it from motion: light moves very fast, is not
;; received by the polished body, and rebounds as a heavy ball dropped on a
;; polished surface rebounds along the perpendicular it fell on.

(page/blockquote
 "Radii incidentiae et reflexionis situs similitudine conveniunt, itaque anguli incidentiae et reflexionis aequantur. ... Quare autem fiat reflexio lucis secundum lineam eiusdem situs cum linea per quam accedit ad speculum, causa ipsa lux est: quoniam lux motu citissimo movetur, et quando cadit in speculum non recipitur, sed eius fixio in corpore illo negatur, et cum in ea perseveret adhuc prioris motus et naturae, reflectitur ad partem a qua processit, et secundum lineas eundem situm cum prioribus habentes."
 "Alhazen, *Opticae thesaurus* IV.10 (the margin cites Witelo V.20), Risner 1572, p. 125. Translation: *The rays of incidence and of reflection agree in likeness of position, and so the angles of incidence and reflection are equal. ... The reason the light is reflected along a line of the same position as the line by which it comes to the mirror is the light itself: light moves with the swiftest motion, and when it falls on the mirror it is not received, its lodging in that body is refused; and since it still keeps its earlier motion and nature, it is turned back toward the side it came from, along lines having the same position as the earlier ones.*")

;; In vectors: a ray along d meets a mirror whose unit normal is n and
;; leaves along d − 2(d·n)n. Emmy checks that this keeps the length and
;; that the outgoing ray makes with the normal the angle the incoming one
;; makes, on the other side; those are the first two grades of section 3.

;; ## 3. Alhazen's problem on a spherical mirror
;;
;; Book V asks where the images in mirrors are, and for that it needs a
;; harder question: given a spherical mirror, an eye and a visible point,
;; find the point of the mirror from which the point is reflected to the
;; eye. Everything lies in the plane through the centre, the eye and the
;; point, so the question is on a circle. When eye and point are equally
;; far from the centre, the answer is on the bisector of the angle they
;; make at the centre (V.31):

(page/blockquote
 "Amplius: dato speculo, et dato puncto viso: est invenire punctum reflexionis. ... Si fuerint duae illae lineae aequales: erit facile invenire."
 "Alhazen V.31, Risner 1572, p. 155 (printed 142). Translation: *Further: given the mirror and a point seen, to find the point of reflection. ... If the two lines [from the eye and from the point to the centre] are equal, it is easy to find.*")

;; When they are not, he first proves the lemmas V.32 to V.38,
;; constructions on a circle and on a right triangle, and then solves the
;; general case in V.39:

(page/blockquote
 "Visu et visibili a centro speculi sphaerici convexi inaequaliter distantibus, punctum reflexionis invenire. ... Redictis habitis, dato speculo sphaerico: erit invenire punctum reflexionis. Verbi gratia: sit a centrum visus, b punctum visum, g centrum sphaerae, et ducantur lineae a g, b g, et sumatur superficies in qua sunt hae duae lineae, et sumatur circulus communis huic superficiei et speculo. Invenietur ergo punctum reflexionis in hoc circulo."
 "Alhazen V.39 (Witelo VI.22 in the margin), Risner 1572, p. 163 (printed 150). Translation: *The eye and the visible thing unequally distant from the centre of a convex spherical mirror: to find the point of reflection. ... With what has been said, given a spherical mirror, the point of reflection can be found. For example, let a be the centre of the eye, b the point seen, g the centre of the sphere; draw the lines ag and bg, take the plane in which these two lines lie, and take the circle common to this plane and the mirror. The point of reflection will be found on this circle.*")

;; The widget asks the same question of a number. With the mirror the unit
;; circle, the eye at A = (a, 0), the point at B = (b cos α, b sin α) and P
;; = (cos φ, sin φ), the law says that the normal OP bisects the angle APB:
;;
;; $$g(\varphi) = \frac{P \times (A - P)}{|A - P|} + \frac{P \times (B - P)}{|B - P|} = 0.$$
;;
;; Move a, b or α: φ is solved by bisection on the raster kernel of g, on
;; the arc that both A and B see. Move P: the distance b is solved back
;; along the ray at α by reflecting the line PA in the normal.

(w/problem)

(page/grade-badges (mirror/graded) #{:grade/proved :grade/numeric})

;; ## 4. The parabolic burning mirror
;;
;; A short treatise of Ibn al-Haytham, *On parabolic burning mirrors*,
;; survives in Arabic and in a medieval Latin version. J. L. Heiberg edited
;; the Latin and E. Wiedemann translated it into German in 1910. They note
;; (p. 201) that later writers credit Archimedes with a parabolic mirror,
;; and that by the introduction of Ibn al-Haytham's treatise this does not
;; seem certain. The proof rests on the law of section 2: every ray
;; parallel to the axis meets the surface at equal angles with the tangent
;; and goes to one point.

(page/blockquote
 "dico ergo, quod omnes ipsi conuertuntur ad punctum unum. et quoniam superficies AGEZ est superficies concaua concauitate corporis mukefi, tunc linee equidistantes axi, cum perueniunt ad illam et conuertuntur ad punctum H, continent cum lineis rectis, que producuntur in superficiebus earum contingentes superficiem concauam angulos equales."
 "Ibn al-Haytham, *De speculis comburentibus* (medieval Latin), ed. Heiberg and Wiedemann, Bibl. math. 3. F. 10 (1910), p. 227. Translation: *I say then that all of them are turned to one point. And since the surface AGEZ is a concave surface with the concavity of a paraboloid, the lines parallel to the axis, when they reach it and are turned to the point H, make equal angles with the straight lines drawn in their planes tangent to the concave surface.*")

(page/blockquote
 "Ziehen wir die Achse (den Pfeil) irgendeiner Parabel und tragen wir von dem Scheitel a ein Viertel der senkrechten Seite (des Parameters) = ae ab; wird ferner eine Linie innerhalb der Parabel parallel zur Achse gezogen, trifft sie auf die Parabel in Punkt b und wird sie zu dem Punkt e reflektiert, so bilden die beiden Linien mit der Tangente an den Schnitt an dem [Reflexions]punkt zwei gleiche Winkel."
 "Heiberg and Wiedemann 1910, p. 208, the German of the first proposition (the OCR reads the point as \"iL\"; the worked example below it, \"ae = ¼L ... verbindet b und e\", names it e). Translation: *Let us draw the axis (the arrow) of any parabola and lay off from the vertex a a quarter of the upright side (the parameter), ae; if further a line is drawn inside the parabola parallel to the axis, meets the parabola at the point b and is reflected to the point e, then the two lines make two equal angles with the tangent to the section at the point [of reflection].* The point a quarter of the parameter from the vertex is the one called the focus since Kepler.")



;; In coordinates the mirror is y² = 4fx with focus (f, 0). A ray at
;; height h meets it at (h²/4f, h), is reflected in the normal and crosses
;; the axis at x = f for every h: Emmy simplifies the gap to 0 (the grade
;; "parabolic mirror" in section 3), and the widget draws each ray from
;; its own raster kernel call.

(w/parabola)

;; The treatise then shows how to make such a mirror, beginning with a
;; plate of good steel cut along a parabola (p. 214). Suter's list of his
;; works (1900, art. 204) names a second treatise, *On the measurement of
;; the paraboloid*; its text is listed among the open parts below.

;; ## 5. Who read him
;;
;; Risner's Latin of 1572 is where the seventeenth century read him. Two
;; authors of new optics name him and quote him by book and number.
;; Kepler, in the *Ad Vitellionem paralipomena* (Frankfurt 1604), names
;; him in 14 passages on 11 pages of our scan, most of them on the place of
;; the image and on refraction:

(page/blockquote
 "Iamdudum Alhazen Arabs, et ex eo Vitellio refractionum materiam diligentius, quam consuevere Veteres, explicare sunt aggressi."
 "Kepler, *Ad Vitellionem paralipomena* (1604), cap. IV, pdf p. 98. Translation: *Long ago Alhazen the Arab, and Witelo from him, set out to explain the matter of refractions more carefully than the ancients used to.*")

(page/blockquote
 "Alhazenus initio prolixe ab experientia probat, locum imaginis semper esse in perpendiculari [ad] superficiem speculi."
 "Kepler 1604, cap. III, pdf p. 78. Translation: *Alhazen at the beginning proves at length from experience that the place of the image is always on the perpendicular to the surface of the mirror.*")

;; Isaac Barrow, in the *Lectiones opticae* (London 1674), names him four
;; times. In Lecture IX he gives his own solution of the reflection-point
;; problem of section 3 and says whose it is:

(page/blockquote
 "... quam olim Alhazenus Arabs scriptis commendavit; ab horribili tamen illa prolixitate simul ac obscuritate, neque non ab incondita sermonis barbarie nonnihil repurgatam."
 "Barrow, *Lectiones opticae* (1674), Lect. IX, p. 91. Translation: *... which Alhazen the Arab once committed to writing; but somewhat cleansed of that dreadful length and obscurity, and of the uncouth barbarity of his language.*")

;; Two other readers use the subject without the name. Descartes' *La
;; Dioptrique* (1637) names no predecessor in optics: of the image in
;; concave and convex mirrors it says only how much *les anciens se sont
;; abusés en leur Catoptrique* (Discours VI, pdf p. 150), the question on which
;; Kepler and Barrow name Alhazen. Huygens' *Traité de la lumière* (1690)
;; does not name him either.
;;
;; **Open parts.** Not in this notebook because the texts are not in hand:
;; the volume of the paraboloid (H. Suter, *Die Abhandlung über die
;; Ausmessung des Paraboloides*, Bibl. math. 3. F. 12, 1912, not obtained),
;; and Huygens' own solution of Alhazen's problem, in his correspondence
;; with Sluse and in the *Œuvres complètes* (not fetched).


^{::clerk/no-cache true}
(page/prev-next "notebooks/alhazen_optics.clj")

(lens/section "alhazen-optics")
