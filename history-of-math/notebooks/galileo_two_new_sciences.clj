;; # Galileo, *Two New Sciences*: falling bodies and the parabola
;;
;; In 1638 the Elzevir press in Leiden printed a book its author could not
;; publish in Italy: Galileo Galilei's *Discorsi e dimostrazioni matematiche
;; intorno a due nuove scienze*, "Discourses and mathematical demonstrations
;; concerning two new sciences". The two sciences are the strength of
;; materials and the science of motion.
;;
;; Galileo had taught mathematics at Padua from 1592 to 1610, and there he
;; did most of the work on motion that this book publishes. In 1609 he built
;; a telescope; in 1610 his *Sidereus Nuncius* ("Starry Messenger") reported
;; mountains on the Moon and four moons of Jupiter, and made him famous. In
;; 1632 his *Dialogue Concerning the Two Chief World Systems* argued for
;; Copernicus. In 1633 the Roman Inquisition tried him, made him abjure,
;; and confined him for the rest of his life to his villa at Arcetri, above
;; Florence. The *Two New Sciences* was written there, under house arrest,
;; and the manuscript left Italy for a Protestant printer.
;;
;; Like the *Dialogue*, it is a conversation among three friends: Salviati
;; speaks for Galileo, Sagredo is the quick amateur, Simplicio the
;; Aristotelian. On the Third Day they read aloud a Latin treatise by "our
;; Academician", Galileo himself, on motion, and stop to discuss it.
;;
;; The text below is the English of Henry Crew and Alfonso de Salvio,
;; *Dialogues Concerning Two New Sciences* (Macmillan, 1914). Page numbers
;; are theirs; numbers in brackets are the pages of the Italian national
;; edition. Every check below is computed by Alexandria
;; (`alexandria.galileo.motion`) with Emmy, a computer algebra system: a
;; badge reading *proved* means Emmy reduced the claim to exactly 0 for
;; all values of its letters.
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns galileo-two-new-sciences
  {:history/year 1638
   :history/title "Galileo, Two New Sciences"
   :history/era "Scientific Revolution"
   :nextjournal.clerk/visibility {:code :hide :result :show}
   :nextjournal.clerk/toc true}
  (:require [alexandria.galileo.motion :as motion]
            [alexandria.galileo.motion-view :as view]
            [history-of-math.page :as page]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:result :hide}}
(defn quote-of
  "A passage of the proposition id, in Crew and de Salvio's words."
  ([id] (quote-of id :statement))
  ([id k]
   (let [p (view/proposition id)]
     (page/blockquote (get p k) (str "Crew and de Salvio, " (:source p))))))

^{::clerk/visibility {:result :hide}}
(def passing
  "The grades that pass here: Emmy's exact proofs, closed forms, and the
   one numeric cross-check."
  #{:grade/proved :grade/closed-form :grade/numeric})

^{::clerk/visibility {:result :hide}}
(defn badges
  "One badge per graded check, through history-of-math.page."
  [rows]
  (page/grade-badges rows passing))

;; ## Before Galileo: Oresme's graph of speed
;;
;; The question is old. Aristotle taught that a heavy body falls at a speed
;; set by its weight. In the 1330s, at Merton College, Oxford, William
;; Heytesbury and Richard Swineshead studied "uniformly difform" motion:
;; motion whose speed grows by equal amounts in equal times. They stated the
;; *mean speed rule*: such a motion covers the same distance as a uniform
;; motion at its middle speed. Around 1350 Nicole Oresme, in Paris, drew it.
;; He laid time along a line, raised at each instant a line as long as the
;; speed, and saw the distance as the area of the figure: a triangle for
;; motion from rest. None of them said that falling bodies move this way.
;; Galileo did, and checked it.

;; ## Third Day, Theorem I: the mean speed
;;
;; *Uniformly accelerated* means, in Galileo's definition, that the speed
;; gains equal amounts in equal times. Write $t$ for the time since rest and
;; $a$ for the gain per unit of time; then the speed is $v = a\,t$.

(quote-of :galileo/third-day-1)

;; His figure is Oresme's graph. $AB$ is the time, $EB$ the last speed; the
;; speeds at every instant fill the triangle $AEB$, and the rectangle
;; $AGFB$ is a uniform motion at half the last speed:

(quote-of :galileo/third-day-1 :quote)

(view/mean-speed)

(quote-of :galileo/third-day-1 :quote-2)

;; In symbols: the distance at time $T$ is $s(T) = \tfrac12 a T^2$ (its
;; rate of change, Emmy's derivative $D s$, is the speed $a t$), and the
;; rectangle is $T \cdot \tfrac{aT}{2}$, the same.

(badges (motion/graded :galileo/third-day-1))

;; ## Third Day, Theorem II: distances as the squares of the times

(quote-of :galileo/third-day-2)

(view/squares)

;; Read it as a ratio, as Galileo does: for two times $t_1$ and $t_2$,
;; $s(t_1) : s(t_2) = t_1^2 : t_2^2$, written below as
;; $s(t_1)\,t_2^2 - s(t_2)\,t_1^2 = 0$.

(quote-of :galileo/third-day-2 :quote)

(badges (motion/graded :galileo/third-day-2))

;; ## Corollary I: the odd numbers

(quote-of :galileo/corollary-1)

;; After 1, 2, 3, 4 equal times the body has fallen 1, 4, 9, 16 units; in
;; each single time it falls 1, 3, 5, 7. Why the odd numbers? Because they
;; are the differences of consecutive squares, as Galileo says:

(quote-of :galileo/corollary-1 :quote)

;; Here is the pattern once, as a figure. A square of $n \times n$ unit
;; cells grows into an $(n+1) \times (n+1)$ square by an L-shaped band, a
;; *gnomon*, of $n + n + 1$ cells. The Pythagoreans laid out pebbles this
;; way, and Euclid's Book II names the gnomon. Every odd number is a
;; gnomon, and the gnomons stacked make the squares.

(view/odd-numbers)

;; The same pattern in algebra is one line, $(n+1)^2 - n^2 = 2n + 1$, and
;; Emmy proves it for every $n$ at once. That one identity is the whole
;; corollary: the distance in the $(n+1)$-th time is $2n+1$ times the first.

(badges (motion/graded :galileo/corollary-1))

;; ## The inclined plane
;;
;; Simplicio asks for an experiment. A falling stone is too fast to time
;; with the clocks of 1600, so Galileo slows the fall: he lets a ball roll
;; down a gentle slope, on which the speed still grows in proportion to the
;; time.

(quote-of :galileo/inclined-plane :quote)

;; He timed it with water:

(quote-of :galileo/inclined-plane :quote-3)

(view/inclined-plane)

(quote-of :galileo/inclined-plane :quote-2)

(badges (motion/plane-graded))

;; ## Fourth Day, Theorem I: the projectile describes a parabola
;;
;; On the Fourth Day Galileo puts two motions together: a uniform motion
;; sideways, which nothing slows, and the natural fall. The sideways motion
;; is the principle we now call inertia, which Newton will make his first
;; law.

(quote-of :galileo/fourth-day-1)

;; To show that the path is a parabola, Galileo needs Apollonius of Perga
;; (about 200 BC), whose *Conics* define the parabola by a *symptom*,
;; Proposition I.11: the square on each ordinate (the distance across from
;; the axis) equals a fixed length $p$, the *latus rectum*, times the
;; abscissa (the distance along the axis). Sagredo admits he has not read
;; far in Apollonius:

(quote-of :galileo/fourth-day-1 :quote)

;; Here the axis is the vertical through the edge $b$ of the plane. After a
;; time $t$ the body is $x = u\,t$ across and $y = k\,t^2$ down, $u$ the
;; uniform speed and $k$ the fall in the first unit of time. Then
;; $x^2 = \frac{u^2}{k}\,y$: the symptom, with $p = u^2/k$.

(view/projectile)

(quote-of :galileo/fourth-day-1 :quote-2)

(badges (motion/graded :galileo/fourth-day-1))

;; ## Fourth Day, Proposition VII: the longest shot is at 45 degrees

(quote-of :galileo/fourth-day-7)

;; Gunners already knew it from practice; Sagredo's delight is in knowing
;; why:

(quote-of :galileo/fourth-day-7 :quote)

(quote-of :galileo/fourth-day-7 :quote-2)

(view/forty-five)

;; Galileo's proof compares momenta along parabolas of equal amplitude,
;; and rests on Euclid II.5: of all rectangles whose sides add up to a
;; given line, the square is the largest. In modern terms: split the speed
;; $v$ into $c$ across and $w$ up, $c^2 + w^2 = v^2$, and let $g$ be the
;; acceleration of fall; the range is $2cw/g = \big(v^2 - (c - w)^2\big)/g$,
;; greatest when $c = w$.

(badges (motion/graded :galileo/fourth-day-7))

;; A numeric cross-check, trying every whole degree from 1 to 89, finds the
;; longest shot at:

(clerk/md (str "**" (motion/max-range-numeric) " degrees**"))

;; ## Before and after
;;
;; Galileo called Archimedes "superhuman". His first scientific work, *La
;; Bilancetta* (1586), rebuilt the hydrostatic balance of Archimedes' *On
;; Floating Bodies*, and his early theorems on centres of gravity of solids
;; follow Archimedes' manner, by exhaustion; the *Two New Sciences* prints
;; them as an appendix. His parabola is Apollonius' parabola, and his
;; mean-speed figure is Oresme's.
;;
;; Galileo died at Arcetri in January 1642. Isaac Newton was born that
;; December. In the *Principia* (1687) Newton credits Galileo with the
;; first two laws of motion and the parabolic path of projectiles, and
;; turns Galileo's constant fall into a force that weakens with the square
;; of the distance, the same force that holds the Moon and the planets in
;; their orbits.

^{::clerk/no-cache true}
(page/prev-next "notebooks/galileo_two_new_sciences.clj")
