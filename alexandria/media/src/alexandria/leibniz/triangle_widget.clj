(ns alexandria.leibniz.triangle-widget
  "The characteristic triangle of Nova Methodus (1684) as a tied widget
   (widget brief, section 5): the parabola x^2 = a y, the point P over x,
   the side dx taken at will, Q over x + dx, the rise dy, the chord PQ and
   its ratio dy/dx; the tangent at P and the subtangent VB, with the finite
   triangle (T, V, P) similar to the vanishing one.

   One shared state {:a :x :dx :dy :s}, every key a live Leva control,
   tied by the equations of alexandria.leibniz.triangle (Emmy's
   derivation):

     x, dx or a moves   the others held, dy and s forward
     dy moves           a and x held, dx = sqrt(x^2 + a dy) - x, s forward
     s moves            a and x held, dx = a s - 2x, dy forward

   dx is clamped to its slider's range, then everything goes forward, so
   no key leaves its range (the source is held at the edge). The solve is
   one raster kernel (`tie`); the numbers of the substituted formula come
   from a second (`readout`); the curve, chord and tangent are Mafs plots
   compiled by emmy.viewer.compile/*backend* :raster. A play control
   shrinks dx toward 0 (alexandria.medium.anim, smooth), through the same
   tie, so the reader watches the chord fall onto the tangent."
  (:require [alexandria.leibniz.triangle :as lt]
            [alexandria.medium.tie :as tie]
            [alexandria.raster :as raster]
            [emmy.clerk :as ec]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [nextjournal.clerk :as clerk]))

(def tied [:a :x :dx :dy :s])

(def ranges
  "Each key's slider range [min max step]; the tie keeps every key in it."
  {:a [1 4 0.01] :x [0.1 2.5 0.01] :dx [0 1.5 0.001] :dy [0 10 0.001] :s [0 7 0.001]})

(def labels {:a "a (parameter)" :x "x (abscissa)" :dx "dx (at will)" :dy "dy" :s "dy / dx"})

(defn- lo [k] (first (ranges k)))
(defn- hi [k] (second (ranges k)))

(defn- state [a x dx]
  (let [dx (tie/clamp dx (lo :dx) (hi :dx))
        [dy s] (lt/forward a x dx)]
    [a x dx dy s]))

(defn tie
  "The tie kernel: for [a x dx dy s], the solved state for each source in
   the order of `tied` (25 numbers)."
  [a x dx dy s]
  (fn [_]
    (vec (concat (state (tie/clamp a (lo :a) (hi :a)) x dx)
                 (state a (tie/clamp x (lo :x) (hi :x)) dx)
                 (state a x dx)
                 (state a x (lt/dx-of-dy a x (tie/at-least dy 0)))
                 (state a x (lt/dx-of-ratio a x s))))))

(def readout-index
  {:y 0 :xq 1 :yq 2 :s 3 :m 4 :gap 5 :vb 6 :dy 7 :dx 8 :x 9 :a 10 :tx 11 :chord 12})

(defn readout
  "The numbers the page shows, from the exact state: y, Q, the ratio, the
   tangent's slope y'(x) (Emmy's D), the gap dx^2/a, the subtangent VB,
   dy, dx, x, a, T's abscissa x - VB and the chord's length."
  [a x dx dy s]
  (fn [_]
    (let [y ((lt/curve a) x)
          m (lt/tangent-slope a x)
          vb (lt/subtangent a x)]
      [y (e/+ x dx) (e/+ y dy) s m (e/- dy (e/* m dx)) vb dy dx x a (e/- x vb)
       (e/sqrt (e/+ (e/square dx) (e/square dy)))])))

(defn init
  "The starting state, by raster on the JVM: a = 2, x = 1, dx = 0.8."
  []
  (zipmap tied (raster/value state 2.0 1.0 0.8)))

(def colours {:curve "#1F6FB4" :chord "#C0392B" :tangent "#E67E22" :triangle "#7B5EA7" :finite "#2f8a3e"})

(def quote-1684
  "Nova Methodus, Acta Eruditorum, Oct. 1684, p. 467."
  "Jam recta aliqua pro arbitrio assumta vocetur dx, et recta quae sit ad dx, ut v est ad VB, vocetur dv, sive differentia ipsarum v.")

(def quote-1684-en
  "Now let some straight line taken at will be called dx, and let the line that is to dx as v is to VB be called dv, the difference of the v's.")

(def quote-child
  "Historia et origo, tr. Child (1920), p. 39."
  "Even though this triangle is indefinite (being infinitely small), yet he perceived that it was always possible to find definite triangles similar to it.")

(defn- n [k] (tie/fmt (list 'aget 'r (readout-index k)) 3))

(defn- formula []
  [:div {:style {:font-size "0.95em" :line-height 1.5}}
   (list 'let ['tex (list 'str "\\frac{dy}{dx} = \\frac{2x + dx}{a} = \\frac{2\\cdot " (n :x) " + " (n :dx) "}{" (n :a) "} = " (n :s))]
         '[:div {:data-formula tex} [:f> nextjournal.clerk.render/render-katex tex {:inline? false}]])
   [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.92em"}}
    [:div "dy = y(x+dx) − y(x) = " (n :yq) " − " (n :y) " = " [:span {:data-dy '(str (aget r 7))} (n :dy)]]
    [:div "tangent y′(x) = 2x/a = " [:span {:data-m '(str (aget r 4))} (n :m)]]
    [:div "dy − y′·dx = dx²/a = " (n :gap) " (what Leibniz drops)"]
    [:div "subtangent VB = y/y′ = x/2 = " (n :vb)]]
   (list 'let ['dx '(aget r 8)
               'kind (list 'if '(< dx 0.0005) "tangent" "chord")]
         [:div {:style {:font-size "1.2em" :font-weight 600 :margin "0.3em 0"}}
          [:span {:data-name 'kind}
           (list 'if '(= kind "tangent")
                 "dx = 0: the chord PQ is the tangent at P, dy : dx = v : VB"
                 "dx > 0: the chord PQ, a little steeper than the tangent")]])])

(defn- figure
  "The Mafs figure on the exact state !s; `marks` is the browser component
   that draws the points and triangles from the readout."
  [!s marks]
  (let [pf (fn [params f] (ev/with-params {:atom !s :params params} f))]
    (mafs/mafs
     {:view-box {:x [-0.5 4] :y [-0.5 4]} :height 440}
     (mafs/cartesian)
     (mafs/of-x {:y (pf [:a] (fn [a] (fn [x] ((lt/curve a) x)))) :color (colours :curve)})
     (mafs/of-x {:y (pf [:a :x] (fn [a x0] (fn [x] (e/+ ((lt/curve a) x0) (e/* (lt/tangent-slope a x0) (e/- x x0))))))
                 :color (colours :tangent) :style "dashed"})
     (mafs/of-x {:y (pf [:a :x :s] (fn [a x0 s] (fn [x] (e/+ ((lt/curve a) x0) (e/* s (e/- x x0))))))
                 :color (colours :chord) :opacity 0.6})
     [marks])))

(defn- marks-form
  "Browser form: the reagent component of P, Q, the characteristic
   triangle and the finite triangle T V P, from the readout kernel."
  [rsym !s]
  (let [r-form (list rsym '(array 0) (tie/state-array #(ev/get !s %) tied))
        P ['(aget r 9) '(aget r 0)]
        Q ['(aget r 1) '(aget r 2)]
        R ['(aget r 1) '(aget r 0)]
        T ['(aget r 11) 0]
        V ['(aget r 9) 0]]
    (list 'fn []
          (list 'let ['r r-form]
                [:<>
                 (mafs/polygon {:points [T V P] :color (colours :finite) :fill-opacity 0.12})
                 (mafs/polygon {:points [P R Q] :color (colours :triangle) :fill-opacity 0.3})
                 (mafs/point {:x (P 0) :y (P 1) :color (colours :chord)})
                 (mafs/point {:x (Q 0) :y (Q 1) :color (colours :chord)})
                 (mafs/text "P" {:x (P 0) :y (P 1) :attach "nw"})
                 (mafs/text "Q" {:x (Q 0) :y (Q 1) :attach "nw"})
                 (mafs/text "dx" {:x '(/ (+ (aget r 9) (aget r 1)) 2) :y '(aget r 0) :attach "s" :size 14})
                 (mafs/text "dy" {:x '(aget r 1) :y '(/ (+ (aget r 0) (aget r 2)) 2) :attach "e" :size 14})
                 (mafs/text "T" {:x (T 0) :y 0 :attach "s"})
                 (mafs/text "V" {:x (V 0) :y 0 :attach "s"})]))))

(defn- runner
  "Browser form: while :shrink is on, dx runs down from 1.2 to 0 in 6 s
   (alexandria.medium.anim/smooth) and starts again, written into the
   panel atom !p so the tie solves dy and the ratio for each frame."
  [!p !x]
  (list 'let ['t0 '(atom nil)
              'step (list 'fn 'step ['now]
                          (list 'if (ev/get !x :shrink)
                                (list 'let ['t (list 'if '@t0 '(mod (/ (- now @t0) 6000) 1.25) 0)
                                            'u '(min 1 t)
                                            'dx '(* 1.2 (- 1 (alexandria.medium.anim/play u 0 1 alexandria.medium.anim/smooth)))]
                                      '(when-not @t0 (reset! t0 now))
                                      (list 'swap! !p 'assoc :dx '(/ (js/Math.round (* dx 1000)) 1000)))
                                '(reset! t0 nil))
                          '(reset! raf (js/requestAnimationFrame step)))]
        '(reset! raf (js/requestAnimationFrame step))))

(defn- template [[tsym tform] [rsym rform] !p !s !x]
  (list 'reagent.core/with-let
        [tsym tform rsym rform
         'pushing '(atom false)
         'pushed '(atom {})
         '_ (tie/watch-form tsym !p !s tied ranges "__leibnizSolves")
         'raf '(atom nil)
         '_ (runner !p !x)
         'marks (marks-form rsym !s)]
        (list 'let ['r (list rsym '(array 0) (tie/state-array #(ev/get !s %) tied))]
              [:div {:data-figure "characteristic-triangle"}
               [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                [:div {:style {:flex "1 1 420px" :min-width "300px"}} (figure !s 'marks)]
                [:div {:style {:flex "0 1 320px" :min-width "260px"}}
                 (leva/sub-panel {:fill true :flat true :title-bar false}
                                 (leva/controls {:atom !p :schema (tie/schema tied ranges labels)})
                                 (leva/controls {:atom !x :schema {:shrink {:label "shrink dx"}}}))
                 (formula)
                 [:p {:style {:font-size "0.85em" :opacity 0.8}}
                  "Violet: the characteristic triangle dx, dy, PQ. Green: the finite triangle T V P cut off by the tangent (dashed); its sides are y′·VB, VB and the tangent, in the ratio dy : dx at dx = 0. Red: the chord PQ."]]]])
        (list 'finally (tie/unwatch-form !p) '(some-> @raf js/cancelAnimationFrame))))

(defn widget
  "The characteristic-triangle widget, an emmy-viewers fragment for a Clerk
   notebook mounted after (history-of-math.widgets/install!). Compiled on
   the raster backend whatever the caller's binding."
  ([] (widget (init)))
  ([start]
   (binding [vc/*backend* :raster]
     (let [s (mapv start tied)]
       (ev/with-let [!p start !s start !x {:shrink false}]
         (template (tie/kernel-binding "tie" tie s)
                   (tie/kernel-binding "readout" readout s)
                   !p !s !x))))))

(defn derivation
  "Emmy's derivation (alexandria.leibniz.triangle) for the page: each
   step's equations by emmy.clerk/->TeX, then the grades."
  []
  (apply clerk/col
         (concat
          (for [{:keys [label eqs]} (lt/equations)]
            (apply clerk/col
                   (clerk/html [:div {:style {:font-size "0.85em" :opacity 0.7 :margin-top "0.5em"}} label])
                   (for [q eqs] (ec/->TeX q :simplify? true))))
          [(clerk/html
            (into [:ul {:style {:font-size "0.9em"}}]
                  (for [{:keys [label grade]} (lt/grades)]
                    [:li {:data-grade (name grade)} [:code (name grade)] " " label])))])))
