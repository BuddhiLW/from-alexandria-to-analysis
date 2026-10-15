(ns alexandria.lagrange.taylor-widget
  "Lagrange's limits of the rest of a series (Theorie des fonctions
   analytiques, 1797) as a tied widget (widget brief, section 5): sin and
   its first n terms P_n, the band P_n(t) +- |t|^n/n! that must hold sin
   (every derived function of sin is at most 1 in size), and at the point
   x the rest R = sin x - P_n(x) beside its bound B = x^n/n!.

   One shared state {:n :x :lgB} (lgB = log10 B), every key a live Leva
   control, tied by the equations of alexandria.lagrange.taylor (Emmy's
   derivation):

     n moves     x held, the bound forward: B = x^n / n!
     x moves     n held, B forward
     lgB moves   n held, x = (n! B)^(1/n) (the inverse map)

   lgB is clamped to what x's range allows at this order (held at the
   edge). The solve is one raster kernel (`tie`) with log n! as Emmy's
   branch-free sum (taylor/log-factorial), the numbers of the substituted
   formula a second (`readout`); the curves are Mafs plots compiled by
   emmy.viewer.compile/*backend* :raster, the order a parameter of one
   compiled polynomial."
  (:require [alexandria.lagrange.taylor :as taylor]
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

(def tied [:n :x :lgB])

(def ranges {:n [1 12 1] :x [0.05 6 0.01] :lgB [-14 3 0.01]})

(def labels {:n "n (terms)" :x "x" :lgB "log₁₀ B (bound)"})

(def ln10 (Math/log 10))

(defn- lo [k] (first (ranges k)))
(defn- hi [k] (second (ranges k)))

(defn- lg-bound
  "log10 B at x for the order n, lnN = log n!."
  [x n lnN]
  (e// (taylor/log-bound x n lnN) ln10))

(defn- forward [n x lnN] [n x (lg-bound x n lnN)])

(defn tie
  "The tie kernel: for [n x lgB], the solved state for each source in the
   order of `tied` (9 numbers). n or x: the bound forward. lgB: clamped to
   the bounds of x's range at this order, then x by the inverse map."
  [n x lgB]
  (fn [_]
    (let [lnN (taylor/log-factorial n)
          l (tie/clamp lgB (lg-bound (lo :x) n lnN) (lg-bound (hi :x) n lnN))]
      (vec (concat (forward n x lnN)
                   (forward n x lnN)
                   [n (taylor/x-of-log-bound (e/* ln10 l) n lnN) l])))))

(def readout-index {:n 0 :x 1 :B 2 :P 3 :sin 4 :R 5 :ratio 6 :next 7 :N 8})

(defn readout
  "The numbers beside the panel: n, x, B, P_n(x), sin x, the rest R, R/B
   (the value of the n-th derived function at Lagrange's u), the factor
   x/(n + 1) of one more term, and n!."
  [n x _lgB]
  (fn [_]
    (let [lnN (taylor/log-factorial n)
          B (e/exp (taylor/log-bound x n lnN))
          P (taylor/partial-sum n x)
          R (e/- (e/sin x) P)]
      [n x B P (e/sin x) R (e// R B) (e// x (e/+ n 1)) (e/exp lnN)])))

(defn poly
  "P_n(t), the order a parameter."
  [n]
  (fn [t] (taylor/partial-sum n t)))

(defn band
  "P_n(t) + side |t|^n / n!."
  [side]
  (fn [n]
    (fn [t] (e/+ (taylor/partial-sum n t)
                 (e/* side (e/exp (e/- (e/* n (e/log (e/+ (e/abs t) 1e-12))) (taylor/log-factorial n))))))))

(defn init
  "The start: n = 5, x = 2, by raster on the JVM."
  []
  (zipmap tied (raster/value (fn [n x] (forward n x (taylor/log-factorial n))) 5.0 2.0)))

(def colours {:sin "#1F6FB4" :poly "#C0392B" :band "#E67E22" :rest "#2f8a3e"})

(def quote-original
  "Oeuvres de Lagrange IX, Theorie des fonctions analytiques, no 40 (Wikisource)."
  "on aura le développement de f(x+i) suivant les puissances de i, et l’on voit que dans ce développement la série infinie, à commencer d’un terme quelconque, est toujours égale à la valeur de ce premier terme en y mettant x+j à la place de x, j étant une quantité entre 0 et i")

(def quote-english
  "one will have the development of f(x+i) in powers of i, and one sees that in this development the infinite series, from any term on, is always equal to the value of that first term with x+j put in place of x, j being a quantity between 0 and i")

(defn- n* [k p] (tie/fmt (list 'aget 'r (readout-index k)) p))

(defn- formula []
  [:div {:style {:font-size "0.95em" :line-height 1.5}}
   (list 'let ['nn '(js/Math.round (aget r 0))
               'tex (list 'str "\\sin x = P_{" 'nn "}(x) + \\frac{x^{" 'nn "}}{" 'nn "!}\\,f^{(" 'nn ")}(u),\\quad "
                          "\\sin " (n* :x 2) " = " (n* :P 5) " + (" '(.toExponential (aget r 5) 2) ")"
                          ",\\quad |R| \\le \\frac{" (n* :x 2) "^{" 'nn "}}{" '(js/Math.round (aget r 8)) "} = "
                          '(.toExponential (aget r 2) 3))]
         '[:div {:data-formula tex} [:f> nextjournal.clerk.render/render-katex tex {:inline? false}]])
   [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.92em"}}
    [:div "B = xⁿ/n! = " [:span {:data-bound '(str (aget r 2))} '(.toExponential (aget r 2) 4)]]
    [:div "R = sin x − Pₙ(x) = " [:span {:data-rest '(str (aget r 5))} '(.toExponential (aget r 5) 4)]]
    [:div "f⁽ⁿ⁾(u) = R/B = " [:span {:data-ratio '(str (aget r 6))} (n* :ratio 4)] ", inside [−1, 1]"]
    [:div "one more term multiplies B by x/(n+1) = " (n* :next 4)]]
   (list 'let ['nn '(js/Math.round (aget r 0))
               'fname '(nth ["sin" "cos" "−sin" "−cos"] (mod nn 4))]
         [:div {:style {:font-size "1.15em" :font-weight 600 :margin "0.3em 0"}}
          "f⁽" 'nn "⁾ = " [:span {:data-derived 'fname} 'fname]
          (list 'if '(< (aget r 7) 1)
                ", x < n + 1: the next term is smaller, the bound shrinks"
                ", x ≥ n + 1: the next term is not smaller yet")])])

(defn- marks-form [rsym !s]
  (let [r-form (list rsym '(array 0) (tie/state-array #(ev/get !s %) tied))]
    (list 'fn []
          (list 'let ['r r-form 'x '(aget r 1) 'P '(aget r 3) 'S '(aget r 4) 'B '(aget r 2)]
                [:<>
                 (mafs/segment {:point1 ['x '(- P (min B 50))] :point2 ['x '(+ P (min B 50))] :color (colours :band) :weight 6 :opacity 0.5})
                 (mafs/segment {:point1 ['x 'P] :point2 ['x 'S] :color (colours :rest) :weight 3})
                 (mafs/point {:x 'x :y 'S :color (colours :sin)})
                 (mafs/point {:x 'x :y 'P :color (colours :poly)})
                 (mafs/text "x" {:x 'x :y -2.8 :attach "e"})]))))

(defn- figure [!s marks]
  (let [pf (fn [f] (ev/with-params {:atom !s :params [:n]} f))]
    (mafs/mafs
     {:view-box {:x [-0.5 6.5] :y [-3 3]} :height 420}
     (mafs/cartesian)
     (mafs/of-x {:y (pf (band 1)) :color (colours :band) :style "dashed"})
     (mafs/of-x {:y (pf (band -1)) :color (colours :band) :style "dashed"})
     (mafs/of-x {:y e/sin :color (colours :sin)})
     (mafs/of-x {:y (pf poly) :color (colours :poly)})
     [marks])))

(defn- template [[tsym tform] [rsym rform] !p !s]
  (list 'reagent.core/with-let
        [tsym tform rsym rform
         'pushing '(atom false)
         'pushed '(atom {})
         '_ (tie/watch-form tsym !p !s tied ranges "__lagrangeSolves")
         'marks (marks-form rsym !s)]
        (list 'let ['r (list rsym '(array 0) (tie/state-array #(ev/get !s %) tied))]
              [:div {:data-figure "lagrange-remainder"}
               [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                [:div {:style {:flex "1 1 440px" :min-width "300px"}} (figure !s 'marks)]
                [:div {:style {:flex "0 1 330px" :min-width "260px"}}
                 (leva/sub-panel {:fill true :flat true :title-bar false}
                                 (leva/controls {:atom !p :schema (tie/schema tied ranges labels)}))
                 (formula)
                 [:p {:style {:font-size "0.85em" :opacity 0.8}}
                  "Blue: sin. Red: its first n terms Pₙ. Orange, dashed: Pₙ(t) ± |t|ⁿ/n!, Lagrange's limits, which hold sin because every derived function of sin is at most 1. At x: the rest R (green) inside the bound B (orange bar)."]]]])
        (list 'finally (tie/unwatch-form !p))))

(defn widget
  "The remainder widget, an emmy-viewers fragment for a notebook mounted
   after (history-of-math.widgets/install!). Raster backend whatever the
   caller's binding."
  ([] (widget (init)))
  ([start]
   (binding [vc/*backend* :raster]
     (let [s (mapv start tied)]
       (ev/with-let [!p start !s start]
         (template (tie/kernel-binding "tie" tie s)
                   (tie/kernel-binding "readout" readout s)
                   !p !s))))))

(defn derivation
  "Emmy's derivation (alexandria.lagrange.taylor) for the page, with grades."
  []
  (apply clerk/col
         (concat
          (for [{:keys [label eqs]} (taylor/equations)]
            (apply clerk/col
                   (clerk/html [:div {:style {:font-size "0.85em" :opacity 0.7 :margin-top "0.5em"}} label])
                   (for [q eqs] (ec/->TeX q :simplify? true))))
          [(clerk/html
            (into [:ul {:style {:font-size "0.9em"}}]
                  (for [{:keys [label grade]} (taylor/grades)]
                    [:li {:data-grade (name grade)} [:code (name grade)] " " label])))])))
