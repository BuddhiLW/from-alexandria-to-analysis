(ns alexandria.newton.orbit-widget
  "Principia Book I as one tied widget (widget brief, section 5): the
   polygon of impulses of Prop. I, stepped sentence by sentence in Motte's
   words with Newton's letters S, A, B, c, C, D, E, F, and the orbit it
   tends to under the inverse square (Props. XI-XIII), the conic named
   live in Apollonius' words.

   One shared state {:r0 :v0 :e :p}, every key a live Leva control, tied
   by the equations of alexandria.newton.orbit (Emmy's derivation, mu = 1):

     r0 moves   v0 held, e and p forward
     v0 moves   r0 held, e = r0 v0^2 - 1 and p = r0^2 v0^2 forward
     e moves    r0 held, v0 = sqrt((1 + e)/r0), p forward
     p moves    r0 held, v0 = sqrt(p)/r0, e forward

   The source is clamped where a solved key would leave its slider (held
   at the edge). The solve is one raster kernel (`tie`), the formula's
   numbers a second (`readout`), and every vertex of the polygon a third
   (`move`, Newton's construction: c = 2B - A, C = c - k B / r^3, with the
   areas SAB, SBc, SBC), called once per moment in the browser. The orbit
   is a Mafs plot compiled by emmy.viewer.compile/*backend* :raster."
  (:require [alexandria.medium.tie :as tie]
            [alexandria.newton.orbit :as orbit]
            [alexandria.newton.principia :as principia]
            [alexandria.newton.principia-view :as pview]
            [alexandria.raster :as raster]
            [emmy.clerk :as ec]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [nextjournal.clerk :as clerk]))

(def mu 1.0)

(def tied [:r0 :v0 :e :p])

(def ranges
  "Each key's slider range [min max step]. The ranges of e and p are the
   images of r0 x v0 under the forward maps (e = r0 v0^2 - 1 runs from
   0.5 x 0.09 - 1 to 1.5 x 3.24 - 1, p = r0^2 v0^2 from 0.25 x 0.09 to
   2.25 x 3.24), so moving r0 or v0 never leaves a range."
  {:r0 [0.5 1.5 0.01] :v0 [0.3 1.8 0.001] :e [-0.96 3.86 0.001] :p [0.02 7.3 0.001]})

(def labels {:r0 "r₀ = SA" :v0 "v₀ (speed at A)" :e "e" :p "p = L/2"})

(def parabola-band 0.005)
(def circle-band 0.005)

(defn- lo [k] (first (ranges k)))
(defn- hi [k] (second (ranges k)))

(defn- state
  "r0 and v0 given: e and p forward."
  [r0 v0]
  [r0 v0 (orbit/ecc mu r0 v0) (orbit/semi-latus mu r0 v0)])

(defn tie
  "The tie kernel: for [r0 v0 e p], the solved state for each source in the
   order of `tied` (16 numbers). r0 or v0: the other held, e and p
   forward. e or p: r0 held, the source first clamped to what v0's range
   allows at this r0 (held at the edge), then v0 by the inverse map."
  [r0 v0 ecc p]
  (fn [_]
    (let [vl2 (* (lo :v0) (lo :v0)) vh2 (* (hi :v0) (hi :v0))
          e' (tie/clamp ecc (e/- (e/* r0 vl2) 1) (e/- (e/* r0 vh2) 1))
          p' (tie/clamp p (e/* (e/square r0) vl2) (e/* (e/square r0) vh2))]
      (vec (concat (state r0 v0)
                   (state r0 v0)
                   (state r0 (orbit/v0-of-e mu r0 e'))
                   (state r0 (orbit/v0-of-p mu r0 p')))))))

(def readout-index {:r0 0 :v0 1 :e 2 :p 3 :h 4 :E 5 :w 6 :rp 7 :ra 8 :ae 9})

(defn readout
  "The numbers beside the panel: r0, v0, e, p, h = r0 v0, the energy E,
   v0^2, the near apsis p/(1 + |e|) and the far one p/(1 - |e|), |e|."
  [r0 v0 ecc p]
  (fn [_]
    (let [ae (e/abs ecc)]
      [r0 v0 ecc p (e/* r0 v0) (orbit/energy mu r0 v0) (e/square v0)
       (e// p (e/+ 1 ae)) (e// p (e/- 1 ae)) ae])))

(defn move
  "One moment of Prop. I: from A and B and the strength k = mu dt^2, the
   unhindered c, the deflected C (principia/impulse-step with the impulse
   k / r^3 at B), and the areas SAB, SBc, SBC (half cross products)."
  [ax ay bx by k]
  (fn [_]
    (let [r2 (e/+ (e/square bx) (e/square by))
          {[cx cy] :c [qx qy] :C} (principia/impulse-step [ax ay] [bx by] (e// k (e/* r2 (e/sqrt r2))))
          area (fn [[px py] [rx ry]] (e/* 0.5 (e/- (e/* px ry) (e/* py rx))))]
      [cx cy qx qy (area [ax ay] [bx by]) (area [bx by] [cx cy]) (area [bx by] [qx qy])])))

(defn conic-xy
  "The orbit r = p/(1 + e cos t) about S, the branch through the start
   (denominator held at 1e-3 or above; far points leave the view)."
  [ecc p]
  (fn [t]
    (let [r (e// p (tie/at-least (e/+ 1 (e/* ecc (e/cos t))) 1e-3))]
      [(e/* r (e/cos t)) (e/* r (e/sin t))])))

(defn init
  "The start: r0 = 1, v0 = 1.2, by raster on the JVM (e = 0.44)."
  []
  (zipmap tied (raster/value state 1.0 1.2)))

(def colours {:orbit "#1F6FB4" :polygon "#C0392B" :tri1 "#E67E22" :tri2 "#7B5EA7" :tri3 "#2f8a3e" :ghost "#888888"})

(defn motte-steps
  "Prop. I's steps in Motte's words (the shelf's :newton/prop-1)."
  []
  (mapv :claim (:steps (pview/proposition :newton/prop-1))))

(defn- n [k] (tie/fmt (list 'aget 'r (readout-index k)) 3))

(defn- formula []
  [:div {:style {:font-size "0.95em" :line-height 1.5}}
   (list 'let ['tex (list 'str "r = \\frac{p}{1 + e\\cos\\theta} = \\frac{" (n :p) "}{1 + " (n :e) "\\,\\cos\\theta}")]
         '[:div {:data-formula tex} [:f> nextjournal.clerk.render/render-katex tex {:inline? false}]])
   [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.92em"}}
    [:div "e = r₀v₀²/μ − 1 = " (n :r0) " × " (n :w) " − 1 = " [:span {:data-e '(str (aget r 2))} (n :e)]]
    [:div "p = (r₀v₀)²/μ = " (n :h) "² = " [:span {:data-p '(str (aget r 3))} (n :p)]]
    [:div "energy E = v₀²/2 − μ/r₀ = " (n :E) ";  e² = 1 + 2Eh²/μ²"]
    [:div "apsides: p/(1+|e|) = " (n :rp) ",  p/(1−|e|) = " (n :ra)]]
   (list 'let ['ae '(aget r 9)
               'kind (list 'cond (list '< 'ae circle-band) "circle"
                           (list '< '(js/Math.abs (- ae 1)) parabola-band) "parabola"
                           '(< ae 1) "ellipse" :else "hyperbola")]
         [:div {:style {:font-size "1.25em" :font-weight 600 :margin "0.3em 0"}}
          [:span {:data-conic-name 'kind} 'kind]
          '(case kind
             "circle" " (e = 0): κύκλος, v₀² = μ/r₀"
             "ellipse" " (e < 1): ἔλλειψις, E < 0, the body returns (Prop. XI)"
             "parabola" " (e = 1): παραβολή, E = 0, the speed of escape (Prop. XIII)"
             " (e > 1): ὑπερβολή, E > 0, the body leaves for good (Prop. XII)")])])

(defn- polygon-form
  "Browser form: the polygon of impulses from the exact state !s and the
   moments per unit time in !x (12 units of time, or until the body is
   far), every vertex by the `move` kernel `msym`. Returns {:ps vertices
   :cs unhindered points :ar [SAB SBc SBC] per moment}."
  [msym !s !x]
  (list 'let ['r0 (ev/get !s :r0) 'v0 (ev/get !s :v0)
              'dt (list '/ 1 (ev/get !x :moments))
              'k '(* dt dt)
              'nmax (list 'js/Math.round (list '* 12 (ev/get !x :moments)))]
        (list 'loop '[ps [[r0 0] [r0 (* dt v0)]] cs [] ar [] i 1]
              (list 'if '(or (>= i nmax) (> (js/Math.hypot (first (peek ps)) (second (peek ps))) 30))
                    '{:ps ps :cs cs :ar ar}
                    (list 'let ['[ax ay] '(nth ps (dec i)) '[bx by] '(nth ps i)
                                'o (list msym '(array 0) '(array ax ay bx by k))]
                          '(recur (conj ps [(aget o 2) (aget o 3)]) (conj cs [(aget o 0) (aget o 1)])
                                  (conj ar [(aget o 4) (aget o 5) (aget o 6)]) (inc i)))))))

(defn- marks-form
  "Browser form: the reagent component drawing the current step's
   construction (Newton's letters), from the polygon."
  [msym !s !x]
  (list 'fn []
        (list 'let ['poly (polygon-form msym !s !x)
                    'ps '(:ps poly) 'cs '(:cs poly)
                    'step (ev/get !x :step)
                    'S [0 0]
                    'pt '(fn [i] (nth ps i))
                    'shown '(case step 0 2 1 2 2 3 3 3 4 6 (count ps))]
              [:<>
               (list 'when '(>= step 4)
                     (mafs/polyline {:points '(vec (take shown ps)) :color (colours :polygon) :weight 2}))
               (list 'when '(>= step 5)
                     '(into [:<>]
                            (for [i (range 0 (dec (count ps)) (max 1 (quot (count ps) 24)))]
                              [mafs.core/Polygon {:points [S (pt i) (pt (inc i))] :color "#C0392B" :fillOpacity 0.1 :strokeOpacity 0.2}])))
               (list 'when '(< step 4)
                     [:<>
                      (mafs/segment {:point1 '(pt 0) :point2 '(pt 1) :color (colours :polygon) :weight 3})
                      (mafs/polygon {:points ['S '(pt 0) '(pt 1)] :color (colours :tri1) :fill-opacity 0.25})])
               (list 'when '(and (>= step 1) (< step 4))
                     [:<>
                      (mafs/segment {:point1 '(pt 1) :point2 '(nth cs 0) :color (colours :ghost) :style "dashed"})
                      (mafs/polygon {:points ['S '(pt 1) '(nth cs 0)] :color (colours :tri2) :fill-opacity 0.2})
                      (mafs/point {:x '(first (nth cs 0)) :y '(second (nth cs 0)) :color (colours :ghost)})
                      (mafs/text "c" {:x '(first (nth cs 0)) :y '(second (nth cs 0)) :attach "e"})])
               (list 'when '(and (>= step 2) (< step 4))
                     [:<>
                      (mafs/segment {:point1 '(nth cs 0) :point2 '(pt 2) :color (colours :tri3) :style "dashed"})
                      (mafs/segment {:point1 '(pt 1) :point2 '(pt 2) :color (colours :polygon) :weight 3})
                      (mafs/segment {:point1 'S :point2 '(pt 1) :color (colours :ghost)})])
               (list 'when '(= step 3)
                     (mafs/polygon {:points ['S '(pt 1) '(pt 2)] :color (colours :tri3) :fill-opacity 0.3}))
               (list 'when '(= step 4)
                     '(into [:<>]
                            (for [i (range 0 (min 5 (dec (count ps))))]
                              [mafs.core/Polygon {:points [S (pt i) (pt (inc i))]
                                                  :color (nth ["#E67E22" "#2f8a3e" "#7B5EA7" "#1F6FB4" "#C0392B"] i)
                                                  :fillOpacity 0.25}])))
               '[mafs.core/Point {:x 0 :y 0 :color "#111111"}]
               '[mafs.core/Text {:x 0 :y 0 :attach "sw"} "S"]
               (list 'into [:<>]
                     '(for [i (range (min shown 6 (count ps)))]
                        [:<> [mafs.core/Point {:x (first (pt i)) :y (second (pt i)) :color "#C0392B"}]
                         [mafs.core/Text {:x (first (pt i)) :y (second (pt i)) :attach "ne"} (nth ["A" "B" "C" "D" "E" "F"] i)]]))])))

(defn- areas-form
  "Browser form: the areas of the first moment's triangles and of the next
   ones, every number from the move kernel."
  [msym !s !x]
  (list 'fn []
        (list 'let ['poly (polygon-form msym !s !x)
                    'ar '(:ar poly)
                    'a0 '(first ar)]
              [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.9em" :margin-top "0.4em"}
                     :data-areas '(pr-str (mapv first (take 6 ar)))}
               [:div "SAB = " (tie/fmt '(nth a0 0) 5) ",  SBc = " (tie/fmt '(nth a0 1) 5) ",  SBC = " (tie/fmt '(nth a0 2) 5)]
               [:div "SCD, SDE, SEF, … = " '(apply str (interpose ", " (map (fn [[x]] (.replace (.toFixed x 5) "-" "−")) (take 5 (rest ar)))))]
               [:div "moments drawn: " '(count ar)]])))

(defn- steps-panel
  "Browser form: Motte's sentences, the current one highlighted."
  [!x sentences]
  [:ol {:start 0 :style {:font-size "0.88em" :line-height 1.4 :padding-left "1.6em"}}
   (list 'doall
         (list 'map-indexed
               (list 'fn ['i 's]
                     [:li {:key 'i
                           :data-step-current (list 'str (list '= 'i (ev/get !x :step)))
                           :style (list 'if (list '= 'i (ev/get !x :step))
                                        {:background "#fff3cd" :font-weight 500}
                                        {:opacity 0.55})}
                      's])
               sentences))])

(defn- figure [!s marks]
  (mafs/mafs
   {:view-box {:x [-3 1.8] :y [-1.8 1.8]} :height 460 :preserve-aspect-ratio "contain"}
   (mafs/cartesian)
   (mafs/parametric {:xy (ev/with-params {:atom !s :params [:e :p]} conic-xy)
                     :t [(- 0.001 Math/PI) (- Math/PI 0.001)] :color (colours :orbit)})
   [marks]))

(defn- template [[tsym tform] [rsym rform] [msym mform] !p !s !x sentences]
  (list 'reagent.core/with-let
        [tsym tform rsym rform msym mform
         'pushing '(atom false)
         'pushed '(atom {})
         '_ (tie/watch-form tsym !p !s tied ranges "__newtonSolves")
         'marks (marks-form msym !s !x)
         'areas (areas-form msym !s !x)]
        (list 'let ['r (list rsym '(array 0) (tie/state-array #(ev/get !s %) tied))]
              [:div {:data-figure "principia-orbit"}
               [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                [:div {:style {:flex "1 1 440px" :min-width "300px"}} (figure !s 'marks) '[areas]]
                [:div {:style {:flex "0 1 330px" :min-width "260px"}}
                 (leva/sub-panel {:fill true :flat true :title-bar false}
                                 (leva/controls {:atom !p :schema (tie/schema tied ranges labels)})
                                 (leva/controls {:atom !x :schema {:step {:min 0 :max 5 :step 1 :label "step of Prop. I"}
                                                                   :moments {:min 2 :max 40 :step 1 :label "moments per unit time"}}}))
                 (formula)]]
               (steps-panel !x sentences)])
        (list 'finally (tie/unwatch-form !p))))

(defn widget
  "The Principia widget (Prop. I stepped, the inverse-square orbit tied),
   an emmy-viewers fragment for a notebook mounted after
   (history-of-math.widgets/install!). Raster backend whatever the
   caller's binding."
  ([] (widget (init)))
  ([start]
   (binding [vc/*backend* :raster]
     (let [s (mapv start tied)]
       (ev/with-let [!p start !s start !x {:step 0 :moments 3}]
         (template (tie/kernel-binding "tie" tie s)
                   (tie/kernel-binding "readout" readout s)
                   (tie/kernel-binding "move" move [1.0 0.0 1.0 0.4 0.1])
                   !p !s !x (motte-steps)))))))

(defn derivation
  "Emmy's derivation (alexandria.newton.orbit) for the page, with grades."
  []
  (apply clerk/col
         (concat
          (for [{:keys [label eqs]} (orbit/equations)]
            (apply clerk/col
                   (clerk/html [:div {:style {:font-size "0.85em" :opacity 0.7 :margin-top "0.5em"}} label])
                   (for [q eqs] (ec/->TeX q :simplify? true))))
          [(clerk/html
            (into [:ul {:style {:font-size "0.9em"}}]
                  (for [{:keys [label grade]} (orbit/grades)]
                    [:li {:data-grade (name grade)} [:code (name grade)] " " label])))])))
