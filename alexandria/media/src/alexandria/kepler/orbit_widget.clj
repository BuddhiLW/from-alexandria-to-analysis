(ns alexandria.kepler.orbit-widget
  "Kepler's orbit as a live conic (widget brief, section 5; the bar is the
   Apollonius conics playground).

   One tied state {:a :T :e :l}, every key a Leva control, tied by the
   equations alexandria.kepler.focal derives with Emmy:

     third law       T^2 = a^3  (T = a sqrt a, a = T^(2/3))
     focal polar     rho = l / (1 - e cos theta),  l = a (1 - e^2)

     a moves   e fixed: T and l follow
     T moves   e fixed: a = T^(2/3), then l
     e moves   a fixed (so T): l = a (1 - e^2)
     l moves   e fixed: a = l / (1 - e^2) (Emmy's solve-linear), then T

   Every key is clamped so no other leaves its slider (the source is held
   at the edge). The tie runs as one raster kernel; the page mounts it with
   alexandria.medium.tie (origin and epsilon guards).

   The equal areas are integrated, not drawn from a formula: Newton's
   inverse-square flow with the swept area as a fifth coordinate, one RK4
   step per call of a raster wasm kernel (alexandria.kepler.focal/rk4-step),
   started at perihelion with the vis-viva speed. One period T = a sqrt a
   is cut into n equal times; each sector is the polygon of the Sun and
   the RK4 points of its time, and the areas printed are the fifth
   coordinate's increments. The orbit curve is rho = l / (1 - e cos theta)
   plotted by Mafs on the raster backend; the RK4 points must land on it.
   The planet runs along the RK4 trajectory (alexandria.medium.anim)."
  (:require [alexandria.kepler.focal :as kf]
            [alexandria.medium.tie :as tie]
            [alexandria.raster :as raster]
            [clojure.walk :as walk]
            [emmy.clerk :as ec]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [nextjournal.clerk :as clerk]))

;; ---------------------------------------------------------------------------
;; The sliders and the tie

(def tied [:a :T :e :l])

(def a-range "The axis' slider: [min max step] in AU." [0.3 4.2 0.01])

(def ranges
  "[min max step] per key. T's range is a's through T = a^(3/2) and l's is
   [0.02, a max], so each source needs only flat clamps: a kernel of
   nested clamps outgrows the JVM's method size once Emmy expands it."
  {:a a-range
   :T [(Math/pow (first a-range) 1.5) (Math/pow (second a-range) 1.5) 0.01]
   :e [0 0.95 0.005]
   :l [0.02 (second a-range) 0.01]})

(def labels {:a "a (AU)" :T "T (years)" :e "e" :l "ℓ (AU)"})

(defn- lo [k] (first (ranges k)))
(defn- hi [k] (second (ranges k)))

(defn- one-minus-e2 [ecc] (e/- 1 (e/square ecc)))

(defn- tied-from-a [a ecc] [a (kf/period a) ecc (kf/semi-latus a ecc)])

(defn from-a
  "e fixed: a held in its range (then T is in its range); l = a (1 - e^2)
   <= a stays below l's max, and is held above l's min by a >= l_min /
   (1 - e^2)."
  [a _T ecc _l]
  (tied-from-a (tie/clamp a (tie/at-least (lo :a) (e// (lo :l) (one-minus-e2 ecc))) (hi :a)) ecc))

(defn from-T
  "e fixed: a = T^(2/3), then as from-a."
  [_a T ecc _l]
  (from-a (kf/axis-of-period T) nil ecc nil))

(defn from-e
  "a fixed: e held where l = a (1 - e^2) stays above l's min."
  [a _T ecc _l]
  (tied-from-a a (tie/clamp ecc (lo :e) (tie/at-most (hi :e) (e/sqrt (e/- 1 (e// (lo :l) a)))))))

(defn from-l
  "e fixed: l held where a = l / (1 - e^2) stays in a's range, then a by
   Emmy's solve-linear."
  [_a _T ecc l]
  (let [w (one-minus-e2 ecc)
        l' (tie/clamp l (tie/at-least (lo :l) (e/* (lo :a) w)) (e/* (hi :a) w))]
    (tied-from-a (kf/axis-of-latus l' ecc) ecc)))

(defn tie
  "The tie kernel: for [a T e l], the solved state for each source in the
   order of `tied` (16 numbers). Compiled unsimplified (its clamps are
   sums of abs that Emmy's simplifier expands): emmy-viewers'
   {:simplify? false} in the browser, `solve` on the JVM."
  [a T ecc l]
  (fn [_] (vec (mapcat #(% a T ecc l) [from-a from-T from-e from-l]))))

(defn solve
  "The tie kernel at a state [a T e l], by raster on the JVM:
   {source [a T e l]}."
  [[a T ecc l]]
  (let [v (vec (binding [raster/*simplify?* false]
                 (raster/value (fn [a T e l] ((tie a T e l) 0)) a T ecc l)))]
    (into {} (map (fn [[k o]] [k (subvec v o (+ o 4))]) (tie/source-block tied)))))

(def readout-index
  {:l 0 :T 1 :T2 2 :a3 3 :b 4 :q 5 :Q 6 :directrix 7 :area 8 :one-minus-e2 9})

(defn readout
  "The page's numbers, by raster: [a(1-e^2), a sqrt a, T^2, a^3, b,
   perihelion a(1-e), aphelion a(1+e), directrix -l/e, pi a b, 1 - e^2]."
  [a T ecc l]
  (fn [_]
    [(kf/semi-latus a ecc) (kf/period a) (e/square T) (e/cube a)
     (e/* a (e/sqrt (one-minus-e2 ecc)))
     (e/* a (e/- 1 ecc)) (e/* a (e/+ 1 ecc))
     (e/- 0 (e// l (tie/at-least ecc 1e-3)))
     (e/* Math/PI a (e/* a (e/sqrt (one-minus-e2 ecc))))
     (one-minus-e2 ecc)]))

(def mu (* 4 Math/PI Math/PI))

(defn start
  "The RK4 start at perihelion, [x y vx vy A], from a, e and the tied l."
  [a ecc l]
  (fn [_] (kf/perihelion-state mu (e/* a (e/- 1 ecc)) l :q-l)))

(defn step
  "One RK4 step of Newton's flow over dt (state [x y vx vy A])."
  [mu' dt]
  (fn [s] (kf/rk4-step mu' dt s)))

(defn orbit-curve
  "rho = l / (1 - e cos t) about the Sun, as (x, y)."
  [l ecc]
  (fn [t] (let [rho (kf/focal-rho l ecc t)] [(e/* rho (e/cos t)) (e/* rho (e/sin t))])))

(defn directrix [l ecc] (fn [_y] (e/- 0 (e// l (tie/at-least ecc 1e-3)))))

;; ---------------------------------------------------------------------------
;; The starting state, by raster

(def mars "Kepler's Mars (Dreyer 1906; Newton's table)." {:a 1.5235 :e 0.09264})

(defn init
  "The starting tied state, from the tie's :a block run by raster on the JVM."
  []
  (zipmap tied (raster/value (fn [a ecc] (tied-from-a a ecc)) (:a mars) (:e mars))))

;; ---------------------------------------------------------------------------
;; The page

(def substeps "RK4 steps per sector." 120)

(def colours {:orbit "#1F6FB4" :sun "#E6A700" :sector-a "#C0392B" :sector-b "#2E86C1"
              :planet "#111111" :directrix "#E67E22" :earth "#9AA5B1"})

(defn- template
  "Fill a quoted browser form: every ?name symbol is replaced by subs."
  [subs form]
  (walk/postwalk (fn [x] (if (and (symbol? x) (contains? subs x)) (subs x) x)) form))

(def caption
  (str "The Sun F sits at a focus. The blue curve is ρ = ℓ/(1 − e cos θ), drawn by Mafs on the raster "
       "backend. The coloured sectors are not drawn from that formula: Newton's inverse-square flow is "
       "integrated by RK4 in a WebAssembly kernel, one period T cut into n equal times, and each sector "
       "is the Sun with the RK4 points of one time. Their areas, the integrated sweep, are printed: equal. "
       "The planet runs along the same RK4 points: fast at perihelion, slow at aphelion. The grey circle "
       "is the Earth's orbit, a = 1, T = 1."))

(defn- body
  "The page: a browser form (reagent with-let) around the four kernels.
   Every ?name in the quoted template is filled from the Clojure side."
  [{:keys [tie-b ro-b start-b step-b !p !s !x]}]
  (let [[tsym tform] tie-b [rsym rform] ro-b [ssym sform] start-b [stsym stform] step-b
        n (fn [i d] (tie/fmt (list 'aget 'r (readout-index i)) d))
        opts {:tie-sym tsym :p !p :s !s :keys tied :extra [] :counter "__keplerSolves"
              :ranges ranges :watch-key ::tie}
        pf (fn [params f] (ev/with-params {:atom !s :params params} f))]
    (template
     {'?ro rsym '?start ssym '?step stsym '?s !s '?x !x '?mu mu '?per substeps
      '?n-l (n :l 4) '?n-e (tie/fmt 'ecc 4) '?n-a (tie/fmt 'a 4) '?n-w (n :one-minus-e2 4)
      '?n-T (n :T 4) '?n-T2 (n :T2 4) '?n-a3 (n :a3 4) '?n-q (n :q 3) '?n-Q (n :Q 3)
      '?n-dir (n :directrix 3)
      '?name (tie/conic-name '(get st :e) {:circle? true})
      '?fmt-area (tie/fmt 'v 5)
      '?leva (leva/sub-panel {:fill true :flat true :title-bar false}
                             (leva/controls {:atom !p :schema (tie/schema tied ranges labels)})
                             (leva/controls {:atom !x :schema {:n {:min 4 :max 24 :step 1 :label "n equal times"}
                                                               :t {:min 0 :max 1 :step 0.001 :label "time t/T"}
                                                               :play {:label "run"}}}))
      '?mafs (mafs/mafs
              {:view-box {:x [-5.2 8.6] :y [-5 5]} :height 460 :zoom true}
              (mafs/cartesian {:subdivisions 1})
              (mafs/circle {:center [0 0] :radius 1 :color (colours :earth) :fill-opacity 0 :stroke-style "dashed"})
              (mafs/of-y {:x (pf [:l :e] directrix) :color (colours :directrix) :style "dashed"})
              'sectors
              (mafs/parametric {:xy (pf [:l :e] orbit-curve) :t [(- Math/PI) Math/PI] :color (colours :orbit) :weight 3})
              (mafs/point {:x 0 :y 0 :color (colours :sun)})
              (mafs/text "F (Sun)" {:x 0 :y 0 :attach "sw" :color (colours :sun)})
              'planet)
      '?sector-a (colours :sector-a) '?sector-b (colours :sector-b) '?planet-c (colours :planet)
      '?caption caption
      '?watch-off (tie/unwatch-form !p)}
     (list 'reagent.core/with-let
           (into [tsym tform rsym rform ssym sform stsym stform]
                 (concat (tie/locals opts)
                         '[cache (atom nil)
                           raf (atom nil) t-last (atom nil) written (atom nil)
                           phase (atom (get @?x :t))
                           _ (reset! raf (js/requestAnimationFrame
                                          (fn step [now]
                                            (let [t (get @?x :t)]
                                              (when (and @written (> (js/Math.abs (- t @written)) 0.002))
                                                (reset! phase t)))
                                            (when (and @t-last (get @?x :play))
                                              (swap! phase (fn [p] (mod (+ p (/ (- now @t-last) 12000)) 1)))
                                              (let [t (alexandria.medium.anim/play @phase 0 1 alexandria.medium.anim/linear)]
                                                (reset! written t)
                                                (swap! ?x assoc :t t)))
                                            (reset! t-last now)
                                            (reset! raf (js/requestAnimationFrame step)))))
                           orbit (fn [a ecc l T n]
                                   (let [k [a ecc l T n]]
                                     (if (= k (first @cache))
                                       (second @cache)
                                       (let [steps (* n ?per)
                                             dt (/ T steps)
                                             s0 (?start (array 0) (array a ecc l))
                                             pts (loop [i 0 s s0 acc (transient [s0])]
                                                   (if (< i steps)
                                                     (let [s2 (?step s (array ?mu dt))]
                                                       (recur (inc i) s2 (conj! acc s2)))
                                                     (persistent! acc)))
                                             out {:pts pts :steps steps}]
                                         (set! (.-__keplerRk4 js/globalThis) (inc (or (.-__keplerRk4 js/globalThis) 0)))
                                         (reset! cache [k out])
                                         out))))]))
           '(let [st @?s
                  a (get st :a) ecc (get st :e) l (get st :l) T (get st :T)
                  n (get @?x :n)
                  r (?ro (array 0) (array a T ecc l))
                  {:keys [pts steps]} (orbit a ecc l T n)
                  areas (mapv (fn [i] (- (aget (nth pts (* (inc i) ?per)) 4) (aget (nth pts (* i ?per)) 4))) (range n))
                  idx (min steps (js/Math.floor (* (get @?x :t) steps)))
                  cur (min (dec n) (js/Math.floor (/ idx ?per)))
                  here (nth pts idx)
                  xy (fn [s] [(aget s 0) (aget s 1)])
                  sectors (into [:<>]
                                (for [i (range (inc cur))
                                      :let [upto (if (= i cur) idx (* (inc i) ?per))]]
                                  [mafs.core/Polygon {:points (into [[0 0]] (map (fn [j] (xy (nth pts j)))) (range (* i ?per) (inc upto)))
                                                      :color (if (even? i) ?sector-a ?sector-b)
                                                      :fill-opacity (if (= i cur) 0.45 0.25)
                                                      :weight 1}]))
                  planet [:<>
                          [mafs.line/Segment {:point1 [0 0] :point2 (xy here) :color ?planet-c :opacity 0.6}]
                          [mafs.core/Point {:x (aget here 0) :y (aget here 1) :color ?planet-c}]]
                  expected (/ (aget r 8) n)
                  spread (- (apply max areas) (apply min areas))]
              [:div {:data-figure "kepler-orbit"
                     :data-areas (js/JSON.stringify (clj->js areas))
                     :data-expected (str expected)
                     :data-rk4-end (str (aget (peek pts) 0) " " (aget (peek pts) 1))
                     :data-rk4-start (str (aget (first pts) 0) " " (aget (first pts) 1))}
               [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                [:div {:style {:flex "1 1 460px" :min-width "320px"}} ?mafs]
                [:div {:style {:flex "0 1 320px" :min-width "270px"}}
                 ?leva
                 ?name
                 (let [tex (str "\\rho = \\frac{\\ell}{1 - e\\cos\\theta} = \\frac{" ?n-l "}{1 - " ?n-e "\\,\\cos\\theta}")]
                   [:div {:data-formula tex} [:f> nextjournal.clerk.render/render-katex tex {:inline? false}]])
                 (let [tex (str "T^2 = " ?n-T2 " = a^3 = " ?n-a3)]
                   [:div {:data-third-law tex} [:f> nextjournal.clerk.render/render-katex tex {:inline? false}]])
                 [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.88em" :line-height 1.5}}
                  [:div "ℓ = a(1 − e²) = " ?n-a " × " ?n-w " = " [:span {:data-l (str (aget r 0))} ?n-l]]
                  [:div "T = a√a = " [:span {:data-T (str (aget r 1))} ?n-T] " years"]
                  [:div "perihelion a(1 − e) = " ?n-q ",  aphelion a(1 + e) = " ?n-Q]
                  [:div "directrix x = −ℓ/e = " ?n-dir]
                  [:div {:style {:margin-top "0.4em"}}
                   n " equal times of T/" n " (RK4, " ?per " steps each), areas swept:"]
                  [:div {:data-area-list "1" :style {:font-size "0.9em"}}
                   (into [:span] (interpose " " (map-indexed (fn [i v] [:span {:style {:color (if (= i cur) "#000" "#777")}} ?fmt-area]) areas)))]
                  [:div "πab/" n " = " (let [v expected] ?fmt-area)
                   ";  spread " [:span {:data-spread (str spread)} (.toExponential spread 1)]]]]]
               [:p {:style {:font-size "0.9em" :opacity 0.85}} ?caption]])
           '(finally
              ?watch-off
              (some-> @raf js/cancelAnimationFrame))))))

(defn widget
  "Kepler's orbit, tied (a, T, e, l), with the equal areas by RK4. An
   emmy-viewers fragment for a notebook after (history-of-math.widgets/install!);
   compiled on the raster backend whatever the caller's binding."
  ([] (widget (init)))
  ([start-state]
   (binding [vc/*backend* :raster]
     (let [s (mapv start-state tied)
           [a T ecc l] s]
       (ev/with-let [!p start-state !s start-state !x {:n 12 :t 0.0 :play true}]
         (body {:tie-b (tie/kernel-binding "tie" tie s)
                :ro-b (tie/kernel-binding "readout" readout s)
                :start-b (tie/kernel-binding "start" start [a ecc l])
                :step-b [(gensym "step") (vc/compiled-fn step [mu 0.01] [0 0 0 0 0] {:simplify? false})]
                :!p !p :!s !s :!x !x}))))))

;; ---------------------------------------------------------------------------
;; The derivation, for the page

(defn derivation
  "Emmy's derivation (alexandria.kepler.focal) as a Clerk column, with the
   grades."
  []
  (apply clerk/col
         (concat
          (for [{:keys [label eqs]} (kf/equations)]
            (apply clerk/col
                   (clerk/html [:div {:style {:font-size "0.85em" :opacity 0.7 :margin-top "0.5em"}} label])
                   (for [q eqs] (ec/->TeX q :simplify? true))))
          [(clerk/html
            (into [:ul {:style {:font-size "0.9em"}}]
                  (for [{:keys [label grade]} (concat (kf/grades) (kf/numeric-grades))]
                    [:li {:data-grade (name grade)} [:code (name grade)] " " label])))])))
