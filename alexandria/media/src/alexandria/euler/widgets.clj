(ns alexandria.euler.widgets
  "The Euler notebook's widgets (widget brief, sections 4 and 5), each an
   emmy-viewers fragment for a Clerk page mounted after
   (history-of-math.widgets/install!), compiled on the raster backend
   whatever the caller's binding.

     basel       S_n = 1 + 1/4 + ... + 1/n^2 against pi^2/6 (Mafs), tied
                 n <-> b = 1/n (the tail bound; alexandria.euler.ties)
     e-limit     (1 + 1/n)^n against e (Mafs), tied n <-> omega = 1/n <->
                 eps = e - (1 + 1/n)^n (inverse: solve-linear start, then
                 Newton steps, one kernel each, iterated on the page)
     cis         e^{ix} on the unit circle (Mafs), tied x <-> cos x <->
                 sin x, the partial sums of the series drawn as Euler's
                 spiral of terms
     polyhedron  the reader picks a solid and cuts off its solid angles
                 (E231) in MathBox: depth t and how many angles k; V, E, F
                 counted live, V - E + F = 2 throughout
     base-steps  E101 ch. VII par. 114-122 stepped one sentence at a time:
                 a^omega = 1 + k omega, k on a slider tied to the base a

   The tie engine is the conics playground's (alexandria.apollonius.
   playground-widget): one kernel returns the solved state for every
   possible source; the page keeps the block of the key the reader moved
   and pushes the others into the Leva panel. Origin guard (`pushing`)
   and epsilon guard (a change within half a slider step of a value just
   pushed is Leva's echo) keep pushed values from re-firing the solver.
   Every number on the page comes from a kernel."
  (:require [alexandria.euler.series :as series]
            [alexandria.euler.ties :as ties]
            [alexandria.euler.polyhedra :as polyhedra]
            [alexandria.raster :as raster]
            [clojure.walk :as walk]
            [emmy.clerk :as ec]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.mathbox.plot :as plot]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [nextjournal.clerk :as clerk]))

;; ---------------------------------------------------------------------------
;; The tie engine (browser forms)

(defn- kernel
  "[sym form]: f compiled by the current backend (unsimplified), called in
   the browser as (sym (array 0) (array ...params))."
  [nm f init-params]
  [(gensym nm) (vc/compiled-fn f init-params [0] {:simplify? false})])

(defn- fmt
  "Browser form: x to n places with a true minus sign."
  ([x] (fmt x 4))
  ([x n] (list 'let ['v x] (list 'if '(js/isFinite v) (list '.replace (list '.toFixed 'v n) "-" "−") "∞"))))

(defn- tie-watch
  "Browser form: watch the panel atom !p; when the reader moves a key of
   `keys`, call (solve src cur) -> solved map, store it in !s and push the
   keys that differ into !p (after the current notification, setTimeout
   0). `half` maps each key to half its slider step. Counts solves in
   globalThis.__eulerSolves."
  [!p !s keys half solve]
  (list 'add-watch !p (keyword (str (gensym "tie")))
        (list 'fn ['_ '_ 'old 'new]
              (list 'when-not '@pushing
                    (list 'let ['src (list 'first (list 'filter '(fn [k] (not= (get old k) (get new k))) keys))
                                'echo? (list 'and 'src '(contains? @pushed src)
                                             (list '<= '(js/Math.abs (- (get new src) (get @pushed src)))
                                                   (list '+ 1e-9 (list 'get half 'src))))]
                          (list 'when '(and src (not echo?))
                                (list 'js/setTimeout
                                      (list 'fn []
                                            (list 'let ['cur (list 'assoc (list 'deref !s) 'src (list 'get (list 'deref !p) 'src))
                                                        'solved (list solve 'src 'cur)
                                                        'shown (list 'deref !p)
                                                        'push '(into {} (filter (fn [[k v]] (> (js/Math.abs (- v (get shown k))) 1e-12)) solved))]
                                                  '(set! (.-__eulerSolves js/globalThis) (inc (or (.-__eulerSolves js/globalThis) 0)))
                                                  (list 'reset! !s (list 'merge (list 'deref !s) 'solved))
                                                  (list 'when '(seq push)
                                                        '(reset! pushing true)
                                                        (list 'swap! !p 'merge 'push)
                                                        '(reset! pushing false)
                                                        '(swap! pushed merge push)
                                                        '(js/setTimeout (fn [] (swap! pushed (fn [m] (apply dissoc m (keys push))))) 600))))
                                      0)))))))

(defn- schema [ranges labels]
  (into {} (map (fn [[k [a b s]]] [k {:min a :max b :step s :label (labels k)}]) ranges)))

(defn- half [ranges] (into {} (map (fn [[k [_ _ s]]] [k (/ s 2)]) ranges)))

(def ^:private box-style
  {:font-family "ui-monospace, monospace" :font-size "0.9em" :line-height 1.6 :margin-top "0.5em"})

(defn- tex-line
  "Browser form: a KaTeX display of the string form `tex`."
  [tex attr]
  [:div attr [:f> 'nextjournal.clerk.render/render-katex tex {:inline? false}]])

;; ---------------------------------------------------------------------------
;; Basel: S_n against pi^2/6, n <-> b

(def basel-terms "Terms the figure carries (n's slider goes this far)." 120)

(def basel-ranges {:n [1 120 1], :b [0.0084 1 1.0E-4]})

(defn- clamp01 [x] (ties/clamp x 0 1))

(defn basel-readout
  "[S_n pi^2/6 gap b lower n]: S_n with term m switched on by
   clamp01(n - m + 1) (n moves continuously), the gap pi^2/6 - S_n, the
   bounds 1/n and 1/(n + 1)."
  [n]
  (fn [_]
    (let [S (reduce e/+ 0 (for [m (range 1 (inc basel-terms))]
                            (e/* (clamp01 (e/- n (dec m))) (/ 1.0 (* m m)))))
          target (e/divide (e/square Math/PI) 6)]
      [S target (e/- target S) (ties/basel-forward n) (ties/basel-lower n) n])))

(defn basel-solve
  "[n b] for each source: from :n (b forward), from :b (n by the inverse,
   held in range, then b forward again)."
  [n b]
  (fn [_]
    (let [n' (ties/clamp (ties/basel-inverse b) 1 basel-terms)]
      [n (ties/basel-forward n) n' (ties/basel-forward n')])))

(defn basel-staircase
  "The partial sums as a staircase: at x in [0, n], S of floor-ish x,
   x continuous (term m switched on between m - 1 and m)."
  [n]
  (fn [x]
    (let [xc (ties/at-most x n)]
      (reduce e/+ 0 (for [m (range 1 (inc basel-terms))]
                      (e/* (clamp01 (e/* 20 (e/- xc (- m 0.95)))) (/ 1.0 (* m m))))))))

(defn basel
  "Euler's partial sums against pi^2/6, with tied n and tail-bound b."
  []
  (binding [vc/*backend* :raster]
    (let [start {:n 10 :b 0.1}]
      (ev/with-let [!p start !s start]
        (let [[rs rf] (kernel "basel-readout" basel-readout [10])
              [ss sf] (kernel "basel-solve" basel-solve [10 0.1])
              solve (list 'fn ['src 'cur]
                          (list 'let ['o (list ss '(array 0) '(array (:n cur) (:b cur)))
                                      'off '(if (= src :n) 0 2)]
                                '{:n (aget o off) :b (aget o (inc off))}))]
          (list 'reagent.core/with-let
                [rs rf ss sf 'pushing '(atom false) 'pushed '(atom {})
                 '_ (tie-watch !p !s [:n :b] (half basel-ranges) solve)]
                (list 'let ['r (list rs '(array 0) (list 'array (ev/get !s :n)))]
                      [:div {:data-figure "euler-basel" :data-n (list 'str '(aget r 5))
                             :data-gap (list 'str '(aget r 2)) :data-b (list 'str '(aget r 3))}
                       [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                        [:div {:style {:flex "1 1 440px" :min-width "300px"}}
                         (mafs/mafs {:height 360 :view-box {:x [-1 41] :y [0.9 1.75]} :preserve-aspect-ratio false}
                                    (mafs/cartesian {:x-axis {:lines 10}, :y-axis {:lines 0.1}})
                                    (mafs/of-x {:y (fn [_] (/ (* Math/PI Math/PI) 6)) :color "#c0392b" :style "dashed"})
                                    (mafs/of-x {:y (ev/with-params {:atom !s :params [:n]} basel-staircase)
                                                :color "#1F6FB4" :weight 3 :min-sampling-depth 10 :max-sampling-depth 14})
                                    (mafs/segment {:point1 [(ev/get !s :n) '(aget r 0)] :point2 [(ev/get !s :n) '(aget r 1)]
                                                   :color "#7B5EA7"})
                                    (mafs/point {:x (ev/get !s :n) :y '(aget r 0) :color "#1F6FB4"})
                                    (mafs/text "π²/6" {:x 38 :y '(aget r 1) :attach "n" :color "#c0392b"}))]
                        [:div {:style {:flex "0 1 300px" :min-width "260px"}}
                         (leva/sub-panel {:fill true :flat true :title-bar false}
                                         (leva/controls {:atom !p :schema (schema basel-ranges {:n "terms n" :b "tail bound b = 1/n"})}))
                         (tex-line (list 'str "S_n = \\sum_{m=1}^{n}\\frac1{m^2} = " (fmt '(aget r 0) 6)
                                         ",\\quad \\frac{\\pi^2}{6} - S_n = " (fmt '(aget r 2) 6)) {})
                         (tex-line (list 'str "\\frac1{n+1} = " (fmt '(aget r 4) 6) " < \\frac{\\pi^2}{6} - S_n < b = \\frac1n = "
                                         (fmt '(aget r 3) 6)) {:data-formula "basel-bound"})
                         [:div {:style box-style}
                          [:div "n = " (fmt '(aget r 5) 2) " terms; Euler summed them to 20 places, 1.64493406684822643647…"]
                          [:div {:style {:opacity 0.7}} "move n or b: the other follows (n = 1/b)."]]]]])
                (list 'finally (list 'remove-watch !p :x))))))))

;; ---------------------------------------------------------------------------
;; e: (1 + 1/n)^n, n <-> omega <-> eps

(def e-ranges
  {:n [1 200 0.01] :omega [0.005 1 0.0001] :eps [0.0068 0.72 0.00001]})

(defn e-readout
  "[(1 + 1/n)^n e eps omega n leading-term]"
  [n]
  (fn [_]
    [(ties/power-n n) (e/exp 1) (ties/e-error n) (ties/omega n) n (e/divide (e/exp 1) (e/* 2 n))]))

(defn e-forward
  "[n omega eps] of n (sources :n and :omega share it)."
  [n]
  (fn [_] [n (ties/omega n) (ties/e-error n)]))

(defn e-curve "y = (1 + 1/x)^x." [x] (ties/power-n (ties/at-least x 0.05)))

(defn e-limit
  "(1 + 1/n)^n against e, n, omega = 1/n and eps tied."
  []
  (binding [vc/*backend* :raster]
    (let [n0 10.0
          start (zipmap [:n :omega :eps] (raster/value (fn [n] ((e-forward n) nil)) n0))]
      (ev/with-let [!p start !s start]
        (let [[rs rf] (kernel "e-readout" e-readout [n0])
              [fs ff] (kernel "e-forward" e-forward [n0])
              [s0 s0f] (kernel "e-start" (fn [eps] (fn [_] [(ties/clamp (e/divide Math/E (e/* 2 eps)) 1 200)])) [0.1])
              [ns nf] (kernel "e-newton" (fn [n eps] (fn [_] [(ties/e-newton n eps)])) [n0 0.1])
              solve (list 'fn ['src 'cur]
                          (list 'let ['n (list 'case 'src
                                               :n '(:n cur)
                                               :omega '(/ 1 (:omega cur))
                                               :eps (list 'reduce (list 'fn ['n '_] (list 'aget (list ns '(array 0) '(array n (:eps cur))) 0))
                                                          (list 'aget (list s0 '(array 0) '(array (:eps cur))) 0)
                                                          (list 'range ties/newton-steps)))
                                      'n '(min 200 (max 1 n))
                                      'o (list fs '(array 0) '(array n))]
                                '{:n (aget o 0) :omega (aget o 1) :eps (aget o 2)}))]
          (list 'reagent.core/with-let
                [rs rf fs ff s0 s0f ns nf 'pushing '(atom false) 'pushed '(atom {})
                 '_ (tie-watch !p !s [:n :omega :eps] (half e-ranges) solve)]
                (list 'let ['r (list rs '(array 0) (list 'array (ev/get !s :n)))]
                      [:div {:data-figure "euler-e" :data-n (list 'str '(aget r 4)) :data-eps (list 'str '(aget r 2))}
                       [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                        [:div {:style {:flex "1 1 440px" :min-width "300px"}}
                         (mafs/mafs {:height 340 :view-box {:x [0 30] :y [1.9 2.8]} :preserve-aspect-ratio false}
                                    (mafs/cartesian {:x-axis {:lines 10}, :y-axis {:lines 0.1}})
                                    (mafs/of-x {:y (fn [_] Math/E) :color "#c0392b" :style "dashed"})
                                    (mafs/of-x {:y e-curve :color "#1F6FB4" :weight 3})
                                    (mafs/segment {:point1 [(ev/get !s :n) '(aget r 0)] :point2 [(ev/get !s :n) '(aget r 1)]
                                                   :color "#7B5EA7"})
                                    (mafs/point {:x (ev/get !s :n) :y '(aget r 0) :color "#1F6FB4"})
                                    (mafs/text "e" {:x 28 :y '(aget r 1) :attach "n" :color "#c0392b"}))]
                        [:div {:style {:flex "0 1 300px" :min-width "260px"}}
                         (leva/sub-panel {:fill true :flat true :title-bar false}
                                         (leva/controls {:atom !p :schema (schema e-ranges {:n "n" :omega "ω = 1/n" :eps "error ε"})}))
                         (tex-line (list 'str "\\left(1 + \\tfrac1n\\right)^{n} = " (fmt '(aget r 0) 6)
                                         ",\\quad e - \\left(1+\\tfrac1n\\right)^n = \\varepsilon = " (fmt '(aget r 2) 6))
                                   {:data-formula "e-error"})
                         (tex-line (list 'str "\\varepsilon \\approx \\frac{e}{2n} = \\frac{e\\,\\omega}{2} = " (fmt '(aget r 5) 6)
                                         ",\\quad \\omega = " (fmt '(aget r 3) 5)) {})
                         [:div {:style box-style}
                          [:div "e = " (fmt '(aget r 1) 12) " (Euler, par. 122: 2.71828182845904523536028)"]
                          [:div {:style {:opacity 0.7}} "move n, ω or ε: the other two follow."]]]]])
                (list 'finally nil)))))))

;; ---------------------------------------------------------------------------
;; e^{ix}: x <-> cos x <-> sin x

(def cis-ranges {:x [-3.14 3.14 0.01] :cos [-1 1 0.001] :sin [-1 1 0.001]})

(defn cis-solve
  "For each source the state [x c s]: from :x forward, from :cos (the sign
   of sin kept), from :sin (the sign of cos kept)."
  [x c s]
  (fn [_]
    (let [xc (ties/x-of-cos c s) xs (ties/x-of-sin s c)]
      [x (e/cos x) (e/sin x)
       xc (e/cos xc) (e/sin xc)
       xs (e/cos xs) (e/sin xs)])))

(def cis-terms "Terms of Euler's spiral drawn." 12)

(defn cis-readout
  "[cos x, sin x, x, then the partial sums re_k im_k for k = 0..cis-terms]
   of 1 + ix + (ix)^2/2! + ..."
  [x]
  (fn [_]
    (into [(e/cos x) (e/sin x) x]
          (mapcat (fn [k] ((series/spiral x) [k])) (range (inc cis-terms))))))

(defn- cis-case
  "Browser form: the point e^{ix} named, from x (r 2), cos (r 0), sin (r 1):
   the four points on the axes (x = pi is Euler's e^{i pi} = -1), else the
   quadrant."
  []
  '(let [x (aget r 2) c (aget r 0) s (aget r 1) near (fn [a b] (< (js/Math.abs (- a b)) 0.006))]
     (cond (near x 0) "x = 0: e^{i·0} = 1, the starting point"
           (near x (/ js/Math.PI 2)) "x = π/2: e^{iπ/2} = i, a quarter turn"
           (near x (- (/ js/Math.PI 2))) "x = −π/2: e^{−iπ/2} = −i"
           (or (near x js/Math.PI) (near x (- js/Math.PI)) (> (js/Math.abs x) 3.135)) "x = π: e^{iπ} = −1, half a turn"
           (and (> c 0) (> s 0)) "first quadrant: cos x > 0, sin x > 0"
           (and (< c 0) (> s 0)) "second quadrant: cos x < 0, sin x > 0"
           (and (< c 0) (< s 0)) "third quadrant: cos x < 0, sin x < 0"
           :else "fourth quadrant: cos x > 0, sin x < 0")))

(defn cis
  "e^{ix} = cos x + i sin x on the unit circle, x, cos x, sin x tied; the
   formula, both series and the atan2 inverse shown with the current
   values substituted, the point named live."
  []
  (binding [vc/*backend* :raster]
    (let [x0 2.0 start {:x x0 :cos (Math/cos x0) :sin (Math/sin x0)}]
      (ev/with-let [!p start !s start]
        (let [[rs rf] (kernel "cis-readout" cis-readout [x0])
              [ss sf] (kernel "cis-solve" cis-solve [x0 (Math/cos x0) (Math/sin x0)])
              solve (list 'fn ['src 'cur]
                          (list 'let ['o (list ss '(array 0) '(array (:x cur) (:cos cur) (:sin cur)))
                                      'off '(case src :x 0 :cos 3 :sin 6)]
                                '{:x (aget o off) :cos (aget o (+ off 1)) :sin (aget o (+ off 2))}))
              pt (fn [k] [(list 'aget 'r (+ 3 (* 2 k))) (list 'aget 'r (+ 4 (* 2 k)))])
              x (fmt '(aget r 2) 3)]
          (list 'reagent.core/with-let
                [rs rf ss sf 'pushing '(atom false) 'pushed '(atom {})
                 '_ (tie-watch !p !s [:x :cos :sin] (half cis-ranges) solve)]
                (list 'let ['r (list rs '(array 0) (list 'array (ev/get !s :x)))
                            'case-name (cis-case)]
                      [:div {:data-figure "euler-cis" :data-x (list 'str '(aget r 2))
                             :data-point (list 'str '(aget r 0) " " '(aget r 1)) :data-name 'case-name}
                       [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                        [:div {:style {:flex "1 1 420px" :min-width "300px"}}
                         (mafs/mafs {:height 420 :view-box {:x [-2.6 1.6] :y [-1.4 2.6]}}
                                    (mafs/cartesian)
                                    (mafs/circle {:center [0 0] :radius 1 :color "#8A9BB0" :fill-opacity 0})
                                    (into [:<>]
                                          (for [k (range cis-terms)]
                                            (mafs/segment {:point1 (pt k) :point2 (pt (inc k))
                                                           :color (["#1F6FB4" "#2f8a3e" "#c8913a" "#7B5EA7"] (mod (inc k) 4))
                                                           :opacity 0.8})))
                                    (mafs/segment {:point1 [0 0] :point2 ['(aget r 0) '(aget r 1)] :color "#c0392b"})
                                    (mafs/segment {:point1 ['(aget r 0) 0] :point2 ['(aget r 0) '(aget r 1)]
                                                   :color "#2f8a3e" :style "dashed"})
                                    (mafs/segment {:point1 [0 0] :point2 ['(aget r 0) 0] :color "#1F6FB4" :weight 4})
                                    (mafs/point {:x '(aget r 0) :y '(aget r 1) :color "#c0392b"})
                                    (mafs/text "e^{ix}" {:x '(aget r 0) :y '(aget r 1) :attach "ne" :color "#c0392b"}))]
                        [:div {:style {:flex "0 1 320px" :min-width "260px"}}
                         (leva/sub-panel {:fill true :flat true :title-bar false}
                                         (leva/controls {:atom !p :schema (schema cis-ranges {:x "x (radians)" :cos "cos x" :sin "sin x"})}))
                         [:div {:data-name "cis" :style {:font-weight 600 :color "#c0392b" :margin "0.4em 0"}} 'case-name]
                         (tex-line (list 'str "e^{i\\,(" x ")} = " (fmt '(aget r 0) 4) " + i\\,(" (fmt '(aget r 1) 4) ")")
                                   {:data-formula "cis"})
                         (tex-line (list 'str "\\cos x = 1 - \\frac{x^2}{2!} + \\frac{x^4}{4!} - \\cdots,\\quad x = " x
                                         "\\ \\Rightarrow\\ " (fmt '(aget r 0) 4)) {:data-formula "cos-series"})
                         (tex-line (list 'str "\\sin x = x - \\frac{x^3}{3!} + \\frac{x^5}{5!} - \\cdots = " (fmt '(aget r 1) 4)) {})
                         (tex-line (list 'str "x = \\operatorname{atan2}(" (fmt '(aget r 1) 4) ",\\, " (fmt '(aget r 0) 4) ") = " x)
                                   {:data-formula "atan2"})
                         [:div {:style box-style}
                          [:div "the spiral: 1, ix, (ix)²/2!, … laid end to end (east, north, west, south);"]
                          [:div "after " cis-terms " terms it ends at (" (fmt (first (pt cis-terms)) 4) ", " (fmt (second (pt cis-terms)) 4) ")"]
                          [:div {:style {:opacity 0.7}} "move x, cos x or sin x: the others follow; x = π gives e^{iπ} = −1."]]]]])
                (list 'finally nil)))))))

;; ---------------------------------------------------------------------------
;; V - E + F on a solid the reader picks and cuts

(def solids
  "The solids offered, in the picker's order."
  (concat polyhedra/platonic [(polyhedra/prism 5) (polyhedra/pyramid 5)]))

(def cut-data
  "Per solid: {:id :name :segments :degree :counts :f}."
  (delay (mapv (fn [s] (merge (select-keys s [:id :name]) (ties/cut-solid s))) solids)))

(defn- prefix [xs] (vec (reductions + 0 xs)))

(defn- solid-scene
  "The MathBox scene: the cut solid's segments, from kernel output `q`,
   shown while solid i is picked (`sel`, the browser symbol bound to the
   current pick)."
  [i segments]
  (list 'when (list '= 'sel i)
        (into [:<>]
              (for [[a b] segments]
                (plot/line {:coords [[(list 'aget 'q (* 3 a)) (list 'aget 'q (+ 1 (* 3 a))) (list 'aget 'q (+ 2 (* 3 a)))]
                                     [(list 'aget 'q (* 3 b)) (list 'aget 'q (+ 1 (* 3 b))) (list 'aget 'q (+ 2 (* 3 b)))]]
                            :color "#1d1f21" :width 3})))))

(def ^:private greek-names
  "The solids' names as the Greeks gave them (Euclid XIII; prism and
   pyramid from Euclid XI-XII), by id."
  {:tetrahedron "τετράεδρον" :cube "κύβος" :octahedron "ὀκτάεδρον"
   :dodecahedron "δωδεκάεδρον" :icosahedron "εἰκοσάεδρον"
   :prism-5 "πρίσμα" :pyramid-5 "πυραμίς"})

(defn polyhedron
  "Pick a solid; cut off its solid angles (k of them, depth t) in MathBox.
   V, E, F counted live; Euler's V - E + F = 2 through every cut, the
   bookkeeping written out with the degrees of the angles cut so far, and
   the solid named live (its Greek name, then how many angles are off)."
  []
  (binding [vc/*backend* :raster]
    (let [data @cut-data
          ks (mapv (fn [{:keys [f]}] (kernel "cut" (fn [t k] (fn [_] (f t k))) [0.4 0])) data)
          table (mapv (fn [{:keys [id counts degree name]}]
                        {:name name :greek (str name ": " (greek-names id name))
                         :V (:V counts) :E (:E counts) :F (:F counts) :pre (prefix degree)
                         :degree degree :nv (count degree)})
                      data)]
      (ev/with-let [!c {:solid 1 :t 0.45 :k 0}]
        (list 'reagent.core/with-let (vec (mapcat identity ks))
              (list 'let ['i (ev/get !c :solid)
                          'row (list 'get table 'i)
                          'kk (list 'min '(:nv row) (list 'js/Math.round (ev/get !c :k)))
                          'sum-d '(get (:pre row) kk)
                          'V '(+ (:V row) (- sum-d kk)) 'E '(+ (:E row) sum-d) 'F '(+ (:F row) kk)
                          'ds '(subvec (:degree row) 0 kk)
                          'plus '(fn [f] (if (zero? kk) "" (apply str (map (fn [d] (str " + " (f d))) ds))))
                          'title '(cond (zero? kk) (:greek row)
                                        (= kk (:nv row)) (str "the " (:name row) " with every solid angle cut off (" kk ")")
                                        :else (str "the " (:name row) " with " kk " of its " (:nv row) " solid angles cut off"))
                          'tex '(str "\\begin{aligned}V &= " (:V row) (plus (fn [d] (str "(" d " - 1)"))) " = " V
                                     "\\\\ E &= " (:E row) (plus str) " = " E
                                     "\\\\ F &= " (:F row) (plus (fn [_] 1)) " = " F
                                     "\\\\ V - E + F &= " V " - " E " + " F " = " (+ (- V E) F) "\\end{aligned}")]
                    [:div {:data-figure "euler-polyhedron" :data-chi '(str (+ (- V E) F)) :data-vef '(str V " " E " " F)
                           :data-name 'title}
                     [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                      [:div {:style {:flex "1 1 440px" :min-width "300px"}}
                       (plot/scene {:range [[-1.8 1.8] [-1.8 1.8] [-1.8 1.8]] :scale [1 1 1]
                                    :camera [2.2 -3.2 1.6] :axes [] :grids []
                                    :container {:style {:height "440px" :width "100%"}}}
                                   (into [:<>]
                                         (for [[i [ksym _]] (map-indexed vector ks)]
                                           (list 'let ['sel (ev/get !c :solid)
                                                       'q (list ksym '(array 0) (list 'array (ev/get !c :t) (ev/get !c :k)))]
                                                 (solid-scene i (:segments (data i)))))))]
                      [:div {:style {:flex "0 1 320px" :min-width "260px"}}
                       (into [:div {:style {:display "flex" :flex-wrap "wrap" :gap "0.3em" :margin-bottom "0.5em"}}]
                             (for [[i {:keys [name]}] (map-indexed vector table)]
                               [:button {:data-solid i
                                         :on-click (list 'fn [] (list 'swap! !c 'assoc :solid i :k 0))
                                         :style {:font-size "0.85em" :padding "0.1em 0.5em" :border-radius "0.3em"
                                                 :border "1px solid currentColor" :cursor "pointer"
                                                 :background (list 'if (list '= 'i i) "rgba(192,57,43,0.15)" "transparent")
                                                 :color "inherit"}}
                                name]))
                       [:div {:data-name "solid" :style {:font-weight 600 :color "#c0392b" :margin "0.3em 0"}} 'title]
                       (leva/sub-panel {:fill true :flat true :title-bar false}
                                       (leva/controls {:atom !c :schema {:k {:min 0 :max 20 :step 1 :label "solid angles cut, k"}
                                                                         :t {:min 0.05 :max 0.95 :step 0.01 :label "cut depth t"}}}))
                       [:div {:data-formula "vef"} [:f> 'nextjournal.clerk.render/render-katex 'tex {:inline? false}]]
                       [:div {:style box-style}
                        [:div "In Euler's letters: S + H = " 'V " + " 'F " = " '(+ V F) " = A + 2 = " 'E " + 2"]
                        [:div "each solid angle of degree d cut off adds d − 1 anguli solidi, d acies, 1 hedra"]
                        [:div {:style {:opacity 0.6}} "drag: rotate · scroll: zoom · right-drag: pan"]]]]])
              (list 'finally nil))))))

;; ---------------------------------------------------------------------------
;; E101 ch. VII, par. 114-122, stepped

(def base-sentences
  "Euler's chapter VII in his order, one sentence per step (our English)."
  ["§114. Since a⁰ = 1, when the exponent of a exceeds 0 by an infinitely small ω, the power exceeds 1 by an infinitely small amount."
   "Write a^ω = 1 + ψ; ψ is infinitely small with ω, so put ψ = kω, with k a number that depends on a."
   "§115. Then a^{jω} = (1 + kω)^j for any j; put j = z/ω, an infinitely large number, so that jω = z is finite."
   "§116. So a^z = (1 + kz/j)^j; expanding by the binomial theorem, with j infinite, a^z = 1 + kz/1 + k²z²/1·2 + k³z³/1·2·3 + …"
   "§122. The base can be chosen so that k = 1: the series 1 + 1/1 + 1/1·2 + 1/1·2·3 + … gives it, 2.71828182845904523536028…, which we write e."])

(def base-ranges {:k [0.1 2.5 0.001] :a [1.11 12.2 0.001]})

(defn base-readout
  "[a k a^omega 1 + k omega e^k omega]: omega = 0.001 stands for the
   infinitely small; the first terms of the series at z = 1."
  [k]
  (fn [_]
    (let [a (ties/a-of-k k) w 0.001]
      [a k (e/expt a w) (e/+ 1 (e/* k w)) (e/exp k) w
       (reduce e/+ 0 (for [m (range 12)] (e/divide (e/expt k m) (reduce * 1 (range 1 (inc m))))))])))

(defn base-solve [a k] (fn [_] [(ties/a-of-k k) k a (ties/k-of-a a)]))

(defn base-steps
  "Chapter VII stepped: the sentence highlighted, the curve a^x, its
   tangent at 0 of slope k, and from §115 the series, k and a tied."
  []
  (binding [vc/*backend* :raster]
    (let [start {:k 1.0 :a Math/E}]
      (ev/with-let [!p start !s start !c {:step 0}]
        (let [[rs rf] (kernel "base-readout" base-readout [1.0])
              [ss sf] (kernel "base-solve" base-solve [Math/E 1.0])
              solve (list 'fn ['src 'cur]
                          (list 'let ['o (list ss '(array 0) '(array (:a cur) (:k cur)))
                                      'off '(if (= src :k) 0 2)]
                                '{:a (aget o off) :k (aget o (inc off))}))
              step (ev/get !c :step)
              n (count base-sentences)
              go (fn [form] (list 'fn [] (list 'swap! !c 'assoc :step form)))
              shown (fn [i frag] (list 'when (list '>= step i) frag))]
          (list 'reagent.core/with-let
                [rs rf ss sf 'pushing '(atom false) 'pushed '(atom {})
                 '_ (tie-watch !p !s [:k :a] (half base-ranges) solve)]
                (list 'let ['r (list rs '(array 0) (list 'array (ev/get !s :k)))]
                      [:div {:data-figure "euler-base" :data-step (list 'inc step) :data-a '(str (aget r 0))}
                       [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                        [:div {:style {:flex "1 1 420px" :min-width "300px"}}
                         (mafs/mafs {:height 380 :view-box {:x [-1.5 2] :y [-0.3 4]}}
                                    (mafs/cartesian)
                                    (mafs/of-x {:y (ev/with-params {:atom !s :params [:a]} (fn [a] (fn [x] (e/expt a x))))
                                                :color "#1F6FB4" :weight 3})
                                    (mafs/point {:x 0 :y 1 :color "#1d1f21"})
                                    (shown 1 (mafs/of-x {:y (ev/with-params {:atom !s :params [:k]} (fn [k] (fn [x] (e/+ 1 (e/* k x)))))
                                                         :color "#c0392b" :style "dashed"}))
                                    (shown 3 (mafs/of-x {:y (ev/with-params {:atom !s :params [:k]}
                                                                            (fn [k] (fn [x] (reduce e/+ 0 (for [m (range 4)] (e/divide (e/expt (e/* k x) m) (reduce * 1 (range 1 (inc m)))))))))
                                                         :color "#2f8a3e" :style "dashed"}))
                                    (shown 4 (mafs/of-x {:y (fn [x] (e/exp x)) :color "#7B5EA7" :opacity 0.6})))]
                        [:div {:style {:flex "0 1 320px" :min-width "260px"}}
                         [:div {:style {:margin-bottom "0.4em"}}
                          [:button {:data-act "prev" :on-click (go (list 'max 0 (list 'dec step)))} "◀"]
                          [:button {:data-act "next" :on-click (go (list 'min (dec n) (list 'inc step)))} "▶▶"]
                          [:span {:style {:opacity 0.6 :font-size "0.85em"}} " step " (list 'inc step) " of " n]]
                         (into [:ol {:style {:margin 0 :padding-left "2em" :font-size "0.93em" :line-height 1.45}}]
                               (for [[i text] (map-indexed vector base-sentences)]
                                 [:li {:data-step (inc i) :on-click (go i)
                                       :style {:cursor "pointer" :padding "0.2em 0.4em" :margin-bottom "0.3em"
                                               :border-left (list 'if (list '= step i) "3px solid #c0392b" "3px solid transparent")
                                               :opacity (list 'if (list '<= i step) 1 0.45)}}
                                  text]))
                         (shown 1 (leva/sub-panel {:fill true :flat true :title-bar false}
                                                  (leva/controls {:atom !p :schema (schema base-ranges {:k "k" :a "base a"})})))
                         (shown 1 (tex-line (list 'str "a^{\\omega} = " (fmt '(aget r 2) 7) " \\approx 1 + k\\omega = " (fmt '(aget r 3) 7)
                                                  ",\\ \\omega = 0.001,\\ a = " (fmt '(aget r 0) 5)) {:data-formula "base"}))
                         (shown 3 (tex-line (list 'str "a = 1 + k + \\frac{k^2}{2} + \\frac{k^3}{6} + \\cdots = " (fmt '(aget r 6) 8)) {}))]]])
                (list 'finally nil)))))))

;; ---------------------------------------------------------------------------
;; The derivations, for the page

(def terms-sentences
  "E230 par. 6-7 (our English from Euler's Latin), one kind of boundary per
   step, on the solid the reader picked."
  ["§6. A solid bounded by planes is known by its boundary, as a plane figure is known by its perimeter."
   "The plane figures that make up the boundary are called hedrae (faces, H)."
   "Where two hedrae meet along their sides, linear boundaries arise; finding no name for them among the writers on solids, Euler calls them acies (edges, A)."
   "The points where three or more hedrae meet are called solid angles, anguli solidi (vertices, S)."
   "§7. So a solid has three kinds of boundary, points, lines and surfaces: S + H = A + 2."])

(def ^:private face-colours ["#5AA0D0" "#E67E22" "#2f8a3e" "#7B5EA7" "#C0392B" "#B8860B"])

(defn- triangle
  "The triangle abc as a surface over [0 1]^2: a + u (b - a) + u v (c - b)."
  [a b c]
  (fn [[u v]] (mapv (fn [ai bi ci] (e/+ ai (e/* u (- bi ai)) (e/* u v (- ci bi)))) a b c)))

(defn- cyclic
  "The vertices of face f (index set) in order round its centroid."
  [vs f]
  (let [ps (mapv vs f)
        c (mapv #(/ % (count ps)) (apply mapv + ps))
        sub (fn [p q] (mapv - p q))
        cross (fn [[a b c] [x y z]] [(- (* b z) (* c y)) (- (* c x) (* a z)) (- (* a y) (* b x))])
        dot (fn [p q] (reduce + (map * p q)))
        u (sub (first ps) c)
        nrm (first (filter #(> (dot % %) 1e-9) (map #(cross u (sub % c)) (rest ps))))
        w (cross nrm u)]
    (mapv vs (sort-by (fn [i] (let [d (sub (vs i) c)] (Math/atan2 (dot d w) (dot d u)))) f))))

(defn terms-steps
  "E230 par. 6-7 stepped on the cube in MathBox: the solid, then its
   hedrae, its acies, its anguli solidi, each counted as it appears, then
   S + H = A + 2."
  []
  (binding [vc/*backend* :raster]
    (let [cube (first (filter #(= :cube (:id %)) solids))
          vsd (mapv (fn [v] (mapv double v)) (:vertices cube))
          faces (mapv #(cyclic vsd (vec %)) (:faces cube))
          {:keys [V E F]} (polyhedra/counts cube)
          n (count terms-sentences)]
      (ev/with-let [!c {:step 0}]
        (let [step (ev/get !c :step)
              go (fn [form] (list 'fn [] (list 'swap! !c 'assoc :step form)))
              shown (fn [i frag] (list 'when (list '>= step i) frag))]
          [:div {:data-figure "euler-terms" :data-step (list 'inc step)}
           [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
            [:div {:style {:flex "1 1 420px" :min-width "300px"}}
             (plot/scene {:range [[-1.6 1.6] [-1.6 1.6] [-1.6 1.6]] :scale [1 1 1]
                          :camera [2.2 -3.0 1.8] :axes [] :grids []
                          :container {:style {:height "400px" :width "100%"}}}
                         [:<> (list 'if (list '>= step 1) (into [:<>] (for [[j f] (map-indexed vector faces) [b c] (partition 2 1 (rest f))] (plot/parametric-surface {:f (triangle (first f) b c), :u [0 1], :v [0 1], :u-samples 2, :v-samples 2, :shaded? false, :color (face-colours (mod j 6)), :opacity 0.55}))) (into [:<>] (for [f faces [b c] (partition 2 1 (rest f))] (plot/parametric-surface {:f (triangle (first f) b c), :u [0 1], :v [0 1], :u-samples 2, :v-samples 2, :shaded? false, :color "#8A9BB0", :opacity 0.25}))))]
                         (shown 2 (into [:<>] (for [[a b] (:edges cube)]
                                                (plot/line {:coords [(vsd a) (vsd b)] :color "#1d1f21" :width 5}))))
                         (shown 3 (into [:<>] (for [p vsd] (plot/point {:coords p :size 16 :color "#c0392b"})))))]
            [:div {:style {:flex "0 1 320px" :min-width "260px"}}
             [:div {:style {:margin-bottom "0.4em"}}
              [:button {:data-act "prev" :on-click (go (list 'max 0 (list 'dec step)))} "◀"]
              [:button {:data-act "next" :on-click (go (list 'min (dec n) (list 'inc step)))} "▶▶"]
              [:span {:style {:opacity 0.6 :font-size "0.85em"}} " step " (list 'inc step) " of " n]]
             (into [:ol {:style {:margin 0 :padding-left "2em" :font-size "0.93em" :line-height 1.45}}]
                   (for [[i text] (map-indexed vector terms-sentences)]
                     [:li {:data-step (inc i) :on-click (go i)
                           :style {:cursor "pointer" :padding "0.2em 0.4em" :margin-bottom "0.3em"
                                   :border-left (list 'if (list '= step i) "3px solid #c0392b" "3px solid transparent")
                                   :opacity (list 'if (list '<= i step) 1 0.45)}}
                      text]))
             [:div {:style box-style}
              (shown 1 [:div "hedrae H = " F])
              (shown 2 [:div "acies A = " E])
              (shown 3 [:div "anguli solidi S = " V])
              (shown 4 [:div {:data-formula "e230"} "S + H = " V " + " F " = " (+ V F) " = A + 2 = " E " + 2"])
              [:div {:style {:opacity 0.6}} "drag: rotate · scroll: zoom · right-drag: pan"]]]]])))))

(defn derivation
  "Emmy's derivation of tie `id` (:basel :e :cis :cut) as ->TeX, then its
   grades."
  [id]
  (let [extra (case id :e [(ties/slope-grade)] [])]
    (apply clerk/col
           (concat
            (for [{:keys [label eqs]} (get (ties/equations) id)]
              (apply clerk/col
                     (clerk/html [:div {:style {:font-size "0.85em" :opacity 0.7 :margin-top "0.5em"}} label])
                     (for [q eqs] (ec/->TeX q))))
            [(clerk/html
              (into [:ul {:style {:font-size "0.9em"}}]
                    (for [{:keys [label grade]} (concat (get (ties/grades) id) extra)]
                      [:li {:data-grade (name grade)} [:code (name grade)] " " label])))]))))

(defn base-derivation []
  (clerk/html
   (into [:ul {:style {:font-size "0.9em"}}]
         (for [{:keys [label grade]} (ties/base-grades)]
           [:li {:data-grade (name grade)} [:code (name grade)] " " label]))))

(defn kernel-forms
  "Every (js/Function. \"fb\" glue) form in a built fragment."
  [form]
  (let [acc (volatile! [])]
    (walk/postwalk (fn [x]
                     (when (and (seq? x) (= 'js/Function. (first x)) (= "fb" (second x)))
                       (vswap! acc conj (nth x 2)))
                     x)
                   form)
    @acc))
