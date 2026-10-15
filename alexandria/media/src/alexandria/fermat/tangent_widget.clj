(ns alexandria.fermat.tangent-widget
  "The Descartes-Fermat widgets for the notebook (widget brief, sections 2
   and 5), emmy-viewers fragments on the raster backend.

   1. `tangent-normal`: a movable point B on Fermat's parabola y^2 = p x
      (vertex D at the origin, diameter DC along x, C the foot of the
      ordinate BC), its tangent BE by adequality (CE = 2 CD) and its normal
      BP by Descartes' double root (DP = CD + p/2). The tied state
      {:p :d :ce :np} (d = CD, ce = CE, np = DP), every key a Leva control:

        d moves (slider or dragging B)   p fixed; ce = 2d, np = d + p/2
        ce moves                         p fixed; d = ce/2, then np
        np moves                         p fixed; d = np - p/2, then ce
        p moves                          the point's CD fixed; np = d + p/2

      The maps are alexandria.fermat.tie's (Emmy derived them and graded
      forward-then-inverse the identity). Fermat's excess E is a second
      control, untied: the chord from B to the point of ordinate b + E,
      whose CE = 2 CD + b E / p turns into the tangent as E goes to 0. A
      third, Descartes' circle about the point at DP + offset, shows two
      meetings with the curve; at offset 0 they are one (the discriminant
      is a perfect square in v, zero only there).

   2. `loci-panel`: Fermat's Isagoge; the reader picks the equation and
      moves D, B, Z and the ratio k; the curve is drawn, the general form
      and its invariant beta^2 - 4 alpha gamma come from a kernel, and the
      name (line, parabola, hyperbola, ellipse, circle) is read off them
      live (alexandria.fermat.tie/general-form, invariants).

   Every curve and number is an Emmy function compiled by
   emmy.viewer.compile/*backend* :raster (bound here): Mafs plots through
   emmy-viewers, and kernels the page calls itself (`tie`, `readout`,
   `loci-readout`). Origin and epsilon guards as in the Apollonius
   playground: a push into the panel never re-fires the solver."
  (:require [alexandria.fermat.tie :as tie]
            [alexandria.raster :as raster]
            [clojure.walk :as walk]
            [emmy.clerk :as ec]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [nextjournal.clerk :as clerk]
            [alexandria.apollonius.conics :as conics]))

;; ---------------------------------------------------------------------------
;; Helpers (branch-free, numbers and symbols alike)

(defn- at-least [x lo] (e/* 1/2 (e/+ x lo (e/abs (e/- x lo)))))
(defn- at-most [x hi] (e/* 1/2 (e/- (e/+ x hi) (e/abs (e/- x hi)))))
(defn- clamp [x lo hi] (at-most (at-least x lo) hi))

(defn- fmt
  "Browser form: x to n places with a true minus sign."
  ([x] (fmt x 2))
  ([x n] (list 'let ['v x] (list 'if '(js/isFinite v) (list '.replace (list '.toFixed 'v n) "-" "−") "—"))))

(defn- kernel-binding
  "[sym form]: f compiled by the current backend, called in the browser as
   (sym (array 0) (array ...params))."
  [nm f init-params]
  [(gensym nm) (vc/compiled-fn f init-params [0] {:simplify? false})])

(defn kernel-forms
  "Every (js/Function. \"fb\" glue) form in a built fragment: the compiled
   kernels it ships."
  [form]
  (let [acc (volatile! [])]
    (walk/postwalk (fn [x]
                     (when (and (seq? x) (= 'js/Function. (first x)) (= "fb" (second x)))
                       (vswap! acc conj (nth x 2)))
                     x)
                   form)
    @acc))

;; ---------------------------------------------------------------------------
;; 1. The tie: tangent and normal

(def ranges
  "Each key's slider range [min max step]; the tie keeps every key in it.
   c is Apollonius' excess in y^2 = p x + c x^2 (the playground's letter)."
  {:p [0.2 3 0.01] :c [-0.9 1.5 0.01] :d [0.05 3 0.01] :ce [0.1 8 0.01] :np [0.1 6 0.01]})

(def tied [:p :c :d :ce :np])

(defn- lo
  "The range's low end as an exact rational: Emmy's simplifier divides
   polynomials exactly and refuses float remainders."
  [k] (rationalize (first (ranges k))))
(defn- hi [k] (rationalize (second (ranges k))))

(defn- d-window
  "[lo hi] of CD on the conic (p, k): CD, CE and DP all in range. CE and DP
   grow with CD (dCE/dCD = 2((p + k d)^2 + k^2 d^2) / (p + 2kd)^2 > 0), so
   the window is the inverse maps at the ends of their ranges."
  [p k]
  [(at-least (at-least (lo :d) (tie/conic-d-from-ce p k (lo :ce))) (tie/conic-d-from-np p k (lo :np)))
   (at-most (at-most (hi :d) (tie/conic-d-from-ce p k (hi :ce))) (tie/conic-d-from-np p k (hi :np)))])

(defn- from-d* [p k d] (let [[a b] (d-window p k) d' (clamp d a b) [ce np] (tie/conic-forward p k d')] [p k d' ce np]))

(defn- from-d [p k d _ce _np] (from-d* p k d))
(defn- from-ce [p k _d ce _np] (from-d* p k (tie/conic-d-from-ce p k ce)))
(defn- from-np [p k _d _ce np] (from-d* p k (tie/conic-d-from-np p k np)))
(defn- from-p
  "p or k moved: the curve changes, CD held (pulled back into its window
   only when CE or DP would leave their ranges)."
  [p k d _ce _np]
  (from-d* p k d))

(def source-block
  "Where each source's state starts in the output of `tie-kernel`."
  {:p 0 :c 5 :d 10 :ce 15 :np 20})

(defn tie-kernel
  "For the state [p k d ce np], the solved state for each source in the
   order of source-block (25 numbers)."
  [p k d ce np]
  (fn [_]
    (vec (concat (from-p p k d ce np) (from-p p k d ce np) (from-d p k d ce np)
                 (from-ce p k d ce np) (from-np p k d ce np)))))

(def readout-index
  {:b 0 :ce 1 :np 2 :chord-ce 3 :qx 4 :qy 5 :v 6 :r 7 :delta 8 :x1 9 :y1 10 :x2 11 :y2 12
   :ec-cp 13 :bc2 14 :cp 15 :k 16 :q 17})

(defn readout
  "Every number on screen for [p k d ex off] on the conic y^2 = p x + k x^2:
   b, CE, DP, Fermat's chord to the point Q at abscissa CD + E (its CE and
   Q), Descartes' centre v = DP + off and radius, the discriminant there,
   the two meetings (x1, y1) (x2, y2) of that circle with the conic, EC.CP
   and BC^2 (equal: the normal), CP, k, and Fermat's quotient p + 2k CD."
  [p k d ex off]
  (fn [_]
    (let [f (fn [x] (tie/symptom p k x))
          y (fn [x] (e/sqrt (at-least (f x) 0)))
          b (y d)
          [ce np] (tie/conic-forward p k d)
          xq (at-least (e/+ d ex) 0)
          bq (y xq)
          v (e/+ np off)
          s2 (e/+ (e/square (e/- d v)) (f d))
          [a2 a1] (tie/conic-circle-quadratic p k v s2)
          delta (tie/conic-discriminant p k d v)
          sq (e/sqrt (at-least delta 0))
          x1 (e// (e/+ (e/- 0 a1) sq) (e/* 2 a2))
          x2 (e// (e/- (e/- 0 a1) sq) (e/* 2 a2))]
      [b ce np (tie/conic-chord-ce b bq ex) xq bq v (e/sqrt s2) delta x1 (y x1) x2 (y x2)
       (e/* ce (e/- np d)) (f d) (e/- np d) k (e/+ p (e/* 2 k d))])))

(defn conic-curve
  "y^2 = p x + k x^2 swept by the line from the vertex D of slope 1/t:
   x = p t^2 / (1 - k t^2), y = p t / (1 - k t^2), t = tan(u smax), u in
   [-1, 1]. smax stops x at 7 (on the hyperbola short of the asymptote's
   direction t^2 = 1/k; on the ellipse it runs on to the far vertex)."
  [p k]
  (fn [u]
    (let [X 7
          smax (e/atan (e/sqrt (e// X (at-least (e/+ p (e/* k X)) 1e-9))))
          t (e/tan (e/* u smax))
          den (e/- 1 (e/* k (e/square t)))]
      [(e// (e/* p (e/square t)) den) (e// (e/* p t) den)])))

(defn- start-state
  "{:p :c :d :ce :np} for a point inside every range: CE and DP from the
   forward map (alexandria.fermat.tie/conic-forward) at exact rationals.
   The page's numbers after the first frame all come from the wasm kernels;
   compiling the clamped tie on the JVM would cost minutes of Emmy
   simplification for two starting values."
  [p c d]
  (let [[ce np] (tie/conic-forward (rationalize p) (rationalize c) (rationalize d))]
    {:p p :c c :d d :ce (double ce) :np (double np)}))

(def init
  "The starting state: Fermat's parabola, p = 1, c = 0, CD = 1 (B = (1, 1))."
  (start-state 1 0 1))

(def descartes-init
  "Descartes' ellipse of Book II: latus rectum r = 2, transverse side q = 4
   (c = -r/q), C at MA = 1, so DP = 3/2."
  (start-state 2 -0.5 1))

(def names
  "The section's name from the sign of c: Apollonius' names as the
   Apollonius pages give them (alexandria.apollonius.conics/names)."
  (into {} (for [[kd {:keys [greek meaning]}] conics/names]
             [kd (str greek " · " (name kd) " (c " ({:parabola "= 0" :ellipse "< 0" :hyperbola "> 0"} kd) "): " meaning)])))

(def parabola-band
  "|c| below this is the parabola (half a slider step)."
  0.005)

(def labels {:p "p (latus rectum)" :c "c (excess: 0, < 0, > 0)" :d "CD (abscissa of B)"
             :ce "CE (subtangent)" :np "DP (foot of the normal)"})

(defn- schema []
  (into {} (map (fn [k] (let [[a b s] (ranges k)] [k {:min a :max b :step s :label (labels k)}])) tied)))

(defn- tie-solve
  "Browser form: solve from the touched key `src` with the tie kernel and
   push the changed keys into the panel atom !p (after the current
   notification, setTimeout 0). Counts solves in
   globalThis.__fermatSolves."
  [tsym !p !s]
  (list 'js/setTimeout
        (list 'fn []
              (list 'let ['cur (list 'assoc (list 'deref !s) 'src (list 'get (list 'deref !p) 'src))
                          'out (list tsym '(array 0) (list* 'array (map (fn [k] (list 'get 'cur k)) tied)))
                          'off (list 'get source-block 'src)
                          'solved (list 'zipmap tied (list 'map '(fn [j] (aget out (+ off j))) (list 'range (count tied))))
                          'shown (list 'deref !p)
                          'push '(into {} (filter (fn [[k v]] (> (js/Math.abs (- v (get shown k))) 1e-12)) solved))]
                    '(set! (.-__fermatSolves js/globalThis) (inc (or (.-__fermatSolves js/globalThis) 0)))
                    (list 'reset! !s 'solved)
                    (list 'when '(seq push)
                          '(reset! pushing true)
                          (list 'swap! !p 'merge 'push)
                          '(reset! pushing false)
                          '(swap! pushed merge push)
                          '(js/setTimeout (fn [] (swap! pushed (fn [m] (apply dissoc m (keys push))))) 600))))
        0))

(defn- tie-watch
  "The watch on !p. Origin guard: nothing while the page pushes. Epsilon
   guard: a change within half a slider step of the value just pushed is
   Leva's echo, not the reader."
  [tsym !p !s]
  (let [half (into {} (map (fn [[k [_ _ s]]] [k (/ s 2)]) ranges))]
    (list 'add-watch !p ::tie
          (list 'fn ['_ '_ 'old 'new]
                (list 'when-not '@pushing
                      (list 'let ['src (list 'first (list 'filter '(fn [k] (not= (get old k) (get new k))) tied))
                                  'echo? (list 'and 'src '(contains? @pushed src)
                                               (list '<= '(js/Math.abs (- (get new src) (get @pushed src)))
                                                     (list '+ 1e-9 (list 'get half 'src))))]
                            (list 'when (list 'and 'src '(not echo?)) (tie-solve tsym !p !s))))))))

(def colours
  {:curve "#1F6FB4" :tangent "#C0392B" :normal "#2E8B57" :chord "#E67E22" :circle "#7B5EA7" :ink "#222"})

(def quotes
  "Each author's own sentence, the Latin or French and the series' English."
  {:fermat {:src "Fermat, Methodus (Tannery and Henry 1891, vol. I, p. 136)"
            :text "Ergo CE probavimus duplam ipsius CD, quod quidem ita se habet."
            :english "So we have proved CE double CD, which is indeed so."}
   :descartes {:src "Descartes, La Géométrie, Book II (Leiden 1637, p. 347)"
               :text "enfin elles sont entièrement égales, s'ils sont tous deux joints en un ; c'est-à-dire si le cercle, qui passe par C, y touche la courbe CE sans la couper."
               :english "at last the two roots are entirely equal, if the two points are joined in one; that is, if the circle through C touches the curve CE there without cutting it."}})

(defn- name-form
  "Browser form: the section's name from the sign of the kernel's c."
  []
  (list 'let ['c '(aget r 16)]
        (list 'cond (list '< '(js/Math.abs c) parabola-band) (names :parabola)
              '(neg? c) (names :ellipse)
              :else (names :hyperbola))))

(defn- kind-form
  "Browser form: parabola, ellipse or hyperbola from the kernel's c."
  []
  (list 'let ['c '(aget r 16)]
        (list 'cond (list '< '(js/Math.abs c) parabola-band) "parabola" '(neg? c) "ellipse" :else "hyperbola")))

(defn- formula
  "Fermat's and Descartes' equations with the current values substituted,
   from the readout kernel `r` and !s; the focused author first."
  [!s focus]
  (let [n (fn [k] (fmt (list 'aget 'r (readout-index k)) 3))
        g (fn [k] (fmt (ev/get !s k) 3))
        fermat (list 'str "\\text{Fermat: } Q = p + 2c\\,CD = " (g :p) " + 2(" (g :c) ")(" (g :d) ") = " (n :q)
                     ",\\quad CE = \\frac{2\\,BC^2}{Q} = \\frac{2 \\times " (n :bc2) "}{" (n :q) "} = " (n :ce))
        descartes (list 'str "\\text{Descartes: } DP = CD\\,(1 + c) + \\tfrac{p}{2} = " (g :d) "\\,(1 + " (g :c) ") + \\tfrac{" (g :p)
                        "}{2} = " (n :np) ",\\quad \\Delta = 4\\,(v - DP)^2 = 4\\,(" (n :v) " - " (n :np) ")^2 = " (fmt '(aget r 8) 4))
        line (fn [who tex] (list 'let ['tex tex] [:div {:data-formula who :data-tex 'tex} [:f> 'nextjournal.clerk.render/render-katex 'tex {:inline? false}]]))]
    [:div {:style {:font-size "0.95em" :line-height 1.5}}
     [:p {:style {:font-size "1.2em" :font-weight 600 :margin "0.3em 0"}}
      [:span {:data-conic-name (kind-form)} (name-form)]]
     [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.9em"}}
      "y² = " (g :p) "·x + (" (g :c) ")·x²"]
     (if (= focus :descartes) (line "descartes" descartes) (line "fermat" fermat))
     (if (= focus :descartes) (line "fermat" fermat) (line "descartes" descartes))
     [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.9em"}}
      [:div "B = (" (g :d) ", " [:span {:data-b '(str (aget r 0))} (n :b)] "),  BC² = " (n :bc2)]
      [:div "EC·CP = " [:span {:data-ec-cp '(str (aget r 13))} (n :ec-cp)] " = BC²: BP is at right angles to EB"]
      [:div "Fermat's chord with E = " (fmt '(aget x 0) 3) ": CE = "
       [:span {:data-chord-ce '(str (aget r 3))} (n :chord-ce)]]
      [:div "Descartes' circle about v = " (n :v) ": Δ = " [:span {:data-delta '(str (aget r 8))} (fmt '(aget r 8) 5)]
       (list 'if '(< (js/Math.abs (aget r 8)) 1e-9) [:strong " — one double root: the normal"] " — two roots")]]]))

(defn- mafs-panel
  "The conic, B (movable, held on the curve), C, D, E, P, the tangent,
   the normal, Fermat's chord, Descartes' circle and its two meetings."
  [!p !s !x]
  (let [pf (fn [f] (ev/with-params {:atom !s :params [:p :c]} f))
        d (ev/get !s :d) p (ev/get !s :p) c (ev/get !s :c)
        B [d '(aget r 0)]
        E [(list '- d '(aget r 1)) 0] C [d 0] P ['(aget r 2) 0]
        Q ['(aget r 4) '(aget r 5)]
        line (fn [a b colour] (mafs/polyline {:points [(mapv #(list '+ %1 (list '* -0.6 (list '- %2 %1))) a b)
                                                         (mapv #(list '+ %1 (list '* 1.8 (list '- %2 %1))) a b)]
                                              :color colour}))
        meet (fn [xi yi] (list 'when (list 'and '(> (aget r 8) 1e-9) (list 'js/isFinite (list 'aget 'r yi)))
                               (mafs/point {:x (list 'aget 'r xi) :y (list 'aget 'r yi) :color (colours :circle)})))]
    (mafs/mafs
     {:view-box {:x [-3.5 6] :y [-3.5 3.5]} :height 440}
     (mafs/cartesian)
     (mafs/parametric {:xy (pf conic-curve) :t [-1 1] :color (colours :curve)})
     (line E B (colours :tangent))
     (line B P (colours :normal))
     (list 'when '(> (js/Math.abs (aget x 0)) 1e-6)
           (mafs/polyline {:points [[(list '- d '(aget r 3)) 0] Q] :color (colours :chord)}))
     (mafs/circle {:center ['(aget r 6) 0] :radius '(aget r 7) :color (colours :circle)})
     (meet 9 10) (meet 11 12)
     (mafs/segment {:point1 B :point2 C :color (colours :ink)})
     (mafs/point {:x 0 :y 0}) (mafs/text "D" {:x 0 :y 0 :attach "sw"})
     (mafs/point {:x d :y 0}) (mafs/text "C" {:x d :y 0 :attach "s"})
     (mafs/point {:x (first E) :y 0 :color (colours :tangent)}) (mafs/text "E" {:x (first E) :y 0 :attach "sw" :color (colours :tangent)})
     (mafs/point {:x (first P) :y 0 :color (colours :normal)}) (mafs/text "P" {:x (first P) :y 0 :attach "se" :color (colours :normal)})
     (mafs/text "B" {:x d :y '(aget r 0) :attach "nw"})
     (mafs/movable-point
      {:point B
       :color (colours :tangent)
       :constrain (list 'fn '[[mx _]] (list 'let ['x '(max mx 0.05)] ['x (list 'js/Math.sqrt (list 'max 0 (list '+ (list '* p 'x) (list '* c 'x 'x))))]))
       :on-move (list 'fn '[[mx _]] (list 'swap! !p 'assoc :d 'mx))}))))

(defn- tangent-body
  [[tsym tform] [rsym rform] !p !s !x focus]
  (let [{:keys [src text english]} (quotes focus)]
    (list 'reagent.core/with-let
          [tsym tform rsym rform
           'pushing '(atom false)
           'pushed '(atom {})
           '_ (tie-watch tsym !p !s)]
          (list 'let ['r (list rsym '(array 0) (list 'array (ev/get !s :p) (ev/get !s :c) (ev/get !s :d) (ev/get !x :ex) (ev/get !x :off)))
                      'x (list 'array (ev/get !x :ex))]
                [:div {:data-figure "fermat-tangent-normal" :data-focus (name focus)}
                 [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                  [:div {:style {:flex "1 1 440px" :min-width "300px"}} (mafs-panel !p !s !x)]
                  [:div {:style {:flex "0 1 340px" :min-width "260px"}}
                   (leva/sub-panel {:fill true :flat true :title-bar false}
                                   (leva/controls {:atom !p :schema (schema)})
                                   (leva/controls {:atom !x :schema {:ex {:min -1 :max 1 :step 0.01 :label "Fermat's E"}
                                                                     :off {:min -1.5 :max 1.5 :step 0.01 :label "circle centre − P"}}}))
                   (formula !s focus)
                   [:blockquote {:style {:font-size "0.88em" :font-style "italic" :opacity 0.85 :margin "0.5em 0"}}
                    text [:br] [:span {:style {:font-style "normal"}} english " (" src ")"]]
                   [:p {:style {:font-size "0.85em" :opacity 0.7}}
                    "Drag B along the curve, or move any of CD, CE, DP: the others follow. p and c change the curve with CD held; c names it."]]]])
          (list 'finally (list 'remove-watch !p ::tie)))))

(defn tangent-normal
  "The tied tangent-and-normal widget (see the ns doc), framed for one
   author: focus :fermat (his quote and equation first) or :descartes.
   Compiled on the raster backend whatever the caller's binding."
  ([] (tangent-normal init :fermat))
  ([start] (tangent-normal start :fermat))
  ([start focus]
   (binding [vc/*backend* :raster]
     (let [s (mapv start tied)]
       (ev/with-let [!p start !s start !x {:ex 0.6 :off 0.6}]
         (tangent-body (kernel-binding "tie" tie-kernel s)
                       (kernel-binding "readout" readout [(:p start) (:c start) (:d start) 0.6 0.6])
                       !p !s !x focus))))))

(defn- tex
  "TeX of an Emmy expression, simplified (a bare symbol as is)."
  [x]
  (if (symbol? x) ({'Delta "\\Delta"} x (str x)) (e/->TeX (e/simplify x))))

(defn derivation
  "Emmy's derivation of the tie on every conic
   (alexandria.fermat.tie/conic-derivation), as a Clerk column of TeX
   equations (emmy.env/->TeX), and the grades."
  []
  (apply clerk/col
         (concat
          (for [{:keys [label eqs]} (tie/conic-derivation)]
            (apply clerk/col
                   (clerk/html [:div {:style {:font-size "0.85em" :opacity 0.7 :margin-top "0.5em"}} label])
                   (for [[l r] eqs] (clerk/tex (str (tex l) " = " (tex r))))))
          [(clerk/html
            (into [:ul {:style {:font-size "0.9em"}}]
                  (for [{:keys [label grade]} (tie/conic-graded)]
                    [:li {:data-grade (name grade)} [:code (name grade)] " " label])))])))

;; ---------------------------------------------------------------------------
;; 2. The loci panel

(def kinds
  "The menu: Fermat's equations (Isagoge) by index."
  [{:id :line :label "D in A = B in E" :latin "punctum I erit ad lineam rectam positione datam"}
   {:id :hyperbola :label "A in E = Z pl." :latin "punctum I est ad hyperbolen"}
   {:id :parabola :label "A q. = D in E" :latin "punctum I est ad parabolen"}
   {:id :circle :label "B q. − A q. = E q." :latin "punctum I est ad circulum positione datum"}
   {:id :ellipse :label "(B q. − A q.) : E q. = k" :latin "punctum I erit ad ellipsin"}
   {:id :hyperbola-ratio :label "(B q. + A q.) : E q. = k" :latin "punctum I est ad hyperbolen"}])

(defn loci-readout
  "For [D B Z k t]: per kind (6 x 7 numbers) beta^2 - 4 alpha gamma,
   alpha - gamma, beta, the sum of squares, and the point I = [A E] at t
   with Fermat's equation evaluated there (0 on the curve)."
  [D B Z k t]
  (fn [_]
    (vec (mapcat (fn [{kd :id}]
                   (let [g (tie/general-form kd D B Z k)
                         [A E] ((tie/locus-point kd D B Z k) t)]
                     (concat (tie/invariants g) [A E (tie/evaluate g [A E])])))
                 kinds))))

(def loci-init {:kind 4 :D 1 :B 1.5 :Z 1 :k 2.5 :t 0.7})

(def loci-schema
  {:kind {:options (into {} (map-indexed (fn [i {:keys [label]}] [label i]) kinds)) :label "equation"}
   :D {:min 0.2 :max 3 :step 0.01} :B {:min 0.2 :max 3 :step 0.01} :Z {:min 0.2 :max 2.5 :step 0.01}
   :k {:min 0.1 :max 4 :step 0.01 :label "ratio k"} :t {:min -1.5 :max 1.5 :step 0.01 :label "I along the curve"}})

(defn- curve-of [kd D B Z k] (tie/locus-point kd D B Z k))

(defn- locus-curves
  "Mafs curves for kind index i (drawn only while the menu shows it)."
  [!q i]
  (let [kd (:id (kinds i))
        pf (fn [f] (ev/with-params {:atom !q :params [:D :B :Z :k]} f))
        par (fn [t-range f] (mafs/parametric {:xy (pf f) :t t-range :color "#1F6FB4"}))
        mirror (fn [f] (fn [D B Z k] (let [c (f D B Z k)] (fn [t] (let [[a b] (c t)] [a (e/- 0 b)])))))]
    (list 'when (list '= (list 'int (ev/get !q :kind)) i)
          (case kd
            :line (par [-5 5] (partial curve-of :line))
            :hyperbola [:<> (par [0.04 6] (partial curve-of :hyperbola)) (par [-6 -0.04] (partial curve-of :hyperbola))]
            :parabola (par [-3 3] (partial curve-of :parabola))
            (:circle :ellipse) (par [0 (* 2 Math/PI)] (partial curve-of kd))
            :hyperbola-ratio [:<> (par [-1.45 1.45] (partial curve-of kd)) (par [-1.45 1.45] (mirror (partial curve-of kd)))]))))

(defn- kind-name-form
  "Browser form: the name from the kernel's invariants of the picked kind."
  []
  (list 'let ['o '(* 7 (int kind)) 'disc '(aget q o) 'diff '(aget q (+ o 1)) 'beta '(aget q (+ o 2)) 'quad '(aget q (+ o 3))]
        '(cond (< (js/Math.abs quad) 1e-9) "line"
               (< (js/Math.abs disc) 1e-9) "parabola"
               (pos? disc) "hyperbola"
               (and (< (js/Math.abs diff) 1e-9) (< (js/Math.abs beta) 1e-9)) "circle"
               :else "ellipse")))

(defn- loci-body [[qsym qform] !q]
  (list 'reagent.core/with-let [qsym qform]
        (list 'let ['kind (list 'int (ev/get !q :kind))
                    'q (list qsym '(array 0) (list 'array (ev/get !q :D) (ev/get !q :B) (ev/get !q :Z) (ev/get !q :k) (ev/get !q :t)))
                    'o '(* 7 kind)
                    'nm (kind-name-form)]
              [:div {:data-figure "fermat-loci"}
               [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                [:div {:style {:flex "1 1 420px" :min-width "300px"}}
                 (mafs/mafs {:view-box {:x [-4 4] :y [-3 3]} :height 400}
                            (mafs/cartesian)
                            (into [:<>] (map #(locus-curves !q %) (range (count kinds))))
                            (mafs/segment {:point1 [0 0] :point2 ['(aget q (+ o 4)) 0] :color "#C0392B"})
                            (mafs/segment {:point1 ['(aget q (+ o 4)) 0] :point2 ['(aget q (+ o 4)) '(aget q (+ o 5))] :color "#2E8B57"})
                            (mafs/point {:x 0 :y 0}) (mafs/text "N" {:x 0 :y 0 :attach "sw"})
                            (mafs/point {:x '(aget q (+ o 4)) :y 0}) (mafs/text "Z" {:x '(aget q (+ o 4)) :y 0 :attach "s"})
                            (mafs/point {:x '(aget q (+ o 4)) :y '(aget q (+ o 5)) :color "#C0392B"})
                            (mafs/text "I" {:x '(aget q (+ o 4)) :y '(aget q (+ o 5)) :attach "ne"}))]
                [:div {:style {:flex "0 1 300px" :min-width "260px"}}
                 (leva/sub-panel {:fill true :flat true :title-bar false}
                                 (leva/controls {:atom !q :schema loci-schema}))
                 [:p {:style {:font-size "1.25em" :font-weight 600 :margin "0.4em 0"}}
                  [:span {:data-locus-name 'nm} 'nm]]
                 [:blockquote {:style {:font-style "italic" :opacity 0.85 :margin "0.3em 0"}}
                  (list ':latin (list 'nth kinds 'kind)) " (Isagoge)"]
                 [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.88em"}}
                  [:div (list ':label (list 'nth kinds 'kind))]
                  [:div "β² − 4αγ = " [:span {:data-disc '(str (aget q o))} (fmt '(aget q o) 3)]]
                  [:div "I: A = NZ = " (fmt '(aget q (+ o 4)) 3) ", E = ZI = " (fmt '(aget q (+ o 5)) 3)]
                  [:div "equation at I: " [:span {:data-residual '(str (aget q (+ o 6)))} (fmt '(aget q (+ o 6)) 6)]]]]]])))

(defn loci-panel
  "Fermat's Isagoge as a panel: pick the equation, move the given lines;
   the curve and its name follow. Raster backend."
  ([] (loci-panel loci-init))
  ([start]
   (binding [vc/*backend* :raster]
     (ev/with-let [!q start]
       (loci-body (kernel-binding "loci" loci-readout (mapv start [:D :B :Z :k :t])) !q)))))
