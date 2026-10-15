(ns alexandria.delian.tie-widgets
  "The doubling of the cube with tied controls (widget brief, section 5),
   for a Clerk notebook that calls (history-of-math.widgets/install!).

   Every quantity linked by an equation is a Leva control; moving one
   re-solves the others through alexandria.delian.tie's forward and inverse
   maps, compiled to one raster tie kernel per widget that returns the
   whole state for each possible source (source-blocks), clamped so that no
   key leaves its slider. Two atoms hold the state: !p is the Leva panel's
   (Leva rounds to its step and echoes), !s the exact solved state every
   plot and number reads. Only the touched key is a source: the watch is
   off while the page pushes (origin guard), and a change that lands within
   half a slider step of the value just pushed is that echo (epsilon
   guard). The formula beside the sliders is rendered with the current
   values substituted, from a raster readout kernel.

     menaechmus  {a x V}: the parabolas x^2 = a y, y^2 = 2a x and their
                 meeting point; x = a 2^(1/3), V = x^3 = 2a^3.
     archytas    {AB AI AK} with AD = 2: the cone, the half-cylinder and
                 the ring in MathBox, their meeting point K moving with
                 any of the three sliders.
     mesolabe    {s b BZ GH} with AE = 2: Eratosthenes' frames slide; the
                 slide, the given lesser line and the two means are tied.

   Each widget is stepped sentence by sentence (alexandria.delian.
   duplication-widgets' step list)."
  (:require [alexandria.delian.duplication-widgets :as w]
            [alexandria.delian.tie :as t]
            [alexandria.grade]
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
;; Branch-free helpers (raster compiles them)

(defn- at-least [x lo] (e/* 0.5 (e/+ x lo (e/abs (e/- x lo)))))
(defn- at-most [x hi] (e/* 0.5 (e/- (e/+ x hi) (e/abs (e/- x hi)))))
(defn- clamp [x lo hi] (at-most (at-least x lo) hi))

(def ^:private step-list @#'w/step-list)
(def ^:private shown @#'w/shown)
(def ^:private step-form @#'w/step-form)

(defn- fmt
  "Browser form: x to n places with a true minus sign."
  ([x] (fmt x 3))
  ([x n] (list 'let ['v x] (list 'if '(js/isFinite v) (list '.replace (list '.toFixed 'v n) "-" "−") "∞"))))

(defn- katex
  "Browser form: the TeX string form `tex` rendered by Clerk's KaTeX, with
   the string on data-formula for the e2e."
  [tex]
  (list 'let ['tex tex]
        '[:div {:data-formula tex} [:f> nextjournal.clerk.render/render-katex tex {:inline? false}]]))

;; ---------------------------------------------------------------------------
;; The tie machinery (browser code)

(defn- tie-solve
  "Browser form: solve from the touched key `src` with the tie kernel
   `tsym`; the exact state !s takes the reader's value of src and merges
   the solved block; keys of !p that differ are pushed into the panel
   after the current notification (setTimeout 0). Counts solves in
   globalThis.__delianSolves."
  [tsym !p !s ks]
  (let [n (count ks)
        index (zipmap ks (range))]
    (list 'js/setTimeout
          (list 'fn []
                (list 'let ['cur (list 'assoc (list 'deref !s) 'src (list 'get (list 'deref !p) 'src))
                            'out (list tsym '(array 0) (list* 'array (map (fn [k] (list 'get 'cur k)) ks)))
                            'off (list '* n (list 'get index 'src))
                            'solved (list 'zipmap ks (list 'map '(fn [j] (aget out (+ off j))) (list 'range n)))
                            'shown (list 'deref !p)
                            'push '(into {} (filter (fn [[k v]] (> (js/Math.abs (- v (get shown k))) 1e-12)) solved))]
                      '(set! (.-__delianSolves js/globalThis) (inc (or (.-__delianSolves js/globalThis) 0)))
                      (list 'swap! !s 'merge 'solved)
                      (list 'when '(seq push)
                            '(reset! pushing true)
                            (list 'swap! !p 'merge 'push)
                            '(reset! pushing false)
                            '(swap! pushed merge push)
                            '(js/setTimeout (fn [] (swap! pushed (fn [m] (apply dissoc m (keys push))))) 600))))
          0)))

(defn- tie-watch
  "Browser form: the watch on !p that re-solves from the touched key,
   with the origin and epsilon guards."
  [tsym !p !s ks ranges]
  (let [half (into {} (map (fn [[k [_ _ s]]] [k (/ s 2)]) ranges))]
    (list 'add-watch !p ::tie
          (list 'fn ['_ '_ 'old 'new]
                (list 'when-not '@pushing
                      (list 'let ['src (list 'first (list 'filter '(fn [k] (not= (get old k) (get new k))) ks))
                                  'echo? (list 'and 'src '(contains? @pushed src)
                                               (list '<= '(js/Math.abs (- (get new src) (get @pushed src)))
                                                     (list '+ 1e-9 (list 'get half 'src))))]
                            (list 'when (list 'and 'src '(not echo?)) (tie-solve tsym !p !s ks))))))))

(defn- template
  "Browser form: bind the kernels [[sym form] ...] and `extra` bindings,
   wait until every kernel is ready, install the tie watch, then render
   body inside (let lets ...)."
  [kernels [tsym !p !s ks ranges] extra lets body]
  (let [ksyms (map first kernels)]
    (list 'reagent.core/with-let
          (vec (concat (mapcat identity kernels)
                       ['ready '(reagent.core/atom false)
                        '_ (list 'let ['poll (list 'fn 'poll []
                                                   (list 'if (list* 'and (map (fn [k] (list '.ready k)) ksyms))
                                                         '(reset! ready true)
                                                         '(js/setTimeout poll 20)))]
                                 '(poll))
                        'pushing '(atom false)
                        'pushed '(atom {})
                        '_ (tie-watch tsym !p !s ks ranges)]
                       extra))
          (list 'when '@ready (list 'let lets body))
          (list 'finally (list 'remove-watch !p ::tie)))))

(defn- schema [ks ranges labels]
  (into {} (map (fn [k] (let [[a b s] (ranges k)] [k {:min a :max b :step s :label (labels k)}])) ks)))

(defn- kernel [nm f params] [(gensym nm) (vc/compiled-fn f params [0] {:simplify? false})])

(def ^:private panel-style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"})
(def ^:private mono {:font-family "ui-monospace, monospace" :font-size "0.88em" :line-height 1.6 :margin-top "0.5em"})

;; ---------------------------------------------------------------------------
;; Menaechmus: {a x V}

(def m-keys [:a :x :V])

(def m-ranges
  "Each key's [min max step]; x and V's ranges are a's carried by the tie."
  (let [lo 0.6 hi 2.2 c (Math/cbrt 2)]
    {:a [lo hi 0.01] :x [(* lo c) (* hi c) 0.001] :V [(* 2 lo lo lo) (* 2 hi hi hi) 0.01]}))

(defn menaechmus-tie
  "The tie kernel on [a x V]: the state solved from a, from x, from V (9
   numbers), the source held at its slider's edge."
  [a x v]
  (fn [_]
    (let [[alo ahi] (m-ranges :a) [xlo xhi] (m-ranges :x) [vlo vhi] (m-ranges :V)
          a' (clamp a alo ahi) x' (clamp x xlo xhi) v' (clamp v vlo vhi)]
      [a' (t/x-of-a a') (t/v-of-a a')
       (t/a-of-x x') x' (t/v-of-x x')
       (t/a-of-v v') (t/x-of-v v') v'])))

(def m-index {:a 0 :x 1 :V 2 :y 3 :ratio 4 :x3 5 :two-a3 6 :vol 7})

(defn menaechmus-readout
  "[a x V y x/a x^3 2a^3 V/a^3] for the state (a, x, V)."
  [a x v]
  (fn [_]
    [a x v (e// (e/square x) a) (e// x a) (e/cube x) (e/* 2 (e/cube a)) (e// v (e/cube a))]))

(defn menaechmus-init
  "The starting state from a = 1.4, x and V by raster."
  []
  (zipmap m-keys (raster/value (fn [a] [a (t/x-of-a a) (t/v-of-a a)]) 1.4)))

(defn- m-formula []
  (let [n (fn [k] (fmt (list 'aget 'r (m-index k))))]
    [:div {:style mono :data-menaechmus-x '(str (aget r 1)) :data-menaechmus-a '(str (aget r 0))
           :data-menaechmus-v '(str (aget r 2))}
     (katex (list 'str "x^3 = 2a^3:\\quad " (n :x) "^3 = " (n :x3) " = 2\\cdot " (n :a) "^3 = " (n :two-a3)))
     (katex (list 'str "x = a\\sqrt[3]{2}:\\quad " (n :x) " = " (n :a) "\\times " (n :ratio)))
     [:div "Θ = (x, x²/a) = (" (n :x) ", " (n :y) ")"]
     [:div {:style {:font-size "1.15em" :font-weight 600 :margin-top "0.3em"}}
      "V / a³ = " [:span {:data-vol-ratio '(str (aget r 7))} (n :vol)]
      ": the cube on x is double the cube on a"]]))

(defn menaechmus
  "Menaechmus' two parabolas, stepped one curve per sentence, with the tied
   controls a, x, V. Compiled on the raster backend whatever the caller's
   binding."
  []
  (binding [vc/*backend* :raster]
    (let [s0 (menaechmus-init)
          init (mapv s0 m-keys)
          [tsym :as tk] (kernel "tie" menaechmus-tie init)
          [rsym :as rk] (kernel "readout" menaechmus-readout init)]
      (ev/with-let [!p s0 !s (assoc s0 :step 0)]
        (let [with-a (fn [f] (ev/with-params {:atom !s :params [:a]} f))
              !c !s]
          (template
           [tk rk] [tsym !p !s m-keys m-ranges] []
           ['r (list rsym '(array 0) (list* 'array (map #(ev/get !s %) m-keys)))]
           [:div {:style panel-style :data-figure "delian-menaechmus-tied" :data-step (list 'inc (step-form !c))}
            [:div {:style {:flex "1 1 460px" :min-width "300px"}}
             (mafs/mafs {:height 440 :view-box {:x [-0.3 4.6] :y [-0.3 4.4]} :zoom true}
                        (mafs/cartesian)
                        (shown !c 1 (mafs/of-x {:y (with-a w/menaechmus-parabola-x) :color (w/colours :parabola-x)}))
                        (shown !c 2 (mafs/parametric {:xy (with-a w/menaechmus-parabola-y) :t [-4.4 4.4]
                                                      :color (w/colours :parabola-y)}))
                        (shown !c 3 (mafs/parametric {:xy (with-a w/menaechmus-hyperbola) :t [0.25 4.6]
                                                      :color (w/colours :hyperbola) :style "dashed"}))
                        (mafs/segment {:point1 [0 -0.12] :point2 ['(aget r 0) -0.12] :color (w/colours :circle)})
                        (mafs/text "a" {:x '(/ (aget r 0) 2) :y -0.12 :attach "s" :color (w/colours :circle)})
                        (shown !c 4 [:<>
                                     (mafs/segment {:point1 ['(aget r 1) 0] :point2 ['(aget r 1) '(aget r 3)]
                                                    :color (w/colours :mean) :style "dashed"})
                                     (mafs/segment {:point1 [0 0] :point2 ['(aget r 1) 0] :color (w/colours :mean)})
                                     (mafs/point {:x '(aget r 1) :y '(aget r 3) :color (w/colours :meet)})
                                     (mafs/text "Θ" {:x '(aget r 1) :y '(aget r 3) :attach "ne"
                                                     :color (w/colours :meet) :size 20})
                                     (mafs/text "x" {:x '(/ (aget r 1) 2) :y 0 :attach "n" :color (w/colours :mean)})]))]
            [:div {:style {:flex "1 1 300px" :min-width "260px"}}
             (leva/sub-panel {:fill true :flat true :title-bar false}
                             (leva/controls {:atom !p :schema (schema m-keys m-ranges {:a "a" :x "x (edge)" :V "V = 2a³"})}))
             (m-formula)
             (step-list !c w/menaechmus-sentences "menaechmus-tied")]]))))))

;; ---------------------------------------------------------------------------
;; Archytas: {b = AB, ai = AI, ak = AK}, AD = 2

(def a-keys [:b :ai :ak])

(def a-ranges
  (let [d w/archytas-a lo 0.7 hi 1.6
        ai #(Math/cbrt (* d % %)) ak #(Math/sqrt (* d (ai %)))]
    {:b [lo hi 0.01] :ai [(ai lo) (ai hi) 0.001] :ak [(ak lo) (ak hi) 0.001]}))

(defn archytas-tie
  "The tie kernel on [AB AI AK] (AD fixed): the state from AB, from AI,
   from AK (9 numbers)."
  [b m k]
  (fn [_]
    (let [d w/archytas-a
          [blo bhi] (a-ranges :b) [mlo mhi] (a-ranges :ai) [klo khi] (a-ranges :ak)
          b' (clamp b blo bhi) m' (clamp m mlo mhi) k' (clamp k klo khi)
          m1 (t/ai-of-ab d b') m3 (t/ai-of-ak d k')]
      [b' m1 (t/ak-of-ai d m1)
       (t/ab-of-ai d m') m' (t/ak-of-ai d m')
       (t/ab-of-ai d m3) m3 k'])))

(defn archytas-init []
  (zipmap a-keys (raster/value (fn [b] (let [m (t/ai-of-ab w/archytas-a b)] [b m (t/ak-of-ai w/archytas-a m)])) 1.0)))

(defn- a-formula []
  (let [n (fn [i] (fmt (list 'aget 'v (w/archytas-index i))))]
    [:div {:style mono}
     (katex (list 'str "AI^3 = AD\\cdot AB^2:\\quad " (n :ai) "^3 = " (n :ai3) " = " (n :ad) "\\cdot " (n :ab) "^2"))
     (katex (list 'str "AK^2 = AD\\cdot AI:\\quad " (n :ak) "^2 = " (n :ad) "\\cdot " (n :ai)))]))

(defn archytas
  "Archytas' solution, stepped, with the tied controls AB, AI, AK (AD = 2):
   any of them moves the cone (and so K). MathBox on the raster backend."
  []
  (binding [vc/*backend* :raster]
    (let [s0 (archytas-init)
          init (mapv s0 a-keys)
          [tsym :as tk] (kernel "tie" archytas-tie init)
          [lsym :as lk] (kernel "letters" w/archytas-letters [w/archytas-a (:b s0)])]
      (ev/with-let [!p s0 !s (assoc s0 :step 0)]
        (template
         [tk lk] [tsym !p !s a-keys a-ranges] ['box '(atom nil)]
         ['v (list lsym '(array 0) (list 'array w/archytas-a (ev/get !s :b)))]
         [:div {:style panel-style :data-figure "delian-archytas-tied" :data-step (list 'inc (step-form !s))}
          [:div {:style {:flex "1 1 460px" :min-width "300px" :position "relative"}}
           ['alexandria.medium.html-labels/overlay
            {:box 'box :range w/archytas-range :scale w/archytas-scale :labels (@#'w/archytas-labels !s)}]
           (@#'w/archytas-scene !s)]
          [:div {:style {:flex "1 1 300px" :min-width "260px"}}
           (leva/sub-panel {:fill true :flat true :title-bar false}
                           (leva/controls {:atom !p :schema (schema a-keys a-ranges {:b "AB" :ai "AI" :ak "AK"})}))
           (a-formula)
           (@#'w/archytas-readout)
           (step-list !s w/archytas-sentences "archytas-tied")]])))))

;; ---------------------------------------------------------------------------
;; Mesolabe: {s b x y}, AE = 2, frames of width 1

(def meso-a "AE, the greater given line (the frames' height)." 2)
(def meso-w "The frames' width." 1)

(def s-keys [:s :b :x :y])

(def s-ranges
  (let [a meso-a lo 0.5 hi 1.0]
    {:s [lo hi 0.001] :b [(* a lo lo lo) (* a hi hi hi) 0.01]
     :x [(* a lo) (* a hi) 0.01] :y [(* a lo lo) (* a hi hi) 0.01]}))

(defn mesolabe-tie
  "The tie kernel on [s b x y]: the state from each (16 numbers)."
  [s b x y]
  (fn [_]
    (let [a meso-a
          lim (fn [k v] (let [[lo hi] (s-ranges k)] (clamp v lo hi)))
          row (fn [s] [s (t/b-of-s a s) (t/mean-1 a s) (t/mean-2 a s)])
          s1 (lim :s s) b2 (lim :b b) x3 (lim :x x) y4 (lim :y y)]
      (vec (concat (row s1)
                   [(t/s-of-b a b2) b2 (t/mean-1 a (t/s-of-b a b2)) (t/mean-2 a (t/s-of-b a b2))]
                   [(t/s-of-mean-1 a x3) (t/b-of-s a (t/s-of-mean-1 a x3)) x3 (t/mean-2 a (t/s-of-mean-1 a x3))]
                   [(t/s-of-mean-2 a y4) (t/b-of-s a (t/s-of-mean-2 a y4)) (t/mean-1 a (t/s-of-mean-2 a y4)) y4])))))

(def s-index
  "Where each number sits in `mesolabe-letters`."
  {:A 0 :B 2 :G 4 :D 6 :f1 8 :f2 9 :f3 10 :s 11 :b 12 :x 13 :y 14 :r1 15 :r2 16 :r3 17})

(defn mesolabe-letters
  "A, B, G, D at slide s (alexandria.delian.duplication/mesolabe-points'
   lettering), the frames' left edges, and the heights and ratios
   AE : BZ, BZ : GH, GH : DΘ."
  [s]
  (fn [_]
    (let [a meso-a wd meso-w
          b (t/b-of-s a s) x (t/mean-1 a s) y (t/mean-2 a s)]
      [0 a wd x (e/* wd (e/+ 1 s)) y (e/* wd (e/+ 1 s (e/square s))) b
       0 (e/* wd s) (e/* wd s (e/+ 1 s))
       s b x y (e// a x) (e// x y) (e// y b)])))

(def mesolabe-sentences
  "Eratosthenes' instrument, one sentence per step (his letter, in Eutocius;
   the series' rendering)."
  ["Take three equal frames AZ, ZI, IΘ, each with its diagonal, standing on one base line; the first stays fixed."
   "AE, the greater of the two given lines, is the first frame's left edge; the lesser, DΘ, is marked on the last frame's right edge."
   "Slide the second and the third frame in under the first until A, the crossings B and G of the diagonals with the edges, and D lie on one straight line."
   "Then AE : BZ = BZ : GH = GH : DΘ: BZ and GH are the two mean proportionals between AE and DΘ."])

(defn mesolabe-init []
  (zipmap s-keys (raster/value (fn [s] [s (t/b-of-s meso-a s) (t/mean-1 meso-a s) (t/mean-2 meso-a s)]) 0.8)))

(defn- frame
  "Browser form: frame with left edge at left-form, and its diagonal."
  [left-form colour]
  (let [r (list '+ left-form meso-w)]
    [:<>
     (mafs/polygon {:points [[left-form 0] [r 0] [r meso-a] [left-form meso-a]] :color colour :fill-opacity 0.06})
     (mafs/segment {:point1 [left-form meso-a] :point2 [r 0] :color colour :opacity 0.7})]))

(defn mesolabe
  "Eratosthenes' mesolabe, stepped, with the tied controls s (the slide),
   b = DΘ and the means BZ, GH: move any, the frames slide."
  []
  (binding [vc/*backend* :raster]
    (let [s0 (mesolabe-init)
          init (mapv s0 s-keys)
          [tsym :as tk] (kernel "tie" mesolabe-tie init)
          [lsym :as lk] (kernel "letters" mesolabe-letters [(:s s0)])
          q (fn [k] (list 'aget 'v (s-index k)))
          pt (fn [k] [(list 'aget 'v (s-index k)) (list 'aget 'v (inc (s-index k)))])
          blue "#4a7fb0"]
      (ev/with-let [!p s0 !s (assoc s0 :step 0)]
        (template
         [tk lk] [tsym !p !s s-keys s-ranges] []
         ['v (list lsym '(array 0) (list 'array (ev/get !s :s)))]
         [:div {:style panel-style :data-figure "delian-mesolabe-tied" :data-step (list 'inc (step-form !s))}
          [:div {:style {:flex "1 1 460px" :min-width "300px"}}
           (mafs/mafs {:height 360 :view-box {:x [-0.3 3.3] :y [-0.3 2.4]}}
                      (mafs/cartesian {:subdivisions false})
                      (frame (q :f1) (w/colours :circle))
                      (frame (q :f2) blue)
                      (frame (q :f3) (w/colours :torus))
                      (mafs/segment {:point1 [0 0] :point2 (pt :A) :color (w/colours :meet) :weight 4})
                      (mafs/text "A" {:x 0 :y meso-a :attach "nw"})
                      (shown !s 1 [:<>
                                   (mafs/segment {:point1 [(q :D) 0] :point2 (pt :D) :color (w/colours :cone) :weight 4})
                                   (mafs/text "D" {:x (q :D) :y (list 'aget 'v 7) :attach "ne"})])
                      (shown !s 2 [:<>
                                   (mafs/segment {:point1 (pt :A) :point2 (pt :D) :color (w/colours :meet) :style "dashed"})
                                   (mafs/point {:x (q :B) :y (list 'aget 'v 3) :color (w/colours :mean)})
                                   (mafs/point {:x (q :G) :y (list 'aget 'v 5) :color (w/colours :mean)})
                                   (mafs/point {:x (q :D) :y (list 'aget 'v 7) :color (w/colours :cone)})
                                   (mafs/text "B" {:x (q :B) :y (list 'aget 'v 3) :attach "ne"})
                                   (mafs/text "G" {:x (q :G) :y (list 'aget 'v 5) :attach "ne"})])
                      (shown !s 3 [:<>
                                   (mafs/segment {:point1 [(q :B) 0] :point2 (pt :B) :color (w/colours :mean) :weight 4})
                                   (mafs/segment {:point1 [(q :G) 0] :point2 (pt :G) :color (w/colours :mean) :weight 4})]))]
          [:div {:style {:flex "1 1 300px" :min-width "260px"}}
           (leva/sub-panel {:fill true :flat true :title-bar false}
                           (leva/controls {:atom !p :schema (schema s-keys s-ranges {:s "slide s" :b "DΘ" :x "BZ" :y "GH"})}))
           [:div {:style mono :data-mesolabe-s (list 'str (q :s)) :data-mesolabe-b (list 'str (q :b))}
            (katex (list 'str "AE : BZ = BZ : GH = GH : D\\Theta:\\quad "
                         (fmt (q :r1)) " = " (fmt (q :r2)) " = " (fmt (q :r3))))
            (katex (list 'str "D\\Theta = AE\\cdot s^3:\\quad " (fmt (q :b)) " = 2\\cdot " (fmt (q :s)) "^3"))
            [:div "BZ = " (fmt (q :x)) ", GH = " (fmt (q :y)) " (AE = 2)"]]
           (step-list !s mesolabe-sentences "mesolabe-tied")]])))))

;; ---------------------------------------------------------------------------
;; The derivation, for the page

(defn derivation
  "Emmy's derivation of x^3 = 2a^3 from the two parabolas, as TeX, and the
   tie grades."
  []
  (apply clerk/col
         (concat
          (for [{:keys [label eqs]} (t/equations)]
            (apply clerk/col
                   (clerk/html [:div {:style {:font-size "0.85em" :opacity 0.7 :margin-top "0.5em"}} label])
                   (for [q eqs] (ec/->TeX q :simplify? true))))
          [(clerk/html
            (into [:ul {:style {:font-size "0.9em"}}]
                  (for [{:keys [label grade]} (:menaechmus (t/grades))]
                    [:li {:data-grade (name grade)} [:code (name grade)] " " label])))])))

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
