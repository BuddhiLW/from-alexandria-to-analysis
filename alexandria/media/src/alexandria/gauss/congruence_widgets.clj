(ns alexandria.gauss.congruence-widgets
  "The Disquisitiones arithmeticae widgets (sections I-IV) on the raster
   backend. Every kernel is a structure value of alexandria.gauss.congruence
   (structure.tie ties, readouts and named cases; the group actions of
   structure.groups compiled through structure.arith/wrap); the integer
   tables come from structure.ring and structure.group.

     steps        the definition of art. 1-2 stepped on the clock of 5 hours
     clock        n, a, b, k tied (b = a + k n); a and b as handles on the
                  n-gon, the rotation of Z/n drawn; a = b (mod n) named live
     powers       the walk 1, a, a^2 ... mod m on the clock, one raster step
                  per click; the order, phi(m), the case named (primitive
                  root / order divides phi / not a unit), the units named
     table        the multiplication table mod n, a row picked: permutation
                  or not; the unit group named (cyclic or not)
     reciprocity  the grid of odd primes coloured by (p/q) and (q/p), the
                  exceptional cells p = q = 3 (mod 4) lit; pick a cell
     primes       pi(x), Li(x), x/log x with a slider for x; the closer
                  count named live

   Each browser body is a quoted form (the defs ending in -body) filled by
   alexandria.medium.flow/template; the names in CAPITALS and ?-symbols
   are its holes."
  (:require [alexandria.gauss.congruence :as c]
            [alexandria.medium.flow :as flow]
            [alexandria.medium.tie :as tie]
            [alexandria.structure.ring :as ring]
            [alexandria.structure.tie :as st]
            [emmy.leva :as leva]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [nextjournal.clerk :as clerk]))

(defn- case-form
  "Browser form naming the case of `named` (structure.tie data) from the
   array o, positions idx."
  [named idx]
  (st/case-form named (fn [k] (list 'aget 'o (idx k)))))

(def ^:private on-circle
  '(fn [[x y]] (let [r (js/Math.hypot x y) r (if (> r 1e-9) r 1)] [(/ x r) (/ y r)])))

;; ---------------------------------------------------------------------------
;; 0. The definition, stepped

(def definition-steps
  [{:who "Gauss, Disquisitiones arithmeticae (1801), art. 1"
    :text "« Si numerus a numerorum b, c differentiam metitur, » If a number a measures the difference of the numbers b, c (our translation),"
    :show #{:b :c}}
   {:who "art. 1"
    :text "« b et c secundum a congrui dicuntur, sin minus, incongrui: » b and c are called congruent according to a, otherwise incongruent;"
    :show #{:b :c :arc}}
   {:who "art. 1"
    :text "« ipsum a modulum appellamus. » we call a itself the modulus. Here the modulus is 5: a clock of five hours."
    :show #{:b :c :arc :hours}}
   {:who "art. 1, his example"
    :text "« E. g. −9 et +16 secundum modulum 5 sunt congrui » For example −9 and +16 are congruent modulo 5: they differ by 25 = 5 · 5, and both stand at the hour 1."
    :show #{:b :c :arc :hours :land}}
   {:who "art. 2"
    :text "« Numerorum congruentiam hoc signo, ≡, in posterum denotabimus » From now on we shall denote the congruence of numbers by this sign, ≡: −16 ≡ 9 (mod 5)."
    :show #{:b :c :arc :hours :land :sign}}])

(def ^:private steps-body
  '(let [i (get @?i :i)
         show (:show (nth STEPS i))
         on? (fn [k] (contains? show k))
         P (fn [k] (let [q (?v (array 0) (array k 5))] [(aget q 0) (aget q 1)]))]
     [:div {:data-widget "congruence-steps" :data-step (str i)}
      [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
       [:div {:style {:flex "1 1 320px" :min-width "260px"}}
        [mafs.core/Mafs {:height 320 :viewBox {:x [-1.4 1.4] :y [-1.4 1.4]}}
         [mafs.core/Circle {:center [0 0] :radius 1 :color "#8A9BB0" :fillOpacity 0}]
         (into [:<>] (for [k (range 5) :let [[x y] (P k)]]
                       [:<>
                        [mafs.core/Point {:x x :y y :color (if (and (on? :land) (= k 1)) "#2E8B57" "#3C4650")}]
                        (when (on? :hours) [mafs.core/Text {:x (* 1.2 x) :y (* 1.2 y) :size 14} (str k)])]))
         (when (on? :arc)
           [mafs.plot/Parametric {:t [0 1] :color "#E6A700" :weight 2
                                  :xy (fn [u] (let [t (* 2 js/Math.PI (+ (/ -9 5) (* 5 u)))
                                                    r (- 1.1 (* 0.5 u))]
                                                [(* r (js/Math.sin t)) (* r (js/Math.cos t))]))}])
         (when (on? :b) [mafs.core/Text {:x -0.9 :y -1.3 :size 15 :color "#1F6FB4"} "b = −9"])
         (when (on? :c) [mafs.core/Text {:x 0.9 :y -1.3 :size 15 :color "#C0392B"} "c = +16"])
         (when (on? :sign) [mafs.core/Text {:x 0 :y 0 :size 16} "−9 ≡ 16 (mod 5)"])]]
       [:div {:style {:flex "1 1 320px"}}
        (into [:ol]
              (for [[j {:keys [who text]}] (map-indexed vector STEPS)]
                [:li {:data-step-sentence j
                      :style {:opacity (if (<= j i) 1 0.35) :font-weight (if (= j i) 600 400)
                              :background (if (= j i) "#E6A70022" "transparent") :padding "0.2em"}}
                 text [:br] [:small {:style {:opacity 0.7}} who]]))
        [:div
         [:button {:type "button" :data-step-prev true :on-click (fn [] (swap! ?i update :i (fn [k] (max 0 (dec k)))))} "previous"]
         [:button {:type "button" :data-step-next true :on-click (fn [] (swap! ?i update :i (fn [k] (min LAST (inc k)))))} "next"]]
        [:p {:style {:font-size "0.85em" :opacity 0.75}}
         "The amber spiral winds five times round the clock from −9 to +16: the modulus 5 measures the difference 25."]]]]))

(defn steps
  "The definition of art. 1-2 stepped, one sentence per step; the clock's
   vertices from the raster kernel of congruence/vertex."
  []
  (binding [vc/*backend* :raster]
    (let [[vsym vform] (tie/kernel-binding "vertex" (fn [k n] (fn [_] (c/vertex k n))) [0 5])]
      (ev/with-let [!i {:i 0}]
        (list 'reagent.core/with-let [vsym vform]
              (flow/template {'?i !i '?v vsym 'STEPS definition-steps 'LAST (dec (count definition-steps))}
                             steps-body))))))

;; ---------------------------------------------------------------------------
;; 1. The clock: b = a + k n

(def clock-start {:n 12 :a 5 :b 29 :k 2})

(def clock-presets
  [["Gauss: −9 ≡ 16 (mod 5)" {:n 5 :a -9 :b 16 :k 5}]
   ["−7 ≡ 15 (mod 11)" {:n 11 :a -7 :b 15 :k 2}]
   ["−7 and 15 mod 3: incongruent" {:n 3 :a -7 :b 15 :k (/ 22 3.0)}]
   ["29 o'clock is 5 o'clock" {:n 12 :a 5 :b 29 :k 2}]])

(def ^:private drag-hand
  "Browser fn: the hand of key k dragged to (x, y) moves k to the nearest
   number on that hour (the hour read by atan2, then the shortest way)."
  '(fn [k [x y]]
     (let [s @?s n (js/Math.round (:n s))
           h (mod (js/Math.round (* (/ n (* 2 js/Math.PI)) (js/Math.atan2 x y))) n)
           v (get s k)
           d (mod (- h (mod v n)) n)
           v' (+ v (- d (if (> d (/ n 2)) n 0)))]
       (when (not= v' v) (swap! ?p assoc k v')))))

(def ^:private clock-body
  '(let [s @?s
         n (js/Math.round (:n s))
         o (?r (array 0) (array (:n s) (:a s) (:b s) (:k s)))
         kind CASE
         ra (js/Math.round (aget o RA)) rb (js/Math.round (aget o RB))
         P (fn [k] (let [q (?v (array 0) (array k n))] [(aget q 0) (aget q 1)]))
         rot (fn [k] (js/Math.round (aget (?h (array 0) (array (:a s) k n)) 0)))
         drag! DRAG
         f (fn [x d] (.replace (.toFixed x d) "-" "−"))]
     [:div {:data-widget "clock" :data-n (str n) :data-a (str (:a s)) :data-b (str (:b s))
            :data-k (str (:k s)) :data-case kind :data-ra (str ra) :data-rb (str rb)}
      [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
       [:div {:style {:flex "1 1 400px" :min-width "300px"}}
        [mafs.core/Mafs {:height 400 :viewBox {:x [-1.4 1.4] :y [-1.4 1.4]}}
         [mafs.core/Circle {:center [0 0] :radius 1 :color "#8A9BB0" :fillOpacity 0}]
         [mafs.core/Polygon {:points (mapv P (range n)) :color "#8A9BB0" :fillOpacity 0.04}]
         (into [:<>] (for [k (range n) :let [[x y] (P k)]]
                       [:<>
                        [mafs.core/Point {:x x :y y :color "#3C4650"}]
                        [mafs.core/Text {:x (* 1.16 x) :y (* 1.16 y) :size 12} (str k)]]))
         (into [:<>] (for [k (range (min 3 n)) :let [[x0 y0] (P k) [x1 y1] (P (rot k))]]
                       [mafs.core/Vector {:tail [(* 0.5 x0) (* 0.5 y0)] :tip [(* 0.5 x1) (* 0.5 y1)] :color "#E6A700"}]))
         [mafs.core/Vector {:tail [0 0] :tip [(* 0.92 (aget o AX)) (* 0.92 (aget o AY))] :color "#1F6FB4"}]
         [mafs.core/Vector {:tail [0 0] :tip [(* 0.78 (aget o BX)) (* 0.78 (aget o BY))] :color "#C0392B"}]
         [mafs.core/Text {:x (* 0.66 (aget o AX)) :y (* 0.66 (aget o AY)) :color "#1F6FB4" :size 15} "a"]
         [mafs.core/Text {:x (* 0.45 (aget o BX)) :y (* 0.45 (aget o BY)) :color "#C0392B" :size 15} "b"]
         [mafs.core/MovablePoint {:point [(aget o AX) (aget o AY)] :color "#1F6FB4" :constrain ONCIRCLE
                                  :on-move (fn [pt] (drag! :a pt))}]
         [mafs.core/MovablePoint {:point [(* 0.999 (aget o BX)) (* 0.999 (aget o BY))] :color "#C0392B" :constrain ONCIRCLE
                                  :on-move (fn [pt] (drag! :b pt))}]]
        [:p {:style {:font-size "0.85em" :opacity 0.75}}
         "Drag the hands a (blue) and b (red) round the clock, or move any control. The amber arrows are the rotation by a: Z/n acting on the hours, hour k carried to a + k (mod n), drawn for k = 0, 1, 2."]]
       [:div {:style {:flex "0 1 340px" :min-width "260px"}}
        PANEL
        [:div {:data-case-name kind
               :style {:font-size "1.2em" :font-weight 600 :margin "0.4em 0"
                       :color (if (= kind "congruent") "#2E8B57" "#C0392B")}}
         (if (= kind "congruent")
           (str (:a s) " ≡ " (:b s) " (mod " n "): congrui")
           (str (:a s) " ≢ " (:b s) " (mod " n "): incongrui"))]
        (let [tex (str "b = a + k\\,n:\\quad " (f (:b s) 0) " = " (f (:a s) 0) " + " (f (:k s) 2) "\\times " n)]
          [:div {:data-clock-tex tex} [:f> nextjournal.clerk.render/render-katex tex {:inline? false}]])
        [:table {:style {:font-size "0.9em"}}
         [:tbody
          [:tr [:td "least residue of a (art. 4):"] [:td {:data-residue-a true} ra]]
          [:tr [:td "least residue of b:"] [:td {:data-residue-b true} rb]]
          [:tr [:td "distance of (b − a)/n from an integer:"] [:td (f (/ (aget o GAP) n) 4)]]]]
        [:p {:style {:margin "0.5em 0 0.2em"}} [:strong "Try this:"]]
        (into [:div] (for [[label st] PRESETS]
                       [:button {:type "button" :data-preset label :style {:margin "0.2em"}
                                 ;; a preset is a solved state: set the exact state, then the panel without a solve
                                 :on-click (fn [] (reset! ?s st) (reset! pushing true) (reset! ?p st) (reset! pushing false))} label]))]]]))

(defn clock
  "n, a, b, k tied by b = a + k n (structure.tie data congruence/clock); a
   and b on the clock of n hours, the rotation of Z/n drawn; congruent or
   not, named live (congruence/clock-cases)."
  []
  (binding [vc/*backend* :raster alexandria.raster/*simplify?* false]
    (let [t c/clock ks (:keys t)
          idx (st/index c/clock-readout)
          st0 (mapv clock-start ks)
          [tsym tform] (tie/kernel-binding "tie" (st/kernel t) st0)
          [rsym rform] (tie/kernel-binding "read" (st/readout-kernel c/clock-readout) st0)
          [vsym vform] (tie/kernel-binding "vertex" (fn [k n] (fn [_] (c/vertex k n))) [0 12])
          [hsym hform] (tie/kernel-binding "hand" (fn [a k n] (fn [_] [(c/hand a k n)])) [0 1 12])]
      (ev/with-let [!p clock-start !s clock-start]
        (list 'reagent.core/with-let
              (into [tsym tform rsym rform vsym vform hsym hform]
                    (tie/locals {:tie-sym tsym :p !p :s !s :keys ks :ranges (:ranges t) :counter "__clockSolves"}))
              (flow/template
               {'?s !s '?p !p '?r rsym '?v vsym '?h hsym
                'CASE (case-form c/clock-cases idx)
                'RA (idx :ra) 'RB (idx :rb) 'AX (idx :ax) 'AY (idx :ay) 'BX (idx :bx) 'BY (idx :by) 'GAP (idx :gap)
                'DRAG (flow/template {'?s !s '?p !p} drag-hand)
                'ONCIRCLE on-circle
                'PRESETS clock-presets
                'PANEL (leva/sub-panel {:fill true :flat true :title-bar false}
                                       (leva/controls {:atom !p :schema (tie/schema ks (:ranges t) c/clock-labels)}))}
               clock-body)
              (list 'finally (tie/unwatch-form !p)))))))

;; ---------------------------------------------------------------------------
;; 2. Powers: Fermat and Euler

(defn powers-data
  "For each modulus of congruence/moduli, the facts the page shows, from
   structure.ring and structure.group (congruence/modulus-facts)."
  []
  (into (sorted-map)
        (for [m c/moduli :let [f (c/modulus-facts m)]]
          [m (-> f (update :orders #(into {} %)) (assoc :prime (= (:phi f) (dec m))))])))

(def ^:private powers-body
  '(let [{:keys [m a n]} @?s
         D (get DATA m)
         unit? (boolean (some #{a} (:units D)))
         walk (loop [i 0 x 1 acc [1]]
                (if (>= i n) acc
                    (let [y (js/Math.round (aget (?step (array 0) (array a x m)) 0))]
                      (recur (inc i) y (conj acc y)))))
         ord (if unit? (get (:orders D) a) 0)
         o (array ord (if unit? (- (:phi D) ord) 0))
         kind CASE
         P (fn [k] (let [q (?v (array 0) (array k m))] [(aget q 0) (aget q 1)]))
         done? (and unit? (>= n ord))]
     [:div {:data-widget "powers" :data-m (str m) :data-a (str a) :data-order (str ord)
            :data-case kind :data-n (str n) :data-last (str (peek walk)) :data-group-name (:name D)}
      [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
       [:div {:style {:flex "1 1 400px" :min-width "300px"}}
        [mafs.core/Mafs {:height 400 :viewBox {:x [-1.4 1.4] :y [-1.4 1.4]}}
         [mafs.core/Circle {:center [0 0] :radius 1 :color "#8A9BB0" :fillOpacity 0}]
         (into [:<>] (for [k (range m) :let [[x y] (P k) u? (some #{k} (:units D))]]
                       [:<>
                        [mafs.core/Point {:x x :y y :color (if u? "#1F6FB4" "#BBBBBB")}]
                        [mafs.core/Text {:x (* 1.15 x) :y (* 1.15 y) :size 11} (str k)]]))
         (when (> (count walk) 1)
           [mafs.core/Polyline {:points (mapv P walk) :color "#C0392B" :weight 2}])
         (into [:<>] (for [x walk :let [[px py] (P x)]] [mafs.core/Point {:x px :y py :color "#C0392B"}]))]
        [:p {:style {:font-size "0.85em" :opacity 0.75}}
         "Blue hours are the units (gcd with m equal to 1), grey the rest. Each click multiplies by a once: a acting on the clock in the group of units mod m, one step of a raster kernel."]]
       [:div {:style {:flex "0 1 360px" :min-width "260px" :font-size "0.95em"}}
        [:label "modulus m "
         [:input {:type "range" :min 3 :max 40 :step 1 :value m :data-control "m"
                  :on-change (fn [e] (let [m' (js/parseInt (.. e -target -value))]
                                       (swap! ?s assoc :m m' :a (min a (dec m')) :n 0)))}] " " m]
        [:br]
        [:label "base a "
         [:input {:type "range" :min 1 :max (dec m) :step 1 :value a :data-control "a"
                  :on-change (fn [e] (swap! ?s assoc :a (js/parseInt (.. e -target -value)) :n 0))}] " " a]
        [:div
         [:button {:type "button" :data-act "next" :on-click (fn [] (swap! ?s update :n (fn [k] (min 60 (inc k)))))} "× a"]
         [:button {:type "button" :data-act "all" :on-click (fn [] (swap! ?s assoc :n (if unit? ord m)))} "until it returns"]
         [:button {:type "button" :data-act "reset" :on-click (fn [] (swap! ?s assoc :n 0))} "start again"]]
        [:div {:style {:font-family "ui-monospace, monospace" :margin "0.4em 0" :font-size "0.9em"} :data-walk true}
         (clojure.string/join ",  " (map-indexed (fn [i x] (str a "^" i " ≡ " x)) walk))]
        [:div {:data-case-name kind
               :style {:font-size "1.12em" :font-weight 600 :margin "0.4em 0"
                       :color (case kind "primitive-root" "#2E8B57" "divides" "#1F6FB4" "#C0392B")}}
         (case kind
           "primitive-root" (str a " is a primitive root of " m ": its order " ord " is φ(" m ") = " (:phi D) " (art. 57)")
           "divides" (str a " has order " ord ", a divisor of φ(" m ") = " (:phi D) " (art. 49)")
           (str a " is not a unit mod " m ": its powers never come back to 1"))]
        [:div {:style {:font-size "0.9em"}}
         "the units mod " m ": " [:strong (:name D)]
         (if (seq (:roots D))
           (str "; primitive roots " (clojure.string/join ", " (:roots D)))
           "; no primitive root")]
        (when done?
          [:div {:data-fermat true :style {:margin-top "0.3em"}}
           (str a "^" (:phi D) " ≡ 1 (mod " m ")"
                (if (:prime D) ": Fermat's theorem, p − 1 = φ(p)" ": Euler's theorem, with φ(m) for p − 1"))])
        [:p {:style {:margin "0.5em 0 0.2em"}} [:strong "Try this:"]]
        (into [:div] (for [[label st] [["Fermat's example: 3 mod 13" {:m 13 :a 3 :n 3}]
                                       ["Gauss: 3 mod 17, a primitive root" {:m 17 :a 3 :n 16}]
                                       ["2 mod 15: Euler, φ = 8" {:m 15 :a 2 :n 4}]
                                       ["6 mod 15: not a unit" {:m 15 :a 6 :n 4}]]]
                       [:button {:type "button" :data-preset label :style {:margin "0.2em"}
                                 :on-click (fn [] (reset! ?s st))} label]))]]]))

(defn powers
  "The walk 1, a, a^2 ... (mod m), one raster step per click, the case
   named from congruence/power-cases."
  []
  (binding [vc/*backend* :raster alexandria.raster/*simplify?* false]
    (let [[ssym sform] (tie/kernel-binding "step" (fn [a x m] (fn [_] [(c/step a x m)])) [3 1 7])
          [vsym vform] (tie/kernel-binding "vertex" (fn [k n] (fn [_] (c/vertex k n))) [0 7])]
      (ev/with-let [!s {:m 13 :a 3 :n 0}]
        (list 'reagent.core/with-let [ssym sform vsym vform]
              (flow/template {'?s !s '?step ssym '?v vsym 'DATA (powers-data)
                              'CASE (case-form c/power-cases {:order 0 :short 1})}
                             powers-body))))))

;; ---------------------------------------------------------------------------
;; 3. The multiplication table

(defn table-data
  "{n {:rows :perm :units :name :factors}} from structure.ring (rows,
   permutation rows, units) and its structure naming."
  []
  (into (sorted-map)
        (for [n c/table-moduli :let [R (ring/zmod n) f (c/table-facts n)]]
          [n {:rows (mapv #(ring/row R %) (ring/elements R)) :perm (:perm f) :units (:units f)
              :name (:name (:structure f)) :factors (count (:factors (:structure f)))}])))

(def ^:private table-body
  '(let [{:keys [n a]} @?s
         a (min a (dec n))
         D (get DATA n)
         row (nth (:rows D) a)
         row-kind (let [o (array (- n (count (distinct row))))] TCASE)
         group-kind (let [o (array (:factors D))] GCASE)
         cell (max 12 (min 26 (/ 480 n)))]
     [:div {:data-widget "table" :data-n (str n) :data-a (str a) :data-row-case row-kind
            :data-group-case group-kind :data-group-name (:name D)}
      [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
       [:div {:style {:flex "1 1 480px" :overflow-x "auto"}}
        [:svg {:viewBox (str "0 0 " (* cell (inc n)) " " (* cell (inc n))) :style {:width "100%" :max-width "520px"}}
         (into [:g] (for [i (range n) j (range n)
                          :let [v (get-in D [:rows i j]) u? (nth (:perm D) i)]]
                      [:g {:on-click (fn [] (swap! ?s assoc :a i)) :style {:cursor "pointer"}}
                       [:rect {:x (* cell (inc j)) :y (* cell (inc i)) :width cell :height cell
                               :fill (if (= i a)
                                       (if u? "#2E8B5766" "#C0392B55")
                                       (str "hsl(" (js/Math.round (* 360 (/ v n))) ",55%," (if u? "72%" "90%") ")"))
                               :stroke "#FFFFFF" :stroke-width 0.5}]
                       (when (>= cell 14)
                         [:text {:x (+ (* cell (inc j)) (/ cell 2)) :y (+ (* cell (inc i)) (* 0.68 cell))
                                 :font-size (* 0.45 cell) :text-anchor "middle" :fill "#222"} v])]))
         (into [:g] (for [i (range n)]
                      [:g
                       [:text {:x (/ cell 2) :y (+ (* cell (inc i)) (* 0.68 cell)) :font-size (* 0.45 cell) :text-anchor "middle"
                               :font-weight (if (nth (:perm D) i) 700 400) :fill (if (nth (:perm D) i) "#2E8B57" "#888")} i]
                       [:text {:x (+ (* cell (inc i)) (/ cell 2)) :y (* 0.68 cell) :font-size (* 0.45 cell)
                               :text-anchor "middle" :fill "#888"} i]]))]]
       [:div {:style {:flex "0 1 340px" :min-width "260px" :font-size "0.95em"}}
        [:label "modulus n "
         [:input {:type "range" :min 2 :max 30 :step 1 :value n :data-control "n"
                  :on-change (fn [e] (swap! ?s assoc :n (js/parseInt (.. e -target -value))))}] " " n]
        [:br]
        [:label "row a "
         [:input {:type "range" :min 0 :max (dec n) :step 1 :value a :data-control "a"
                  :on-change (fn [e] (swap! ?s assoc :a (js/parseInt (.. e -target -value))))}] " " a]
        [:div {:style {:font-family "ui-monospace, monospace" :margin "0.4em 0" :font-size "0.9em"}}
         a " × (0 … " (dec n) ") = " (clojure.string/join " " row)]
        [:div {:data-case-name row-kind
               :style {:font-weight 600 :font-size "1.1em" :color (if (= row-kind "permutation") "#2E8B57" "#C0392B")}}
         (if (= row-kind "permutation")
           (str "row " a " is a permutation: gcd(" a ", " n ") = 1, " a " is a unit")
           (str "row " a " is not a permutation: gcd(" a ", " n ") > 1 and values repeat"))]
        [:div {:style {:margin-top "0.6em"}}
         "the units mod " n ": " (clojure.string/join ", " (:units D)) " (" (count (:units D)) " of them)"]
        [:div {:data-group true :style {:font-size "1.1em" :font-weight 600 :margin "0.3em 0"
                                        :color (if (= group-kind "cyclic") "#2E8B57" "#7B5EA7")}}
         "their group: " (:name D) (if (= group-kind "cyclic") ", cyclic" ", not cyclic: no primitive root")]
        [:p {:style {:font-size "0.85em" :opacity 0.75}}
         "Click a row or move the sliders. Green row labels are the units. Their rows are permutations of 0 … n − 1, and the units compose as a group, named here as a product of cyclic groups from the orders of its elements."]
        (into [:div] (for [[label st] [["n = 8: Klein's four-group" {:n 8 :a 3}]
                                       ["n = 9: cyclic of order 6" {:n 9 :a 2}]
                                       ["n = 15: Z/2 × Z/4" {:n 15 :a 2}]
                                       ["n = 12, a = 4: not a unit" {:n 12 :a 4}]]]
                       [:button {:type "button" :data-preset label :style {:margin "0.2em"}
                                 :on-click (fn [] (reset! ?s st))} label]))]]]))

(defn table
  "The multiplication table mod n; a row picked is a permutation exactly
   when it is a unit's row; the unit group named (congruence/table-cases,
   group-cases)."
  []
  (ev/with-let [!s {:n 12 :a 5}]
    (flow/template {'?s !s 'DATA (table-data)
                    'TCASE (case-form c/table-cases {:repeats 0})
                    'GCASE (case-form c/group-cases {:factors 0})}
                   table-body)))

;; ---------------------------------------------------------------------------
;; 4. Reciprocity

(def ^:private reciprocity-body
  '(let [{:keys [i j]} @?s
         C (get-in GRID [i j])
         {:keys [p q pq qp]} C
         o (array (- p q) (mod p 4) (mod q 4))
         kind CASE
         cell 22
         sym (fn [s] (case s 1 "+1" -1 "−1" "0"))]
     [:div {:data-widget "reciprocity" :data-p (str p) :data-q (str q) :data-case kind
            :data-pq (str pq) :data-qp (str qp) :data-law (str (:law? C))}
      [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
       [:div {:style {:flex "1 1 440px" :overflow-x "auto"}}
        [:svg {:viewBox (str "0 0 " (* cell (inc N)) " " (* cell (inc N))) :style {:width "100%" :max-width "460px"}}
         (into [:g] (for [[a row] (map-indexed vector GRID) [b c] (map-indexed vector row)]
                      [:rect {:x (* cell (inc b)) :y (* cell (inc a)) :width cell :height cell
                              :data-cell (str (:p c) "," (:q c))
                              :fill (cond (= (:p c) (:q c)) "#DDDDDD"
                                          (= (:pq c) (:qp c)) (if (= 1 (:pq c)) "#2E8B57" "#1F6FB4")
                                          :else "#C0392B")
                              :opacity (if (:both3 c) 1 0.45)
                              :stroke (if (and (= a i) (= b j)) "#111" "#FFF")
                              :stroke-width (if (and (= a i) (= b j)) 2.5 0.6)
                              :style {:cursor "pointer"}
                              :on-click (fn [] (reset! ?s {:i a :j b}))}]))
         (into [:g] (for [[a pr] (map-indexed vector PS)]
                      [:g
                       [:text {:x (/ cell 2) :y (+ (* cell (inc a)) 15) :font-size 10 :text-anchor "middle"
                               :fill (if (= 3 (mod pr 4)) "#C0392B" "#555")} pr]
                       [:text {:x (+ (* cell (inc a)) (/ cell 2)) :y 15 :font-size 10 :text-anchor "middle"
                               :fill (if (= 3 (mod pr 4)) "#C0392B" "#555")} pr]]))]]
       [:div {:style {:flex "0 1 360px" :min-width "260px" :font-size "0.95em"}}
        [:label "p " [:input {:type "range" :min 0 :max (dec N) :step 1 :value i :data-control "p"
                              :on-change (fn [e] (swap! ?s assoc :i (js/parseInt (.. e -target -value))))}] " " p]
        [:br]
        [:label "q " [:input {:type "range" :min 0 :max (dec N) :step 1 :value j :data-control "q"
                              :on-change (fn [e] (swap! ?s assoc :j (js/parseInt (.. e -target -value))))}] " " q]
        [:div {:style {:font-family "ui-monospace, monospace" :margin-top "0.4em"}}
         "p = " p " ≡ " (mod p 4) ",  q = " q " ≡ " (mod q 4) " (mod 4)"]
        [:div {:data-symbols true :style {:margin "0.4em 0"}}
         "(p/q) = (" p "/" q ") = " (sym pq) ",   (q/p) = (" q "/" p ") = " (sym qp)]
        [:div {:data-case-name kind :style {:font-size "1.12em" :font-weight 600
                                            :color (case kind "exceptional" "#C0392B" "ordinary" "#2E8B57" "#888")}}
         (case kind
           "same-prime" "p = q: nothing to compare"
           "exceptional" (str "both ≡ 3 (mod 4), the exceptional case: the symbols are opposite, (p/q)(q/p) = " (* pq qp))
           (str "one of them ≡ 1 (mod 4): the symbols agree, (p/q)(q/p) = " (* pq qp)))]
        [:div {:data-law-check true} (if (:law? C) "the fundamental theorem holds in this cell" "the law fails here")]
        [:p {:style {:font-size "0.85em" :opacity 0.8}}
         "Row p, column q. Green: both symbols +1; blue: both −1; red: they differ. The bright cells are those with p ≡ q ≡ 3 (mod 4), and exactly those are red. Primes ≡ 3 (mod 4) are labelled in red."]
        [:p {:data-grid-summary true :style {:font-size "0.9em"}}
         (count (filter :law? (apply concat GRID))) " of " (* N N) " cells obey the law."]]]]))

(defn reciprocity
  "The grid of odd primes below 60, coloured by the Legendre character of
   structure.ring; the case of art. 131 named (congruence/reciprocity-cases)."
  []
  (ev/with-let [!s {:i 0 :j 2}]
    (flow/template {'?s !s 'GRID (c/reciprocity-grid) 'PS c/odd-primes 'N (count c/odd-primes)
                    'CASE (case-form c/reciprocity-cases {:pq-diff 0 :p4 1 :q4 2})}
                   reciprocity-body)))

;; ---------------------------------------------------------------------------
;; 5. pi(x) against Li(x)

(def ^:private primes-body
  '(let [i (:i @?s)
         {:keys [x pi li xlog better]} (nth ROWS i)
         kind (let [o (array better)] CASE)
         W 520 H 260
         lx (fn [v] (* W (/ (- (js/Math.log10 v) 1) 5)))
         ly (fn [v] (- H (* H (/ (js/Math.log10 (max 1 v)) 5))))
         pts (fn [k] (clojure.string/join " " (for [r ROWS] (str (lx (:x r)) "," (ly (get r k))))))]
     [:div {:data-widget "primes" :data-x (str x) :data-pi (str pi) :data-li (str li) :data-case kind}
      [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
       [:div {:style {:flex "1 1 520px"}}
        [:svg {:viewBox (str "-30 -10 " (+ W 40) " " (+ H 30)) :style {:width "100%" :max-width "560px"}}
         [:polyline {:points (pts :pi) :fill "none" :stroke "#111" :stroke-width 2}]
         [:polyline {:points (pts :li) :fill "none" :stroke "#C0392B" :stroke-width 1.5 :stroke-dasharray "5 3"}]
         [:polyline {:points (pts :xlog) :fill "none" :stroke "#1F6FB4" :stroke-width 1.5}]
         [:line {:x1 (lx x) :x2 (lx x) :y1 0 :y2 H :stroke "#E6A700"}]
         (into [:g] (for [e (range 1 7)]
                      [:text {:x (lx (js/Math.pow 10 e)) :y (+ H 16) :font-size 11 :text-anchor "middle"} (str "10^" e)]))
         [:text {:x 6 :y 14 :font-size 12} "log–log; black π(x), red dashed Li(x), blue x/log x"]]]
       [:div {:style {:flex "0 1 320px" :min-width "260px"}}
        [:label "x " [:input {:type "range" :min 0 :max (dec (count ROWS)) :step 1 :value i :data-control "x"
                              :on-change (fn [e] (swap! ?s assoc :i (js/parseInt (.. e -target -value))))}] " " x]
        [:table {:style {:font-size "0.95em"}}
         [:tbody
          [:tr [:td "π(x), the primes up to x"] [:td {:data-pi true} pi]]
          [:tr [:td "Li(x) = ∫₂ˣ dt / log t"] [:td (.toFixed li 1)]]
          [:tr [:td "x / log x"] [:td (.toFixed xlog 1)]]
          [:tr [:td "Li(x) − π(x)"] [:td (.toFixed (- li pi) 1)]]
          [:tr [:td "π(x) − x / log x"] [:td (.toFixed (- pi xlog) 1)]]]]
        [:div {:data-case-name kind :style {:font-weight 600 :margin "0.4em 0"}}
         (if (= kind "li-closer") "Li(x) is the closer count here (Gauss)" "x / log x is the closer count here (Legendre)")]]]]))

(defn primes
  "pi(x), Li(x) and x/log x on log-spaced samples 10 .. 10^6 (sieve and
   raster quadgk), the slider for x, the closer count named
   (congruence/prime-cases)."
  []
  (ev/with-let [!s {:i 72}]
    (flow/template {'?s !s 'ROWS (c/primes-table) 'CASE (case-form c/prime-cases {:better 0})}
                   primes-body)))

;; ---------------------------------------------------------------------------
;; The derivation

(defn derivation
  "The relations as TeX and Emmy's own forms; the grades are shown by
   history-of-math.page/grade-badges."
  []
  (clerk/col
   (apply clerk/col (map clerk/tex (vals (select-keys c/tex [:congruence :order :euler :law :li]))))
   (clerk/html [:div {:style {:font-size "0.9em"}} "Emmy's own forms: "
                (into [:span] (interpose ", " (for [[k v] (c/derived-tex)] [:code (str (name k) ": " v)])))])))
