(ns alexandria.klein.widgets
  "The widgets of the Erlangen notebook (Klein 1872), emmy-viewers fragments
   on the raster backend.

   `hierarchy-player`  the chain Euclid < similarity < affine < projective,
                       stepped: each step applies one map of the next group to
                       Klein's figure and lists, live from the raster kernel,
                       which properties survived.
   `playground`        the figure (triangle ABC with its circumcircle, DE
                       through C parallel to AB, the midpoint M of AB, the
                       cross-ratio of D, C, F, E) with draggable vertices;
                       the reader picks a group (or the hyperbolic motions of
                       Poincare's disk, alexandria.non-euclidean.disk) and
                       moves its parameters (sliders, and a draggable
                       translation handle). Each property carries a live
                       invariant / not-invariant badge; the smallest group
                       holding the current map is named live.
   `invariant-table`   the table of invariants per group (raster on the JVM).

   Curves are not drawn with mafs/parametric (its simplifier exhausts the
   heap on these maps): the image of each segment and of the circumcircle is
   one unsimplified raster kernel (alexandria.klein.erlangen/image-curve)
   sampled into a mafs/polyline by the pts! sampler of
   alexandria.non-euclidean.widgets."
  (:require [alexandria.klein.erlangen :as k]
            [alexandria.medium.flow :as flow]
            [alexandria.non-euclidean.disk :as d]
            [alexandria.non-euclidean.widgets :as nw]
            [emmy.env :as e]
            [emmy.mafs :as mafs]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [nextjournal.clerk :as clerk]))

(def colours {:orig "#8A9BB0" :tri "#1F6FB4" :circle "#7B5EA7" :par "#E39B2D" :mid "#2E8B57"
              :cr "#C0392B" :point "#111111" :handle "#C0392B" :ok "#2E8B57" :bad "#C0392B"})

(def page-tolerance
  "The page's tolerance for 'unchanged' (relative): the browser's raster
   atan is good to about 1e-4 rad, every real change in the playground is
   far larger."
  1e-3)

(defn- button [attrs title on-click]
  [:button (merge {:type "button" :on-click on-click
                   :style {:margin "0.2em" :padding "0.3em 0.6em" :cursor "pointer" :border-radius "0.3em"
                           :border "1px solid currentColor" :background "transparent" :color "inherit"}}
                  attrs)
   title])

(def fig-keys [:ax0 :ay0 :bx0 :by0 :cx0 :cy0])

(def args-form
  "Quoted (fn [s] js-array): the compare kernel's arguments for state s."
  (list 'fn '[s]
        (list 'into-array
              (list 'concat (list 'get k/flags '(:g s))
                    (list 'map '(fn [kk] (get s kk)) (vec (concat k/params fig-keys)))))))

(def verdict-form
  "Quoted (fn [o i] bool): property i (order of erlangen/verdict-keys)
   survived, by the page tolerance; i = 3 4 6 are 'zero' properties, i = 7
   the hyperbolic length, which also needs both image points in the disk."
  (list 'fn '[o i]
        (list 'let ['b '(aget o i) 'a '(aget o (+ 9 i)) 'tol page-tolerance
                    'close '(< (js/Math.abs (- a b)) (* tol (max 1 (js/Math.abs b))))]
              '(cond (contains? #{3 4 6} i) (< (js/Math.abs a) tol)
                     (= i 7) (and (> (aget o 17) 0) close)
                     :else close))))

(def smallest-form
  "Quoted (fn [s o] kw): the smallest group of the chain holding the map."
  (list 'fn '[s o]
        (list 'let ['eps 1e-9 'dv '(fn [j] (aget o (+ 32 j)))]
              '(cond (= :hyperbolic (:g s)) (if (and (< (dv 3) eps) (< (js/Math.abs (:theta s)) eps)) :identity :hyperbolic)
                     (> (dv 2) eps) :projective
                     (> (dv 1) eps) :affine
                     (> (dv 0) eps) :similarity
                     (> (dv 4) eps) :euclid
                     :else :identity))))

(defn- scene
  "Mafs children of the figure and its image. Browser locals: o and o0 (the
   kernel at the current map and at the identity), base and base0 (their
   argument vectors), hyp (the disk is drawn), and when drag? the drag!
   and handle! functions."
  [csym drag?]
  (let [g (fn [o kk] (list 'aget o (k/compare-index kk)))
        P (fn [o n] [(g o (keyword (str n "'x"))) (g o (keyword (str n "'y")))])
        crv (fn [b kind [x0 y0] [x1 y1] opts]
              (mafs/polyline (assoc opts :points (list 'pts! csym (list 'conj b kind x0 y0 x1 y1) 0 1 (if (= kind 1) 96 40)))))
        figure (fn [o b orig?]
                 (let [st (fn [c] (if orig? {:color (colours :orig) :style "dashed" :weight 1.5} {:color (colours c) :weight 3}))
                       [A B C D E] (map #(P 'o0 %) ["A" "B" "C" "D" "E"])]
                   [:<>
                    (crv b 1 [0 0] [0 0] (st :circle))
                    (crv b 0 A B (st :tri)) (crv b 0 B C (st :tri)) (crv b 0 C A (st :tri))
                    (crv b 0 D E (st :par))]))]
    [:<>
     (list 'when 'hyp (mafs/circle {:center [0 0] :radius 1 :color "#555555" :fill-opacity 0.03}))
     (figure 'o0 'base0 true)
     (figure 'o 'base false)
     (into [:<>] (for [[n c] [["M" :mid] ["D" :par] ["E" :par] ["F" :cr]]]
                   [:<> (mafs/point {:x (first (P 'o n)) :y (second (P 'o n)) :color (colours c)})
                    (mafs/text (str n "'") {:x (first (P 'o n)) :y (second (P 'o n)) :attach "s" :attach-distance 14})]))
     (into [:<>] (for [n ["A" "B" "C"]]
                   [:<> (mafs/point {:x (first (P 'o n)) :y (second (P 'o n)) :color (colours :tri)})
                    (mafs/text (str n "'") {:x (first (P 'o n)) :y (second (P 'o n)) :attach "nw" :attach-distance 16})]))
     (if drag?
       (into [:<>] (for [[n kx ky] [["A" :ax0 :ay0] ["B" :bx0 :by0] ["C" :cx0 :cy0]]]
                     [:<>
                      (mafs/movable-point {:point (P 'o0 n) :color (colours :point)
                                           :on-move (list 'fn '[[x y]] (list 'drag! kx ky 'x 'y))})
                      (mafs/text n {:x (first (P 'o0 n)) :y (second (P 'o0 n)) :attach "se" :attach-distance 24})]))
       (into [:<>] (for [n ["A" "B" "C"]]
                     [:<> (mafs/point {:x (first (P 'o0 n)) :y (second (P 'o0 n)) :color (colours :orig)})
                      (mafs/text n {:x (first (P 'o0 n)) :y (second (P 'o0 n)) :attach "se" :attach-distance 14})])))]))

(def property-rows
  "[i key label unit-form] of the readout rows."
  [[0 :length "length |AB|"]
   [1 :angle "angle ACB"]
   [2 :ratio "ratio |AB| : |BC|"]
   [3 :parallel "AB ∥ DE  (|sin| of their angle)"]
   [4 :midpoint "M the midpoint  (|M − mid AB| / |AB|)"]
   [5 :cross-ratio "cross-ratio (D, E; C, F)"]
   [6 :circle "on one circle  (gap of a 4th point)"]
   [7 :hyp "hyperbolic length d(A, B)"]])

(defn- readouts
  "The table of properties before / after with live badges (browser locals o, ok?)."
  []
  [:table {:style {:font-family "ui-monospace, monospace" :font-size "0.85em" :border-collapse "collapse"}}
   [:thead [:tr [:th {:style {:text-align "left"}} "property"] [:th "before"] [:th "after"] [:th ""]]]
   (into [:tbody]
         (for [[i kk label] property-rows
               :let [conv (if (= kk :angle) (/ 180 Math/PI) 1)
                     f (fn [j] (flow/fmt (list '* (list 'aget 'o j) conv) 4))]]
           [:tr {:data-property (name kk) :data-invariant (list 'if (list 'ok? 'o i) "yes" "no")}
            [:td {:style {:padding "0.1em 0.5em 0.1em 0"}} label]
            [:td {:style {:padding "0 0.5em"}} (f i) (when (= kk :angle) "°")]
            [:td {:style {:padding "0 0.5em"}} (f (+ 9 i)) (when (= kk :angle) "°")]
            [:td [:span {:style {:display "inline-block" :padding "0.05em 0.6em" :border-radius "0.6em" :color "white"
                                 :background (list 'if (list 'ok? 'o i) (colours :ok) (colours :bad))}}
                  (list 'if (list 'ok? 'o i) "invariant" "not invariant")]]]))])

(def case-text
  {:identity "the identity: every map keeps everything"
   :euclid "a Euclidean motion: Euclidean metric geometry, lengths and everything below them kept"
   :similarity "a similarity (Klein's principal group): elementary geometry, shapes kept, sizes not"
   :affine "an affine map: affine geometry, parallels, midpoints and ratios along a line kept, angles and circles not"
   :projective "a projective map: projective geometry, only incidence and the cross-ratio kept"
   :hyperbolic "a motion of Poincaré's disk: hyperbolic geometry, its length, angles and circles kept, Euclid's lengths not"})

(defn- kernels []
  (let [init (vec (concat (k/flags :euclid) (map k/identity-params k/params) (map k/figure-start fig-keys)))]
    {:compare (flow/kernel "erlangen" k/compare-kernel init [0])
     :curve (nw/curve-kernel "image" k/image-curve (into init [0 0 0 1 1]))
     :drag (flow/kernel "drag" d/drag-kernel [0 0.1 0.1] [0])}))

;; ---------------------------------------------------------------------------
;; 1. The hierarchy, stepped

(def player-steps
  [{:g :euclid :p {:theta 0.6 :tx 0.35 :ty -0.25 :sigma -1}
    :text "Euclidean motions: turn the figure, slide it, or reflect it. Klein: properties that do not depend on the position in space."}
   {:g :similarity :p {:theta 0.6 :tx 0.35 :ty -0.25 :sigma -1 :s 1.5}
    :text "Add a change of scale: the principal group. Klein: properties independent of the absolute magnitude. Lengths go; angles and ratios stay."}
   {:g :affine :p {:theta 0.6 :tx 0.2 :ty -0.25 :s 1.1 :r 1.3 :k 0.8}
    :text "Add stretches and shears: the affine group. The circle becomes an ellipse and angles change, but parallels stay parallel and M stays the midpoint."}
   {:g :projective :p {:theta 0.3 :s 1.0 :r 1.2 :k 0.5 :p 0.45 :q 0.3}
    :text "Add projection from a point: the projective group. Parallels now meet and midpoints drift; the cross-ratio of D, C, F, E survives, and nothing else here."}])

(defn hierarchy-player
  "The chain Euclid < similarity < affine < projective, one step per group."
  []
  (binding [vc/*backend* :raster]
    (let [{[csym cform] :compare [vsym vform] :curve} (kernels)
          steps (mapv (fn [{:keys [g p text]}]
                        {:s (merge k/identity-params k/figure-start p {:g g}) :text text
                         :name (k/group-names g) :geometry (k/geometry-names g)})
                      player-steps)
          s0 (merge k/identity-params k/figure-start {:g :euclid})]
      (ev/with-let [!s {:step 0}]
        (flow/template
         {'?s !s}
         (list 'reagent.core/with-let
               [csym cform vsym vform 'pts! nw/pts-form 'args args-form 'ok? verdict-form]
               (list 'let ['i '(max 0 (min 3 (:step @?s)))
                           'st (list 'nth steps 'i)
                           'base '(vec (args (:s st)))
                           'base0 (list 'vec (list 'args s0))
                           'o (list csym '(array 0) '(into-array base))
                           'o0 (list csym '(array 0) '(into-array base0))
                           'hyp false
                           'kept '(filterv (fn [j] (ok? o j)) (range 8))]
                     [:div {:data-figure "erlangen-hierarchy" :data-step '(str i) :data-kept '(str (count kept))}
                      [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                       [:div {:style {:flex "1 1 400px" :min-width "300px"}}
                        (mafs/mafs {:view-box {:x [-1.6 1.6] :y [-1.4 1.4]} :height 380}
                                   (scene vsym false))]
                       [:div {:style {:flex "1 1 300px" :min-width "260px" :font-size "0.95em" :line-height 1.5}}
                        [:div {:style {:opacity 0.7}} "step " '(inc i) " of 4: " [:strong '(:name st)]]
                        [:p {:data-step-text true} '(:text st)]
                        [:div {:style {:font-weight 600}} "kept: " [:span {:data-kept-count true} '(count kept)] " of 8 properties"]
                        (into [:ul {:style {:margin "0.2em 0" :font-size "0.9em"}}]
                              (for [[j _ label] property-rows]
                                [:li {:style {:color (list 'if (list 'ok? 'o j) (colours :ok) (colours :bad))}}
                                 (list 'if (list 'ok? 'o j) "✓ " "✗ ") label]))
                        [:div
                         (button {:data-act "prev"} "◀ smaller group" (list 'fn [] (list 'swap! !s 'update :step '(fn [j] (max 0 (dec j))))))
                         (button {:data-act "next"} "larger group ▶" (list 'fn [] (list 'swap! !s 'update :step '(fn [j] (min 3 (inc j))))))
                         (button {:data-act "first"} "back to Euclid" (list 'fn [] (list 'reset! !s {:step 0})))]]]
                      [:p {:style {:font-size "0.85em" :opacity 0.8}}
                       "Dashed grey: Klein's figure. Coloured: its image. Each step adds transformations to the group, and the list of what survives gets shorter."]])))))))

;; ---------------------------------------------------------------------------
;; 2. The playground

(def start (merge k/identity-params k/figure-start {:g :euclid}))

(def presets
  [["shear" "A shear kills angles but keeps midpoints"
    (merge start {:g :affine :k 0.9})]
   ["projective" "A projective map kills parallels but keeps the cross-ratio"
    (merge start {:g :projective :p 0.4 :q 0.25 :theta 0.2})]
   ["moebius" "A Moebius map keeps hyperbolic length"
    (merge start {:g :hyperbolic :ax 0.45 :ay -0.2 :theta 0.6})]
   ["similarity" "A similarity changes only the size"
    (merge start {:g :similarity :s 1.6 :theta 0.5 :tx 0.2})]
   ["subgroup" "Choose the projective group but move only the angle: a smaller group's map"
    (merge start {:g :projective :theta 0.9})]])

(def slider-of
  "Which sliders each group shows."
  {:euclid [:theta :tx :ty :sigma]
   :similarity [:theta :s :tx :ty :sigma]
   :affine [:theta :s :r :k :tx :ty :sigma]
   :projective [:theta :s :r :k :tx :ty :sigma :p :q]
   :hyperbolic [:theta :ax :ay]})

(def slider-labels
  {:theta "rotation θ" :s "scale s" :r "stretch r" :k "shear k" :sigma "mirror (−1) or not (+1)"
   :tx "translation tₓ" :ty "translation t_y" :p "perspective p" :q "perspective q"
   :ax "disk point a, x" :ay "disk point a, y"})

(defn- slider [kk]
  (let [[lo hi step] (k/param-ranges kk)]
    [:label {:style {:display "block" :font-size "0.85em"}}
     (slider-labels kk) " = " (flow/fmt (list 'get '@?s kk) 2)
     [:input {:type "range" :min lo :max hi :step step :data-param (name kk) :style {:width "100%"}
              :value (list 'get '@?s kk)
              :on-change (list 'fn '[ev] (list 'swap! '?s 'assoc kk '(js/parseFloat (.. ev -target -value))))}]]))

(defn playground
  "Pick a group, move its map, watch which properties survive."
  ([] (playground start))
  ([start]
   (binding [vc/*backend* :raster]
     (let [{[csym cform] :compare [vsym vform] :curve [dsym dform] :drag} (kernels)]
       (ev/with-let [!s start]
         (flow/template
          {'?s !s '?d dsym}
          (list 'reagent.core/with-let
                [csym cform vsym vform dsym dform 'pts! nw/pts-form 'args args-form 'ok? verdict-form
                 'smallest smallest-form
                 'drag! '(fn [kx ky x y]
                           (let [r (?d (array 0) (array 0 x y))]
                             (swap! ?s assoc kx (aget r 0) ky (aget r 1))))]
                (list 'let ['s '@?s
                            'hyp '(= :hyperbolic (:g s))
                            'base '(vec (args s))
                            'base0 (list 'vec (list 'args (list 'merge 's k/identity-params {:g :euclid})))
                            'o (list csym '(array 0) '(into-array base))
                            'o0 (list csym '(array 0) '(into-array base0))
                            'kind '(smallest s o)
                            'kept '(filterv (fn [j] (ok? o j)) (range 8))]
                      [:div {:data-playground "erlangen" :data-group '(name (:g s)) :data-case '(name kind)
                             :data-kept '(apply str (interpose "," kept))}
                       [:div {:style {:margin-bottom "0.4em"}}
                        [:strong "Group: "]
                        (into [:span]
                              (for [g k/groups]
                                (button {:data-group g
                                         :style {:margin "0.2em" :padding "0.3em 0.6em" :cursor "pointer" :border-radius "0.3em"
                                                 :border "1px solid currentColor" :color "inherit"
                                                 :background (list 'if (list '= g '(:g s)) "#1F6FB433" "transparent")}}
                                        (k/group-names g)
                                        (list 'fn [] (list 'swap! '?s 'assoc :g g)))))]
                       [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                        [:div {:style {:flex "1 1 440px" :min-width "300px"}}
                         (mafs/mafs {:view-box {:x [-1.7 1.7] :y [-1.5 1.5]} :height 460}
                                    (scene vsym true)
                                    (list 'if 'hyp
                                          (mafs/movable-point {:point ['(:ax s) '(:ay s)] :color (colours :handle)
                                                               :on-move '(fn [[x y]] (let [r (?d (array 0) (array 0 x y))
                                                                                           ax (max -0.9 (min 0.9 (aget r 0)))
                                                                                           ay (max -0.9 (min 0.9 (aget r 1)))]
                                                                                       (swap! ?s assoc :ax ax :ay ay)))})
                                          (mafs/movable-point {:point ['(:tx s) '(:ty s)] :color (colours :handle)
                                                               :on-move '(fn [[x y]] (swap! ?s assoc :tx (max -1.5 (min 1.5 x)) :ty (max -1.5 (min 1.5 y))))})))
                         [:div {:style {:font-size "0.85em" :opacity 0.8}}
                          "Drag A, B, C (black) to change the figure, and the red handle to move the map: the translation, or in the disk the point a that the centre goes to. Dashed grey: the figure; coloured: its image."]]
                        [:div {:style {:flex "0 1 360px" :min-width "280px"}}
                         (into [:div] (for [kk k/params]
                                        (list 'when (list 'some #{kk} (list 'get slider-of '(:g s))) (slider kk))))
                         (readouts)
                         [:div {:data-case-name '(name kind) :style {:font-weight 600 :font-size "1.05em" :margin "0.5em 0"}}
                          (list 'str "This map is " (list 'get case-text 'kind))]
                         [:div {:style {:font-size "0.9em"}}
                          "kept: " [:span {:data-kept-count true} '(count kept)] " of 8. "
                          (list 'when (list 'and '(not= kind (:g s)) '(not= kind :identity) '(not hyp))
                                (list 'str "You chose " (list 'get k/group-names '(:g s))
                                      ", but this map lies in the smaller group of " (list 'get k/group-names 'kind)
                                      ", so it keeps that group's invariants too."))]]]
                       [:p [:strong "Try this:"]]
                       (into [:div] (for [[id label v] presets]
                                      (button {:data-preset id} label (list 'fn [] (list 'reset! '?s v)))))
                       (button {:data-preset "reset"} "Reset" (list 'fn [] (list 'reset! '?s start)))]))))))))

;; ---------------------------------------------------------------------------
;; 3. The table

(defn invariant-table
  "Groups against properties, from raster on the JVM (erlangen/invariant-table)."
  []
  (let [tab (k/invariant-table)]
    (clerk/html
     [:table {:data-table "erlangen-invariants" :style {:font-size "0.85em" :border-collapse "collapse"}}
      (into [:thead [:tr [:th "group"] [:th "parameters"]]]
            (concat (for [kk k/verdict-keys] [:th {:style {:padding "0 0.4em"}} (k/property-labels kk)])
                    [[:th "invariants"]]))
      (into [:tbody]
            (for [{:keys [group invariant count dimension]} tab]
              (into [:tr [:td (k/group-names group)] [:td {:style {:text-align "center"}} dimension]]
                    (concat (for [kk k/verdict-keys]
                              [:td {:style {:text-align "center" :color (if (invariant kk) (colours :ok) (colours :bad))}}
                               (if (invariant kk) "✓" "✗")])
                            [[:td {:style {:text-align "center"}} count]]))))])))

(defn derivation
  "The two formulas the page proves, as TeX."
  []
  (clerk/col
   (clerk/tex "x \\mapsto \\frac{ax+b}{cx+d}:\\qquad (x_1,x_2;x_3,x_4) = \\frac{(x_3-x_1)(x_4-x_2)}{(x_4-x_1)(x_3-x_2)} \\;\\text{is unchanged}")
   (clerk/tex (str "\\text{Emmy: } (x_1',x_2';x_3',x_4') - (x_1,x_2;x_3,x_4) = " (e/->TeX (e/simplify (k/cross-ratio-difference)))))
   (clerk/tex "z \\mapsto \\frac{e^{i\\theta}z + a}{1 + \\bar a e^{i\\theta} z}:\\qquad \\cosh d(p,q) = 1 + \\frac{2|p-q|^2}{(1-|p|^2)(1-|q|^2)} \\;\\text{is unchanged}")))
