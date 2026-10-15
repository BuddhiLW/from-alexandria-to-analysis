(ns alexandria.moebius.widgets
  "The widgets of the Möbius notebook (1865).

     band-widget        a strip of k cells with t = 0..3 half twists, drawn in
                        space; a normal arrow carried once round the core
                        (slider) returns flipped or not; V, E, F, chi, the
                        orientation character and the boundary count come from
                        alexandria.structure.surfaces/strip-report; a cut along
                        the core button gives the pieces.
     content-widget     Möbius' content of a cube, of the cube with one face
                        written the other way round, of the decahedron of §8
                        and of the twisted strip: the orientation check and
                        the signed volume from two different apexes.

   The numbers are computed on the JVM from the cell structure; the browser
   draws the strip (a Clerk render-fn in SVG) and switches between cases."
  (:require [alexandria.structure.surface :as s]
            [alexandria.structure.surfaces :as ss]
            [nextjournal.clerk :as clerk]))

(def cells "Cells in the strip the widget draws." 8)

(def twists "The half-twist counts the selector offers." [0 1 2 3])

(defn band-data
  "{:k :twists [t ...] :reports {t report} :cuts {t [report ...]}} for every t."
  []
  {:k cells :twists twists
   :reports (into {} (for [t twists] [t (ss/strip-report cells t)]))
   :cuts (into {} (for [t twists] [t (ss/cut-along-core cells t)]))})

(defn finite-band-data?
  "Every report holds the whole of V E F chi holonomy and a character with
   entries in {+1 -1}, and every cut is non-empty."
  [{:keys [reports cuts twists]}]
  (boolean
   (and (seq twists)
        (every? (fn [t]
                  (let [r (get reports t)]
                    (and (every? integer? (map r [:V :E :F :chi :holonomy :boundary]))
                         (every? #{1 -1} (:character r))
                         (seq (get cuts t)))))
                twists))))

(def ^:private band-render
  '(fn [{:keys [k twists reports cuts]}]
     (reagent.core/with-let [t (reagent.core/atom 0) u (reagent.core/atom 0.0) cut? (reagent.core/atom false)]
       (let [r (get reports @t)
             pi js/Math.PI
             R 1.0 hw 0.38
             rot (fn [x y z] ;; tilt the scene so the band is seen from above-front
                   (let [a 0.9] [x (- (* y (js/Math.cos a)) (* z (js/Math.sin a))) (+ (* y (js/Math.sin a)) (* z (js/Math.cos a)))]))
             pt (fn [th v]
                  (let [h (* @t (/ th 2)) rr (+ R (* v (js/Math.cos h)))]
                    (rot (* rr (js/Math.cos th)) (* rr (js/Math.sin th)) (* v (js/Math.sin h)))))
             scr (fn [[x y z]] [(+ 190 (* 130 x)) (- 130 (* 130 y))])
             quad (fn [i]
                    (let [t0 (* 2 pi (/ i k)) t1 (* 2 pi (/ (inc i) k))
                          ps (map scr [(pt t0 (- hw)) (pt t1 (- hw)) (pt t1 hw) (pt t0 hw)])
                          depth (nth (pt (/ (+ t0 t1) 2) 0) 2)]
                      {:i i :depth depth
                       :d (str "M" (apply str (interpose " L" (map (fn [[a b]] (str (.toFixed a 1) " " (.toFixed b 1))) ps))) " Z")}))
             quads (sort-by :depth (map quad (range k)))
             th (* 2 pi @u)
             h (* @t (/ th 2))
             c0 (pt th 0)
             n [(* -1 (js/Math.sin h) (js/Math.cos th)) (* -1 (js/Math.sin h) (js/Math.sin th)) (js/Math.cos h)]
             tip (fn [s] (let [[x y z] (pt th 0)
                               [nx ny nz] (rot (nth n 0) (nth n 1) (nth n 2))
                               [ox oy oz] [x y z]]
                           (scr [(+ ox (* s 0.5 nx)) (+ oy (* s 0.5 ny)) (+ oz (* s 0.5 nz))])))
             [bx by] (scr c0) [ex ey] (tip 1)
             n0 (rot 0 0 1)
             [sx sy] (scr (pt 0 0)) [s2x s2y] (scr [(first (pt 0 0)) (+ (second (pt 0 0)) (* 0.5 (second n0))) (+ (nth (pt 0 0) 2) (* 0.5 (nth n0 2)))])
             flipped (and (>= @u 0.999) (= -1 (:holonomy r)))
             btn (fn [label act on-click on?]
                   [:button {:type "button" :data-act act :on-click on-click
                             :style {:margin "0.2em" :padding "0.3em 0.6em" :border-radius "0.3em" :cursor "pointer"
                                     :border "1px solid currentColor" :color "inherit"
                                     :background (if on? "#1F6FB433" "transparent")}} label])]
         [:div {:data-figure "moebius-band" :data-twists (str @t) :data-chi (str (:chi r))
                :data-orientable (str (:orientable? r)) :data-holonomy (str (:holonomy r))
                :data-cut (str @cut?) :data-pieces (str (count (get cuts @t)))
                :data-flipped (str flipped) :style {:margin "1em 0"}}
          [:div (for [x twists] ^{:key x} (btn (str x " half twist" (when (not= x 1) "s")) (str "twist-" x)
                                               #(do (reset! t x) (reset! u 0.0) (reset! cut? false)) (= x @t)))]
          [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "center"}}
           [:svg {:width 380 :height 260 :viewBox "0 0 380 260" :style {:max-width "100%"}}
            (for [{:keys [i d]} quads]
              ^{:key i} [:path {:d d :fill (if (even? i) "#1F6FB4" "#E39B2D") :fill-opacity (if @cut? 0.35 0.75)
                                :stroke "#223" :stroke-width 0.8}])
            (when @cut?
              [:path {:d (str "M" (apply str (interpose " L" (map (fn [i] (let [[a b] (scr (pt (* 2 pi (/ i 64)) 0))] (str (.toFixed a 1) " " (.toFixed b 1)))) (range 65)))))
                      :fill "none" :stroke "#C0392B" :stroke-width 2.2 :stroke-dasharray "5 3"}])
            [:line {:x1 sx :y1 sy :x2 s2x :y2 s2y :stroke "#8A9BB0" :stroke-width 2 :stroke-dasharray "3 3"}]
            [:line {:x1 bx :y1 by :x2 ex :y2 ey :data-arrow true :stroke (if flipped "#C0392B" "#2E8B57") :stroke-width 3}]
            [:circle {:cx ex :cy ey :r 4 :fill (if flipped "#C0392B" "#2E8B57")}]]
           [:div {:style {:flex "1 1 260px" :font-size "0.95em" :line-height 1.5}}
            [:div "cells: " [:strong (str "V " (:V r) " − E " (:E r) " + F " (:F r) " = ") [:span {:data-chi true} (:chi r)]]]
            [:div "faces can be oriented: " [:strong {:data-orientable-text true} (if (:orientable? r) "yes" "no")]]
            [:div "orientation character of the core loop, on 0, 1: "
             [:strong {:data-character true} (pr-str (:character r))]]
            [:div "boundary curves: " [:strong {:data-boundary true} (:boundary r)]]
            [:div {:style {:margin-top "0.5em"}}
             [:label "carry the arrow round the core: "]
             [:input {:type "range" :min 0 :max 100 :value (js/Math.round (* 100 @u)) :data-act "slider"
                      :on-change #(reset! u (/ (js/parseFloat (.. % -target -value)) 100))}]]
            [:div {:data-verdict true :style {:font-weight 600 :color (if flipped "#C0392B" "#2E8B57")}}
             (cond (< @u 0.999) "the green arrow is the normal at the start; the dashed grey one stays behind"
                   flipped "back at the start the arrow points the other way: one side only"
                   :else "back at the start the arrow points the same way: two sides")]
            [:div {:style {:margin-top "0.5em"}}
             (btn (if @cut? "put the strip back" "cut along the core") "cut" #(swap! cut? not) @cut?)]
            (when @cut?
              [:div {:data-cut-result true}
               (let [ps (get cuts @t)]
                 (if (= 2 (count ps))
                   (str "two bands of " (:k (first ps)) " cells and " (:twists (first ps)) " half twists each, chi 0")
                   (let [p (first ps)]
                     (str "one band of " (:k p) " cells and " (:twists p) " half twists, orientable: "
                          (if (:orientable? p) "yes" "no") ", boundary curves " (:boundary p)))))])]]]))))

(defn band-widget
  "The strip with its half twists, normal arrow, cell counts and the cut."
  []
  (let [d (band-data)]
    (assert (finite-band-data? d) "the band reports are incomplete")
    (clerk/with-viewer {:render-fn band-render :transform-fn clerk/mark-presented} d)))

;; ---------------------------------------------------------------------------

(defn- origin-pair [p]
  (let [o (s/orientation p)
        senses (when (:orientable? o) (:senses o))]
    {:as-written [(ss/signed-volume p [0 0 0]) (ss/signed-volume p [5 -3 2])]
     :repaired (when senses [(ss/signed-volume p [0 0 0] senses) (ss/signed-volume p [5 -3 2] senses)])}))

(defn content-data
  "One row per case: name, counts, whether the faces can be oriented, whether
   the faces as written already agree, and the signed volume from two apexes
   (as written, and after repairing the senses); nil where undefined."
  []
  (let [row (fn [nm c p]
              (merge {:name nm} (s/counts c)
                     (select-keys (ss/content c) [:closed? :listed-consistent?])
                     {:oriented? (s/orientable? c)}
                     (when p (origin-pair c))))]
    [(row "cube" ss/cube true)
     (row "cube, top face written backwards" ss/cube-flipped true)
     (row "Möbius' decahedron (1865, §8)" ss/moebius-decahedron nil)
     (row "strip of 8 cells, one half twist" (ss/strip cells 1) nil)]))

(def ^:private content-render
  '(fn [rows]
     (reagent.core/with-let [sel (reagent.core/atom 0)]
       (let [r (nth rows @sel)
             f (fn [x] (if (number? x) (.toFixed x 3) "undefined"))
             [a b] (:as-written r) [ra rb] (:repaired r)]
         [:div {:data-figure "moebius-content" :data-case (str @sel) :data-oriented (str (:oriented? r))
                :style {:margin "1em 0"}}
          [:div (map-indexed (fn [i x] ^{:key i}
                               [:button {:type "button" :data-act (str "case-" i) :on-click #(reset! sel i)
                                         :style {:margin "0.2em" :padding "0.3em 0.6em" :border-radius "0.3em" :cursor "pointer"
                                                 :border "1px solid currentColor" :color "inherit"
                                                 :background (if (= i @sel) "#1F6FB433" "transparent")}} (:name x)])
                             rows)]
          [:div {:style {:line-height 1.6}}
           [:div (str "V " (:V r) ", E " (:E r) ", F " (:F r) ", chi " (:chi r) "; closed: " (:closed? r))]
           [:div "law of opposite edges (faces can be oriented): "
            [:strong {:data-check true :style {:color (if (:oriented? r) "#2E8B57" "#C0392B")}} (if (:oriented? r) "holds" "fails")]]
           [:div "faces as written agree on every edge: " [:strong (str (boolean (:listed-consistent? r)))]]
           [:div "signed volume from the corner (0,0,0), as written: " [:strong {:data-vol-a true} (f (when a a))]
            "; from (5,-3,2): " [:strong {:data-vol-b true} (f (when b b))]]
           (when ra [:div "after reversing the faces that disagree: " [:strong {:data-vol-fixed true} (str (f ra) " and " (f rb))]])
           (when-not ra [:div {:data-undefined true :style {:font-weight 600 :color "#C0392B"}}
                         "no choice of senses makes the edges opposite, so the content is not defined"])]]))))

(defn content-widget
  "Möbius' content: orientation check and signed volume of four solids."
  []
  (clerk/with-viewer {:render-fn content-render :transform-fn clerk/mark-presented} (content-data)))
