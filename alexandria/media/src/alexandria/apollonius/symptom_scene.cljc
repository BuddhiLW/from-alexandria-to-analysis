(ns alexandria.apollonius.symptom-scene
  "The symptoma live: the notation figure's cone (alexandria.apollonius.
   notation-steps: its kernels, letters and palette), now with one state
   {:tilt :x} a reader moves, beside the equation y^2 = p x + c x^2 with
   the current numbers and the application-of-areas picture to scale.

   The tie (widget brief, section 5). Three controls, tilt, x = PV and
   y = QV; one equation y^2 = p x + c x^2 between x and y at a given tilt:

     tilt moves  x is held (clamped to the section's reach), y follows
     x moves     the tilt is held, y follows
     y moves     the tilt is held, x is solved on the current branch: the
                 root through P, x = 2y^2 / (p + sqrt(p^2 + 4 c y^2)); on
                 the ellipse past its centre (x > d/2), the far root
                 (p + sqrt(p^2 + 4 c y^2)) / (-2 c). A y above the
                 ellipse's greatest ordinate b puts V at the centre and
                 pushes y = b back; a root beyond the drawn reach is
                 clamped and y pushed back

   Only the touched key is a source; a pushed value carries :origin
   :pushed and is never solved again, and a value within 1e-9 of the
   current one is ignored. Both maps are one raster kernel
   (alexandria.apollonius.symptom/symptom-figure); Emmy proves they invert
   each other (alexandria.apollonius.symptom/identities).

   `tie` is pure and takes the kernel as a function, so the JVM tests run
   it on the Emmy oracle; the browser runs it on the wasm kernel. The
   browser only compares and formats kernel outputs."
  (:require [clojure.string :as str]
            #?(:cljs [alexandria.apollonius.notation-steps :as notation])
            #?(:cljs [alexandria.medium.figure :as figure])
            #?(:cljs [alexandria.medium.html-labels :as labels])
            #?(:cljs [mathbox.core])
            #?(:cljs [mathbox.primitives :as mb])
            #?(:cljs [nextjournal.clerk.render :as render])
            #?(:cljs [reagent.core :as r])))

;; ---------------------------------------------------------------------------
;; The tie, pure: (solve deg yin x) -> {output value} of the symptom kernel

(def tilt-range "Degrees: an ellipse at 25, the parabola near 59, a hyperbola at 80." [25 80])
(def x-min "The smallest abscissa (V off P, so Q is visible)." 0.02)
(defn- mag "|v| (the browser's SCI has no clojure.core/abs)." [v] (if (neg? v) (- v) v))

(def eps "A change smaller than this is no change." 1e-9)

(defn kind
  "The section's name from c: within tol of 0 the parabola."
  ([c] (kind c 0.004))
  ([c tol] (cond (< (mag c) tol) :parabola (pos? c) :hyperbola :else :ellipse)))

(defn reach
  "The largest abscissa the figure draws: PM up to the base, and on the
   ellipse not past P'."
  [{:keys [xbase d c]}]
  (if (= :ellipse (kind c)) (min xbase (* 0.999 d)) xbase))

(defn- clamp [lo hi v] (max lo (min hi v)))

(defn settle
  "The state at tilt and x (clamped to the reach), y computed forward from
   the cone."
  [solve tilt x origin]
  (let [x (clamp x-min (reach (solve tilt 0 x)) x)]
    {:tilt tilt :x x :y (:y (solve tilt 0 x)) :origin origin}))

(defn tie
  "The state after the reader sets key (:tilt :x :y) to v. See the ns doc
   for the policy."
  [solve {:keys [tilt x] :as st} key v]
  (let [cur (get st key)]
    (if (and (number? cur) (< (mag (- v cur)) eps))
      st
      (case key
        :tilt (settle solve (clamp (first tilt-range) (second tilt-range) v) x :tilt)
        :x (settle solve tilt v :x)
        :y (let [v (max 0 v)
                 n (solve tilt v x)
                 ell? (= :ellipse (kind (:c n)))
                 x' (cond (and ell? (neg? (:disc n))) (:half n)
                          (and ell? (> x (:half n))) (:xfar n)
                          :else (:xnear n))
                 s (settle solve tilt x' :y)]
             (if (and (< (mag (- (:x s) x')) eps) (not (neg? (:disc n))))
               (assoc s :y v)
               ;; clamped: y is pushed back from the clamped x
               (assoc s :origin :pushed)))))))

;; ---------------------------------------------------------------------------
;; TeX of the live equation: Emmy's template, numbers substituted

(defn fill
  "TeX template (emmy.env/->TeX of an expression in the symbols YYY PPP
   XXX CCC, which it writes \\mathsf{YYY} ...) with each {name text}
   substituted."
  [template subs]
  (reduce (fn [s [k v]] (str/replace s (str "\\mathsf{" k "}") v)) template subs))

(defn num-tex
  "A number for TeX, 3 decimals, negative ones in parentheses."
  [n]
  (let [s #?(:clj (format "%.3f" (double n)) :cljs (.toFixed (js/Number n) 3))]
    (if (str/starts-with? s "-") (str "(" s ")") s)))

(def names
  "The three names (alexandria.apollonius.conics/names) with the
   proposition."
  {:parabola {:greek "παραβολή" :latin "parabole" :meaning "applied exactly" :prop "I.11"}
   :hyperbola {:greek "ὑπερβολή" :latin "hyperbole" :meaning "applied, exceeding" :prop "I.12"}
   :ellipse {:greek "ἔλλειψις" :latin "elleipsis" :meaning "applied, falling short" :prop "I.13"}})

;; ---------------------------------------------------------------------------
;; Browser

#?(:cljs
   (do
     (def ^:private n-u 73)
     (def ^:private n-v 13)
     (def ^:private n-sec 481)
     (def ^:private n-plane 12)
     (def ^:private n-curve 241)
     (def ^:private cone-grid (vec (for [j (range n-v) i (range n-u)] [(/ i (dec n-u)) (/ j (dec n-v))])))
     (def ^:private circle-states (mapv (fn [i] [(/ i (dec n-u)) 0]) (range n-u)))
     (def ^:private sec-states (mapv (fn [i] [(- (* 2 (/ i (dec n-sec))) 1)]) (range n-sec)))
     (def ^:private plane-grid (vec (for [j (range n-plane) i (range n-plane)] [(/ i (dec n-plane)) (/ j (dec n-plane))])))
     (def ^:private curve-states (mapv (fn [i] [(/ i (dec n-curve))]) (range n-curve)))
     (def ^:private ends [[0] [1]])
     (def ^:private nan3 [js/NaN js/NaN js/NaN])

     (def ^:private colours
       "The notation figure's palette (alexandria.apollonius.notation-steps)."
       {:cone "#9aa7b4" :nappe2 "#9aa7b4" :circle "#cfd8dc" :plane "#ECEEE4"
        :AB "#8FC7E8" :AC "#8FC7E8" :BC "#8FC7E8"
        :PM "#ECEEE4" :section "#E8735A" :chord "#7FD18B" :QV "#7FD18B" :PV "#8FC7E8"
        :PL "#F2D16B" :PP' "#FF9F43" :letter "#ECEEE4" :strip-ell "#d9534f" :strip-hyp "#3aa655"})

     (defn- in-box? [{:keys [y-lo y-hi]} [_ y _ :as q]]
       (and q (js/isFinite y) (<= y-lo y y-hi)))

     (defn- clip [data pts] (mapv #(if (in-box? data %) % nan3) pts))

     (defn- solver
       "(solve deg yin x) on the wasm kernel: {output value}."
       [fig {:keys [k h zb outputs]}]
       (fn [deg yin x]
         (zipmap outputs (first (figure/points fig [k h zb deg yin] [[x]])))))

     (defn- frame!
       "Every array and number of one state, one batch per kernel."
       [{:keys [cone section plane segment named curve]} solve
        {:keys [k h zb ztop shift s-near s-far s1 ylim] :as data} {:keys [tilt x]}]
       (let [n (solve tilt 0 x)
             theta (:theta n)
             nm (notation/named-map (first (figure/points named [k theta h zb ztop x] [[0]])))
             seg (fn [a b] (figure/points segment (conj (into (nm a) (nm b)) 1) ends))
             kd (kind (:c n))
             s0 (if (= kd :ellipse) s-near s-far)
             ok-p' (and (not= kd :parabola) (in-box? data (:P' nm)))
             cv (figure/points curve [k h tilt (reach n)] curve-states)]
         {:n n :kind kd :ok-p' ok-p'
          :cone (figure/points cone [k 1 0 zb 1] cone-grid)
          :nappe2 (figure/points cone [k 1 0 (- ztop) 1] cone-grid)
          :circle (figure/points cone [k 1 zb zb 1] circle-states)
          :section (clip data (figure/points section [k theta h 1] sec-states))
          :plane (figure/points plane [k theta h 1 shift s0 s1 ylim] plane-grid)
          :AB (seg :A :B) :AC (seg :A :C) :BC (seg :B :C)
          :PM (seg :P :M) :chord (seg :Q2 :Q) :QV (seg :V :Q) :PV (seg :P :V) :PL (seg :P :L)
          :PP' (if ok-p' (seg :P :P') [nan3 nan3])
          :at nm
          :upper (mapv (fn [[a b _]] [a b]) cv)
          :lower (mapv (fn [[a _ c]] [a c]) cv)}))

     (defn- line [frame key n width]
       [:<>
        [mb/Array {:width n :channels 3 :items 1 :live true
                   :expr (fn [emit i _t] (let [[a b c] (nth (get @frame key) i [0 0 0])] (emit a b c)))}]
        [mb/Line {:width width :color (colours key) :zBias 6 :zIndex 2}]])

     (defn- surface [frame key w h opacity opts]
       [:<>
        [mb/Area {:width w :height h :channels 3 :items 1 :live true
                  :expr (fn [emit _x _y i j _t] (let [[a b c] (nth (get @frame key) (+ i (* w j)) [0 0 0])] (emit a b c)))}]
        [mb/Surface (merge {:shaded false :color (colours key) :zBias 0 :zOrder 1 :opacity opacity} opts)]])

     (def ^:private marks
       "[named point, letter, opts]: the notation figure's letters, as HTML."
       [[:A "A" {:dot? false}] [:B "B" {}] [:C "C" {}] [:P "P" {}] [:M "M" {}]
        [:V "V" {}] [:Q "Q" {}] [:L "L" {}] [:P' "P′" {:p'? true}]
        [:mid-PV "x" {:dot? false :italic? true :colour "#8FC7E8"}]
        [:mid-QV "y" {:dot? false :italic? true :colour "#7FD18B"}]
        [:mid-PL "p" {:dot? false :italic? true :colour "#F2D16B"}]
        [:mid-PP' "d" {:dot? false :italic? true :colour "#FF9F43" :p'? true}]])

     (defn- mark [frame at {:keys [dot? colour p'?] :or {dot? true colour (colours :letter)}}]
       (when dot?
         [:<>
          [mb/Array {:width 1 :channels 3 :items 1 :live true
                     :expr (fn [emit _i _t]
                             (let [[a b c] (if (and p'? (not (:ok-p' @frame)))
                                             nan3
                                             (get-in @frame [:at at] [0 0 0]))]
                               (emit a b c)))}]
          [mb/Point {:size 9 :color colour :zIndex 3 :zBias 10}]]))

     (defn- letters [frame]
       (let [{:keys [at ok-p']} @frame]
         (for [[pt text {:keys [italic? colour p'?]}] marks]
           {:text text :at (get at pt) :opacity (if (and p'? (not ok-p')) 0 1) :italic? italic?
            :colour (or colour (colours :letter)) :size 17 :offset [11 13]
            :background "rgba(29,31,33,0.85)"})))

     (def ^:private scene-range [[-2.4 2.4] [-2.6 2.2] [-2.4 2.4]])

     (defn- scene [frame]
       (let [box (atom nil)
             reset-view (fn [] (when-let [^js b @box] (.reset (.. b -three -controls))))]
         (fn [_]
           [:div {:on-double-click reset-view :data-figure "apollonius-symptom"
                  :style {:width "100%" :position "relative"}}
            [labels/overlay {:box box :range scene-range :labels #(letters frame)}]
            [mathbox.core/MathBox
             {:container {:style {:height "460px" :width "100%"}}
              :focus 3
              :renderer {:background-color "#1d1f21"}
              :threestrap {:plugins ["core" "controls" "cursor"]}
              :ref (fn [b]
                     (reset! box b)
                     (when b (js/setTimeout #(some-> ^js b .-three .-controls .saveState) 400)))}
             [mb/Camera {:proxy true :position [1.3 0.4 1.75]}]
             [mb/Cartesian {:range scene-range :scale [1 1 1]}
              [surface frame :cone n-u n-v 0.42 {:shaded true}]
              [surface frame :nappe2 n-u n-v 0.42 {:shaded true}]
              [surface frame :plane n-plane n-plane 0.22 {:zOrder 3 :zBias 2}]
              [line frame :circle n-u 3]
              [line frame :AB 2 3] [line frame :AC 2 3] [line frame :BC 2 3]
              [line frame :section n-sec 6]
              [line frame :PM 2 4]
              [line frame :chord 2 4]
              [line frame :QV 2 7]
              [line frame :PV 2 7]
              [line frame :PL 2 7]
              [line frame :PP' 2 6]
              (for [[at _ opts] marks] ^{:key (str at)} [mark frame at opts])]]])))

     (defn- tex [s & [display?]] [:f> render/render-katex s {:inline? (not display?)}])

     (defn- fixed [v d] (if (js/isFinite v) (.toFixed (js/Number v) d) "—"))

     ;; -- the application of areas, to scale (SVG, y down)

     (defn- pts-attr [pts] (str/join " " (map (fn [[x y]] (str x "," y)) pts)))

     (defn- poly [pts colour fill & [dashed?]]
       [:polygon {:points (pts-attr pts) :fill colour :fill-opacity fill :stroke colour :stroke-width 0.02
                  :stroke-dasharray (when dashed? "0.06 0.04")}])

     (defn- txt [x y s & [{:keys [colour italic? anchor size] :or {colour "#ECEEE4" anchor "middle" size 0.16}}]]
       [:text {:x x :y y :fill colour :font-size size :text-anchor anchor :dominant-baseline "middle"
               :font-family "Georgia, 'Times New Roman', serif" :font-style (if italic? "italic" "normal")} s])

     (defn- areas [{:keys [n kind upper lower]}]
       (let [{:keys [x p ny xy lo hi vr mx mp my mq]} n
             strip (if (= kind :ellipse) (colours :strip-ell) (colours :strip-hyp))]
         [:svg {:viewBox "-0.35 -1.75 4.7 3.25" :data-areas "symptom"
                :style {:width "100%" :height "auto" :background "#1d1f21" :border-radius "6px"}}
          [:polyline {:points (pts-attr (filter #(every? js/isFinite %) upper)) :fill "none"
                      :stroke (colours :section) :stroke-width 0.025 :opacity 0.8}]
          [:polyline {:points (pts-attr (filter #(every? js/isFinite %) lower)) :fill "none"
                      :stroke (colours :section) :stroke-width 0.025 :opacity 0.8}]
          [:line {:x1 -0.2 :y1 0 :x2 4.2 :y2 0 :stroke "#ECEEE4" :stroke-width 0.012 :opacity 0.5}]
          ;; the rectangle on PL with height PV: p x
          [:g {:data-area "px"} (poly [[0 0] [x 0] [x p] [0 p]] (colours :PL) 0.25)]
          ;; the strip c x^2, between PL and VR
          (when (not= kind :parabola)
            [:g {:data-area "cx2"} (poly [[0 lo] [x lo] [x hi] [0 hi]] strip 0.45 (= kind :ellipse))])
          ;; the square on QV
          [:g {:data-area "y2"} (poly [[x 0] [xy 0] [xy ny] [x ny]] (colours :QV) 0.35)]
          [:line {:x1 0 :y1 0 :x2 0 :y2 p :stroke (colours :PL) :stroke-width 0.03}]
          [:line {:x1 x :y1 0 :x2 x :y2 ny :stroke (colours :QV) :stroke-width 0.03}]
          (txt -0.1 -0.08 "P") (txt -0.12 p "L") (txt x 0.12 "V" {:anchor "start"})
          (txt x ny "Q" {:anchor "end"})
          (when (not= kind :parabola) (txt x vr "R" {:anchor "start"}))
          (txt mx mp "p·x" {:italic? true :size 0.13})
          (txt mq my "y²" {:italic? true :size 0.14})]))

     (defn- panel [{:keys [n kind]} st put! {:keys [template]}]
       (let [{:keys [x y y2 rhs p c d px cx2 gap]} n
             nm (names kind)
             slider (fn [key lo hi step label v]
                      [:label {:style {:display "flex" :align-items "center" :gap "0.6em" :margin "0.2em 0"}}
                       [:span {:style {:width "6.5em"}} label]
                       [:input {:type "range" :min lo :max hi :step step :value v :data-control (name key)
                                :style {:flex "1"}
                                :on-change #(put! key (js/parseFloat (.. % -target -value)))}]
                       [:span {:style {:width "4em" :text-align "right"}} (fixed v 3)]])]
         [:div {:style {:font-size "0.92em" :line-height 1.5}}
          [:div {:data-name (name kind) :style {:font-size "1.15em" :margin-bottom "0.3em"}}
           [:strong (:greek nm)] " · " [:em (:latin nm)] " — " (:meaning nm) " (" (:prop nm) ")"]
          [:div {:data-symptom-tex "true" :data-lhs (str y2) :data-rhs (str rhs) :data-c (str c)}
           (tex (fill template {"YYY" (num-tex y) "PPP" (num-tex p) "XXX" (num-tex x) "CCC" (num-tex c)}) true)]
          [:div (tex (str "QV^2 = " (fixed y2 4) ",\\quad px + cx^2 = " (fixed px 4) " + " (num-tex cx2)
                          " = " (fixed rhs 4) ",\\quad |\\Delta| = " (.toExponential (js/Math.abs gap) 1)))]
          [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.85em" :margin "0.3em 0"}}
           (str "p = PL = " (fixed p 3) "   c = " (fixed c 3)
                (when (not= kind :parabola) (str "   d = PP′ = " (fixed (js/Math.abs d) 3))))]
          (slider :tilt (first tilt-range) (second tilt-range) 0.1 "tilt (°)" (:tilt st))
          (slider :x x-min 3 0.001 "x = PV" (:x st))
          (slider :y 0 1.6 0.001 "y = QV" (:y st))
          [:div {:style {:opacity 0.6 :font-size "0.82em" :margin-top "0.3em"}}
           "x and y are tied by the equation: moving one solves the other at the same tilt "
           "(on the ellipse, on V's side of the centre); moving the tilt holds x."]]))

     (defn render
       "Clerk render-fn. value: {:kernels {name kernel} :data {...}}."
       [{:keys [kernels data]}]
       (r/with-let [figs (into {} (map (fn [[k v]] [k (figure/kernel-figure v)])) kernels)
                    solve (solver (:symptom figs) data)
                    st (r/atom (settle solve (:tilt0 data) (:x0 data) :init))
                    frame (atom (frame! figs solve data @st))
                    shown (r/atom @frame)
                    _ (add-watch st ::frame (fn [_ _ _ s]
                                              (let [f (frame! figs solve data s)]
                                                (reset! frame f)
                                                (reset! shown f))))
                    put! (fn [key v] (swap! st #(tie solve % key v)))
                    sweeping (r/atom nil)
                    alive (atom true)
                    sweep! (fn []
                             (if @sweeping
                               (reset! sweeping nil)
                               (let [t0 (atom nil)]
                                 (reset! sweeping true)
                                 ((fn step [ts]
                                    (when (and @alive @sweeping)
                                      (when-not @t0 (reset! t0 ts))
                                      (let [[lo hi] tilt-range
                                            q (min 1 (/ (- ts @t0) 9000))]
                                        (put! :tilt (+ lo (* q (- hi lo))))
                                        (if (< q 1)
                                          (js/requestAnimationFrame step)
                                          (reset! sweeping nil)))))
                                  (js/performance.now)))))]
         (let [s @st f @shown]
           [:div {:data-symptom "true" :data-tilt (str (:tilt s)) :data-x (str (:x s)) :data-y (str (:y s))
                  :data-origin (name (:origin s))
                  :style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
            [:div {:style {:flex "1 1 480px" :min-width "300px"}}
             [scene frame]
             [:div {:style {:display "flex" :align-items "center" :gap "0.6em" :margin-top "0.4em"}}
              [:button {:data-act "sweep" :on-click sweep!
                        :style {:font-family "ui-monospace, monospace" :padding "0.15em 0.7em"
                                :border "1px solid currentColor" :border-radius "0.35em"
                                :background "transparent" :color "inherit" :cursor "pointer"}}
               (if @sweeping "❚❚" "▶ sweep the tilt")]
              [:span {:style {:opacity 0.55 :font-size "0.82em"}}
               "drag: rotate · scroll: zoom · double-click: reset"]]]
            [:div {:style {:flex "1 1 340px"}}
             [panel f s put! data]
             [:div {:style {:margin-top "0.6em"}} [areas f]]
             [:div {:style {:opacity 0.6 :font-size "0.8em"}}
              "The square on QV (green) equals the rectangle on PL with height PV (yellow), "
              "less the strip c·x² (ellipse, dashed red) or plus it (hyperbola, green)."]]])
         (finally (reset! alive false) (reset! sweeping nil))))))
