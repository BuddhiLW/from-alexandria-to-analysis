(ns alexandria.apollonius.book-one-scene
  "Conics I.11-14, one proposition per figure, stepped through Heath's
   sentences (alexandria.apollonius.book-one: the edn, the kernel). Each
   figure is Heath's: the cone with the axial triangle ABC, the base line
   DME, the diameter PM, the circle HQK through V, the upright side PL,
   and in I.12-14 P', AF, P'L and R; I.14 adds the opposite cone with
   B'C', D'E', P'L'. Every point is an output of the :heath kernel
   (raster, WebAssembly); the page draws them in SVG, letters as SVG text.

   Beside the steps, the proposition's widget: tilt (within the
   proposition's case; I.11 is held where PM is parallel to AC), x = PV
   and y = QV are three controls tied by the symptoma, through the
   symptoma widget's own kernel and tie
   (alexandria.apollonius.symptom-scene/tie):

     tilt moves  x held, y follows
     x moves     tilt held, y follows
     y moves     tilt held, x solved on V's branch

   Beside it: QV² and PV·VR (or PL·PV) with their numbers, each from the
   kernel; HV·VK from the circle; the ratio Heath compares, both sides;
   the name in Greek, from the sign of VR − PL (equal, exceeding, falling
   short); and the two areas drawn to scale.

   `progress-of`, the transport, `case-tilt`, `kind-of` and `named` are
   pure; the JVM tests run them. The browser only compares and formats
   kernel outputs."
  (:require [alexandria.apollonius.symptom-scene :as sym]
            [alexandria.medium.anim :as anim]
            [clojure.string :as str]
            #?(:cljs [alexandria.medium.figure :as figure])
            #?(:cljs [nextjournal.clerk.render :as render])
            #?(:cljs [reagent.core :as rg])))

;; ---------------------------------------------------------------------------
;; The steps, pure

(def step-ms "Milliseconds per step." 5500)

(defn progress-of
  "{element progress} at transport state {:step :progress} for steps (each
   with :introduces): elements of earlier steps 1, of the current one eased
   in over the step with a lag each (Manim's LaggedStart), later ones 0."
  [steps {:keys [step progress]}]
  (into {}
        (for [[j {:keys [introduces]}] (map-indexed vector steps)
              :let [n (count introduces)
                    fs (cond (< j step) (repeat n 1)
                             (= j step) (anim/lagged progress n 0.35 0 0.85)
                             :else (repeat n 0))]
              [el f] (map vector introduces fs)]
          [el f])))

(defn start [] {:step 0 :progress 0 :playing? false :through? false})

(defn advance
  "The transport after dt ms over n steps: a step plays once; play
   (:through?) runs on to the end."
  [{:keys [step progress playing? through?] :as st} dt n]
  (if-not playing?
    st
    (let [p (+ progress (/ dt step-ms))]
      (cond (< p 1) (assoc st :progress p)
            (and through? (< step (dec n))) (assoc st :step (inc step) :progress 0)
            :else (assoc st :progress 1 :playing? false)))))

(defn goto "Step i (clamped), playing once." [_ i n]
  {:step (max 0 (min (dec n) i)) :progress 0 :playing? true :through? false})

(defn toggle "Pause, or play on through the steps." [{:keys [step progress playing?] :as st} n]
  (cond playing? (assoc st :playing? false)
        (and (>= progress 1) (= step (dec n))) {:step 0 :progress 0 :playing? true :through? true}
        (>= progress 1) {:step (inc step) :progress 0 :playing? true :through? true}
        :else (assoc st :playing? true :through? true)))

(defn case-tilt
  "The tilt kept in the proposition's case: I.11 has one tilt, the
   parabola's; the others their :range."
  [{:keys [tilt]} parabola v]
  (if (= :parabola (:at tilt))
    parabola
    (let [[lo hi] (:range tilt)] (max lo (min hi v)))))

(defn kind-of
  "The name from VR − PL against PL: equal (parabola), overlapping
   (hyperbola), falling short (ellipse)."
  [overlap p]
  (sym/kind (/ overlap (max p 1e-9)) 1e-4))

(def greek
  {:parabola ["παραβολή" "parabolē" "applied exactly" "I.11"]
   :hyperbola ["ὑπερβολή" "hyperbolē" "applied, exceeding" "I.12"]
   :ellipse ["ἔλλειψις" "elleipsis" "applied, falling short" "I.13"]})

(def view
  "The cone's slope, P's height, the two bases, the view."
  {:k 0.6 :h 0.7 :zb 2.2 :zb2 1.6 :yaw 0.42 :pitch 0.22})

(def outputs
  "The :heath kernel's outputs, in order: the letters (2 numbers each),
   then the numbers (alexandria.apollonius.book-one/points and /scalars;
   the test checks they agree)."
  {:points [:A :B :C :P :M :D :E :V :Q :Q' :H :K :L :P' :R :F :B' :C' :F' :M' :D' :E' :L' :V2 :Q2 :R2]
   :scalars [:theta :p :c :d :x :vr :y :qv2 :hv :vk :hvvk :rect :plpv :pa :ba :ac :bc :bf :fc :af :p'v
             :w :w2 :x2 :qv2' :rect2 :overlap :zv :rat-bc :rat-pa :rat-bf :rat-pp]})

(defn named
  "{name value} of one output row of the :heath kernel."
  [row]
  (let [{:keys [points scalars]} outputs
        np (count points)]
    (merge (zipmap points (map vec (partition 2 (take (* 2 np) row))))
           (zipmap scalars (drop (* 2 np) row)))))

;; ---------------------------------------------------------------------------
;; Browser

#?(:cljs
   (do
     (def ^:private n-sec 361)
     (def ^:private n-circ 97)
     (def ^:private sec-states (mapv (fn [i] [(- (* 2 js/Math.PI (/ i (dec n-sec))) js/Math.PI)]) (range n-sec)))
     (def ^:private circ-states (mapv (fn [i] [(* 2 js/Math.PI (/ i (dec n-circ)))]) (range n-circ)))

     (def ^:private colours
       {:cone "#9aa7b4" :axial "#8FC7E8" :PM "#ECEEE4" :section "#E8735A" :QV "#7FD18B" :PL "#F2D16B"
        :PP' "#FF9F43" :AF "#B9A3E3" :circle "#cfd8dc" :VR "#F2D16B" :letter "#ECEEE4"
        :short "#d9534f" :over "#3aa655"})

     (defn- fin? [[x y]] (and (js/isFinite x) (js/isFinite y)))

     ;; conics/project's screen has Z up and the cone opens upward from the
     ;; apex: drawn as is (SVG y down) the apex is on top, the base below, as
     ;; Heath draws it, and the opposite cone above the apex
     (defn- pts-attr [pts] (str/join " " (map (fn [[x y]] (str x "," y)) pts)))

     (defn- seg [a b colour f & [{:keys [w dash] :or {w 0.025}}]]
       (when (and (pos? f) a b (fin? a) (fin? b))
         [:polyline (merge {:points (pts-attr [a b]) :fill "none" :stroke colour :stroke-width w
                            :stroke-linecap "round"}
                           (if dash {:stroke-dasharray dash :opacity f} (anim/create f)))]))

     (defn- curve [pts colour f & [w]]
       (when (and (pos? f) (seq pts))
         (into [:g]
               (for [run (partition-by fin? pts) :when (and (fin? (first run)) (> (count run) 1))]
                 [:polyline (merge {:points (pts-attr run) :fill "none" :stroke colour
                                    :stroke-width (or w 0.03) :stroke-linejoin "round"}
                                   (anim/create f))]))))

     (defn- letter [[x y :as q] text f & [{:keys [dx dy colour] :or {dx 0.06 dy -0.07 colour (colours :letter)}}]]
       (when (and (pos? f) q (fin? q))
         [:text {:x (+ x dx) :y (+ y dy) :fill colour :opacity f :font-size 0.15
                 :font-family "Georgia, 'Times New Roman', serif" :data-label text} text]))

     (defn- dot [[x y :as q] colour f]
       (when (and (pos? f) q (fin? q))
         [:circle {:cx x :cy y :r 0.025 :fill colour :opacity f}]))

     (defn- between [zlo zhi pts]
       (mapv (fn [[u v z]] (if (and (js/isFinite z) (<= zlo z zhi)) [u v] [js/NaN js/NaN])) pts))

     (defn- frame!
       "Every point and number of one state, one batch per kernel."
       [{:keys [heath section circle]} solve {:keys [k h zb zb2 yaw pitch]} {:keys [tilt x]} x2]
       (let [n (solve tilt 0 x)
             H (named (first (figure/points heath [k tilt h zb zb2 x x2 yaw pitch] [[0]])))
             sec (figure/points section [k tilt h yaw pitch] sec-states)]
         {:n n :H H
          :near (between 0 zb sec)
          :far (between (- zb2) 0 sec)
          :base (figure/points circle [k zb yaw pitch] circ-states)
          :base2 (figure/points circle [k (- zb2) yaw pitch] circ-states)
          :hk (figure/points circle [k (:zv H) yaw pitch] circ-states)}))

     (defn- svg [{:keys [H near far base base2 hk]} v opposite?]
       (let [a (fn [k] (get v k 0))
             {:keys [A B C P M D E V Q Q' K L P' R F B' C' F' M' D' E' L' V2 Q2 R2 w w2 d]} H
             H* (:H H)
             ok-p' (and (js/isFinite d) (< (js/Math.abs d) 6))
             opp (fn [el] (if opposite? el :never))]
         [:svg {:viewBox (if opposite? "-1.75 -1.75 3.5 4.3" "-1.75 -0.3 3.5 2.75")
                :data-figure "apollonius-book-one"
                :style {:width "100%" :height "auto" :background "#1d1f21" :border-radius "6px"}}
          ;; the cone: the base circle(s) and the outline
          (curve base (colours :cone) (a :cone) 0.012)
          (when opposite?
            [:g (curve base2 (colours :cone) (a :nappe2) 0.012)
             (seg A B' (colours :cone) (a :nappe2) {:w 0.012})
             (seg A C' (colours :cone) (a :nappe2) {:w 0.012})])
          ;; the axial triangle ABC
          (seg A B (colours :axial) (a :axial)) (seg A C (colours :axial) (a :axial))
          (seg B C (colours :axial) (a :axial))
          (when opposite? [:g (seg B' C' (colours :axial) (a :axial2))])
          ;; DE on the base, BC produced to M
          (when (pos? w) (seg D E (colours :QV) (a :DE)))
          (seg C M (colours :axial) (a :DE) {:w 0.012 :dash "0.04 0.03"})
          (when (and opposite? (pos? w2)) (seg D' E' (colours :QV) (a :D'E')))
          ;; the section(s)
          (curve near (colours :section) (a :section) 0.035)
          (when opposite? (curve far (colours :section) (a :section2) 0.035))
          ;; the diameter PM, PP'
          (seg P M (colours :PM) (a :PM))
          (when ok-p' (seg P P' (colours :PP') (a :PP') {:w 0.02 :dash "0.05 0.03"}))
          (when opposite? (seg P' M' (colours :PM) (a :MM')))
          ;; AF parallel to PM, BC produced to F
          (seg A F (colours :AF) (a :AF) {:w 0.02})
          (seg C F (colours :AF) (a :AF) {:w 0.01 :dash "0.03 0.03"})
          (when opposite? [:g (seg A F' (colours :AF) (a :AF') {:w 0.02})
                           (seg C' F' (colours :AF) (a :AF') {:w 0.01 :dash "0.03 0.03"})])
          ;; the circle through V and its diameter HK
          (curve hk (colours :circle) (a :circle) 0.015)
          (seg H* K (colours :circle) (a :HK))
          ;; the ordinate, the upright side, P'L and VR
          (seg Q Q' (colours :QV) (* 0.5 (a :QV)) {:w 0.012 :dash "0.03 0.03"})
          (seg V Q (colours :QV) (a :QV) {:w 0.035})
          (seg P L (colours :PL) (a :PL) {:w 0.035})
          (when ok-p' (seg P' R (colours :PP') (a :P'L) {:w 0.015}))
          (seg V R (colours :VR) (a :VR) {:w 0.025})
          (when opposite? [:g (seg P' L' (colours :PL) (a :P'L') {:w 0.035})
                           (seg V2 Q2 (colours :QV) (a :Q2V2) {:w 0.035})
                           (seg V2 R2 (colours :VR) (a :Q2V2) {:w 0.02})])
          ;; the similar triangles: HV and VK on the circle's diameter
          (seg H* V (colours :short) (a :similar) {:w 0.03 :dash "0.06 0.02"})
          (seg V K (colours :over) (a :similar) {:w 0.03 :dash "0.06 0.02"})
          ;; letters
          (for [[q t el opts] [[A "A" :cone {:dy 0.18}] [B "B" :axial] [C "C" :axial {:dx -0.16}]
                               [P "P" :PM] [M "M" :DE] [D "D" :DE] [E "E" :DE] [V "V" :QV {:dx 0.07 :dy 0.12}]
                               [Q "Q" :QV] [H* "H" :HK] [K "K" :HK {:dx -0.14}] [L "L" :PL]
                               [P' "P′" (if ok-p' :PP' :never)] [R "R" :VR] [F "F" :AF]
                               [B' "B′" (opp :axial2)] [C' "C′" (opp :axial2)] [F' "F′" (opp :AF')]
                               [M' "M′" (opp :MM')] [D' "D′" (opp :D'E')] [E' "E′" (opp :D'E')]
                               [L' "L′" (opp :P'L')] [Q2 "Q₂" (opp :Q2V2)] [V2 "V₂" (opp :Q2V2)]]
                :when (and (not= el :never) (or (not (#{"D" "E"} t)) (pos? w)))]
            ^{:key t} [:g (dot q (colours :letter) (a el)) (letter q t (a el) opts)])]))

     (defn- tex [s & [display?]] [:f> render/render-katex s {:inline? (not display?)}])
     (defn- fx [v d] (if (js/isFinite v) (.toFixed (js/Number v) d) "—"))

     (defn- areas
       "The square on QV and the rectangle PV·VR (on PL, with the strip
        VR − PL) to scale, in Heath's frame: PL up from P, PV to the right."
       [{:keys [H]} kind]
       (let [{:keys [x p vr y]} H
             lo (min p vr) hi (max p vr)
             strip (if (= kind :ellipse) (colours :short) (colours :over))
             box (fn [x0 y0 x1 y1] (str/join " " (map (fn [[a b]] (str a "," (- b))) [[x0 y0] [x1 y0] [x1 y1] [x0 y1]])))]
         [:svg {:viewBox "-0.3 -3.2 4.4 3.5" :data-areas "book-one"
                :style {:width "100%" :height "auto" :background "#1d1f21" :border-radius "6px"}}
          [:g {:data-area "rect"} [:polygon {:points (box 0 0 x lo) :fill (colours :PL) :fill-opacity 0.3 :stroke (colours :PL) :stroke-width 0.02}]]
          (when (not= kind :parabola)
            [:g {:data-area "strip"} [:polygon {:points (box 0 lo x hi) :fill strip :fill-opacity 0.45 :stroke strip :stroke-width 0.02
                                                 :stroke-dasharray (when (= kind :ellipse) "0.06 0.04")}]])
          [:g {:data-area "square"} [:polygon {:points (box (+ x 0.15) 0 (+ x 0.15 y) y) :fill (colours :QV) :fill-opacity 0.35 :stroke (colours :QV) :stroke-width 0.02}]]
          [:line {:x1 0 :y1 0 :x2 0 :y2 (- p) :stroke (colours :PL) :stroke-width 0.03}]
          (for [[px py t] [[-0.15 0.1 "P"] [-0.15 (- p) "L"] [x 0.15 "V"] [(+ x 0.08) (- vr) (if (= kind :parabola) "" "R")]
                           [(+ x 0.15 (/ y 2)) (- (/ y 2)) "QV²"] [(/ x 2) (- (/ lo 2)) (if (= kind :parabola) "PL·PV" "PV·VR")]]]
            ^{:key (str t px)} [:text {:x px :y py :fill "#ECEEE4" :font-size 0.16 :text-anchor "middle"
                                       :font-family "Georgia, serif" :font-style "italic"} t])]))

     (defn- slider [key lo hi step label v put! & [disabled?]]
       [:label {:style {:display "flex" :align-items "center" :gap "0.6em" :margin "0.2em 0"}}
        [:span {:style {:width "6.5em"}} label]
        [:input {:type "range" :min lo :max hi :step step :value v :data-control (name key) :disabled disabled?
                 :style {:flex "1"} :on-change #(put! key (js/parseFloat (.. % -target -value)))}]
        [:span {:style {:width "4em" :text-align "right"}} (fx v 3)]])

     (defn- widget-panel [{:keys [H]} prop st put!]
       (let [{:keys [qv2 rect plpv hvvk hv vk p vr x d overlap rat-bc rat-pa rat-bf rat-pp]} H
             kind (kind-of overlap p)
             [gr lat meaning pr] (greek kind)
             par? (= "I.11" (:id prop))
             rng (:range (:tilt prop))]
         [:div {:style {:font-size "0.92em" :line-height 1.55}
                :data-book-widget (:id prop) :data-kind (name kind)
                :data-qv2 (str qv2) :data-rect (str (if par? plpv rect)) :data-hvvk (str hvvk)
                :data-ratio-l (str (if par? rat-pa rat-pp)) :data-ratio-r (str (if par? rat-bc rat-bf))}
          [:div {:data-name (name kind) :style {:font-size "1.15em" :margin-bottom "0.3em"}}
           [:strong gr] " · " [:em lat] " — " meaning " (" pr ")"]
          (if par?
            [:div (tex (str "QV^2 = " (fx qv2 4) " \\;=\\; PL \\cdot PV = " (fx p 3) " \\cdot " (fx x 3) " = " (fx plpv 4)) true)
             (tex (str "\\frac{PL}{PA} = " (fx rat-pa 4) " = \\frac{BC^2}{BA \\cdot AC} = " (fx rat-bc 4)))]
            [:div (tex (str "QV^2 = " (fx qv2 4) " \\;=\\; PV \\cdot VR = " (fx x 3) " \\cdot " (fx vr 3) " = " (fx rect 4)) true)
             (tex (str "\\frac{PL}{PP'} = " (fx rat-pp 4) " = \\frac{BF \\cdot FC}{AF^2} = " (fx rat-bf 4)
                       ",\\quad VR - PL = " (fx overlap 4)))])
          [:div (tex (str "HV \\cdot VK = " (fx hv 3) " \\cdot " (fx vk 3) " = " (fx hvvk 4)
                          (when-not par? (str ",\\quad PP' = " (fx (js/Math.abs d) 3)))))]
          (slider :tilt (if par? 0 (first rng)) (if par? 90 (second rng)) 0.1
                  (if par? "tilt (held)" "tilt (°)") (:tilt st) put! par?)
          (slider :x sym/x-min 2.5 0.001 "x = PV" (:x st) put!)
          (slider :y 0 1.6 0.001 "y = QV" (:y st) put!)
          [:div {:style {:opacity 0.6 :font-size "0.82em"}}
           (if par?
             "I.11 holds PM parallel to AC. x and y are tied by QV² = PL·PV: move one and the other is solved."
             "x and y are tied by QV² = PV·VR: move one and the other is solved at the same tilt; the tilt holds x.")]]))

     (def ^:private button-style
       {:font-family "ui-monospace, monospace" :padding "0.15em 0.7em" :margin-right "0.35em"
        :border "1px solid currentColor" :border-radius "0.35em" :background "transparent"
        :color "inherit" :cursor "pointer" :opacity 0.8})

     (defn- sentence [text]
       (into [:span]
             (for [s (re-seq #"\*\*[^*]+\*\*|\*[^*]+\*|\$[^$]+\$|[^*$]+" text)]
               (cond (str/starts-with? s "**") [:strong (subs s 2 (- (count s) 2))]
                     (str/starts-with? s "*") [:em (subs s 1 (dec (count s)))]
                     (str/starts-with? s "$") (tex (subs s 1 (dec (count s))))
                     :else s))))

     (defn render
       "Clerk render-fn. value: {:prop (the edn's proposition) :kernels
        {:heath :section :circle :symptom} :data {view + :outputs
        :parabola}}."
       [{:keys [prop kernels data]}]
       (rg/with-let [figs (into {} (map (fn [[k v]] [k (figure/kernel-figure v)])) kernels)
                     {:keys [k h zb outputs parabola]} data
                     solve (fn [deg yin x] (zipmap outputs (first (figure/points (:symptom figs) [k h zb deg yin] [[x]]))))
                     steps (:steps prop)
                     n (count steps)
                     opposite? (= :opposite (:case prop))
                     tilt0 (case-tilt prop parabola (get-in prop [:tilt :at]))
                     st (rg/atom (sym/settle solve tilt0 (:x prop) :init))
                     tr (rg/atom (start))
                     x2 0.9
                     shown (rg/atom (frame! figs solve data @st x2))
                     _ (add-watch st ::frame (fn [_ _ _ s] (reset! shown (frame! figs solve data s x2))))
                     put! (fn [key v]
                            (swap! st (fn [s] (sym/tie solve s key (if (= key :tilt) (case-tilt prop parabola v) v)))))
                     act (fn [f & args] (apply swap! tr f args))
                     alive (atom true)
                     last-ts (atom nil)
                     _ ((fn loop! [ts]
                          (when @alive
                            (let [dt (if (and @last-ts ts) (- ts @last-ts) 0)]
                              (reset! last-ts ts)
                              (when (:playing? @tr) (swap! tr advance dt n)))
                            (js/requestAnimationFrame loop!)))
                        nil)]
         (let [t @tr f @shown s @st
               v (progress-of steps t)]
           [:div {:data-book-one (:id prop) :data-step (inc (:step t)) :data-tilt (str (:tilt s))
                  :data-x (str (:x s)) :data-y (str (:y s)) :data-origin (name (:origin s))
                  :style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start" :margin "0.6em 0"}}
            [:div {:style {:flex "1 1 460px" :min-width "300px"}}
             [svg f v opposite?]
             [:div {:style {:display "flex" :align-items "center" :margin-top "0.4em"}}
              [:button {:style button-style :data-act "prev" :on-click #(act goto (dec (:step t)) n)} "◀"]
              [:button {:style button-style :data-act "play" :on-click #(act toggle n)} (if (:playing? t) "❚❚" "▶")]
              [:button {:style button-style :data-act "next" :on-click #(act goto (inc (:step t)) n)} "▶▶"]
              [:span {:style {:opacity 0.6 :font-size "0.85em"}} (str "step " (inc (:step t)) " of " n)]]
             [:div {:style {:margin-top "0.6em"}} [areas f (kind-of (get-in f [:H :overlap]) (get-in f [:H :p]))]]]
            [:div {:style {:flex "1 1 340px"}}
             [:ol {:style {:margin 0 :padding-left "2em" :font-size "0.92em" :line-height 1.45}}
              (for [[i {:keys [id text quote cite]}] (map-indexed vector steps)
                    :let [cur? (= i (:step t))]]
                ^{:key id}
                [:li {:data-step-id id :data-current (str cur?) :on-click #(act goto i n)
                      :style {:cursor "pointer" :margin-bottom "0.4em" :padding "0.2em 0.5em"
                              :border-left (str "3px solid " (if cur? "#E8735A" "transparent"))
                              :background (if cur? "rgba(232,115,90,0.08)" "transparent")
                              :opacity (if cur? 1 0.5)}}
                 [sentence text]
                 (when (and cur? quote)
                   [:div {:data-quote id :style {:font-size "0.88em" :margin-top "0.35em" :opacity 0.85}}
                    [:em (str "“" quote "”")] " " [:span {:style {:opacity 0.7}} (str "(" cite ")")]])])]
             [:div {:style {:margin-top "0.8em" :padding-top "0.6em" :border-top "1px solid rgba(128,128,128,0.3)"}}
              [widget-panel f prop s put!]]]])
         (finally (reset! alive false))))))
