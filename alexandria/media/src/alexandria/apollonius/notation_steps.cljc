(ns alexandria.apollonius.notation-steps
  "Apollonius I.11-13, the notation passage drawn step by step: each
   sentence of resources/alexandria/apollonius/notation_steps.edn is one
   step, and the 3D figure grows by exactly what the sentence introduces.

     S1  a generator through the apex A sweeps round the base circle and
         leaves the cone behind it
     S2  the line runs on past A and sweeps the upper nappe
     S3  the axial triangle ABC: the cut through A and the diameter BC
     S4  the cutting plane slides in; its trace on the axial triangle is
         the diameter PM, P the vertex; the section is drawn on it
     S5  DE, the plane's trace on the base (at right angles to BC at M); a
         chord parallel to DE, bisected by PM at V; QV = y
     S6  PV = x along PM
     S7  the upright side PL = p at P, at right angles to PM in the plane
         of the section (the parabola: PL : PA = BC^2 : BA.AC, I.11)
     S8  the plane tilts on: to an ellipse (P' on the side AC, same nappe),
         then to a hyperbola (P' on AC produced past A, the other nappe);
         d = PP'

   Space as in alexandria.apollonius.conics: the cone X^2 + Y^2 = k^2 Z^2,
   apex A at the origin, P = (k h, 0, h) on the side AB, the base circle at
   Z = zb. The kernels return MathBox's axes, (X, -Z, Y): the apex sits on
   top, the base below, as in Apollonius' figures.

   JVM side: the Emmy figures (`figures`, each a raster kernel through
   alexandria.medium.kernel), `steps` (the edn), `data` (constants from
   raster). Both sides: the step schedule (`schedule`, `scene-state`) and
   the transport (`start` `advance` `goto` `toggle` `seek`), pure. Browser
   side: `render`, the Clerk render-fn: one MathBox scene mounted once;
   a requestAnimationFrame loop advances the transport and recomputes one
   frame of kernel batches; MathBox's live arrays and liveProps read it.
   No arithmetic on coordinates in the browser: every point is a kernel
   output; the browser only tests whether a point lies in the drawn box."
  (:require [alexandria.medium.anim :as anim]
            [clojure.string :as str]
            #?(:clj [alexandria.apollonius.conics :as conics])
            #?(:clj [alexandria.raster :as raster])
            #?(:clj [clojure.edn :as edn])
            #?(:clj [clojure.java.io :as io])
            #?(:clj [emmy.env :as e])
            #?(:cljs [alexandria.medium.figure :as figure])
            #?(:cljs [alexandria.medium.html-labels :as labels])
            #?(:cljs [mathbox.core])
            #?(:cljs [mathbox.primitives :as mb])
            #?(:cljs [reagent.core :as r])))

;; ---------------------------------------------------------------------------
;; The steps: which element appears over which part of which step

(def schedule
  "Per step, {element [a b]}: the element's progress runs 0 -> 1 (Manim's
   smooth) over the part [a b] of the step. Elements are what the sentence
   introduces (the edn's :introduces); `motions` are the moves that carry
   them (a sweep, a tilt), not new things."
  [{:apex [0 0.12] :circle [0.04 0.32] :generator [0.28 0.36] :cone [0.34 0.42] :sweep1 [0.34 1]}
   {:generator2 [0 0.3] :nappe2 [0.3 0.38] :sweep2 [0.3 1]}
   {:generator-off [0 0.25] :axial [0.1 0.4] :AB [0.1 0.55] :AC [0.2 0.65] :BC [0.3 0.75]
    :A [0.55 0.7] :B [0.65 0.8] :C [0.75 0.9]}
   {:plane [0 0.4] :PM [0.38 0.65] :P [0.38 0.5] :M [0.6 0.72] :section [0.62 1]}
   {:DE [0 0.28] :D [0.15 0.32] :E [0.18 0.35] :chord [0.35 0.62] :V [0.55 0.68]
    :QV [0.65 0.85] :Q [0.72 0.85] :y [0.8 0.95]}
   {:PV [0 0.6] :x [0.5 0.85]}
   {:PL [0 0.5] :L [0.4 0.6] :p [0.55 0.8]}
   {:tilt-ell [0.02 0.35] :PP' [0.36 0.5] :P' [0.36 0.48] :d [0.42 0.55]
    :AC' [0.55 0.65] :widen [0.58 0.7] :tilt-hyp [0.62 1]}])

(def motions
  "Schedule keys that move what is already drawn instead of adding to it."
  #{:sweep1 :sweep2 :generator-off :tilt-ell :tilt-hyp :widen})

(def durations
  "Milliseconds per step."
  [7000 5000 5000 6000 6500 3500 5000 9000])

(def step-count (count schedule))

(defn progress-of
  "{key progress} at transport state {:step :progress}: steps before the
   current one complete, later ones not begun."
  [{:keys [step progress]}]
  (into {}
        (for [[j sched] (map-indexed vector schedule)
              :let [q (cond (< j step) 1 (= j step) progress :else 0)]
              [key [a b]] sched]
          [key (anim/play q a b)])))

(defn scene-state
  "What the figure shows at transport state st, for data's tilts: {:v
   {key progress} :theta tilt :sweep generator turn ...}. The tilt is the
   parabola's until S8, then runs to the ellipse's and on to the
   hyperbola's."
  [st {:keys [theta-par theta-ell theta-hyp s-near s-far]}]
  (let [v (progress-of st)
        te (:tilt-ell v) th (:tilt-hyp v)]
    {:v v
     :theta (+ theta-par (* (- theta-ell theta-par) te) (* (- theta-hyp theta-ell) th))
     :turn (+ (:sweep1 v) (:sweep2 v))
     :s0 (+ s-near (* (- s-far s-near) (:widen v)))}))

;; ---------------------------------------------------------------------------
;; The transport: prev / play / next over the steps, pure

(defn start "Step 1, playing once." [] {:step 0 :progress 0 :playing? true :through? false})

(defn last-step? [{:keys [step]}] (= step (dec step-count)))

(defn advance
  "st after dt milliseconds. A step started by next/prev or a click plays
   once and stops; play (:through? true) runs on to the end."
  [{:keys [step progress playing? through?] :as st} dt]
  (if-not playing?
    st
    (let [p (+ progress (/ dt (nth durations step)))]
      (cond (< p 1) (assoc st :progress p)
            (and through? (not (last-step? st))) (assoc st :step (inc step) :progress 0)
            :else (assoc st :progress 1 :playing? false)))))

(defn goto
  "Step i (clamped) from its start, playing once."
  [_ i]
  {:step (max 0 (min (dec step-count) i)) :progress 0 :playing? true :through? false})

(defn toggle
  "Pause, or play on through the steps (from the next step when the current
   one has ended, from the start after the last)."
  [{:keys [step progress playing?] :as st}]
  (cond playing? (assoc st :playing? false)
        (and (>= progress 1) (last-step? st)) {:step 0 :progress 0 :playing? true :through? true}
        (>= progress 1) {:step (inc step) :progress 0 :playing? true :through? true}
        :else (assoc st :playing? true :through? true)))

(defn seek "Paused at progress p of the current step." [st p]
  (assoc st :progress (max 0 (min 1 p)) :playing? false))

;; ---------------------------------------------------------------------------
;; The sentence's Markdown, as segments for the step list

(defn segments
  "[[kind text] ...] of a sentence: :b **bold**, :i *italic*, :m $math$
   (P' as P′), :t plain."
  [text]
  (mapv (fn [s]
          (cond (str/starts-with? s "**") [:b (subs s 2 (- (count s) 2))]
                (str/starts-with? s "*") [:i (subs s 1 (dec (count s)))]
                (str/starts-with? s "$") [:m (str/replace (subs s 1 (dec (count s))) "'" "′")]
                :else [:t s]))
        (re-seq #"\*\*[^*]+\*\*|\*[^*]+\*|\$[^$]+\$|[^*$]+" text)))

;; ---------------------------------------------------------------------------
;; The named points, in the order the :named kernel returns them

(def named-points
  "Points (3 numbers each, MathBox axes) of the :named kernel, in order."
  [:A :B :C :M :D :E :P :V :Q :Q2 :L :P' :mid-PV :mid-QV :mid-PL :mid-PP' :C'])

(def named-scalars
  "The numbers after the points: w (DE inside the base when > 0), wq (the
   chord inside the cone when > 0), p, c, d, y = QV, y^2 and p x (the
   symptoma's two sides for the parabola)."
  [:w :wq :p :c :d :y :y2 :px])

(defn named-map
  "{name value} of one output row of the :named kernel."
  [row]
  (let [n (count named-points)]
    (merge (zipmap named-points (map vec (partition 3 (take (* 3 n) row))))
           (zipmap named-scalars (drop (* 3 n) row)))))

;; ---------------------------------------------------------------------------
;; JVM: the figures (Emmy, compiled to raster kernels), the steps, the data

#?(:clj
   (do
     (defn- ->mb
       "Space [X Y Z] in MathBox's axes: Z down the screen, Y toward the eye."
       [[X Y Z]] [X (e/- 0 Z) Y])

     (defn cone-figure
       "The cone's point at turn fraction sweep*u of the way round and height
        z0 + (z1 - z0) g v. Also the base circle (z0 = z1) and a generator
        (u = 1, v along it)."
       [k sweep z0 z1 g]
       (fn [[u v]]
         (let [phi (e/* 2 Math/PI sweep u)
               z (e/+ z0 (e/* (e/- z1 z0) g v))]
           (->mb [(e/* k z (e/cos phi)) (e/* k z (e/sin phi)) z]))))

     (defn section-figure
       "The section's point on the generator at angle pi f u (u in [-1 1]),
        conics/section-at: from P (u = 0) out both ways as f grows."
       [k theta h f]
       (fn [[u]] (->mb ((conics/section-at k theta h) (e/* Math/PI f u)))))

     (defn plane-figure
       "The cutting plane at u, v in [0 1]: abscissa s0 + (s1 - s0) u,
        ordinate ylim (2v - 1), pushed (1 - f) shift along the normal (sin
        theta, 0, cos theta) so it slides in as f runs to 1."
       [k theta h f shift s0 s1 ylim]
       (fn [[u v]]
         (let [s (e/+ s0 (e/* (e/- s1 s0) u))
               y (e/* ylim (e/- (e/* 2 v) 1))
               [X Y Z] ((conics/section-point k theta h) [s y])
               off (e/* (e/- 1 f) shift)]
           (->mb [(e/+ X (e/* off (e/sin theta))) Y (e/+ Z (e/* off (e/cos theta)))]))))

     (defn segment-figure
       "The point u of the way from a to b, f of the segment drawn (Create)."
       [ax ay az bx by bz f]
       (fn [[u]]
         (let [t (e/* f u)]
           [(e/+ ax (e/* t (e/- bx ax))) (e/+ ay (e/* t (e/- by ay))) (e/+ az (e/* t (e/- bz az)))])))

     (defn named-figure
       "Every lettered point of the passage, for the cone of slope k, the
        plane tilted theta through P at height h, the base at zb, the upper
        nappe to ztop, the ordinate at abscissa x: `named-points` then
        `named-scalars`. M where PM meets the base, D E on the trace at
        right angles to BC (sqrt |w|), V at PV = x, Q Q2 the chord's ends
        (QV^2 = HV.VK = r^2 - XV^2), L at PL = p across PM, P' at PP' = d,
        C' on CA produced past A; midpoints for the letters x y p d."
       [k theta h zb ztop x]
       (fn [[_]]
         (let [pt (conics/section-point k theta h)
               kz (e/* k zb)
               [mx _ _] (pt [(e// (e/- zb h) (e/sin theta)) 0])
               w (e/- (e/square kz) (e/square mx))
               half (e/sqrt (e/abs w))
               P [(e/* k h) 0 h]
               [vx _ vz] (pt [x 0])
               wq (e/- (e/square (e/* k vz)) (e/square vx))
               y (e/sqrt (e/abs wq))
               p (conics/parameter k theta h)
               d (conics/transverse k theta h)
               P' (pt [d 0])
               V [vx 0 vz] Q [vx y vz] L [(e/* k h) (e/- 0 p) h]
               mid (fn [a b] (mapv (fn [s t] (e// (e/+ s t) 2)) a b))
               pts [[0 0 0] [kz 0 zb] [(e/- 0 kz) 0 zb] [mx 0 zb] [mx (e/- 0 half) zb] [mx half zb]
                    P V Q [vx (e/- 0 y) vz] L P' (mid P V) (mid V Q) (mid P L) (mid P P')
                    [(e/* k ztop) 0 (e/- 0 ztop)]]]
           (vec (concat (mapcat ->mb pts) [w wq p (conics/excess k theta) d y (e/square y) (e/* p x)])))))

     (def view
       "The cone's slope k, P's height h, the base zb, the upper nappe's
        reach ztop, the abscissa x of the drawn ordinate; the tilts of S8
        in degrees; the plane's reach along PM (s-near widens to s-far in
        S8, so P' on the upper nappe lies on it); the drawn box in MathBox
        y (Z from -ztop to zb)."
       {:k 0.6 :h 0.7 :zb 2.2 :ztop 1.6 :x 0.85
        :ell-deg 35 :hyp-deg 80
        :shift 1.4 :s-near -0.7 :s-far -2.4 :s1 3.2 :ylim 1.7
        :y-lo -2.25 :y-hi 1.65})

     (def figures
       "Every moving figure of the scene, by name, for alexandria.medium.kernel."
       {:cone {:f cone-figure :params [0.6 1 0 2.2 1] :state [0.5 0.5]}
        :section {:f section-figure :params [0.6 1.03 0.7 1] :state [0.3]}
        :plane {:f plane-figure :params [0.6 1.03 0.7 1 1.4 -0.7 3.2 1.7] :state [0.5 0.5]}
        :segment {:f segment-figure :params [0 0 0 1 1 1 1] :state [1]}
        :named {:f named-figure :params [0.6 1.03 0.7 2.2 1.6 0.85] :state [0] :opts {:simplify? false}}})

     (def resource "alexandria/apollonius/notation_steps.edn")

     (defn passage
       "The passage: {:title :source :steps [{:id :text :introduces}]}."
       []
       (edn/read-string (slurp (io/resource resource))))

     (defn data
       "The constants the scene reads: view, the three tilts (radians) and
        I.11's ratio for the parabola, each by a raster kernel of its Emmy
        expression."
       []
       (let [{:keys [k h zb ell-deg hyp-deg]} view
             [tp te th] (raster/value (fn [k a b] [(conics/parabola-tilt k)
                                                   (e/* a (e// Math/PI 180)) (e/* b (e// Math/PI 180))])
                                      k ell-deg hyp-deg)
             [pl-pa bc2-ba-ac pl pa]
             (raster/value (fn [k h zb t]
                             (let [p (conics/parameter k t h)
                                   pa (e/* h (e/sqrt (e/+ 1 (e/square k))))]
                               [(e// p pa)
                                (e// (e/square (e/* 2 k zb)) (e/* (e/square zb) (e/+ 1 (e/square k))))
                                p pa]))
                           k h zb tp)]
         (assoc view
                :theta-par tp :theta-ell te :theta-hyp th
                :ratio {:pl-pa pl-pa :bc2-ba-ac bc2-ba-ac :pl pl :pa pa}
                :source raster/source)))))

;; ---------------------------------------------------------------------------
;; Browser: the scene

#?(:cljs
   (do
     (def ^:private n-u 73)
     (def ^:private n-v 13)
     (def ^:private n-sec 481)
     (def ^:private n-plane 12)
     (def ^:private cone-grid (vec (for [j (range n-v) i (range n-u)] [(/ i (dec n-u)) (/ j (dec n-v))])))
     (def ^:private circle-states (mapv (fn [i] [(/ i (dec n-u)) 0]) (range n-u)))
     (def ^:private sec-states (mapv (fn [i] [(- (* 2 (/ i (dec n-sec))) 1)]) (range n-sec)))
     (def ^:private plane-grid (vec (for [j (range n-plane) i (range n-plane)] [(/ i (dec n-plane)) (/ j (dec n-plane))])))
     (def ^:private ends [[0] [1]])

     (def ^:private colours
       {:cone "#9aa7b4" :circle "#cfd8dc" :generator "#F2D16B" :axial "#8FC7E8" :plane "#ECEEE4"
        :PM "#ECEEE4" :section "#E8735A" :DE "#B9A3E3" :chord "#7FD18B" :QV "#7FD18B" :PV "#8FC7E8"
        :PL "#F2D16B" :PP' "#FF9F43" :letter "#ECEEE4"})

     (defn- in-box?
       "Whether MathBox point q lies within the drawn reach of the cone."
       [{:keys [y-lo y-hi]} [_ y _ :as q]]
       (and q (js/isFinite y) (<= y-lo y y-hi)))

     (defn- clip [data pts] (mapv #(if (in-box? data %) % [js/NaN js/NaN js/NaN]) pts))

     (defn- frame!
       "One frame: every array the scene reads and every element's opacity,
        from one batch per kernel, at transport state st."
       [{:keys [cone section plane segment named]} {:keys [k h zb ztop x shift s1 ylim] :as data} st]
       (let [{:keys [v theta turn s0]} (scene-state st data)
             nm (named-map (first (figure/points named [k theta h zb ztop x] [[0]])))
             seg (fn [a b f] (figure/points segment (conj (into (nm a) (nm b)) f) ends))
             cz (- ztop)
             gen-off (- 1 (:generator-off v))
             ok-de (pos? (:w nm))
             ok-q (pos? (:wq nm))
             ok-p' (in-box? data (:P' nm))
             alpha (fn [key] (get v key 0))]
         {:cone (figure/points cone [k (:sweep1 v) 0 zb 1] cone-grid)
          :nappe2 (figure/points cone [k (:sweep2 v) 0 cz 1] cone-grid)
          :circle (figure/points cone [k (:circle v) zb zb 1] circle-states)
          :generator (figure/points cone [k turn 0 zb 1] [[1 0] [1 1]])
          :generator2 (figure/points cone [k turn 0 cz (:generator2 v)] [[1 0] [1 1]])
          :section (clip data (figure/points section [k theta h (:section v)] sec-states))
          :plane (figure/points plane [k theta h (:plane v) shift s0 s1 ylim] plane-grid)
          :AB (seg :A :B (:AB v)) :AC (seg :A :C (:AC v)) :BC (seg :B :C (:BC v))
          :axial [(nm :A) (nm :A) (nm :B) (nm :C)]
          :PM (seg :P :M (:PM v)) :DE (seg :D :E (:DE v)) :chord (seg :Q2 :Q (:chord v))
          :QV (seg :V :Q (:QV v)) :PV (seg :P :V (:PV v)) :PL (seg :P :L (:PL v))
          :PP' (seg :P :P' (:PP' v)) :AC' (seg :A :C' (:AC' v))
          :at nm
          :alpha (merge (into {} (map (fn [key] [key (if (pos? (alpha key)) 1 0)]))
                              [:circle :AB :AC :BC :PM :section :chord :QV :PV :PL :AC'])
                        {:apex (alpha :apex)
                         :generator (* (alpha :generator) gen-off)
                         :generator2 (* (if (pos? (alpha :generator2)) 1 0) gen-off)
                         :cone (* 0.42 (alpha :cone)) :nappe2 (* 0.42 (alpha :nappe2))
                         :axial (* 0.16 (alpha :axial))
                         :plane (* 0.22 (alpha :plane))
                         :A (alpha :A) :B (alpha :B) :C (alpha :C) :P (alpha :P) :M (alpha :M)
                         :V (alpha :V) :L (alpha :L) :x (alpha :x) :p (alpha :p)
                         :DE (if (and ok-de (pos? (alpha :DE))) 1 0)
                         :D (if ok-de (alpha :D) 0) :E (if ok-de (alpha :E) 0)
                         :Q (if ok-q (alpha :Q) 0) :y (if ok-q (alpha :y) 0)
                         :PP' (if (and ok-p' (pos? (alpha :PP'))) 1 0)
                         :P' (if ok-p' (alpha :P') 0) :d (if ok-p' (alpha :d) 0)})
          :ok-p' ok-p'}))

     (defn- opacity [frame key] #js {:opacity (fn [] (get-in @frame [:alpha key] 0))})

     (defn- line [frame key n width]
       [:<>
        [mb/Array {:width n :channels 3 :items 1 :live true
                       :expr (fn [emit i _t] (let [[a b c] (nth (get @frame key) i [0 0 0])] (emit a b c)))}]
        [mb/Line {:width width :color (colours key) :zBias 6 :zIndex 2 :liveProps (opacity frame key)}]])

     (def ^:private marks
       "The lettered points: [schedule key, :named point, text, opts]. The
        letters are HTML (alexandria.medium.html-labels), never MathBox
        text: MathBox's Label reads glyphs back from a 2D canvas, which
        anti-fingerprinting browsers answer with noise."
       [[:apex :A "" {}]
        [:A :A "A" {:dot? false}]
        [:B :B "B" {}] [:C :C "C" {}]
        [:P :P "P" {}] [:M :M "M" {}]
        [:D :D "D" {}] [:E :E "E" {}]
        [:V :V "V" {}] [:Q :Q "Q" {}]
        [:L :L "L" {}] [:P' :P' "P′" {}]
        [:x :mid-PV "x" {:dot? false :italic? true :colour (colours :PV)}]
        [:y :mid-QV "y" {:dot? false :italic? true :colour (colours :QV)}]
        [:p :mid-PL "p" {:dot? false :italic? true :colour (colours :PL)}]
        [:d :mid-PP' "d" {:dot? false :italic? true :colour (colours :PP')}]])

     (defn- mark
       "A lettered point's dot (its letter is in `letters`)."
       [frame key at {:keys [dot? colour] :or {dot? true colour (colours :letter)}}]
       (when dot?
         [:<>
          [mb/Array {:width 1 :channels 3 :items 1 :live true
                         :expr (fn [emit _i _t] (let [[a b c] (get-in @frame [:at at] [0 0 0])] (emit a b c)))}]
          [mb/Point {:size 9 :color colour :zIndex 3 :zBias 10 :liveProps (opacity frame key)}]]))

     (defn- letters
       "The letters of `marks` at the current frame, for the HTML overlay."
       [frame]
       (let [{:keys [at alpha]} @frame]
         (for [[key pt text {:keys [italic? colour]}] marks
               :when (seq text)]
           {:text text :at (get at pt) :opacity (get alpha key 0) :italic? italic?
            :colour (or colour (colours :letter)) :size 17 :offset [11 13]
            :background "rgba(29,31,33,0.85)"})))

     (defn- surface [frame key w h opts]
       [:<>
        [mb/Area {:width w :height h :channels 3 :items 1 :live true
                      :expr (fn [emit _x _y i j _t] (let [[a b c] (nth (get @frame key) (+ i (* w j)) [0 0 0])] (emit a b c)))}]
        [mb/Surface (merge {:shaded false :color (colours key) :zBias 0 :zOrder 1
                                :liveProps (opacity frame key)} opts)]])

     (def ^:private scene-range [[-2.4 2.4] [-2.6 2.2] [-2.4 2.4]])

     (defn- scene
       "The MathBox canvas, mounted once; it reads the plain atom frame. The
        letters ride over it as HTML (alexandria.medium.html-labels)."
       [frame]
       (let [box (atom nil)
             reset-view (fn [] (when-let [^js b @box] (.reset (.. b -three -controls))))]
         (fn [_]
           [:div {:on-double-click reset-view :data-figure "apollonius-notation"
                  :style {:width "100%" :position "relative"}}
            [labels/overlay {:box box :range scene-range :labels #(letters frame)}]
            [mathbox.core/MathBox
             {:container {:style {:height "520px" :width "100%"}}
              :focus 3
              :renderer {:background-color "#1d1f21"}
              :threestrap {:plugins ["core" "controls" "cursor"]}
              :ref (fn [b]
                     (reset! box b)
                     (when b (js/setTimeout #(some-> ^js b .-three .-controls .saveState) 400)))}
             [mb/Camera {:proxy true :position [1.3 0.4 1.75]}]
             [mb/Cartesian {:range scene-range :scale [1 1 1]}
              [surface frame :cone n-u n-v {:shaded true}]
              [surface frame :nappe2 n-u n-v {:shaded true}]
              [surface frame :axial 2 2 {:zOrder 2}]
              [surface frame :plane n-plane n-plane {:zOrder 3 :zBias 2}]
              [line frame :circle n-u 3]
              [line frame :generator 2 5]
              [line frame :generator2 2 5]
              [line frame :AB 2 4] [line frame :AC 2 4] [line frame :BC 2 4]
              [line frame :AC' 2 3]
              [line frame :section n-sec 6]
              [line frame :PM 2 4]
              [line frame :DE 2 4]
              [line frame :chord 2 4]
              [line frame :QV 2 7]
              [line frame :PV 2 7]
              [line frame :PL 2 7]
              [line frame :PP' 2 6]
              (for [[key at _ opts] marks] ^{:key (str key)} [mark frame key at opts])]]])))

     (def ^:private button-style
       {:font-family "ui-monospace, monospace" :font-size "0.95em" :padding "0.15em 0.7em"
        :margin-right "0.35em" :border "1px solid currentColor" :border-radius "0.35em"
        :background "transparent" :color "inherit" :cursor "pointer" :opacity 0.8})

     (defn- transport [st act]
       [:div {:style {:display "flex" :align-items "center" :margin-top "0.5em"}}
        [:button {:style button-style :data-act "prev" :on-click #(act goto (dec (:step st)))} "◀"]
        [:button {:style button-style :data-act "play" :on-click #(act toggle)} (if (:playing? st) "❚❚" "▶")]
        [:button {:style button-style :data-act "next" :on-click #(act goto (inc (:step st)))} "▶▶"]
        [:input {:type "range" :min 0 :max 1000 :style {:flex "1"}
                 :value (js/Math.round (* 1000 (:progress st)))
                 :on-change #(act seek (/ (js/parseInt (.. % -target -value)) 1000))}]])

     (defn- sentence [text]
       (into [:span]
             (for [[kind s] (segments text)]
               (case kind
                 :b [:strong s]
                 :i [:em s]
                 :m [:span {:style {:font-family "Georgia, 'Times New Roman', serif" :font-style "italic"}} s]
                 s))))

     (defn- step-list [steps st act]
       [:ol {:style {:margin 0 :padding-left "2.2em" :font-size "0.95em" :line-height 1.45}}
        (for [[i {:keys [id text]}] (map-indexed vector steps)
              :let [current? (= i (:step st))]]
          ^{:key id}
          [:li {:data-step (inc i) :data-current (str current?) :on-click #(act goto i)
                :style {:cursor "pointer" :margin-bottom "0.45em" :padding "0.2em 0.5em"
                        :border-left (str "3px solid " (if current? "#E8735A" "transparent"))
                        :background (if current? "rgba(232,115,90,0.08)" "transparent")
                        :opacity (if current? 1 0.45) :transition "opacity 300ms"}}
           [sentence text]])])

     (defn- fixed [n] (.toFixed (js/Number n) 3))

     (defn- readout
       "The numbers of the lengths drawn so far, all kernel outputs (the
        ratio of I.11 from raster on the JVM)."
       [shown {:keys [ratio x]}]
       (let [{:keys [alpha at]} @shown
             on? (fn [key] (> (get alpha key 0) 0.5))
             {:keys [y p c d y2 px]} at]
         [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.82em" :line-height 1.6
                        :margin-top "0.6em" :padding-left "2.2em"}}
          (when (on? :y) [:div {:data-y (str y)} (str "y = QV = " (fixed y))])
          (when (on? :x) [:div {:data-x (str x)} (str "x = PV = " (fixed x) "   y² = " (fixed y2))])
          (when (on? :p)
            [:div {:data-p (str p)} (str "p = PL = " (fixed p) "   p·x = " (fixed px))])
          (when (and (on? :p) (not (on? :d)) (< (js/Math.abs c) 0.004))
            [:div (str "I.11: PL : PA = " (fixed (:pl-pa ratio)) " = BC² : BA·AC = " (fixed (:bc2-ba-ac ratio)))])
          (when (on? :d)
            [:div {:data-d (str d)}
             (str "d = PP′ = " (fixed (js/Math.abs d))
                  (cond (< (js/Math.abs c) 0.004) "  parabola"
                        (pos? c) "  hyperbola: P′ on the other nappe"
                        :else "  ellipse: P′ on the same nappe"))])
          [:div {:style {:opacity 0.55 :margin-top "0.5em"}}
           "drag: rotate · scroll: zoom · right-drag: pan · double-click: reset"]]))

     (defn render
       "Clerk render-fn. value: {:steps [{:id :text}] :kernels {name kernel}
        :data (alexandria.apollonius.notation-steps/data)}."
       [{:keys [steps kernels data]}]
       (r/with-let [figures (into {} (map (fn [[k v]] [k (figure/kernel-figure v)])) kernels)
                    st (r/atom (start))
                    act (fn [f & args] (apply swap! st f args))
                    frame (atom (frame! figures data @st))
                    shown (r/atom (select-keys @frame [:alpha :at]))
                    alive (atom true)
                    drawn (atom nil)
                    last-ts (atom nil)
                    loop! (fn loop! [ts]
                            (when @alive
                              (let [dt (if @last-ts (- ts @last-ts) 0)]
                                (reset! last-ts ts)
                                (when (:playing? @st) (swap! st advance dt))
                                (let [s @st]
                                  (when-not (identical? s @drawn)
                                    (reset! drawn s)
                                    (let [f (frame! figures data s)]
                                      (reset! frame f)
                                      (reset! shown (select-keys f [:alpha :at]))))))
                              (js/requestAnimationFrame loop!)))
                    _ (js/requestAnimationFrame loop!)]
         (let [s @st]
           [:div {:data-notation-step (inc (:step s)) :data-progress (str (:progress s))
                  :style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
            [:div {:style {:flex "1 1 520px" :min-width "300px"}}
             [scene frame]
             [transport s act]]
            [:div {:style {:flex "1 1 300px"}}
             [step-list steps s act]
             [readout shown data]]])
         (finally (reset! alive false))))))
