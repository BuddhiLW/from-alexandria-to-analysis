(ns alexandria.apollonius.conics-3d
  "Apollonius I.11-13 in space, on emmy-viewers' MathBox: the double cone
   shaded, the cutting plane translucent, the section drawn on the plane,
   the circle HK through V parallel to the base and the ordinate QV, all
   from raster kernels (alexandria.apollonius.conics :cone-3d :section-3d
   :plane-3d :ordinate-3d :height). MathBox only draws them.

   The scene mounts once. The tilt slider is the player's controls-panel
   (an uncontrolled range input): it writes a plain atom, and one
   requestAnimationFrame loop recomputes the kernel batches into JS arrays
   when the tilt has changed; MathBox's live data primitives read those
   arrays every frame. Only the readout re-renders, never the canvas.

   Camera (three.js OrbitControls via threestrap): drag rotates, scroll
   (or CTRL+scroll) zooms, shift-drag or right-drag pans, double-click
   resets.

   Space [X Y Z] (Z the cone's axis) goes to MathBox as (X, Z, -Y), so the
   axis stands up on the screen."
  (:require [alexandria.medium.figure :as figure]
            [alexandria.medium.html-labels :as labels]
            [alexandria.medium.player :as player]
            [mathbox.core]
            [mathbox.primitives :as mb]
            [reagent.core :as r]))

(def ^:private zmax 2.4)
(def ^:private colours {:parabola "#F2D16B" :ellipse "#8FC7E8" :hyperbola "#E8735A"})
(def ^:private names
  {:parabola ["παραβολή · parabola" "c = 0: the rectangle on p fits exactly"]
   :ellipse ["ἔλλειψις · ellipse" "c < 0: it falls short"]
   :hyperbola ["ὑπερβολή · hyperbola" "c > 0: it exceeds"]})

(defn kind-of
  "The section's name from the sign of its excess c."
  [c]
  (cond (< (js/Math.abs c) 0.004) :parabola (pos? c) :hyperbola :else :ellipse))

(def ^:private n-phi 361)
(def ^:private n-circle 97)
(def ^:private plane-s [-1.9 3.4])
(def ^:private plane-y [-1.75 1.75])
(def ^:private plane-n 16)
(def ^:private abscissa
  "PV, the abscissa of the ordinate QV drawn in the figure."
  0.85)

(defn- emit3 [emit [x y z]] (emit x z (- y)))

(defn- ->mb "Space [X Y Z] in the Cartesian's axes, as emit3 emits it." [[x y z]] [x z (- y)])

(def ^:private scene-range [[-2.6 2.6] [-2.6 2.6] [-2.6 2.6]])

(def ^:private point-names
  "The letters of (:points frame), in order."
  ["P" "H" "K" "V" "Q"])

(defn- letters
  "The letters of the lettered points as HTML labels
   (alexandria.medium.html-labels), never MathBox text: MathBox's Label
   reads glyphs back from a 2D canvas, which anti-fingerprinting browsers
   answer with noise."
  [frame]
  (map (fn [text p] {:text text :at (->mb p) :colour "#ECEEE4" :size 16 :offset [10 14]})
       point-names (:points @frame)))

(defn- frame!
  "The arrays the live primitives read, for tilt theta (radians), from one
   batch per kernel. Off-cone points of the section (|Z| > zmax, the far
   reach of a parabola or hyperbola) are NaN, so MathBox draws no segment
   through them."
  [{:keys [section3 plane3 ordinate3 height]} {:keys [k h]} theta]
  (let [phis (mapv (fn [i] [(* 2 js/Math.PI (/ (+ i 0.5) (dec n-phi)))]) (range n-phi))
        sec (figure/points section3 [k theta h] phis)
        sec (mapv (fn [[_ _ z :as p]] (if (< (js/Math.abs z) zmax) p [js/NaN js/NaN js/NaN])) sec)
        [s0 s1] plane-s [y0 y1] plane-y
        grid (vec (for [j (range plane-n) i (range plane-n)]
                    [(+ s0 (* (- s1 s0) (/ i (dec plane-n)))) (+ y0 (* (- y1 y0) (/ j (dec plane-n))))]))
        plane (figure/points plane3 [k theta h] grid)
        [vx vz r qy w] (first (figure/points ordinate3 [k theta h] [[abscissa]]))
        c (second (first (figure/points height [k theta h] [[0]])))
        P [(* k h) 0 h]
        V [vx 0 vz]
        Q (if (pos? w) [vx qy vz] V)]
    {:section sec
     :plane plane
     :circle (mapv (fn [i] (let [a (* 2 js/Math.PI (/ i (dec n-circle)))]
                             [(* r (js/Math.cos a)) (* r (js/Math.sin a)) vz]))
                   (range n-circle))
     :points [P [(- r) 0 vz] [r 0 vz] V Q]
     :diameter (figure/points plane3 [k theta h] [[(first plane-s) 0] [(second plane-s) 0]])
     :qv [V Q]
     :c c
     :kind (kind-of c)}))

(defn- cone-grid
  "The double cone, once: (phi, z) on a 72 x 48 grid."
  [cone3 k]
  (vec (for [j (range 48) i (range 72)]
         (first (figure/points cone3 [k] [[(* 2 js/Math.PI (/ i 71)) (+ (- zmax) (* 2 zmax (/ j 47)))]])))))

(defn- live-line
  "A live line through the points (get @frame key)."
  [frame key n colour width]
  [:<>
   [mb/Array {:width n :channels 3 :items 1 :live true
              :expr (fn [emit i _t] (emit3 emit (nth (get @frame key) i [0 0 0])))}]
   [mb/Line {:width width :color colour :zBias 8 :zIndex 2}]])

(defn- scene
  "The MathBox canvas, mounted once; frame is a plain atom of arrays."
  [{:keys [cone3]} {:keys [k]} frame colour]
  (let [cone (cone-grid cone3 k)
        box (atom nil)
        reset-view (fn [] (when-let [^js b @box] (.reset (.. b -three -controls))))]
    (fn [_ _ _ colour]
      [:div {:on-double-click reset-view :data-figure "apollonius-3d"
             :style {:flex "1 1 520px" :min-width "320px" :position "relative"}}
       [labels/overlay {:box box :range scene-range :labels #(letters frame)}]
       [mathbox.core/MathBox
        {:container {:style {:height "540px" :width "100%"}}
         :focus 3
         :renderer {:background-color "#1d1f21"}
         :threestrap {:plugins ["core" "controls" "cursor"]}
         :ref (fn [b]
                (reset! box b)
                ;; the camera primitive places the camera after the controls
                ;; are built; save that pose as the one double-click restores
                (when b (js/setTimeout #(some-> ^js b .-three .-controls .saveState) 400)))}
        [mb/Camera {:proxy true :position [2.1 0.9 3.0]}]
        [mb/Cartesian {:range scene-range :scale [1 1 1]}
         ;; the double cone, shaded
         [mb/Area {:width 72 :height 48 :channels 3 :items 1 :live false
                   :expr (fn [emit _x _y i j _t] (emit3 emit (nth cone (+ i (* 72 j)))))}]
         [mb/Surface {:shaded true :color "#9aa7b4" :opacity 0.42 :zBias 0 :zOrder 1}]
         [mb/Surface {:shaded false :lineX true :lineY true :fill false :color "#cfd8dc" :opacity 0.18
                      :width 1 :zOrder 1}]
         ;; the cutting plane, translucent
         [mb/Area {:width plane-n :height plane-n :channels 3 :items 1 :live true
                   :expr (fn [emit _x _y i j _t] (emit3 emit (nth (:plane @frame) (+ i (* plane-n j)) [0 0 0])))}]
         [mb/Surface {:shaded false :color "#ECEEE4" :opacity 0.22 :zBias 2 :zOrder 2}]
         ;; the diameter PM, the section on the plane, the circle HK, QV
         [live-line frame :diameter 2 "#ECEEE4" 2]
         [live-line frame :section n-phi colour 6]
         [live-line frame :circle n-circle "#B9A3E3" 3]
         [live-line frame :qv 2 "#7FD18B" 5]
         [mb/Array {:width 5 :channels 3 :items 1 :live true
                    :expr (fn [emit i _t] (emit3 emit (nth (:points @frame) i [0 0 0])))}]
         [mb/Point {:size 9 :color "#ECEEE4" :zBias 10 :zIndex 3}]]]])))

(defn- readout
  "The caption beside the canvas: the tilt, c and the name it gives."
  [values frame-view]
  (let [{:keys [c kind]} @frame-view
        [greek meaning] (names kind)]
    [:div {:style {:flex "0 1 260px" :font-family "ui-monospace, monospace" :font-size "0.85em"
                   :line-height 1.6}}
     [:div {:data-kind (name kind) :style {:font-size "1.25em" :color (colours kind)}} greek]
     [:div {:style {:opacity 0.75 :font-style "italic"}} meaning]
     [:div "y² = p x + c x²"]
     [:div (str "tilt " (.toFixed (js/Number (:tilt @values)) 1) "°")]
     [:div {:data-c (str c)} (str "c = " (.toFixed (js/Number c) 3))]
     [:div {:style {:opacity 0.6 :margin-top "0.6em"}}
      "drag: rotate · scroll: zoom · shift- or right-drag: pan · double-click: reset"]]))

(defn render
  "Clerk render-fn. value: {:kernels {name kernel} :data {:k :h
   :parabola-tilt} :controls [{:id :tilt ...}]}."
  [{:keys [kernels data controls]}]
  (r/with-let [figures (into {} (map (fn [[k v]] [k (figure/kernel-figure v)])) kernels)
               init (into {} (map (juxt :id :init)) controls)
               tilt (atom (:tilt init))
               values (r/atom init)
               rad (fn [deg] (* deg (/ js/Math.PI 180)))
               frame (atom (frame! figures data (rad @tilt)))
               shown (r/atom (select-keys @frame [:c :kind]))
               colour (r/atom (colours (:kind @frame)))
               alive (atom true)
               drawn (atom @tilt)
               loop! (fn loop! []
                       (when @alive
                         (let [t @tilt]
                           (when (not= t @drawn)
                             (reset! drawn t)
                             (let [f (frame! figures data (rad t))]
                               (reset! frame f)
                               (reset! shown (select-keys f [:c :kind]))
                               (reset! values {:tilt t}))))
                         (js/requestAnimationFrame loop!)))
               _ (js/requestAnimationFrame loop!)]
    [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
     [scene figures data frame (colours (:kind @frame))]
     [:div {:style {:flex "0 1 280px"}}
      [readout values shown]
      [player/controls-panel controls @values (fn [_ v] (reset! tilt v))]]]
    (finally (reset! alive false))))
