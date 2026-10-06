(ns alexandria.apollonius.conics-scenes
  "The drawings of Apollonius' Conics (alexandria.apollonius.conics), one
   method per stage (alexandria.medium.scene/draw), in Manim's idiom
   (alexandria.medium.anim). Three scenes:

     :apollonius/sections  a plane tilting through the double cone, seen in
                           orthographic projection; the section and its
                           symptoma, the application-of-areas rectangle that
                           names it. ctx :controls :tilt (degrees) sets the
                           plane in the stages that do not move it themselves.
     :apollonius/focal     the foci, the reflection at the tangent, the
                           gardener's string (sum) and its hyperbolic twin.
     :apollonius/dandelin  the two spheres in the cone touching the plane.

   ctx :figures carries the kernels of alexandria.apollonius.conics/figures:
   :cone, :section and :plane take [k theta h yaw pitch], :ellipse and
   :hyperbola [a b]. ctx :data carries k, h, the view and the parabola's
   tilt."
  (:require [alexandria.medium.anim :as a]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.math :as m]
            [alexandria.medium.plane :as plane]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            #?(:cljs [alexandria.medium.player :as player])))

#?(:cljs (def render
           "Clerk's render-fn for these scenes."
           player/render))

(defn- fmt [x digits]
  #?(:clj (format (str "%." digits "f") (double x))
     :cljs (.toFixed (js/Number x) digits)))

;; ---------------------------------------------------------------------------
;; Drawing in Manim's idiom

(defn- write
  ([palette at s] (write palette at s {}))
  ([palette at s opts]
   (svg/text at s (merge {:colour (:ink palette) :size 0.17} opts))))

(defn- label [palette at s f & [{:keys [dx dy colour] :or {dx 0.07 dy 0.07}}]]
  (svg/layer (a/fade f [0 -0.12])
             (write palette (plane/translate at [dx dy]) s
                    {:italic? true :anchor "start" :colour (or colour (:ink palette))})))

(defn- stroke [ps colour width f & [dash]]
  (when (pos? f) (svg/polyline ps {:stroke colour :width width :dash dash
                                   :attrs (when (< f 1) (a/create f))})))

(defn- dot [at colour f]
  (when (pos? f) (svg/circle at (* 0.05 f) {:fill colour})))

(defn- readout
  "Lines of mono text from the top-left corner at, faded in together."
  [palette f at lines]
  (svg/layer (a/fade f [0.2 0])
             (into [:g] (map-indexed (fn [i s] (write palette (plane/translate at [0 (* -0.24 i)]) s
                                                      {:anchor "start" :size 0.13 :mono? true}))
                                     lines))))

(def ^:private colours
  {:parabola "#F2D16B" :ellipse "#8FC7E8" :hyperbola "#E8735A"})

;; ---------------------------------------------------------------------------
;; The cone and the section, from the kernels

(def ^:private rad (/ m/pi 180))
(def ^:private zmax 2.4)

(def ^:private caption
  "The top-left corner of the caption band, right of the cone: the window
   is [[-3 6.2] [-3.1 3.1]], the cone fills x in [-3 2], captions and the
   application panel x in [2.2 6.2], so no text ever lies over the drawing."
  [2.25 2.85])

(defn- deg->tilt [deg] (* deg rad))

(defn- excess-at
  "c = k^2 sin^2 - cos^2 of the section at tilt theta, from the :height
   kernel."
  [figures {:keys [k h]} theta]
  (second (first (figure/points (:height figures) [k theta h] [[0]]))))

(defn- kind-of
  "The section's name from the sign of its excess c (a number)."
  [c]
  (cond (< (m/abs c) 0.004) :parabola (pos? c) :hyperbola :else :ellipse))

(defn- params [{:keys [k h yaw pitch]} theta] [k theta h yaw pitch])

(def ^:private rim-states
  (memoize (fn [z] (mapv (fn [i] [(* 2 m/pi (/ i 72)) z]) (range 73)))))

(def ^:private generator-states
  (memoize (fn [phi] [[phi (- zmax)] [phi zmax]])))

(defn- cone
  "The double cone: the rims at +-zmax, the outline generators, a few
   generators faint, everything at progress f."
  [palette figures {:keys [k yaw pitch]} f]
  (let [pts (fn [states] (figure/points (:cone figures) [k yaw pitch] states))
        ink (:muted palette)]
    (into [:g]
          (concat
           [(stroke (pts (rim-states zmax)) ink 0.016 f)
            (stroke (pts (rim-states (- zmax))) ink 0.016 f)]
           (for [i (range 12) :let [phi (* 2 m/pi (/ i 12))]]
             (stroke (pts (generator-states phi)) ink 0.006 f))
           ;; the outline: generators at the silhouette, phi = -yaw and pi - yaw
           (for [phi [(- yaw) (- m/pi yaw)]]
             (stroke (pts (generator-states phi)) (:ink palette) 0.014 f))))))

(def ^:private phi-states
  (memoize (fn [n] (mapv (fn [i] [(* 2.0 m/pi (/ (+ i 0.5) n))]) (range n)))))

(defn- section-runs
  "The section at tilt theta as polylines, split where it runs off the
   drawn cone (|Z| > zmax) or jumps between nappes. The heights come from
   the :height kernel and the points from the :section kernel, one batch
   each. The angles sit at half steps, so none falls on the generator
   parallel to the plane (the parabola's point at infinity)."
  [figures data theta n]
  (let [ps (params data theta)
        states (phi-states n)
        zs (zipmap (map first states)
                   (map first (figure/points (:height figures) [(:k data) theta (:h data)] states)))
        xy (zipmap (map first states) (figure/points (:section figures) ps states))
        side (fn [phi] (let [z (zs phi)] (if (< (m/abs z) zmax) (if (pos? z) :up :down) :off)))]
    (->> (map first states)
         (partition-by side)
         (remove (fn [[phi]] (= :off (side phi))))
         (map #(mapv xy %)))))

(defn- section [palette figures data theta f]
  (let [c (colours (kind-of (excess-at figures data theta)))]
    (into [:g] (map #(stroke % c 0.035 f) (section-runs figures data theta 240)))))

(defn- cutting-plane
  "The cutting plane as a translucent quadrilateral with an outline, so it
   reads as a plane and not as a stray line."
  [figures data theta f]
  (let [ps (params data theta)
        corners (figure/points (:plane figures) ps [[-0.9 -1.3] [3.4 -1.3] [3.4 1.3] [-0.9 1.3]])]
    (when (pos? f)
      (svg/polygon corners {:fill "#ECEEE4" :opacity (* 0.16 f) :stroke "#ECEEE4" :width 0.012}))))

(defn- axial-triangle
  "Apollonius' own frame (I.11, Heath p. 7): the apex A, the axial
   triangle ABC on the base circle at height zmax, and the trace DME of the
   cutting plane on the base, at right angles to BC at M. Points from the
   :space and :trace kernels; the trace is skipped where the plane misses
   the base inside the cone (w < 0: an ellipse closing above it)."
  [palette figures {:keys [k h yaw pitch]} theta f]
  (let [[A B C] (figure/points (:space figures) [yaw pitch]
                               [[0 0 0] [(- (* k zmax)) 0 zmax] [(* k zmax) 0 zmax]])
        [mx my dx dy ex ey w] (first (figure/points (:trace figures) [k theta h yaw pitch] [[zmax]]))
        M [mx my] D [dx dy] E [ex ey]
        ink (:construction palette)]
    [:g
     (stroke [B A C] ink 0.02 f)
     (stroke [B C] ink 0.016 f [0.06 0.04])
     (dot A (:ink palette) f) (label palette A "A" f {:dx 0.1 :dy -0.05})
     (label palette B "B" f {:dx -0.22 :dy 0.04}) (label palette C "C" f {:dx 0.08 :dy 0.04})
     (when (pos? w)
       [:g (stroke [D E] (:found palette) 0.024 f)
        (dot M (:found palette) f)
        (label palette D "D" f {:dx 0.06 :dy 0.08}) (label palette E "E" f {:dx 0.06 :dy -0.2})
        (label palette M "M" f {:dx 0.08 :dy -0.16})])]))

(defn- vertex [figures data theta]
  (first (figure/points (:plane figures) (params data theta) [[0 0]])))

(defn- tilt-of
  "The tilt the slider sets (degrees in ctx :controls :tilt), or default
   when there is no slider."
  [ctx default]
  (if-let [deg (get-in ctx [:controls :tilt])] (deg->tilt deg) default))

(defn- sweep
  "The tilt of a stage that sweeps the plane from the ellipse through the
   parabola to the hyperbola and back, at progress p; the slider, when
   moved away from the parabola, holds it still."
  [ctx t0 p]
  (let [deg (get-in ctx [:controls :tilt])]
    (if (and deg (> (m/abs (- (deg->tilt deg) t0)) 0.01))
      (deg->tilt deg)
      (+ t0 (* 0.55 (- (* 2 (a/play p 0.05 0.95 a/there-and-back)) 1))))))

(defn- name-card
  "The section's name, Greek and English, at the top of the caption band."
  [palette kind f]
  (let [[greek meaning] (case kind
                          :parabola ["παραβολή · parabola" "applied exactly"]
                          :ellipse ["ἔλλειψις · ellipse" "applied, falling short"]
                          :hyperbola ["ὑπερβολή · hyperbola" "applied, exceeding"])]
    (svg/layer (a/fade f [0 -0.1])
               (write palette caption greek {:anchor "start" :size 0.2 :colour (colours kind)})
               (write palette (plane/translate caption [0 -0.27]) meaning
                      {:anchor "start" :size 0.13 :italic? true :colour (:muted palette)}))))

(declare application)

;; ---------------------------------------------------------------------------
;; :apollonius/sections

(def ^:private sections :apollonius/sections)

(defmethod scene/draw [sections :cone] [_ _ p {:keys [palette figures data]}]
  [:g (cone palette figures data (a/play p 0 0.7))
   (readout palette (a/play p 0.5 0.8) caption
            ["a double cone:" "a line through a fixed point" "turned round a circle"])])

(defmethod scene/draw [sections :diameter] [_ _ p {:keys [palette figures data] :as ctx}]
  (let [theta (tilt-of ctx 0.45)]
    [:g (cone palette figures data 1)
     (cutting-plane figures data theta (a/play p 0 0.4))
     (section palette figures data theta (a/play p 0.3 0.9))
     (dot (vertex figures data theta) (:ink palette) (a/play p 0.6 0.8))
     (label palette (vertex figures data theta) "P" (a/play p 0.6 0.8))
     (readout palette (a/play p 0.7 0.95) caption
              ["the plane meets the cone" "P: the vertex of the diameter"])]))

(defmethod scene/draw [sections :circle] [_ _ p {:keys [palette figures data] :as ctx}]
  (let [theta (tilt-of ctx 0.45)
        {:keys [k h yaw pitch]} data
        ;; the height of the ordinate's foot V, 0.9 along the diameter, from the :plane-height kernel
        [z] (first (figure/points (:plane-height figures) [k theta h] [[0.9]]))
        ring (figure/points (:cone figures) [k yaw pitch] (rim-states z))
        [hk-h hk-k] (figure/points (:cone figures) [k yaw pitch] [[m/pi z] [0 z]])]
    [:g (cone palette figures data 1)
     (cutting-plane figures data theta 1)
     (section palette figures data theta 1)
     (stroke ring (:construction palette) 0.022 (a/play p 0 0.4))
     (stroke [hk-h hk-k] (:construction palette) 0.018 (a/play p 0.3 0.55))
     (label palette hk-h "H" (a/play p 0.4 0.6) {:dx -0.2})
     (label palette hk-k "K" (a/play p 0.4 0.6))
     (readout palette (a/play p 0.5 0.8) caption
              ["a circle parallel to the base" "through the ordinate's foot V:" "QV² = HV · VK"])]))

(defmethod scene/draw [sections :similar] [_ _ p {:keys [palette figures data] :as ctx}]
  (let [theta (tilt-of ctx 0.45)]
    [:g (cone palette figures data 1)
     (cutting-plane figures data theta 1)
     (section palette figures data theta 1)
     (readout palette (a/play p 0 0.3) caption
              ["HV : PV = BF : AF" "VK : P'V = FC : AF" "so QV² : PV·P'V" "  = BF·FC : AF²," "one ratio for the section"])]))

(defmethod scene/draw [sections :symptoma] [_ _ p {:keys [palette figures data] :as ctx}]
  ;; the plane tilts from the ellipse through the parabola to the hyperbola
  (let [t0 (:parabola-tilt data)
        theta (sweep ctx t0 p)
        c (excess-at figures data theta)
        kind (kind-of c)]
    [:g (cone palette figures data 1)
     (axial-triangle palette figures data theta 1)
     (cutting-plane figures data theta 1)
     (section palette figures data theta 1)
     (name-card palette kind 1)
     (readout palette 1 [2.25 2.2]
              ["y² = p x + c x²"
               (str "tilt " (fmt (/ theta rad) 1) "°")
               (str "c = " (fmt c 3))
               (case kind :parabola "c = 0" :ellipse "c < 0" :hyperbola "c > 0")])
     (application palette figures data theta 0.6 1)]))

(def ^:private panel-scale 1.15)

(defn- application
  "Apollonius' application of areas for the section at tilt theta, drawn TO
   SCALE in the caption band (one scale for every side): the upright side
   PL = p at P, the rectangle PV.VR applied to it with breadth PV = x, the
   piece LR by which VR exceeds PL (hyperbola, shaded) or the piece by
   which it falls short (ellipse, outlined), and beside it the square on
   the ordinate QV = y, equal in area to PV.VR. x, p, VR, y and c come from
   the :application kernel at the fraction t of a safe abscissa."
  [palette figures {:keys [k h]} theta t f]
  (let [[x p vr y c] (first (figure/points (:application figures) [k theta h] [[t]]))
        kind (kind-of c)
        s panel-scale
        o [2.45 -2.95]
        at (fn [[dx dy]] (plane/translate o [(* s dx) (* s dy)]))
        rect (fn [x0 x1 y0 y1] (mapv at (plane/rect x0 x1 y0 y1)))
        col (colours kind)
        gap (/ 0.3 s)]
    (svg/layer (a/fade f)
               ;; the applied rectangle PV by VR
               (svg/polygon (rect 0 x 0 (min p vr)) {:fill col :opacity 0.35 :stroke col :width 0.012})
               (when (= kind :hyperbola)
                 (svg/polygon (rect 0 x p vr) {:fill col :opacity 0.7 :stroke col :width 0.012}))
               (when (= kind :ellipse)
                 (svg/polygon (rect 0 x vr p) {:fill "none" :stroke (:muted palette) :width 0.012}))
               ;; PL, the upright side, at P
               (svg/segment (at [0 0]) (at [0 p]) {:stroke (:ink palette) :width 0.026})
               (write palette (plane/translate (at [0 (/ p 2)]) [-0.08 0]) "p" {:anchor "end" :italic? true})
               (write palette (plane/translate (at [0 0]) [-0.08 -0.05]) "P" {:anchor "end" :italic? true})
               (write palette (plane/translate (at [0 p]) [-0.08 0]) "L" {:anchor "end" :italic? true})
               (write palette (plane/translate (at [x vr]) [0.06 0.04]) "R" {:anchor "start" :italic? true})
               (write palette (plane/translate (at [(/ x 2) 0]) [0 -0.17]) "x = PV" {:italic? true :size 0.12})
               ;; the square on the ordinate, the same area
               (svg/polygon (rect (+ x gap) (+ x gap y) 0 y)
                            {:fill (:ink palette) :opacity 0.14 :stroke (:ink palette) :width 0.012})
               (write palette (plane/translate (at [(+ x gap (/ y 2)) (/ y 2)]) [0 -0.05]) "QV²" {:italic? true :size 0.13})
               (readout palette 1 [2.25 1.25]
                        [(str "PV·VR = " (fmt (* x vr) 3))
                         (str "QV²   = " (fmt (* y y) 3))
                         (str "p = " (fmt p 3) "  c = " (fmt c 3))]))))

(defmethod scene/draw [sections :apply] [_ _ p {:keys [palette figures data] :as ctx}]
  ;; the plane the slider sets (or the sweep through the three, when the
  ;; slider sits at the parallel tilt); the name follows the sign of c
  (let [t0 (:parabola-tilt data)
        theta (sweep ctx t0 p)
        c (excess-at figures data theta)
        kind (kind-of c)
        t (+ 0.35 (* 0.5 (a/there-and-back (a/clamp01 (* 2 (mod p 0.5))))))]
    [:g (cone palette figures data 1)
     (axial-triangle palette figures data theta 1)
     (cutting-plane figures data theta 1)
     (section palette figures data theta 1)
     (name-card palette kind 1)
     (readout palette 1 [2.25 2.2]
              (case kind
                :parabola ["QV² = PL·PV" "the rectangle on p" "fits exactly (c = 0)"]
                :ellipse ["QV² = PV·VR, VR < PL" "it falls short" "by a figure like PP' by PL"]
                :hyperbola ["QV² = PV·VR, VR > PL" "it exceeds" "by a figure like PP' by PL"]))
     (application palette figures data theta t 1)]))

(defmethod scene/draw [sections :names] [_ _ p {:keys [palette figures data] :as ctx}]
  (let [t0 (:parabola-tilt data)
        tilts [(- t0 0.45) t0 (+ t0 0.4)]
        fs (a/lagged p 3 0.6 0 0.8)]
    (into [:g (cone palette figures data 1)]
          (concat
           (map (fn [theta f] (section palette figures data theta f)) tilts fs)
           [(readout palette (a/play p 0.6 0.9) caption
                     ["one cone, one equation:" "y² = p x + c x²" "the sign of c names it" "ellipse · parabola · hyperbola"])]))))

;; ---------------------------------------------------------------------------
;; :apollonius/focal (the central conics in their own plane). ctx :data
;; :focal carries the semi-axes and the foci (computed on the JVM by
;; raster); every point and distance per frame comes from a kernel.

(def ^:private focal :apollonius/focal)

(def ^:private loop-states (mapv (fn [i] [(* 2 m/pi (/ i 120))]) (range 121)))

(defn- ellipse-ab [data] (get-in data [:focal :ellipse]))
(defn- hyperbola-ab [data] (get-in data [:focal :hyperbola]))

(defn- ellipse-curve [figures data f]
  (stroke (figure/points (:ellipse figures) (ellipse-ab data) loop-states) (colours :ellipse) 0.03 f))

(defn- on-ellipse [figures data t] (first (figure/points (:ellipse figures) (ellipse-ab data) [[t]])))

(defn- focal-pair [figures data t which]
  (first (figure/points (:ellipse-focal figures) (ellipse-ab data) [[t which]])))

(defn- axes [palette f]
  [:g (stroke [[-2.4 0] [2.4 0]] (:muted palette) 0.008 f "0.05 0.04")])

(defn- foci [palette data f]
  (let [[ea] (ellipse-ab data) ec (get-in data [:focal :ellipse-c])
        s [ec 0] s' [(- ec) 0]]
    [:g (dot s (:found palette) f) (dot s' (:found palette) f)
     (label palette s "S" f {:dy -0.25}) (label palette s' "S'" f {:dy -0.25})
     (dot [ea 0] (:ink palette) f) (dot [(- ea) 0] (:ink palette) f)
     (label palette [ea 0] "A" f) (label palette [(- ea) 0] "A'" f {:dx -0.25})]))

(defmethod scene/draw [focal :foci] [_ _ p {:keys [palette figures data]}]
  (let [f (a/play p 0.4 0.8)
        [ea] (ellipse-ab data) ec (get-in data [:focal :ellipse-c])
        {:keys [as-sa cb2]} (:focal data)]
    [:g (axes palette 1) (ellipse-curve figures data (a/play p 0 0.4)) (foci palette data f)
     (svg/layer (a/fade (a/play p 0.5 0.85))
                (svg/polygon (plane/rect (- ec) ea -0.25 0) {:fill (:construction palette) :opacity 0.3}))
     (readout palette (a/play p 0.6 0.9) [-2.4 1.85]
              [(str "AS · SA' = " (fmt as-sa 3))
               (str "CB² = " (fmt cb2 3) ": a fourth of the figure")])]))

(defmethod scene/draw [focal :tangent] [_ _ p {:keys [palette figures data]}]
  (let [t (+ 0.5 (* 1.6 (a/there-and-back-with-pause p)))
        P (on-ellipse figures data t)
        tan (figure/points (:ellipse-tangent figures) (ellipse-ab data) [[t -0.9] [t 0.9]])
        ec (get-in data [:focal :ellipse-c])
        [a1 a2] (focal-pair figures data t 1)]
    [:g (axes palette 1) (ellipse-curve figures data 1) (foci palette data 1)
     (stroke tan (:ink palette) 0.016 1)
     (stroke [[ec 0] P [(- ec) 0]] (:found palette) 0.02 1)
     (dot P (:ink palette) 1) (label palette P "P" 1)
     (readout palette 1 [-2.4 1.85]
              ["the focal distances make" "equal angles with the tangent"
               (str "angles: " (fmt (/ a1 rad) 1) "° vs " (fmt (/ a2 rad) 1) "°")])]))

(defmethod scene/draw [focal :sum] [_ _ p {:keys [palette figures data]}]
  (let [t (* 2 m/pi (a/play p 0 1 a/linear))
        P (on-ellipse figures data t)
        ec (get-in data [:focal :ellipse-c])
        [d1 d2] (focal-pair figures data t 0)]
    [:g (axes palette 1) (foci palette data 1)
     (stroke (figure/points (:ellipse figures) (ellipse-ab data) (mapv (fn [i] [(* t (/ i 120))]) (range 121)))
             (colours :ellipse) 0.03 1)
     (stroke [[ec 0] P [(- ec) 0]] (:found palette) 0.016 1)
     (dot P (:ink palette) 1)
     (readout palette 1 [-2.4 1.85]
              [(str "SP + S'P = " (fmt d1 3) " + " (fmt d2 3)) (str "        = " (fmt (+ d1 d2) 3) " = AA'")])]))

(defmethod scene/draw [focal :string] [_ _ p ctx]
  (scene/draw focal :sum p ctx))

(def ^:private branch-states (mapv (fn [i] [(- (* 2.3 (/ i 80)) 1.15)]) (range 81)))

(defmethod scene/draw [focal :difference] [_ _ p {:keys [palette figures data]}]
  (let [ab (hyperbola-ab data) hc (get-in data [:focal :hyperbola-c])
        t (* 1.15 (- (* 2 (a/there-and-back p)) 1))
        branch (fn [sgn] (mapv (fn [[x y]] [(* sgn x) y]) (figure/points (:hyperbola figures) ab branch-states)))
        P (first (figure/points (:hyperbola figures) ab [[t]]))
        [d1 d2] (first (figure/points (:hyperbola-focal figures) ab [[t]]))]
    [:g (axes palette 1)
     (stroke (branch 1) (colours :hyperbola) 0.03 1) (stroke (branch -1) (colours :hyperbola) 0.03 1)
     (dot [hc 0] (:found palette) 1) (dot [(- hc) 0] (:found palette) 1)
     (label palette [hc 0] "S" 1 {:dy -0.25}) (label palette [(- hc) 0] "S'" 1 {:dy -0.25})
     (stroke [[hc 0] P [(- hc) 0]] (:found palette) 0.016 1)
     (dot P (:ink palette) 1)
     (readout palette 1 [-2.4 1.85]
              [(str "S'P − SP = " (fmt d2 3) " − " (fmt d1 3)) (str "         = " (fmt (- d2 d1) 3) " = AA'")])]))

;; ---------------------------------------------------------------------------
;; :apollonius/dandelin. ctx :data :spheres was computed by a raster kernel
;; on the JVM; every point of space is projected by the :space kernel and
;; the moving Q and its distances come from the :section and :dandelin
;; kernels.

(def ^:private dandelin :apollonius/dandelin)

(defn- proj [figures {:keys [yaw pitch]} pts]
  (figure/points (:space figures) [yaw pitch] pts))

(defmethod scene/draw [dandelin :spheres] [_ _ p {:keys [palette figures data]}]
  (let [theta (:dandelin-tilt data) {:keys [centres radii contact feet]} (:spheres data)
        fs (a/lagged p 2 0.5 0 0.7)
        ;; an orthographic sphere is a circle of radius r about the projected centre
        cs (proj figures data centres)
        fps (proj figures data feet)]
    (into [:g (cone palette figures data 1)
           (cutting-plane figures data theta 1)
           (section palette figures data theta 1)]
          (concat
           (map (fn [at r f] (svg/layer (a/fade f) (svg/circle at r {:fill (:construction palette) :opacity 0.18
                                                                     :stroke (:construction palette) :width 0.014})))
                cs radii fs)
           (map (fn [z f] (stroke (figure/points (:cone figures) [(:k data) (:yaw data) (:pitch data)] (rim-states z))
                                  (:construction palette) 0.014 f))
                contact fs)
           (map (fn [at s] [:g (dot at (:found palette) (a/play p 0.7 0.9))
                            (label palette at s (a/play p 0.7 0.9))])
                fps ["F₁" "F₂"])))))

(defmethod scene/draw [dandelin :tangents] [_ _ p {:keys [palette figures data] :as ctx}]
  (let [theta (:dandelin-tilt data) {:keys [contact]} (:spheres data)
        {:keys [k h yaw pitch]} data
        phi (+ 0.6 (* 2 m/pi (a/play p 0 1 a/linear)))
        Q (first (figure/points (:section figures) [k theta h yaw pitch] [[phi]]))
        [f1 f2] (proj figures data (:feet (:spheres data)))
        [g1 g2] (figure/points (:cone figures) [k yaw pitch] (mapv (fn [z] [phi z]) contact))
        [d1 d2] (first (figure/points (:dandelin figures) [k theta h] [[phi]]))]
    [:g (scene/draw dandelin :spheres 1 ctx)
     (stroke [f1 Q f2] (:found palette) 0.02 1)
     (stroke [g1 g2] "#E8735A" 0.024 1)
     (dot Q (:ink palette) 1) (label palette Q "Q" 1)
     (readout palette 1 caption
              [(str "QF₁ = QG₁ = " (fmt d1 3))
               (str "QF₂ = QG₂ = " (fmt d2 3))
               (str "sum " (fmt (+ d1 d2) 3) " = G₁G₂")])]))

(defmethod scene/draw [dandelin :generator] [_ _ p ctx]
  (scene/draw dandelin :tangents p ctx))
