(ns alexandria.apollonius.cone-widget
  "Apollonius I.11-14 in space, as an emmy-viewers MathBox widget
   (emmy.mathbox.plot) on the raster backend: the double cone shaded, the
   cutting plane translucent, the section drawn on the plane, Apollonius'
   letters on the figure, Leva sliders for the plane, and a caption beside
   the scene (never over it).

   Apollonius' figure (Heath 1896, I.11-14). The cone has apex A and base
   circle BC (here at height zb above A; the axis is Z, up on the screen).
   The axial triangle ABC is the cone's section by the plane Y = 0. The
   cutting plane meets the base in DME at right angles to BC, and the
   axial triangle in the diameter PM (P on AB). V is a point of PM, the
   circle HK through V is parallel to the base (H on AB, K on AC), and Q,
   Q' are where that circle meets the cutting plane. QV is the ordinate,
   PV the abscissa, and PL (at right angles to PM, in the cutting plane)
   the parameter. In I.12-13 P' is where PM meets AC (or CA produced
   beyond A), and AF, parallel to PM, meets BC (or BC produced) in F. In
   I.14 the plane cuts the opposite cone too: its base B'C' (at height
   -zb), its trace D'M'E', and the opposite branch through P'.

   Every coordinate and every number the widget shows comes from a kernel
   compiled by raster (emmy.viewer.compile/*backend* :raster, bound here,
   not left to the caller): the surfaces and curves through emmy-viewers'
   MathBox compile, the lettered points and the caption's numbers through
   one kernel (`letters`) that the page calls in the browser, and the fixed
   letters A, B, C, B', C' through alexandria.raster on the JVM. The
   section is the plane-cone intersection of
   alexandria.apollonius.conics/section-at, the plane its section-point.

   Sections that run out of the drawn cone (the parabola's and the
   hyperbola's far reach) end on the base circles: the generator angle of
   each arc runs between the angles of D and E (atan2 of a clamped square
   root, no branching). A curve or a letter that does not exist for the
   present tilt (the opposite branch of an ellipse, P' of a parabola) is
   multiplied by sqrt(q)/|sqrt(q)|: 1 where q >= 0, and NaN otherwise
   (f64.sqrt of a negative number), so MathBox draws no segment there and
   the page draws no letter."
  (:require [alexandria.apollonius.conics :as conics]
            [alexandria.apollonius.conics-view :as conics-view]
            [alexandria.raster :as raster]
            [clojure.string]
            [clojure.walk :as walk]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.mathbox.plot :as plot]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]))

;; ---------------------------------------------------------------------------
;; The figure's constants

(def k
  "The slope of the cone's generator (tan of the half-angle), the same cone
   as the proof player's."
  (:k conics-view/view))

(def zb
  "The height of the base circle BC above the apex A (and of the opposite
   base B'C' below it): the drawn double cone."
  2.4)

(def reach
  "The radius of the drawn region about the axis: the cutting plane and
   the diameter are cut off there, a little outside the base circle."
  (* 1.15 k zb))

(def defaults
  "The sliders' initial values. h is the height of P above the apex. It
   sits lower on the cone than the proof player's 0.7, so that the
   section's parabola keeps a long arc inside the drawn cone. v places V
   on PM as a fraction of the drawn diameter (from P to P' or to M)."
  {:h 0.55 :v 0.55})

;; ---------------------------------------------------------------------------
;; Emmy helpers (they act on numbers and on symbols alike)

(defn- rad [deg] (e/* deg (/ Math/PI 180)))

(defn- max0
  "max(x, 0), without branching."
  [x]
  (e// (e/+ x (e/abs x)) 2))

(defn- mask
  "1 where q >= 0, NaN where q < 0 (in the compiled kernel)."
  [q]
  (e// (e/sqrt q) (e/abs (e/sqrt q))))

(defn- smooth-min-inv
  "1 / (a^8 + b^8)^(1/8): within 9% below min(1/a, 1/b) for a, b >= 0, and
   finite when one of them is 0."
  [a b]
  (e// 1 (e/sqrt (e/sqrt (e/sqrt (e/+ (e/expt a 8) (e/expt b 8)))))))

(defn- scale [m [x y z]] [(e/* m x) (e/* m y) (e/* m z)])

(defn- at-height
  "The abscissa s of the point of the diameter PM at height z."
  [theta h z]
  (e// (e/- z h) (e/sin theta)))

(defn- diameter-range
  "[s-lo s-hi], the abscissae where the diameter PM leaves the drawn
   region (|X| <= reach, |Z| <= zb), by smooth minima."
  [theta h]
  (let [kh (e/* k h) s (e/sin theta) c (e/cos theta)]
    [(e/- 0 (smooth-min-inv (e// s (e/+ zb h)) (e// c (e/- reach kh))))
     (smooth-min-inv (e// s (e/- zb h)) (e// c (e/+ reach kh)))]))

(defn- abscissa-of-v
  "PV for the slider value v in (0, 1): v times the drawn length of the
   section's diameter from P, which ends at P' (an ellipse closing inside
   the cone) or at the base (otherwise). 1/PP' = (cos - k sin)/(2 k h),
   negative past the parabola, so it is clamped at 0."
  [theta h v]
  (e/* v (smooth-min-inv (e// (e/sin theta) (e/- zb h))
                         (max0 (e// (e/- (e/cos theta) (e/* k (e/sin theta))) (e/* 2 k h))))))

(defn- arc-half-angle
  "The half-angle of the generator angles of the section's arc between the
   trace points on the circle of height z = zt (the base, or the opposite
   base), where the plane meets that circle; pi when the section closes
   before reaching it. n is (k zt sin - (zt - h) cos)-like, d is k |zt| sin:
   cos of the half-angle = n / d, clamped."
  [n d]
  (e/atan (e/sqrt (max0 (e/- (e/square d) (e/square n)))) n))

;; ---------------------------------------------------------------------------
;; The figures, as Emmy functions of the slider values

(defn cone-surface
  "The double cone at height z and generator angle phi."
  [[z phi]]
  [(e/* k z (e/cos phi)) (e/* k z (e/sin phi)) z])

(defn plane-surface
  "The cutting plane at (u, w) in [0 1] x [-1 1], cut to the drawn
   region: abscissa s from s-lo to s-hi along PM, ordinate w * reach."
  [tilt h]
  (let [theta (rad tilt)
        [lo hi] (diameter-range theta h)]
    (fn [[u w]]
      ((conics/section-point k theta h) [(e/+ lo (e/* u (e/- hi lo))) (e/* w reach)]))))

(defn diameter
  "The diameter PM (with P'), t in [0 1] across the drawn region."
  [tilt h]
  (let [theta (rad tilt)
        [lo hi] (diameter-range theta h)]
    (fn [t] ((conics/section-point k theta h) [(e/+ lo (e/* t (e/- hi lo))) 0]))))

(defn section-near
  "The section on the cone of the base BC (I.11-13): the arc DPE through P
   at generator angle phi = t * Phi, t in [-1 1], Phi the angle of D and E
   (pi for an ellipse closing above the base)."
  [tilt h]
  (let [theta (rad tilt) s (e/sin theta) c (e/cos theta)
        big-phi (arc-half-angle (e/- (e/* k h s) (e/* (e/- zb h) c)) (e/* k zb s))]
    (fn [t] ((conics/section-at k theta h) (e/* t big-phi)))))

(defn section-opposite
  "The opposite branch (I.14) on the cone of the base B'C': the arc D'P'E'
   about phi = pi, t in [-1 1]. NaN (not drawn) unless the arc lies on the
   opposite cone inside the drawn height: for an ellipse or a parabola, and
   for a hyperbola whose branch starts beyond B'C'."
  [tilt h]
  (let [theta (rad tilt) s (e/sin theta) c (e/cos theta)
        big-psi (arc-half-angle (e/+ (e/* k h s) (e/* (e/+ zb h) c)) (e/* k zb s))]
    (fn [t]
      (let [[_ _ z :as p] ((conics/section-at k theta h) (e/+ Math/PI (e/* t big-psi)))]
        (scale (e/* (mask (e/- 0 z)) (mask (e/- (e/square (* 1.0001 zb)) (e/square z)))) p)))))

(defn- v-point
  "[V r]: the foot V of the ordinate on PM, and the radius r of the circle
   HK through V parallel to the base."
  [theta h v]
  (let [[vx _ vz :as V] ((conics/section-point k theta h) [(abscissa-of-v theta h v) 0])]
    [V (e/* k vz) vx vz]))

(defn circle-hk
  "The circle HQKQ' through V parallel to the base, phi in [0 2pi]."
  [tilt h v]
  (let [[_ r _ vz] (v-point (rad tilt) h v)]
    (fn [phi] [(e/* r (e/cos phi)) (e/* r (e/sin phi)) vz])))

(defn chord-hk
  "HK, the circle's diameter in the axial triangle, t in [-1 1]."
  [tilt h v]
  (let [[_ r _ vz] (v-point (rad tilt) h v)]
    (fn [t] [(e/* t r) 0 vz])))

(defn chord-qq
  "QQ', the chord through V parallel to DE (QV the ordinate), t in [-1 1];
   NaN where V lies outside the cone."
  [tilt h v]
  (let [[_ r vx vz] (v-point (rad tilt) h v)]
    (fn [t] [vx (e/* t (e/sqrt (e/- (e/square r) (e/square vx)))) vz])))

(defn- trace-at
  "[M D E w] where the cutting plane meets the plane Z = z: M on the
   diameter, D and E on the circle of radius k|z| (the base), w = DM^2.
   D and E are NaN when the plane misses the circle (w < 0)."
  [theta h z]
  (let [[mx _ _ :as M] ((conics/section-point k theta h) [(at-height theta h z) 0])
        w (e/- (e/square (e/* k z)) (e/square mx))
        half (e/sqrt w)]
    [M [mx half z] [mx (e/- 0 half) z] w]))

(defn trace-de
  "DME, the plane's trace on the base BC (I.11), t in [-1 1]; NaN when the
   plane misses the base circle."
  [tilt h]
  (let [[[mx _ z] _ _ w] (trace-at (rad tilt) h zb)]
    (fn [t] [mx (e/* t (e/sqrt w)) z])))

(defn trace-de'
  "D'M'E', the trace on the opposite base B'C' (I.14); NaN unless the
   plane meets that circle."
  [tilt h]
  (let [[[mx _ z] _ _ w] (trace-at (rad tilt) h (- zb))]
    (fn [t] [mx (e/* t (e/sqrt w)) z])))

(defn parameter-pl
  "PL, the parameter, at right angles to PM in the cutting plane, t in
   [0 1]; PL = p = 2 k h (k sin + cos) (alexandria.apollonius.conics/parameter)."
  [tilt h]
  (let [theta (rad tilt)]
    (fn [t] [(e/* k h) (e/* t (conics/parameter k theta h)) h])))

(defn line-af
  "AF through the apex parallel to PM, to F on BC or BC produced (I.12-13),
   t in [0 1]; NaN when F lies outside the drawn region."
  [tilt h]
  (let [theta (rad tilt)
        fx (e/- 0 (e// (e/* zb (e/cos theta)) (e/sin theta)))
        m (mask (e/- (e/square reach) (e/square fx)))]
    (fn [t] (scale (e/* t m) [fx 0 zb]))))

(def letter-index
  "Where each letter's [X Y Z] starts in the output of `letters`."
  {"P" 0 "M" 3 "D" 6 "E" 9 "V" 12 "Q" 15 "Q'" 18 "H" 21 "K" 24 "L" 27
   "P'" 30 "F" 33 "M'" 36 "D'" 39 "E'" 42})

(def number-index
  "Where each number of the caption sits in the output of `letters`: p =
   PL, c the excess, PV, QV^2 (from Q and V), HV.VK (from H, V, K), PV.VR
   (the symptoma, VR = p + c PV), PP' (the transverse; negative across the
   apex), and the I.14 flag (positive when the opposite branch is drawn)."
  {:p 45 :c 46 :pv 47 :qv2 48 :hv-vk 49 :pv-vr 50 :pp' 51 :opposite 52})

(defn letters
  "Every lettered point of the figure and the caption's numbers, for the
   sliders [tilt h v], as one flat vector (letter-index, number-index).
   The state is a dummy [0]: the page calls this kernel once per render."
  [tilt h v]
  (fn [_]
    (let [theta (rad tilt)
          sp (conics/section-point k theta h)
          P (sp [0 0])
          [V r vx vz] (v-point theta h v)
          w (e/- (e/square r) (e/square vx))
          Q [vx (e/sqrt w) vz]
          Q' [vx (e/- 0 (e/sqrt w)) vz]
          H [r 0 vz]
          K [(e/- 0 r) 0 vz]
          p (conics/parameter k theta h)
          c (conics/excess k theta)
          pv (abscissa-of-v theta h v)
          [M D E _] (trace-at theta h zb)
          [M' D' E' w'] (trace-at theta h (- zb))
          M (scale (mask (e/- (e/square reach) (e/square (first M)))) M)
          M' (scale (mask w') M')
          d (conics/transverse k theta h)
          [_ _ z' :as P'] (sp [d 0])
          P' (scale (mask (e/- (e/square (* 1.0001 zb)) (e/square z'))) P')
          fx (e/- 0 (e// (e/* zb (e/cos theta)) (e/sin theta)))
          F (scale (mask (e/- (e/square reach) (e/square fx))) [fx 0 zb])
          L [(e/* k h) p h]
          dist (fn [a b] (e/sqrt (reduce e/+ (map (fn [x y] (e/square (e/- x y))) a b))))
          qv (dist Q V)]
      (vec (concat P M D E V Q Q' H K L P' F M' D' E'
                   [p c pv (e/* qv qv) (e/* (dist H V) (dist V K)) (e/* pv (e/+ p (e/* c pv))) d w'])))))

;; ---------------------------------------------------------------------------
;; Fixed letters, by raster on the JVM

(defn fixed-letters
  "{letter [X Y Z]} of the apex and the two bases' ends: A, B, C (B on the
   side of P) and B', C' (I.14: B' on AB produced beyond A)."
  []
  (let [[bx bz] (raster/value (fn [k zb] [(e/* k zb) zb]) k zb)]
    {"A" [0.0 0.0 0.0] "B" [bx 0.0 bz] "C" [(- bx) 0.0 bz]
     "B'" [(- bx) 0.0 (- bz)] "C'" [bx 0.0 (- bz)]}))

(defn parabola-tilt-deg
  "The tilt (degrees) at which PM runs parallel to AC (I.11), by raster."
  []
  (raster/value (fn [k] (e/* (conics/parabola-tilt k) (/ 180 Math/PI))) k))

;; ---------------------------------------------------------------------------
;; The page side: browser forms around the compiled kernels

(def colours
  {:parabola "#B8860B" :ellipse "#1F6FB4" :hyperbola "#C0392B"
   :cone "#8A9BB0" :plane "#5AA0D0" :line "#3C4650" :circle "#7B5EA7" :ordinate "#2E8B57"})

(def quotes
  "One sentence of Heath (1896) per proposition, for the caption."
  {:parabola ["I.11" "It follows that the square on any ordinate to the fixed diameter PM is equal to a rectangle applied to the fixed straight line PL drawn at right angles to PM with altitude equal to the corresponding abscissa PV. Hence the section is called a Parabola."]
   :hyperbola ["I.12" "It follows that the square on the ordinate is equal to a rectangle whose height is equal to the abscissa and whose base lies along the fixed straight line PL but overlaps it by a length equal to the difference between VR and PL. Hence the section is called a Hyperbola."]
   :ellipse ["I.13" "Thus the square on the ordinate is equal to a rectangle whose height is equal to the abscissa and whose base lies along the fixed straight line PL but falls short of it by a length equal to the difference between VR and PL. The section is therefore called an Ellipse."]
   :opposite ["I.14" "If a plane cuts both parts of a double cone and does not pass through the apex, the sections of the two parts of the cone will both be hyperbolas which will have the same diameter and equal latera recta corresponding thereto. And such sections are called opposite branches."]})

(def names
  {:parabola "παραβολή · parabola" :ellipse "ἔλλειψις · ellipse" :hyperbola "ὑπερβολή · hyperbola"})

(defn- kernel-binding
  "[sym form]: `letters` compiled by the current backend, called in the
   browser as (sym (array 0) (array tilt h v))."
  [init]
  [(gensym "letters") (vc/compiled-fn letters init [0] {:simplify? false})])

(defn- coords [i] [(list 'aget 'v i) (list 'aget 'v (+ i 1)) (list 'aget 'v (+ i 2))])

(def live-letters
  "The letters whose points the page reads from the letters kernel."
  ["P" "M" "D" "E" "V" "Q" "Q'" "H" "K" "L" "P'" "F" "M'" "D'" "E'"])

(defn- live-point
  "A lettered point whose coordinates the page reads from the letters
   kernel; drawn only while all three are finite. Its letter is an HTML
   label (`label-form`)."
  [label colour]
  (let [i (letter-index label)]
    (list 'when (list* 'and 'v (map #(list 'js/isFinite %) (coords i)))
          (plot/point {:coords (coords i) :size 9 :color colour}))))

(def scene-range
  "The Cartesian's range: the double cone with a margin, every axis alike."
  (vec (repeat 3 [(- (* 1.1 zb)) (* 1.1 zb)])))

(defn- primed [l] (clojure.string/replace l "'" "′"))

(defn- label-form
  "Browser code: the HTML labels (alexandria.medium.html-labels) of the
   fixed letters and, once the kernel has answered, of the live ones at
   the kernel's points (hidden where they are NaN). MathBox's own Label is
   not used: it reads glyphs back from a 2D canvas, which
   anti-fingerprinting browsers (Mullvad, Tor) answer with noise."
  [fixed]
  (let [style {:colour (colours :line) :halo "rgba(255,255,255,0.95)" :size 16 :offset [9 12]}]
    (list 'into
          (vec (for [[l xyz] fixed] (assoc style :text (primed l) :at xyz)))
          (list 'when 'v
                (vec (for [l live-letters] (assoc style :text (primed l) :at (coords (letter-index l)))))))))

(defn- curve
  "A parametric curve of the sliders `params` (keys of the atom !c)."
  [!c params f t opts]
  (plot/parametric-curve
   (merge {:f (ev/with-params {:atom !c :params params} f) :t t :simplify? false} opts)))

(defn- kind-form
  "Browser code: the section's kind from the sign of c (a raster number)."
  []
  (list 'let ['c (list 'aget 'v (number-index :c))]
        '(cond (< (js/Math.abs c) 1e-3) :parabola (pos? c) :hyperbola :else :ellipse)))

(defn- readout
  "The caption beside the scene: the name, Heath's sentence, and the
   numbers of the symptoma, all read from the letters kernel."
  []
  (let [n (fn [key] (list '.toFixed (list 'aget 'v (number-index key)) 3))]
    (list 'when 'v
          (list 'let ['kind (kind-form)
                      'opposite (list 'pos? (list 'aget 'v (number-index :opposite)))]
                [:div {:style {:font-size "0.9em" :line-height 1.55}}
                 [:div {:data-kind '(name kind)
                        :style {:font-size "1.3em" :font-weight 600 :color (list 'get colours 'kind)}}
                  (list 'get names 'kind)]
                 [:div {:style {:font-family "ui-monospace, monospace" :margin "0.4em 0"}}
                  [:div "p = PL = " (n :p) ", c = " [:span {:data-c (list 'str (list 'aget 'v (number-index :c)))} (n :c)]]
                  [:div "PV = " (n :pv)]
                  [:div "QV² = " (n :qv2)]
                  [:div "HV·VK = " (n :hv-vk)]
                  [:div "PV·VR = PV·(p + c·PV) = " (n :pv-vr)]]
                 (list 'let ['[prop text] (list 'get quotes 'kind)]
                       '[:blockquote {:style {:margin "0.5em 0" :font-style "italic" :opacity 0.85}}
                         [:strong prop] " " text])
                 (list 'when 'opposite
                       (list 'let ['[prop text] (quotes :opposite)]
                             '[:blockquote {:data-opposite "true"
                                            :style {:margin "0.5em 0" :font-style "italic" :opacity 0.85}}
                               [:strong prop] " " text]))
                 [:div {:style {:opacity 0.6 :margin-top "0.5em"}}
                  "drag: rotate · scroll: zoom · right-drag: pan"]]))))

(defn- scene
  "The MathBox scene: cone, plane, section, lines and letters."
  [!c fixed]
  (let [tilt-h [:tilt :h] all [:tilt :h :v]
        line (fn [a b opts] (plot/line (merge {:coords [(fixed a) (fixed b)] :color (colours :line) :width 3} opts)))]
    (plot/scene
     {:range scene-range
      :scale [1 1 1]
      :ref '(fn [b] (reset! box b))
      :camera [0.85 -2.1 0.42]
      :axes [] :grids []
      :container {:style {:height "560px" :width "100%"}}}
     ;; the double cone and its two bases
     (plot/parametric-surface {:f cone-surface :u [(- zb) zb] :v [0 (* 2 Math/PI)]
                               :u-samples 48 :v-samples 72
                               :color (colours :cone) :opacity 0.35
                               :grid-u 8 :grid-v 12 :grid-opacity 0.25})
     (plot/parametric-curve {:f (fn [phi] [(e/* k zb (e/cos phi)) (e/* k zb (e/sin phi)) zb])
                             :t [0 (* 2 Math/PI)] :samples 97 :color (colours :line) :width 2})
     (plot/parametric-curve {:f (fn [phi] [(e/* k zb (e/cos phi)) (e/* k zb (e/sin phi)) (- zb)])
                             :t [0 (* 2 Math/PI)] :samples 97 :color (colours :line) :width 1.5 :opacity 0.6})
     ;; the axial triangle ABC, its sides produced beyond A (I.14), B'C'
     (line "B" "C" {}) (line "B" "B'" {}) (line "C" "C'" {}) (line "B'" "C'" {:opacity 0.6})
     ;; the cutting plane, translucent
     (plot/parametric-surface {:f (ev/with-params {:atom !c :params tilt-h} plane-surface)
                               :u [0 1] :v [-1 1] :u-samples 24 :v-samples 12
                               :color (colours :plane) :opacity 0.22 :shaded? false :simplify? false})
     ;; on the plane: the diameter PM, the traces DME and D'M'E', PL, AF
     (curve !c tilt-h diameter [0 1] {:samples 2 :color (colours :line) :width 2.5})
     (curve !c tilt-h trace-de [-1 1] {:samples 2 :color (colours :line) :width 3})
     (curve !c tilt-h trace-de' [-1 1] {:samples 2 :color (colours :line) :width 2 :opacity 0.6})
     (curve !c tilt-h parameter-pl [0 1] {:samples 2 :color (colours :parabola) :width 4})
     (curve !c tilt-h line-af [0 1] {:samples 2 :color (colours :line) :width 1.5 :opacity 0.7})
     ;; the section, coloured by its name, and the opposite branch
     (curve !c tilt-h section-near [-1 1] {:samples 512 :width 6
                                           :color (list 'if 'v (list 'get colours (kind-form)) (colours :line))})
     (curve !c tilt-h section-opposite [-1 1] {:samples 512 :width 6 :color (colours :hyperbola)})
     ;; I.11: the circle HK through V, its diameter HK, the ordinate QV
     (curve !c all circle-hk [0 (* 2 Math/PI)] {:samples 97 :color (colours :circle) :width 2.5})
     (curve !c all chord-hk [-1 1] {:samples 2 :color (colours :circle) :width 2.5})
     (curve !c all chord-qq [-1 1] {:samples 2 :color (colours :ordinate) :width 5})
     ;; the lettered points (their letters are HTML, `label-form`)
     (into [:<>]
           (concat
            (for [[_ xyz] fixed]
              (plot/point {:coords xyz :size 8 :color (colours :line)}))
            (for [l live-letters]
              (live-point l (colours :line))))))))

(defn- template-body
  "The page's body around the letters kernel [ksym kform], the atom !c and the
   scene: the kernel is bound once (reagent with-let), polled until its
   module is ready, then called once per render."
  [[ksym kform] !c scene-form schema fixed]
  (list 'reagent.core/with-let
        [ksym kform
         'box '(atom nil)
         'ready '(reagent.core/atom false)
         '_ (list 'let ['poll (list 'fn 'poll []
                                    (list 'if (list '.ready ksym)
                                          '(reset! ready true)
                                          '(js/setTimeout poll 20)))]
                  '(poll))]
        (list 'let ['v (list 'when '@ready
                             (list ksym '(array 0)
                                   (list 'array (ev/get !c :tilt) (ev/get !c :h) (ev/get !c :v))))]
              [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}
                     :data-figure "apollonius-cone"}
               [:div {:style {:flex "1 1 420px" :min-width "300px" :position "relative"}}
                ['alexandria.medium.html-labels/overlay
                 {:box 'box :range scene-range :labels (label-form fixed)}]
                scene-form]
               [:div {:style {:flex "0 1 250px" :min-width "220px"}}
                (leva/sub-panel {:fill true :flat true :title-bar false}
                                (leva/controls {:atom !c :schema schema}))
                (readout)]])))

(defn cone
  "I.11-14 on the double cone, as an emmy-viewers fragment for a Clerk
   notebook (mount it after (history-of-math.widgets/install!)). Sliders:
   the plane's tilt (degrees from the horizontal; the parabola's tilt
   first, as Apollonius begins with I.11), the height h of P above the
   apex, and V's place on PM. Compiled on the raster backend whatever the
   caller's binding."
  []
  (binding [vc/*backend* :raster]
    (let [t0 (parabola-tilt-deg)
          fixed (fixed-letters)
          schema {:tilt {:min 0 :max 85 :step 0.5 :label "tilt °"}
                  :h {:min 0.3 :max 1.2 :step 0.01 :label "P: h"}
                  :v {:min 0.02 :max 0.98 :step 0.01 :label "V on PM"}}]
      (ev/with-let [!c (assoc defaults :tilt t0)]
        (template-body (kernel-binding [t0 (:h defaults) (:v defaults)]) !c (scene !c fixed) schema fixed)))))

(defn kernel-forms
  "Every (js/Function. \"fb\" glue) form in a built fragment: the compiled
   kernels it ships. For tests: under :raster each glue loads WebAssembly."
  [form]
  (let [acc (volatile! [])]
    (walk/postwalk (fn [x]
                     (when (and (seq? x) (= 'js/Function. (first x)) (= "fb" (second x)))
                       (vswap! acc conj (nth x 2)))
                     x)
                   form)
    @acc))
