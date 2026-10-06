(ns alexandria.descartes.geometrie-scenes
  "The drawings of La Geometrie (alexandria.descartes.geometrie), one method
   per [scene stage] on alexandria.medium.scene/draw, in Manim's idiom
   (alexandria.medium.anim).

     :descartes/unit    the unit segment: product, quotient, square root;
                        ctx :controls :a and :b are the player's sliders
     :descartes/pappus  four lines, a point running on the locus with its
                        four oblique lines drawn, the ratio held; ctx
                        :controls :lam the ratio, ctx :figures :pappus the
                        locus kernel, ctx :data the lines and the
                        discriminant as a quadratic in lam
     :descartes/normal  the circle about P cutting, then touching, the
                        parabola y^2 = x
     :descartes/signs   the quartic, its signs and its four roots"
  (:require [alexandria.medium.anim :as a]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.math :as m]
            [alexandria.medium.plane :as plane]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            #?(:cljs [alexandria.medium.player :as player])))

#?(:cljs (def render "Clerk's render-fn for these scenes." player/render))

;; ---------------------------------------------------------------------------
;; Drawing helpers

(defn- fmt [x digits]
  #?(:clj (format (str "%." digits "f") (double x))
     :cljs (.toFixed (js/Number x) digits)))

(defn- write
  ([palette at s] (write palette at s {}))
  ([palette at s opts]
   (svg/text at s (-> (merge {:colour (:ink palette) :size 0.13} opts) (update :size * 1.45)))))

(defn- label [palette at s f & [{:keys [dx dy] :or {dx 0.07 dy 0.08}}]]
  (svg/layer (a/fade f [0 -0.12])
             (write palette (plane/translate at [dx dy]) s {:italic? true :anchor "start"})))

(defn- stroke [ps colour width f]
  (when (pos? f) (svg/polyline ps {:stroke colour :width width :attrs (a/create f)})))

(defn- dot [at colour f]
  (when (pos? f) (svg/circle at (* 0.05 f) {:fill colour})))

(defn- readout
  "Lines of mono text from the top-left corner `at`, faded in together."
  [palette at f lines]
  (svg/layer (a/fade f [0.2 0])
             (into [:g] (map-indexed (fn [i s] (write palette (plane/translate at [0 (* -0.3 i)]) s
                                                      {:anchor "start" :size 0.09 :mono? true}))
                                     lines))))

(defn- control [ctx k default] (double (get-in ctx [:controls k] default)))
(defn- one-hot [i n] (mapv #(if (= % i) 1 0) (range n)))

;; ---------------------------------------------------------------------------
;; :descartes/unit. Every point is the :multiplication or :square-root
;; kernel at one-hot weights; the arc is the :root-arc kernel.

(def ^:private theta 0.9)
(def ^:private unit-order [:B :A :D :C :E])
(def ^:private root-order [:F :G :H :K :I])

(defn- unit-points [figures a b]
  (zipmap unit-order (figure/points (:multiplication figures) [a b theta] (mapv #(one-hot % 5) (range 5)))))

(defn- root-points [figures a]
  (zipmap root-order (figure/points (:square-root figures) [a] (mapv #(one-hot % 5) (range 5)))))

(defn- multiplication-figure
  [palette {:keys [B A D C E]} {:keys [rays unit ca de prod]}]
  [:g
   (stroke [B (plane/lerp-point B D 1.15)] (:muted palette) 0.012 rays)
   (stroke [B (plane/lerp-point B E 1.1)] (:muted palette) 0.012 rays)
   (stroke [B A] (:found palette) 0.05 unit)
   (stroke [A C] (:construction palette) 0.02 ca)
   (stroke [D E] (:construction palette) 0.02 de)
   (stroke [B E] (:found palette) 0.035 prod)
   (dot B (:ink palette) rays) (dot A (:found palette) unit) (dot D (:ink palette) rays)
   (dot C (:ink palette) rays) (dot E (:found palette) prod)])

(defn- unit-letters [palette pts f]
  (into [:g] (for [k [:B :A :D :C :E]] (label palette (pts k) (name k) f))))

(defn- length [[x0 y0] [x1 y1]] (m/hypot (- x1 x0) (- y1 y0)))

(defmethod scene/draw [:descartes/unit :unit] [_ _ p {:keys [palette figures] :as ctx}]
  (let [pts (unit-points figures (control ctx :a 2) (control ctx :b 1.5))]
    [:g (multiplication-figure palette pts {:rays (a/play p 0 0.4) :unit (a/play p 0.3 0.7) :ca 0 :de 0 :prod 0})
     (label palette (:A pts) "A" (a/play p 0.5 0.8))
     (label palette (:B pts) "B" (a/play p 0.5 0.8))
     (readout palette [2.6 2.9] (a/play p 0.6 0.9) ["BA = 1, chosen at will"])]))

(defmethod scene/draw [:descartes/unit :multiply] [_ _ p {:keys [palette figures] :as ctx}]
  (let [a (control ctx :a 2) b (control ctx :b 1.5) {:keys [B E] :as pts} (unit-points figures a b)]
    [:g (multiplication-figure palette pts {:rays 1 :unit 1 :ca (a/play p 0 0.3) :de (a/play p 0.3 0.6) :prod (a/play p 0.6 0.9)})
     (unit-letters palette pts 1)
     (readout palette [2.6 2.9] (a/play p 0.7 0.95)
              [(str "BD = " (fmt a 2) "   BC = " (fmt b 2)) (str "BE = " (fmt (length B E) 2))])]))

(defmethod scene/draw [:descartes/unit :similar] [_ _ p {:keys [palette figures] :as ctx}]
  (let [a (control ctx :a 2) b (control ctx :b 1.5) {:keys [B A C D E] :as pts} (unit-points figures a b)
        grow (a/there-and-back-with-pause p)
        small (map #(plane/lerp-point % (plane/lerp-point B % a) grow) [B A C])]
    [:g (multiplication-figure palette pts {:rays 1 :unit 1 :ca 1 :de 1 :prod 1})
     (svg/polygon [B A C] {:fill (:construction palette) :opacity 0.25})
     (svg/polygon small {:stroke (:found palette) :width 0.02 :fill (:found palette) :opacity 0.15})
     (svg/polygon [B D E] {:stroke (:construction palette) :width 0.01})
     (unit-letters palette pts 1)
     (readout palette [2.6 2.9] (a/play p 0 0.2)
              ["BAC scaled by BD is BDE" "BE : BD = BC : BA" (str (fmt (length B E) 2) " : " (fmt a 2) " = " (fmt b 2) " : 1")])]))

(defmethod scene/draw [:descartes/unit :divide] [_ _ p {:keys [palette figures] :as ctx}]
  (let [a (control ctx :a 2) b (control ctx :b 1.5) {:keys [B C E] :as pts} (unit-points figures a b)
        f (a/play p 0.1 0.6)]
    [:g (multiplication-figure palette pts {:rays 1 :unit 1 :ca f :de 1 :prod 1})
     (unit-letters palette pts 1)
     (svg/segment B C {:stroke (:found palette) :width (* 0.04 (a/play p 0.5 0.8))})
     (readout palette [2.6 2.9] (a/play p 0.5 0.8) [(str "BE / BD = " (fmt (length B E) 2) " / " (fmt a 2)) (str "      = BC = " (fmt (length B C) 2))])]))

(def ^:private arc-states (mapv (fn [i] [(* m/pi (/ i 60))]) (range 61)))

(defn- root-figure [palette figures a {:keys [line circle perp letters]}]
  (let [{:keys [F G H K I] :as pts} (root-points figures a)
        arc (figure/points (:root-arc figures) [a] arc-states)]
    [:g (stroke [F G] (:found palette) 0.05 line)
     (stroke [G H] (:ink palette) 0.025 line)
     (stroke arc (:construction palette) 0.018 circle)
     (stroke [G I] (:found palette) 0.04 perp)
     (dot K (:muted palette) circle) (dot I (:found palette) perp)
     (into [:g] (map (fn [k] (label palette (pts k) (name k) letters)) root-order))]))

(defmethod scene/draw [:descartes/unit :root] [_ _ p {:keys [palette figures] :as ctx}]
  (let [a (control ctx :a 2) {:keys [I]} (root-points figures a)]
    [:g (svg/layer {:transform "translate(0.5,0)"}
                   (root-figure palette figures a {:line (a/play p 0 0.25) :circle (a/play p 0.25 0.6)
                                                   :perp (a/play p 0.6 0.85) :letters (a/play p 0.1 0.4)}))
     (readout palette [2.6 2.9] (a/play p 0.75 1) [(str "GH = " (fmt a 2)) (str "GI = " (fmt (second I) 3))])]))

(defmethod scene/draw [:descartes/unit :mean] [_ _ p {:keys [palette figures] :as ctx}]
  (let [a (control ctx :a 2) s (second (:I (root-points figures a)))]
    [:g (svg/layer {:transform "translate(0.5,0)"}
                   (root-figure palette figures a {:line 1 :circle 1 :perp 1 :letters 1})
                   (svg/polygon [[0 0] [0 s] [(- s) s] [(- s) 0]]
                                {:fill (:found palette) :opacity (* 0.3 (a/play p 0 0.4))})
                   (svg/polygon [[0 0] [a 0] [a -1] [0 -1]]
                                {:fill (:construction palette) :opacity (* 0.3 (a/play p 0.3 0.7))}))
     (readout palette [2.6 2.9] (a/play p 0.5 0.8)
              ["GI x GI = FG x GH" (str (fmt (* s s) 3) " = 1 x " (fmt a 2)) "a line times a line is a line"])]))

;; ---------------------------------------------------------------------------
;; :descartes/pappus. The locus is the :pappus kernel; the feet of the
;; oblique lines, the two products and the discriminant come from the
;; :pappus-foot, :pappus-products and :pappus-discriminant kernels.

(def ^:private sweep-states (mapv (fn [i] [(+ -1.5 (* 3.0 (/ i 240)))]) (range 241)))

(defn- locus-points
  "The swept points of the locus at lam, split where the chord runs off to
   infinity (a hyperbola's two branches)."
  [figures lam]
  (let [ps (figure/points (:pappus figures) [lam] sweep-states)
        ok? (fn [[x y]] (and (< (m/abs x) 30) (< (m/abs y) 30)))]
    (->> (partition-by ok? ps) (filter (comp ok? first)) (filter #(> (count %) 1)))))

(defn- discriminant-at [figures lam]
  (first (first (figure/points (:pappus-discriminant figures) [] [[lam]]))))

(defn- conic-type [figures lam]
  (let [d (discriminant-at figures lam)]
    (cond (< (m/abs d) 1e-6) "parabola" (neg? d) "ellipse" :else "hyperbola")))

(defn- given-lines [palette lines f]
  (into [:g] (for [[i {:keys [p u]}] (map-indexed vector lines)
                   :let [a (plane/translate p (mapv #(* -0.6 %) u)) b (plane/translate p (mapv #(* 1.6 %) u))]]
               (stroke [a b] (:ink palette) 0.016 (nth f i)))))

(defn- obliques
  "From the point C, the four lines at their given angles, and the feet."
  [palette figures [x y :as C] f]
  (into [:g]
        (for [[i F] (map-indexed vector (figure/points (:pappus-foot figures) [] (mapv (fn [i] [x y i]) (range 4))))
              :let [colour (if (even? i) (:found palette) (:construction palette))]]
          [:g (stroke [C F] colour 0.028 f) (dot F colour f)])))

(defn- ratio-readout [palette figures C lam f]
  (let [[p13 p24] (first (figure/points (:pappus-products figures) [] [C]))]
    (readout palette [4.4 4.2] f
             [(str "d1 d3 = " (fmt p13 3))
              (str "d2 d4 = " (fmt p24 3))
              (str "ratio = " (fmt (if (zero? p24) 0 (/ p13 p24)) 3))
              (str "lam   = " (fmt lam 3))])))

(defn- locus-curves [palette segs f]
  (into [:g] (map #(stroke (vec %) (:found palette) 0.03 f) segs)))

(defn- running-point
  "A point of the locus at phase u in [0 1], walking along its first branch."
  [segs u]
  (let [seg (vec (first segs))]
    (when (seq seg) (nth seg (min (dec (count seg)) (int (* u (dec (count seg)))))))))

(defmethod scene/draw [:descartes/pappus :lines] [_ _ p {:keys [palette data]}]
  [:g (given-lines palette (:lines data) (a/lagged p 4 0.4 0 0.8))
   (readout palette [4.4 4.2] (a/play p 0.6 0.9) ["four lines given" "in position"])])

(defmethod scene/draw [:descartes/pappus :history] [_ _ p {:keys [palette data]}]
  [:g (given-lines palette (:lines data) [1 1 1 1])
   (readout palette [4.4 4.2] (a/play p 0 0.4) ["Pappus: the point lies" "on a conic section;" "he does not say which."])])

(defmethod scene/draw [:descartes/pappus :distances] [_ _ p {:keys [palette data figures] :as ctx}]
  (let [lam (control ctx :lam 1) segs (locus-points figures lam)
        C (running-point segs (+ 0.3 (* 0.4 (a/there-and-back p))))]
    [:g (given-lines palette (:lines data) [1 1 1 1])
     (when C [:g (obliques palette figures C (a/play p 0 0.3)) (dot C (:ink palette) 1) (label palette C "C" 1)])
     (readout palette [4.4 4.2] (a/play p 0.2 0.5) ["each line from C" "at its given angle:" "x and y, once"])]))

(defmethod scene/draw [:descartes/pappus :equation] [_ _ p {:keys [palette data figures] :as ctx}]
  (let [lam (control ctx :lam 1) segs (locus-points figures lam)
        C (running-point segs (+ 0.2 (* 0.6 (a/smooth p))))]
    [:g (given-lines palette (:lines data) [1 1 1 1])
     (locus-curves palette segs (a/play p 0 1 a/linear))
     (when C [:g (obliques palette figures C 1) (dot C (:ink palette) 1)
              (ratio-readout palette figures C lam 1)])]))

(defmethod scene/draw [:descartes/pappus :locus] [_ _ p {:keys [palette data figures] :as ctx}]
  (let [lam (control ctx :lam 1) segs (locus-points figures lam)
        C (running-point segs (a/linear p))]
    [:g (given-lines palette (:lines data) [1 1 1 1])
     (locus-curves palette segs 1)
     (when C [:g (obliques palette figures C 1) (dot C (:ink palette) 1) (label palette C "C" 1)
              (ratio-readout palette figures C lam 1)])]))

(defmethod scene/draw [:descartes/pappus :classify] [_ _ p {:keys [palette data figures] :as ctx}]
  (let [lam (control ctx :lam 1) segs (locus-points figures lam)]
    [:g (given-lines palette (:lines data) [1 1 1 1])
     (locus-curves palette segs 1)
     (readout palette [4.4 4.2] (a/play p 0 0.3)
              [(str "lam = " (fmt lam 3)) (str "an " (conic-type figures lam))
               "B^2 - 4AC: 0 parabola," "< 0 ellipse, > 0 hyperbola"])]))

(defmethod scene/draw [:descartes/pappus :ratio] [_ _ p {:keys [palette data figures]}]
  (let [[lo hi] (:parabola-at data)
        ;; the ratio itself sweeps through both parabolas
        lam (+ (* 1.3 lo) (* p (- (* 0.5 hi) (* 1.3 lo))))
        segs (locus-points figures lam)]
    [:g (given-lines palette (:lines data) [1 1 1 1])
     (locus-curves palette segs 1)
     (readout palette [4.4 4.2] 1 [(str "lam = " (fmt lam 4)) (str "an " (conic-type figures lam))])]))

;; ---------------------------------------------------------------------------
;; :descartes/normal: the parabola y^2 = x (the :parabola kernel), C = (1, 1),
;; P = (v, 0); the circle is the :normal kernel, its second meeting E the
;; :normal-meet kernel.

(def ^:private parabola-states (mapv (fn [i] [(- (* 3.2 (/ i 80)) 1.6)]) (range 81)))
(def ^:private circle-states (mapv (fn [i] [(* 2 m/pi (/ i 120))]) (range 121)))

(defn- normal-frame [palette figures v f]
  (let [[[x2 y2] E'] (figure/points (:normal-meet figures) [v 1 1] [[1] [-1]])]
    [:g (stroke [[-1.5 0] [3 0]] (:muted palette) 0.01 1)
     (stroke (figure/points (:parabola figures) [1] parabola-states) (:found palette) 0.03 1)
     (stroke (figure/points (:normal figures) [v 1 1] circle-states) (:construction palette) 0.018 f)
     (stroke [[v 0] [1 1]] (:construction palette) 0.016 f)
     (dot [1 1] (:ink palette) 1) (label palette [1 1] "C" 1)
     (dot [v 0] (:construction palette) 1) (label palette [v 0] "P" 1 {:dy -0.22})
     (when (and (pos? x2) (> (m/abs (- x2 1)) 0.02))
       [:g (dot [x2 y2] (:construction palette) f) (dot E' (:construction palette) f)
        (label palette [x2 y2] "E" f)])
     (readout palette [-1.4 1.65] 1
              [(str "PA = v = " (fmt v 3)) (str "roots: x = 1 and x = " (fmt x2 3))])]))

(defmethod scene/draw [:descartes/normal :curve] [_ _ p {:keys [palette figures]}]
  [:g (stroke [[-1.5 0] [3 0]] (:muted palette) 0.01 (a/play p 0 0.3))
   (stroke (figure/points (:parabola figures) [1] parabola-states) (:found palette) 0.03 (a/play p 0.1 0.7))
   (dot [1 1] (:ink palette) (a/play p 0.6 0.8)) (label palette [1 1] "C" (a/play p 0.6 0.9))])

(defmethod scene/draw [:descartes/normal :cutting] [_ _ p {:keys [palette figures]}]
  (normal-frame palette figures 2.2 (a/play p 0 0.5)))

(defmethod scene/draw [:descartes/normal :two-roots] [_ _ p {:keys [palette figures]}]
  (normal-frame palette figures (+ 2.2 (* -0.4 (a/there-and-back p))) 1))

(defmethod scene/draw [:descartes/normal :touching] [_ _ p {:keys [palette figures]}]
  (normal-frame palette figures (+ 2.2 (* -0.7 (a/smooth p))) 1))

(defmethod scene/draw [:descartes/normal :normal] [_ _ p {:keys [palette figures]}]
  [:g (normal-frame palette figures 1.5 1)
   (stroke [[-1 0] [1 1] [2 1.5]] (:ink palette) 0.012 (a/play p 0.2 0.6))
   (readout palette [-1.4 1.05] (a/play p 0.4 0.8) ["double root at v = x0 + r/2" "PC meets the tangent" "at right angles"])])

;; ---------------------------------------------------------------------------
;; :descartes/signs: the quartic is the :quartic kernel.

(def ^:private q-states (mapv (fn [i] [(- (* 11 (/ i 220)) 6)]) (range 221)))

(defn- terms [palette f highlight]
  (let [ts ["+x^4" "-4x^3" "-19x^2" "+106x" "-120"]]
    (into [:g] (map-indexed (fn [i t]
                              (svg/layer (a/fade f)
                                         (write palette [(+ -5.6 (* 1.55 i)) 2.6] t
                                                {:mono? true :anchor "start" :size 0.12
                                                 :colour (if (highlight i) (:found palette) (:ink palette))})))
                            ts))))

(defn- axes-and-curve [palette figures f]
  [:g (stroke [[-6.2 0] [5.4 0]] (:muted palette) 0.012 1)
   (stroke (vec (filter (fn [[_ y]] (< (m/abs y) 2.2)) (figure/points (:quartic figures) [0.01] q-states)))
           (:found palette) 0.03 f)])

(defmethod scene/draw [:descartes/signs :build] [_ _ p {:keys [palette figures]}]
  [:g (axes-and-curve palette figures (a/play p 0 0.8))
   (into [:g] (map (fn [r f] [:g (dot [r 0] (:construction palette) f) (label palette [r 0] (str r) f {:dy -0.25})])
                   [2 3 4] (a/lagged p 3 0.5 0.3 0.9)))])

(defmethod scene/draw [:descartes/signs :false-root] [_ _ p {:keys [palette figures]}]
  [:g (axes-and-curve palette figures 1)
   (into [:g] (map (fn [r] [:g (dot [r 0] (:construction palette) 1) (label palette [r 0] (str r) 1 {:dy -0.25})]) [2 3 4]))
   (dot [-5 0] (:ink palette) (a/play p 0.3 0.6)) (label palette [-5 0] "-5 (false)" (a/play p 0.3 0.6) {:dy -0.25})
   (terms palette (a/play p 0.5 0.9) #{})])

(defmethod scene/draw [:descartes/signs :rule] [_ _ p {:keys [palette figures]}]
  [:g (axes-and-curve palette figures 1) (terms palette 1 #{})
   (readout palette [-5.6 2.15] (a/play p 0.2 0.6) ["+ - : a change     -> a true root" "- - : a permanence -> a false root"])])

(defmethod scene/draw [:descartes/signs :count] [_ _ p {:keys [palette figures]}]
  (let [k (int (* 4.999 (a/linear p)))
        pairs [[0 1 "change"] [1 2 "permanence"] [2 3 "change"] [3 4 "change"]]
        shown (take k pairs)]
    [:g (axes-and-curve palette figures 1)
     (terms palette 1 (into #{} (mapcat (fn [[i j]] [i j]) (take-last 1 shown))))
     (into [:g] (map-indexed (fn [n [i _ kind]]
                               (write palette [(+ -5.0 (* 1.55 i)) (- 2.25 (* 0 n))] kind
                                      {:size 0.07 :mono? true :anchor "start"
                                       :colour (if (= kind "change") (:construction palette) (:muted palette))}))
                             shown))
     (readout palette [-5.6 1.8] (a/play p 0.8 1) ["3 changes: 2, 3, 4" "1 permanence: 5"])]))

;; ---------------------------------------------------------------------------
;; :descartes/plane: figs. 3 and 4. The points are the :plane-root and
;; :chord-root kernels at one-hot weights; the circles the :circle kernel.
;; ctx :controls :a and :b are the sliders.

(def ^:private plane-order [:L :N :M :O :P])
(def ^:private chord-order [:M :N :L :Q :R])

(defn- plane-points [figures a b]
  (zipmap plane-order (figure/points (:plane-root figures) [a b] (mapv #(one-hot % 5) (range 5)))))

(defn- chord-points [figures a b]
  (zipmap chord-order (figure/points (:chord-root figures) [a b] (mapv #(one-hot % 5) (range 5)))))

(defn- circle-points [figures [cx cy] rad]
  (figure/points (:circle figures) [cx cy rad] circle-states))

(defn- fig-3 [palette figures a b {:keys [tri circle produce subtract letters]}]
  (let [{:keys [L N M O P] :as pts} (plane-points figures a b)]
    [:g (stroke [L N M L] (:ink palette) 0.02 tri)
     (stroke (circle-points figures N (/ a 2)) (:construction palette) 0.016 circle)
     (stroke [M O] (:found palette) 0.04 produce)
     (stroke [M P] (:construction palette) 0.04 subtract)
     (into [:g] (map (fn [k] (label palette (pts k) (name k) letters)) plane-order))]))

(defn- plane-readout [palette f lines]
  (readout palette [2.4 3.0] f lines))

(defmethod scene/draw [:descartes/plane :plane] [_ _ p {:keys [palette]}]
  (readout palette [-0.8 2.0] (a/play p 0 0.5)
           ["z^2 = az + b^2" "y^2 = -ay + b^2" "z^2 = az - b^2" "a square, its root, a known line"]))

(defmethod scene/draw [:descartes/plane :triangle] [_ _ p {:keys [palette figures] :as ctx}]
  (let [a (control ctx :a 2) b (control ctx :b 1)]
    [:g (fig-3 palette figures a b {:tri (a/play p 0 0.5) :circle 0 :produce 0 :subtract 0 :letters (a/play p 0.3 0.7)})
     (plane-readout palette (a/play p 0.5 0.9) [(str "LM = b = " (fmt b 2)) (str "LN = a/2 = " (fmt (/ a 2) 2))])]))

(defmethod scene/draw [:descartes/plane :produce] [_ _ p {:keys [palette figures] :as ctx}]
  (let [a (control ctx :a 2) b (control ctx :b 1) {:keys [O M]} (plane-points figures a b) z (length O M)]
    [:g (fig-3 palette figures a b {:tri 1 :circle (a/play p 0 0.4) :produce (a/play p 0.4 0.8) :subtract 0 :letters 1})
     (plane-readout palette (a/play p 0.7 1)
                    [(str "OM = z = " (fmt z 4)) (str "z^2 - az = " (fmt (- (* z z) (* a z)) 4)) (str "b^2 = " (fmt (* b b) 4))])]))

(defmethod scene/draw [:descartes/plane :subtract] [_ _ p {:keys [palette figures] :as ctx}]
  (let [a (control ctx :a 2) b (control ctx :b 1) {:keys [P M]} (plane-points figures a b) y (length P M)]
    [:g (fig-3 palette figures a b {:tri 1 :circle 1 :produce 0.4 :subtract (a/play p 0 0.5) :letters 1})
     (plane-readout palette (a/play p 0.5 0.9)
                    [(str "PM = y = " (fmt y 4)) (str "y^2 + ay = " (fmt (+ (* y y) (* a y)) 4)) (str "b^2 = " (fmt (* b b) 4))])]))

(defmethod scene/draw [:descartes/plane :square] [_ _ p {:keys [palette figures] :as ctx}]
  (let [a (control ctx :a 2) b (control ctx :b 1) {:keys [P M]} (plane-points figures a b)]
    [:g (fig-3 palette figures a b {:tri 1 :circle 1 :produce 0 :subtract 1 :letters 1})
     (plane-readout palette (a/play p 0.2 0.6)
                    ["x^4 = -ax^2 + b^2:" (str "PM = x^2 = " (fmt (length P M) 4)) "x: the root of PM by the unit"])]))

(defn- fig-4 [palette figures a b f]
  ;; past b = a/2 the kernel's root is not real: Q and R are drawn only below it
  (let [meets? (<= b (/ a 2))
        {:keys [M N Q R]} (chord-points figures a (min b (/ a 2)))
        L [0 b]
        pts {:M M :N N :L L}]
    [:g (stroke [M N] (:ink palette) 0.02 1) (stroke [M L] (:ink palette) 0.02 1)
     (stroke [(plane/translate L [-0.5 0]) (plane/translate L [(+ a 0.5) 0])] (:muted palette) 0.014 1)
     (stroke (circle-points figures N (/ a 2)) (:construction palette) 0.016 f)
     (when meets?
       [:g (stroke [L Q] (:found palette) 0.045 f) (stroke [L R] (:construction palette) 0.025 f)
        (label palette Q "Q" f) (label palette R "R" f)])
     (into [:g] (map (fn [k] (label palette (pts k) (name k) 1)) [:M :N :L]))]))

(defmethod scene/draw [:descartes/plane :chord] [_ _ p {:keys [palette figures] :as ctx}]
  (let [a (+ 1 (control ctx :a 2)) b (min (control ctx :b 1) (* 0.45 a))
        {:keys [L Q R]} (chord-points figures a b)]
    [:g (fig-4 palette figures a b (a/play p 0 0.6))
     (plane-readout palette (a/play p 0.6 1)
                    [(str "a = " (fmt a 2) "  b = " (fmt b 2)) (str "LQ = " (fmt (length L Q) 4) "  LR = " (fmt (length L R) 4))
                     "both solve z^2 = az - b^2"])]))

(defmethod scene/draw [:descartes/plane :none] [_ _ p {:keys [palette figures]}]
  (let [a 2.4 b (+ 0.6 (* 0.9 (a/there-and-back p)))]
    [:g (fig-4 palette figures a b 1)
     (plane-readout palette 1 [(str "b = " (fmt b 2) "  a/2 = " (fmt (/ a 2) 2))
                               (if (<= b (/ a 2)) "the circle meets LQR: two roots" "the circle misses LQR: no root")])]))

;; ---------------------------------------------------------------------------
;; :descartes/ellipse: Descartes' own example of Book II, the ellipse
;; x^2 = ry - (r/q)y^2 with r = 2, q = 4, C at MA = e = 1. Drawn with the
;; diameter MA horizontal: the kernels return [y x]. P runs along the
;; diameter; at v = e - (r/q)e + r/2 = 3/2 the second root E joins C.

(def ^:private ell [2 4 1])
(def ^:private ell-v* 1.5)

(defn- ellipse-frame [palette figures v f]
  (let [[r q e0] ell
        ;; :ellipse-meet returns [y x^2]; the root is taken here, E drawn on both sides
        meet (fn [v] (let [[y xx] (first (figure/points (:ellipse-meet figures) [r q v e0] [[1]]))]
                       [y #?(:clj (Math/sqrt (max 0 xx)) :cljs (js/Math.sqrt (max 0 xx)))]))
        C (meet ell-v*)
        [y2 x2] (meet v)
        E' [y2 (- x2)]]
    [:g (stroke [[-0.3 0] [4.4 0]] (:muted palette) 0.01 1)
     (stroke (figure/points (:ellipse figures) [r q] circle-states) (:found palette) 0.03 1)
     (stroke (figure/points (:ellipse-circle figures) [r q v e0] circle-states) (:construction palette) 0.018 f)
     (stroke [[v 0] C] (:construction palette) 0.016 f)
     (stroke [C [(first C) 0]] (:muted palette) 0.012 1)
     (dot C (:ink palette) 1) (label palette C "C" 1)
     (dot [0 0] (:ink palette) 1) (label palette [0 0] "A" 1 {:dy -0.25})
     (dot [(first C) 0] (:ink palette) 1) (label palette [(first C) 0] "M" 1 {:dy -0.25})
     (dot [v 0] (:construction palette) 1) (label palette [v 0] "P" 1 {:dy -0.25})
     (when (and (> (m/abs (- y2 e0)) 0.02) (< 0 y2 4))
       [:g (dot [y2 x2] (:construction palette) f) (dot E' (:construction palette) f) (label palette [y2 x2] "E" f)])
     (readout palette [2.6 1.85] 1 [(str "PA = v = " (fmt v 3)) (str "MA = " (fmt e0 3) ",  QA = " (fmt y2 3))])]))

(defmethod scene/draw [:descartes/ellipse :names] [_ _ p {:keys [palette figures]}]
  [:g (ellipse-frame palette figures 2.2 (a/play p 0.2 0.7))
   (readout palette [-0.3 -1.6] (a/play p 0.4 0.8) ["CM = x, MA = y, PC = s, PA = v"])])

(defmethod scene/draw [:descartes/ellipse :circle] [_ _ p {:keys [palette figures]}]
  [:g (ellipse-frame palette figures 2.2 1)
   (readout palette [-0.3 -1.6] (a/play p 0.2 0.6) ["s^2 = x^2 + v^2 - 2vy + y^2"])])

(defmethod scene/draw [:descartes/ellipse :ellipse] [_ _ p {:keys [palette figures]}]
  [:g (ellipse-frame palette figures 2.2 1)
   (readout palette [-0.3 -1.6] (a/play p 0.2 0.6) ["Apollonius I.13: x^2 = ry - (r/q)y^2,  r = 2, q = 4"])])

(defmethod scene/draw [:descartes/ellipse :eliminate] [_ _ p {:keys [palette figures]}]
  [:g (ellipse-frame palette figures 2.2 1)
   (readout palette [-0.3 -1.6] (a/play p 0.2 0.6) ["y^2 + (qry - 2qvy + qv^2 - qs^2)/(q - r) = 0"])])

(defmethod scene/draw [:descartes/ellipse :cut] [_ _ p {:keys [palette figures]}]
  (ellipse-frame palette figures (+ 2.2 (* -0.3 (a/there-and-back p))) 1))

(defmethod scene/draw [:descartes/ellipse :touch] [_ _ p {:keys [palette figures]}]
  (ellipse-frame palette figures (+ 2.2 (* (- ell-v* 2.2) (a/smooth p))) 1))

(defmethod scene/draw [:descartes/ellipse :compare] [_ _ p {:keys [palette figures]}]
  [:g (ellipse-frame palette figures ell-v* 1)
   (readout palette [-0.3 -1.6] (a/play p 0.2 0.6) ["compare with y^2 - 2ey + e^2"])])

(defmethod scene/draw [:descartes/ellipse :solve] [_ _ p {:keys [palette figures]}]
  [:g (ellipse-frame palette figures ell-v* 1)
   (readout palette [-0.3 -1.6] (a/play p 0.2 0.6) ["v = e - (r/q)e + r/2 = 1 - 1/2 + 1 = 3/2"])])

;; ---------------------------------------------------------------------------
;; :descartes/construction: fig. 27. The parabola is the :parabola-iii
;; kernel, the circle the :construction-circle kernel at [p q r]; ctx :data
;; carries the two cases (:trisection, :means) with their roots from raster.

(def ^:private z-states (mapv (fn [i] [(- (* 4.4 (/ i 120)) 2.2)]) (range 121)))

(defn- construction-frame [palette figures {:keys [p q r roots]} {:keys [par centre circle feet]}]
  (let [E [(/ q -2) (/ (+ p 1) 2)]
        D [0 (/ (+ p 1) 2)]]
    [:g (stroke [[0 -0.3] [0 4.6]] (:muted palette) 0.01 1)
     (stroke (figure/points (:parabola-iii figures) [] z-states) (:found palette) 0.03 par)
     (dot [0 0] (:ink palette) par) (label palette [0 0] "A" par {:dy -0.25})
     (dot [0 0.5] (:ink palette) centre) (label palette [0 0.5] "C" centre)
     (dot D (:ink palette) centre) (label palette D "D" centre)
     (stroke [D E] (:construction palette) 0.016 centre)
     (dot E (:construction palette) centre) (label palette E "E" centre)
     (stroke (figure/points (:construction-circle figures) [p q r] circle-states) (:construction palette) 0.018 circle)
     (into [:g] (for [z roots :let [G [z (* z z)] K [0 (* z z)]]]
                  [:g (stroke [G K] (if (pos? z) (:found palette) (:muted palette)) 0.03 feet)
                   (dot G (:ink palette) feet)]))]))

(defmethod scene/draw [:descartes/construction :reduce] [_ _ p {:keys [palette]}]
  (readout palette [-2.0 4.2] (a/play p 0 0.5) ["z^4 = * p z^2 * q z * r" "the second term taken away;" "the unit a = 1"]))

(defmethod scene/draw [:descartes/construction :parabola] [_ _ p {:keys [palette figures data]}]
  (construction-frame palette figures (:trisection data) {:par (a/play p 0 0.7) :centre 0 :circle 0 :feet 0}))

(defmethod scene/draw [:descartes/construction :centre] [_ _ p {:keys [palette figures data]}]
  [:g (construction-frame palette figures (:trisection data) {:par 1 :centre (a/play p 0 0.6) :circle 0 :feet 0})
   (readout palette [-2.0 4.2] (a/play p 0.4 0.8) ["AC = 1/2, CD = p/2, DE = q/2"])])

(defmethod scene/draw [:descartes/construction :radius] [_ _ p {:keys [palette figures data]}]
  [:g (construction-frame palette figures (:trisection data) {:par 1 :centre 1 :circle (a/play p 0 0.7) :feet 0})
   (readout palette [-2.0 4.2] (a/play p 0.4 0.8) ["radius^2 = AE^2 + AH^2, AH = sqrt r" "(here r = 0: the circle through A)"])])

(defmethod scene/draw [:descartes/construction :roots] [_ _ p {:keys [palette figures data]}]
  [:g (construction-frame palette figures (:trisection data) {:par 1 :centre 1 :circle 1 :feet (a/play p 0 0.6)})
   (readout palette [-2.0 4.2] (a/play p 0.4 0.8) ["each meeting G: GK a root" "true on one side, false on the other"])])

(defn- demo [palette figures data p lines]
  [:g (construction-frame palette figures (:trisection data) {:par 1 :centre 1 :circle 1 :feet 1})
   (readout palette [-2.0 4.2] (a/play p 0.2 0.6) lines)])

(defmethod scene/draw [:descartes/construction :parabola-sq] [_ _ p {:keys [palette figures data]}]
  (demo palette figures data p ["GK = z, AK = z^2" "DK = EM = z^2 - p/2 - 1/2"]))

(defmethod scene/draw [:descartes/construction :pythagoras] [_ _ p {:keys [palette figures data]}]
  (demo palette figures data p ["GM = z + q/2" "GE^2 = EM^2 + GM^2"]))

(defmethod scene/draw [:descartes/construction :equate] [_ _ p {:keys [palette figures data]}]
  (demo palette figures data p ["GE^2 = AE^2 + r" "z^4 = p z^2 - q z + r"]))

(defmethod scene/draw [:descartes/construction :means] [_ _ p {:keys [palette figures data]}]
  (let [{:keys [roots] :as c} (:means data) z (first (filter pos? roots))]
    [:g (construction-frame palette figures c {:par 1 :centre (a/play p 0 0.3) :circle (a/play p 0.2 0.6) :feet (a/play p 0.5 0.8)})
     (readout palette [-2.0 4.2] (a/play p 0.6 0.9)
              ["z^3 = a^2 q, a = 1, q = 2" (str "FL = z = " (fmt z 6)) (str "LA = z^2 = " (fmt (* z z) 6))
               "1 : FL = FL : LA = LA : 2"])]))

(defmethod scene/draw [:descartes/construction :trisect] [_ _ p {:keys [palette figures data]}]
  (let [{:keys [q arc]} (:trisection data)]
    [:g (construction-frame palette figures (:trisection data) {:par 1 :centre 1 :circle 1 :feet 0})
     (readout palette [-2.0 4.2] (a/play p 0.2 0.6)
              [(str "arc = " (fmt arc 3) ", NP = q = " (fmt q 5)) "z^3 = 3z - q"])]))

(defmethod scene/draw [:descartes/construction :three] [_ _ p {:keys [palette figures data]}]
  (let [{:keys [gk GK FL third-chord rest-chord]} (:trisection data)]
    [:g (construction-frame palette figures (:trisection data) {:par 1 :centre 1 :circle 1 :feet (a/play p 0 0.5)})
     (readout palette [-2.0 4.2] (a/play p 0.4 0.8)
              [(str "gk = " (fmt gk 6) "  NQ = " (fmt third-chord 6))
               (str "GK = " (fmt GK 6) "  NV = " (fmt rest-chord 6))
               (str "FL = " (fmt FL 6) "  -(QN+NV) = " (fmt (- (+ third-chord rest-chord)) 6))])]))
