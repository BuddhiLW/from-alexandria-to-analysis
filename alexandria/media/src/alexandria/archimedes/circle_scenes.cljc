(ns alexandria.archimedes.circle-scenes
  "The drawings of Measurement of a Circle, one scene per proposition, one
   method per stage of its proof (alexandria.medium.scene/draw). The moving
   points come from the figures of alexandria.archimedes.circle through
   ctx :figures; everything else is drawn from the stage and its progress.

     :archimedes/circle-1  K, the polygons that exhaust the circle, the
                           triangles of a polygon set in a row
     :archimedes/circle-2  K rearranged against the square on the diameter
     :archimedes/circle-3  the bisections, Archimedes' numbers, the bounds"
  (:require [alexandria.medium.figure :as figure]
            [alexandria.medium.plane :as plane]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            [alexandria.medium.timeline :as tl]
            [alexandria.medium.math :as m]
            #?(:cljs [alexandria.medium.player :as player])))

#?(:cljs (def render
           "Clerk's render-fn for these scenes."
           player/render))

(def ^:private pi m/pi)

;; ---------------------------------------------------------------------------
;; States: which points of a figure a frame asks for (memoized, so a kernel
;; adapter reuses its input buffer)

(def ^:private indices (memoize (fn [m] (mapv vector (range m)))))
(def ^:private fractions (memoize (fn [m] (mapv #(vector (/ % (dec m))) (range m)))))
(def ^:private triangle-vertices
  (memoize (fn [n] (vec (for [k (range n) j (range 3)] [k j])))))

;; ---------------------------------------------------------------------------
;; Shared drawing

(def ^:private text-scale
  "Text size relative to the figure sizes written below."
  1.45)

(defn- write
  "Text in the palette's ink unless :colour is given; :size in figure units
   before text-scale."
  ([palette at s] (write palette at s {}))
  ([palette at s opts]
   (svg/text at s (-> (merge {:colour (:ink palette) :size 0.11} opts)
                      (update :size * text-scale)))))

(defn- fmt [x digits]
  #?(:clj (format (str "%." digits "f") (double x))
     :cljs (.toFixed (js/Number x) digits)))

(defn- inset
  "A magnified view of the square of half-width w about centre (y up),
   drawn into the box [x y width height] (svg units, y up at the top edge).
   Strokes are thinned by the magnification so lines stay lines."
  [[bx by bw bh] [cx cy] w palette & children]
  (let [class (str "alexandria-inset-" (long (* w 10000)) "-" (long (* bw 100)))]
    [:g
     [:style (str "." class " * { stroke-width: " (/ (* 2 w) bw 100) "px !important; }")]
     (into [:svg {:class class :x bx :y (- by) :width bw :height bh
                  :view-box (svg/view-box [(- cx w) (+ cx w)] [(- cy w) (+ cy w)])}]
           children)
     [:rect {:x bx :y (- by) :width bw :height bh :fill "none"
             :stroke (:muted palette) :stroke-width 0.01}]]))

;; ===========================================================================
;; Proposition 1: the circle equals K

(def ^:private x0 1.7)
(def ^:private y0 -1.25)
(def ^:private circumference (* 2 pi))
(def ^:private inscribed-ns [4 8 16])
(def ^:private circumscribed-ns [4 8])

(defn- k-triangle [] [[x0 y0] [(+ x0 circumference) y0] [x0 (+ y0 1)]])

(defn- ground [palette]
  (svg/segment [-1.3 y0] [8.2 y0] {:stroke (:muted palette) :width 0.008 :dash "0.05 0.04"}))

(defn- draw-k [palette opacity]
  (svg/group opacity
             (svg/polygon (k-triangle) {:stroke (:ink palette) :fill (:muted palette) :opacity 0.25})
             (write palette [(+ x0 1.3) (+ y0 0.28)] "K")
             (write palette [(- x0 0.08) (+ y0 0.5)] "r" {:anchor "end" :italic? true})
             (write palette [(+ x0 (/ circumference 2)) (- y0 0.14)] "C" {:italic? true})))

(defn- polygon-in
  "The inscribed polygon while 4 sides double to 32, progress p."
  [figures p]
  (let [[i t] (tl/stages p (count inscribed-ns))
        n (inscribed-ns i)]
    (figure/points (:inscribed figures) [n t 1] (indices (* 2 n)))))

(defn- polygon-out
  "The circumscribed polygon while 4 sides double to 16, progress p."
  [figures p]
  (let [[i t] (tl/stages p (count circumscribed-ns))
        n (circumscribed-ns i)]
    (figure/points (:circumscribed figures) [n t 1] (indices (* 4 n)))))

(defn- leftover-bar
  "The bar of what lies between polygon and circle, against the excess."
  [palette label leftover excess]
  (let [scale 3.0 bx 1.9 by 1.05]
    [:g
     (write palette [bx (+ by 0.17)] label {:anchor "start" :size 0.1})
     (svg/polygon (plane/rect bx (+ bx (min 6.2 (* scale leftover))) (- by 0.08) by)
                  {:fill (:found palette) :opacity 0.7})
     (svg/segment [(+ bx (* scale excess)) (- by 0.16)] [(+ bx (* scale excess)) (+ by 0.06)]
                  {:stroke (:ink palette) :width 0.015})
     (write palette [(+ bx (* scale excess)) (- by 0.28)] "ε" {:italic? true})
     (write palette [(+ bx 6.2) (- by 0.28)]
            (str (fmt leftover 4) (if (< leftover excess) " < " " > ") "ε = " excess)
            {:anchor "end" :size 0.1 :mono? true})]))

(defn- exhaustion
  "Circle and polygon with what lies between them shaded."
  [palette poly inside?]
  (let [{:keys [background found construction ink]} palette]
    (if inside?
      [:g
       (svg/circle [0 0] 1 {:fill found :opacity 0.45})
       (svg/polygon poly {:fill background :stroke found :width 0.012})
       (svg/circle [0 0] 1 {:stroke ink})]
      [:g
       (svg/polygon poly {:fill construction :opacity 0.45 :stroke construction :width 0.012})
       (svg/circle [0 0] 1 {:fill background :stroke ink})])))

(defn- row
  "The triangles of the n-gon (out 0 inscribed, 1 circumscribed) falling
   into a row (first 55% of p) and leaning onto one apex (last 40%)."
  [figures n out p]
  (let [t1 (tl/window p 0 0.55)
        t2 (tl/window p 0.6 1)
        ps (figure/points (:sectors figures) [n t1 t2 1 out x0 y0] (triangle-vertices n))]
    (partition 3 ps)))

(defn- row-figure [palette figures n out p]
  (let [colour (if (zero? out) (:found palette) (:construction palette))
        tris (row figures n out p)
        ;; side and apothem of the n-gon from the :polygon kernel
        [[side apothem]] (figure/points (:polygon figures) [n 1 out] [[0]])
        perimeter (* n side)
        shapes (for [tri tris] (svg/polygon tri {:fill colour :opacity 0.35 :stroke colour :width 0.006}))
        k (svg/polygon (k-triangle) {:stroke (:ink palette) :width 0.008})
        cmp (fn [a b] (if (< a b) " < " " > "))]
    (into [:g (svg/circle [0 0] 1 {:stroke (:muted palette)}) (ground palette)]
          (concat shapes
                  [(draw-k palette 1)
                   (svg/group (tl/window p 0.85 1)
                              (inset [2.0 1.3 1.3 1.3] [x0 (+ y0 1)] 0.03 palette k (into [:g] shapes))
                              (write palette [2.65 -0.08] "at the apex" {:size 0.09})
                              (inset [6.55 1.3 1.3 1.3] [(+ x0 circumference) y0] 0.06 palette k (into [:g] shapes))
                              (write palette [7.2 -0.08] "at the end of the base" {:size 0.09})
                              (write palette [3.6 1.2] (str "height " (fmt apothem 4) (cmp apothem 1) "r = 1")
                                     {:anchor "start" :size 0.1 :mono? true})
                              (write palette [3.6 1.0] (str "base " (fmt perimeter 4) (cmp perimeter circumference)
                                                            "C = " (fmt circumference 4))
                                     {:anchor "start" :size 0.1 :mono? true}))]))))

(defmethod scene/draw [:archimedes/circle-1 :unroll] [_ _ p {:keys [figures palette]}]
  (let [rolled (map #(plane/translate % [x0 y0])
                    (figure/points (:unroll figures) [p 1] (fractions 241)))]
    [:g
     (ground palette)
     (svg/circle [0 0] 1 {:stroke (:ink palette)})
     (svg/segment [0 0] [1 0] {:stroke (:muted palette)})
     (write palette [0.5 0.06] "r" {:italic? true})
     (svg/polyline rolled {:stroke (:ink palette) :width 0.016})
     (draw-k palette (tl/window p 0.8 1))]))

(defn- supposition [palette excess sign]
  (let [side (m/sqrt excess)]
    [:g
     (svg/polygon (plane/rect -1.25 (+ -1.25 side) 1.1 (+ 1.1 side)) {:fill (:found palette) :opacity 0.8})
     (write palette [(+ -1.15 side) 1.15] (str "ε: the supposed " (if (pos? sign) "excess" "defect"))
            {:anchor "start" :size 0.1})
     (write palette [4.9 0.55] (str "suppose  circle = K " (if (pos? sign) "+" "−") " ε") {:size 0.16})]))

(defmethod scene/draw [:archimedes/circle-1 :suppose-greater] [_ _ p {:keys [palette data]}]
  [:g (ground palette) (svg/circle [0 0] 1 {:stroke (:ink palette) :fill (:found palette) :opacity 0.25})
   (draw-k palette 1) (svg/group (tl/window p 0 0.5) (supposition palette (:excess data) 1))])

(defmethod scene/draw [:archimedes/circle-1 :bisect-in] [_ _ p {:keys [figures palette data]}]
  (let [poly (polygon-in figures p)
        leftover (- pi (plane/area poly))]
    [:g (ground palette) (draw-k palette 0.6)
     (exhaustion palette poly true)
     (leftover-bar palette (str "segments left by the " (count (distinct (map (fn [[x y]] [(fmt x 3) (fmt y 3)]) poly))) "-gon")
                   leftover (:excess data))]))

(defmethod scene/draw [:archimedes/circle-1 :polygon-greater] [_ _ _ {:keys [figures palette]}]
  [:g (ground palette) (draw-k palette 0.6)
   (exhaustion palette (polygon-in figures 1) true)
   (write palette [4.9 0.75] "polygon = circle − segments" {:size 0.14})
   (write palette [4.9 0.5] "> circle − ε = K" {:size 0.14 :colour (:found palette)})])

(defmethod scene/draw [:archimedes/circle-1 :row-in] [_ _ p {:keys [figures palette]}]
  (row-figure palette figures 32 0 p))

(defmethod scene/draw [:archimedes/circle-1 :contradiction-in] [_ _ p {:keys [figures palette]}]
  [:g (row-figure palette figures 32 0 1)
   (svg/group (tl/window p 0 0.4)
              (write palette [4.9 0.45] "polygon > K  and  polygon < K" {:size 0.15 :colour (:found palette)})
              (write palette [4.9 0.22] "impossible: the circle is not greater than K" {:size 0.11}))])

(defmethod scene/draw [:archimedes/circle-1 :suppose-less] [_ _ p {:keys [palette data]}]
  [:g (ground palette) (svg/circle [0 0] 1 {:stroke (:ink palette) :fill (:construction palette) :opacity 0.25})
   (draw-k palette 1) (svg/group (tl/window p 0 0.5) (supposition palette (:excess data) -1))])

(defmethod scene/draw [:archimedes/circle-1 :bisect-out] [_ _ p {:keys [figures palette data]}]
  (let [poly (polygon-out figures p)
        leftover (- (plane/area poly) pi)]
    [:g (ground palette) (draw-k palette 0.6)
     (exhaustion palette poly false)
     (leftover-bar palette "the polygon's excess over the circle" leftover (:excess data))]))

(defmethod scene/draw [:archimedes/circle-1 :polygon-less] [_ _ _ {:keys [figures palette]}]
  [:g (ground palette) (draw-k palette 0.6)
   (exhaustion palette (polygon-out figures 1) false)
   (write palette [4.9 0.75] "polygon < circle + ε = K" {:size 0.14 :colour (:construction palette)})])

(defmethod scene/draw [:archimedes/circle-1 :row-out] [_ _ p {:keys [figures palette]}]
  (row-figure palette figures 16 1 p))

(defmethod scene/draw [:archimedes/circle-1 :qed] [_ _ p {:keys [palette]}]
  [:g (ground palette)
   (svg/circle [0 0] 1 {:stroke (:ink palette) :fill (:found palette) :opacity (* 0.5 (tl/window p 0 0.5))})
   (svg/polygon (k-triangle) {:stroke (:ink palette) :fill (:found palette) :opacity (* 0.5 (tl/window p 0 0.5))})
   (draw-k palette 1)
   (write palette [4.9 0.6] "circle = K = ½ · r · C" {:size 0.18})])

;; ===========================================================================
;; Proposition 2: 11 to 14

(def ^:private stretched (* 44 (/ 1 7.0)))
(def ^:private strip (/ 2 14.0))
(def ^:private piece (/ stretched 4))
(def ^:private square-x 2.4)

(defn- reference-circle [palette]
  [:g (svg/circle [5.5 1.35] 1 {:stroke (:ink palette) :fill (:found palette) :opacity 0.2})
   (write palette [5.5 1.3] "circle" {:size 0.12})])

(defn- triangle-of [b] [[0 0] [b 0] [0 1]])

(defmethod scene/draw [:archimedes/circle-2 :triangle] [_ _ p {:keys [palette]}]
  [:g (reference-circle palette)
   (svg/polygon (triangle-of circumference) {:stroke (:ink palette) :fill (:found palette) :opacity (* 0.5 (tl/window p 0 0.5))})
   (write palette [1.4 0.25] "K = circle  (Prop. 1)" {:size 0.13})])

(defmethod scene/draw [:archimedes/circle-2 :stretch] [_ _ p {:keys [palette]}]
  (let [b (plane/lerp circumference stretched (tl/window p 0 0.7))]
    [:g (reference-circle palette)
     (svg/polygon (triangle-of b) {:stroke (:ink palette) :fill (:found palette) :opacity 0.5})
     (write palette [1.6 0.6] (str "C = " (fmt circumference 4) "  <  (22/7) · 2r = " (fmt stretched 4))
            {:anchor "start" :size 0.11 :mono? true})]))

(defmethod scene/draw [:archimedes/circle-2 :fold] [_ _ p {:keys [palette]}]
  (let [half (/ stretched 2) slide (* half (tl/window p 0 0.35)) flip-y (- 1 (* 2 (tl/window p 0.35 0.65))) flip-x (- 1 (* 2 (tl/window p 0.65 1))) centre-x (+ half (/ half 2)) place (fn [[x y]] (let [x (+ x slide) y (+ 0.5 (* flip-y (- y 0.5)))] [(+ centre-x (* flip-x (- x centre-x))) y])) m [half 0.5] top (map place [[0 0.5] [0 1] m])] [:g (reference-circle palette)
     (svg/polygon [[0 0] [stretched 0] m [0 0.5]] {:stroke (:ink palette) :fill (:found palette) :opacity 0.5})
     (svg/polygon top {:stroke (:ink palette) :fill (:construction palette) :opacity 0.6})]))

(defmethod scene/draw [:archimedes/circle-2 :stack] [_ _ p {:keys [palette]}]
  (into [:g (reference-circle palette)]
        (for [i (range 4)
              :let [t (tl/window p (* i 0.2) (+ 0.4 (* i 0.2)))
                    [dx dy] (plane/lerp-point [(* i piece) 0] [0 (* i 0.5)] t)]]
          (svg/polygon (plane/rect dx (+ dx piece) dy (+ dy 0.5))
                       {:stroke (:ink palette) :fill (:found palette) :opacity 0.5 :width 0.008}))))

(defmethod scene/draw [:archimedes/circle-2 :compare] [_ _ p {:keys [palette]}]
  (let [shift (* square-x (tl/window p 0 0.6))]
    (into [:g (reference-circle palette)
           (svg/polygon (plane/rect square-x (+ square-x 2) 0 2) {:stroke (:ink palette) :width 0.015})]
          (concat
           (for [k (range 1 14)]
             (svg/segment [(+ square-x (* k strip)) 0] [(+ square-x (* k strip)) 2]
                          {:stroke (:muted palette) :width 0.006}))
           [(svg/polygon (plane/rect shift (+ shift piece) 0 2) {:fill (:found palette) :opacity 0.5})
            (svg/group (tl/window p 0.7 1)
                       (write palette [(+ square-x 1) -0.22] "11 strips of 14: circle : square on d = 11 : 14"
                              {:size 0.12}))]))))

;; ===========================================================================
;; Proposition 3: Archimedes' numbers

(def ^:private letters-up ["C" "D" "E" "F" "G"])
(def ^:private letters-down ["B" "d" "e" "f" "g"])
(def ^:private base-y 0.15)
(def ^:private span 2.6)
(def ^:private height 1.8)
(def ^:private thumb [4.15 1.0])
(def ^:private thumb-r 0.8)

(defn- number-line
  "A number line from x 0 to 8.3 at height y over the interval [lo hi], with
   marks [[value label colour]] and bound ticks [[value colour]]."
  [palette y [lo hi] marks ticks]
  (let [at (fn [v] (* 8.3 (/ (- v lo) (- hi lo))))]
    (into [:g (svg/segment [0 y] [8.3 y] {:stroke (:muted palette) :width 0.01})
           (write palette [0 (- y 0.17)] (fmt lo 3) {:size 0.08 :mono? true})
           (write palette [8.3 (- y 0.17)] (fmt hi 3) {:size 0.08 :mono? true})]
          (concat
           (for [[v colour] ticks :when (<= lo v hi)]
             (svg/segment [(at v) (- y 0.07)] [(at v) (+ y 0.07)] {:stroke colour :width 0.02}))
           (for [[v s colour] marks]
             [:g (svg/segment [(at v) (- y 0.1)] [(at v) (+ y 0.1)] {:stroke colour :width 0.008})
              (write palette [(at v) (+ y 0.14)] s {:size 0.09 :colour colour})])))))

(defn- bounds-lines [palette {:keys [lo hi]} ticks]
  (let [marks [[pi "π" (:ink palette)] [lo "3 10/71" (:muted palette)] [hi "3 1/7" (:muted palette)]]]
    [:g (number-line palette -0.55 [3.0 3.5] (map (fn [[v _ c]] [v "" c]) marks) ticks)
     (number-line palette -1.15 [3.139 3.145] marks ticks)]))

(defn- table [palette rows heading colour]
  (into [:g (write palette [5.1 1.95] heading {:anchor "start" :size 0.085 :mono? true})]
        (map-indexed (fn [i {:keys [sides ratio bound]}]
                       (write palette [5.1 (- 1.75 (* 0.19 i))]
                              (str sides ": " ratio "  " bound)
                              {:anchor "start" :size 0.08 :mono? true :colour colour}))
                     rows)))

(defn- thumbnail [palette figures kind sides angle]
  (let [n (/ sides 2)
        [cx cy] thumb
        poly (if (= kind :upper)
               (figure/points (:circumscribed figures) [n 1 thumb-r] (indices (* 4 n)))
               (figure/points (:inscribed figures) [n 1 thumb-r] (indices (* 2 n))))
        colour (if (= kind :upper) (:construction palette) (:found palette))
        corner (if (= kind :upper)
                 [[cx cy] [(+ cx thumb-r) cy] [(+ cx thumb-r) (+ cy (* thumb-r (m/tan angle)))]]
                 [[(- cx thumb-r) cy] [(+ cx thumb-r) cy]
                  (plane/translate (plane/rotate-about [thumb-r 0] [0 0] (- pi (* 2 angle))) thumb)])]
    [:g (svg/circle thumb thumb-r {:stroke (:ink palette)})
     (svg/polygon (map #(plane/translate % thumb) poly) {:stroke colour :width 0.008})
     (svg/polygon corner {:fill colour :opacity 0.45})]))

(defn- step-angle
  "The angle bisected at step k (the one before bisection; k = 0: none)."
  [k]
  (/ (/ pi 6) (m/pow 2 (max 0 (dec k)))))

(defmethod scene/draw [:archimedes/circle-3 :upper] [_ [_ k] p {:keys [figures palette data]}]
  (let [theta (step-angle k)
        bisect? (pos? k)
        m (/ height (* span (m/tan theta)))
        o [0 base-y] a [span base-y] c [span (+ base-y height)]
        d [span (+ base-y (* m span (m/tan (/ theta 2))))]
        shown (min (if (or (not bisect?) (> p 0.7)) (inc k) k) 5)
        rows (take shown (:upper data))
        ink (:ink palette) blue (:construction palette) gold (:found palette)]
    [:g
     (svg/group (tl/window p 0 0.3)
                (svg/polygon [o a c] {:stroke ink :width 0.012})
                (write palette [-0.08 (- base-y 0.05)] "O" {:anchor "end"})
                (write palette [(+ span 0.08) (- base-y 0.05)] "A" {:anchor "start"})
                (write palette [(+ span 0.08) (+ base-y height)] (letters-up (max 0 (dec k))) {:anchor "start"})
                (when (> m 1.05)
                  (write palette [1.3 2.05] (str "heights × " (fmt m 1)) {:size 0.09 :colour (:muted palette)})))
     (when bisect?
       [:g
        (svg/segment o (plane/lerp-point o d (tl/window p 0.3 0.6)) {:stroke gold :width 0.014})
        (svg/group (tl/window p 0.6 0.75)
                   (write palette [(+ span 0.08) (second d)] (letters-up k) {:anchor "start"})
                   (svg/segment c d {:stroke blue :width 0.03})
                   (svg/segment d a {:stroke gold :width 0.03})
                   (write palette [1.3 -0.12] (str "C" (letters-up k) " : " (letters-up k) "A = CO : OA")
                          {:size 0.11}))])
     (thumbnail palette figures :upper (* 6 (m/pow 2 (dec shown))) (/ theta (if (and bisect? (> p 0.7)) 2 1)))
     (table palette (map (fn [{:keys [sides a-str bound-str]}]
                           {:sides sides :ratio (str a-str " : 153") :bound (str "< " bound-str)})
                         rows)
            "about: OA : AC >, so perimeter/d <" blue)
     (bounds-lines palette data (map (fn [{:keys [bound]}] [bound blue]) rows))]))

(defmethod scene/draw [:archimedes/circle-3 :lower] [_ [_ k] p {:keys [figures palette data]}]
  (let [phi (step-angle k)
        bisect? (pos? k)
        a [0 base-y] b [span base-y]
        on-circle (fn [ang] (let [l (* span (m/cos ang))] [(* l (m/cos ang)) (* l (m/sin ang))]))
        [cx cy] (on-circle phi)
        m (/ height cy)
        lift (fn [[x y]] [x (+ base-y (* m y))])
        c (lift [cx cy])
        d (lift (on-circle (/ phi 2)))
        arc (map #(lift [(+ (/ span 2) (* (/ span 2) (m/cos %))) (* (/ span 2) (m/sin %))])
                 (map #(* pi (/ % 120)) (range 121)))
        shown (min (if (or (not bisect?) (> p 0.7)) (inc k) k) 5)
        rows (take shown (:lower data))
        ink (:ink palette) blue (:construction palette) gold (:found palette)]
    [:g
     (svg/group (tl/window p 0 0.3)
                (svg/polyline arc {:stroke (:muted palette) :width 0.008})
                (svg/polygon [a b c] {:stroke ink :width 0.012})
                (write palette [-0.08 (- base-y 0.05)] "A" {:anchor "end"})
                (write palette [(+ span 0.08) (- base-y 0.05)] "B" {:anchor "start"})
                (write palette [(first c) (+ (second c) 0.08)] "C")
                (when (> m 1.05)
                  (write palette [1.3 2.05] (str "heights × " (fmt m 1)) {:size 0.09 :colour (:muted palette)})))
     (when bisect?
       [:g
        (svg/segment a (plane/lerp-point a d (tl/window p 0.3 0.6)) {:stroke gold :width 0.014})
        (svg/group (tl/window p 0.6 0.75)
                   (write palette [(first d) (+ (second d) 0.08)] (letters-down k))
                   (svg/segment d b {:stroke blue :width 0.03})
                   (write palette [1.3 -0.12] (str "A" (letters-down k) " : " (letters-down k) "B = (AC + AB) : CB")
                          {:size 0.11}))])
     (thumbnail palette figures :lower (* 6 (m/pow 2 (dec shown))) (/ phi (if (and bisect? (> p 0.7)) 2 1)))
     (table palette (map (fn [{:keys [sides h-str b bound-str]}]
                           {:sides sides :ratio (str h-str " : " b) :bound (str "> " bound-str)})
                         rows)
            "in: AB : BC <, so perimeter/d >" gold)
     (bounds-lines palette data (map (fn [{:keys [bound]}] [bound gold]) rows))]))

(defmethod scene/draw [:archimedes/circle-3 :squeeze] [_ _ p {:keys [palette data]}]
  [:g
   (write palette [4.15 1.3] "3 10/71  <  C / d  <  3 1/7" {:size 0.22})
   (svg/group (tl/window p 0 0.5)
              (write palette [4.15 0.85] (str (fmt (:lo data) 6) " < " (fmt (:bound (last (:lower data))) 6)
                                              "  …  " (fmt (:bound (last (:upper data))) 6) " < " (fmt (:hi data) 6))
                     {:size 0.11 :mono? true}))
   (bounds-lines palette data (concat (map (fn [{:keys [bound]}] [bound (:construction palette)]) (:upper data))
                                      (map (fn [{:keys [bound]}] [bound (:found palette)]) (:lower data))))])
