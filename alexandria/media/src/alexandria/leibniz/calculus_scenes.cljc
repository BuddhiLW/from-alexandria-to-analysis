(ns alexandria.leibniz.calculus-scenes
  "The drawings of Leibniz's calculus (alexandria.leibniz.calculus), one
   method per stage (alexandria.medium.scene/draw), in Manim's idiom
   (alexandria.medium.anim).

     :leibniz/transmutation          the characteristic triangle sliding
                                     along the circle and shrinking, the
                                     sectors from O carried onto the strips
                                     of the figure of z, the series
     :leibniz/arithmetical-quadrature the partial sums closing on pi/4
     :leibniz/nova-methodus          dx, dv on a curve; the rectangle xv
                                     growing by x dv + v dx

   ctx :figures carries the core's kernels :triangle, :tangent and
   :transmute; everything else is drawn in plain numbers."
  (:require [alexandria.medium.anim :as a]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.math :as m]
            [alexandria.medium.plane :as plane]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            #?(:cljs [alexandria.medium.player :as player])))

#?(:cljs (def render "Clerk's render-fn for these scenes." player/render))

;; ---------------------------------------------------------------------------
;; Leibniz's circle, diameter 2 on the x-axis from O, in plain numbers

(defn- cy [x] (m/sqrt (max 0 (- (* 2 x) (* x x)))))
(defn- cz [x] (/ x (cy x)))
(def ^:private arc (mapv (fn [i] (let [x (/ i 60)] [x (cy x)])) (range 121)))

(defn- fmt [x digits]
  #?(:clj (format (str "%." digits "f") (double x))
     :cljs (.toFixed (js/Number x) digits)))

(defn- write
  ([palette at s] (write palette at s {}))
  ([palette at s opts]
   (svg/text at s (merge {:colour (:ink palette) :size 0.12} opts))))

(defn- label [palette at s f & [{:keys [dx dy] :or {dx 0.05 dy 0.06}}]]
  (svg/layer (a/fade f [0 -0.1])
             (write palette (plane/translate at [dx dy]) s {:italic? true :anchor "start"})))

(defn- stroke [ps colour width f]
  (when (pos? f) (svg/polyline ps {:stroke colour :width width :attrs (a/create f)})))

(defn- dot [at colour f] (when (pos? f) (svg/circle at (* 0.03 f) {:fill colour})))

(defn- axes [palette f]
  [:g (stroke [[-0.2 0] [2.2 0]] (:muted palette) 0.008 f)
   (stroke [[0 -0.15] [0 2.3]] (:muted palette) 0.008 f)
   (label palette [0 0] "O" f {:dx -0.12 :dy -0.14})])

(defn- readout [palette f at lines]
  (svg/layer (a/fade f [0.1 0])
             (into [:g] (map-indexed (fn [i s] (write palette (plane/translate at [0 (* -0.17 i)]) s
                                                      {:anchor "start" :size 0.085 :mono? true}))
                                     lines))))

(defn- circle-figure [palette f]
  [:g (axes palette f) (stroke arc (:found palette) 0.022 f)])

(defn- char-triangle
  "The characteristic triangle at x0 with side h, through the :triangle
   kernel, and the tangent at P through :tangent."
  [palette figures x0 h f]
  (let [[P R Q] (figure/points (:triangle figures) [x0 h] [[0 0] [1 0] [1 1]])
        [Z T] (figure/points (:tangent figures) [x0] [[0] [(+ x0 h)]])
        blue (:construction palette)]
    [:g
     (svg/layer (a/fade f) (svg/polygon [P R Q] {:fill blue :opacity 0.25 :stroke blue :width 0.01}))
     (stroke [Z T] (:muted palette) 0.008 f)
     (stroke [P Q] (:found palette) 0.016 f)
     (dot P (:ink palette) f) (dot Q (:ink palette) f)
     (label palette P "P" f {:dx -0.13 :dy 0.04}) (label palette Q "Q" f)
     (svg/layer (a/fade f) (write palette (plane/lerp-point P R 0.5) "dx" {:size 0.07 :anchor "middle"}))
     (svg/layer (a/fade f) (write palette (plane/translate (plane/lerp-point R Q 0.5) [0.08 0]) "dy" {:size 0.07 :anchor "start"}))]))

(def ^:private tid :leibniz/transmutation)

(defmethod scene/draw [tid :curve] [_ _ p {:keys [palette]}]
  (let [f (a/play p 0 0.6) x 0.4]
    [:g (circle-figure palette f)
     (dot [x (cy x)] (:ink palette) (a/play p 0.5 0.7))
     (label palette [x (cy x)] "P" (a/play p 0.5 0.7) {:dx -0.13 :dy 0.04})
     (readout palette (a/play p 0.6 0.9) [1.25 2.2] ["y = sqrt(2x - x^2)" "the circle of diameter 2"])]))

(defmethod scene/draw [tid :triangle] [_ _ p {:keys [palette figures]}]
  (let [x0 (+ 0.25 (* 0.5 (a/there-and-back p)))]
    [:g (circle-figure palette 1)
     (char-triangle palette figures x0 0.45 (a/play p 0 0.25))
     (readout palette (a/play p 0.2 0.4) [1.25 2.2] ["dx, dy and the chord PQ:" "the characteristic triangle"
                                                       (str "x = " (fmt x0 2))])]))

(defmethod scene/draw [tid :shrink] [_ _ p {:keys [palette figures]}]
  (let [h (* 0.6 (m/pow 0.04 (a/play p 0.05 0.9 a/linear)))]
    [:g (circle-figure palette 1)
     (char-triangle palette figures 0.4 h 1)
     (readout palette 1 [1.25 2.2] [(str "dx = " (fmt h 3))
                                     (str "dy/dx = " (fmt (/ (- (cy (+ 0.4 h)) (cy 0.4)) h) 3))
                                     (str "slope at P = " (fmt (/ (- 1 0.4) (cy 0.4)) 3))
                                     "the chord falls on the tangent"])]))

(defn- sector [palette x0 h f]
  (svg/layer (a/fade f)
             (svg/polygon [[0 0] [x0 (cy x0)] [(+ x0 h) (cy (+ x0 h))]]
                          {:fill (:found palette) :opacity 0.3 :stroke (:found palette) :width 0.008})))

(defmethod scene/draw [tid :sector] [_ _ p {:keys [palette figures]}]
  (let [x0 (+ 0.3 (* 0.9 (a/play p 0.1 0.9 a/smooth))) h 0.12]
    [:g (circle-figure palette 1)
     (sector palette x0 h (a/play p 0 0.15))
     (char-triangle palette figures x0 h 1)
     (readout palette (a/play p 0 0.2) [1.25 2.2] ["triangles OPQ, all meeting at O," "not Cavalieri's parallel strips"])]))

(defmethod scene/draw [tid :z] [_ _ p {:keys [palette figures]}]
  (let [x0 0.6 h 0.15 z (cz x0)
        [Z] (figure/points (:tangent figures) [x0] [[0]])
        f (a/play p 0.1 0.4)]
    [:g (circle-figure palette 1)
     (sector palette x0 h 1)
     (char-triangle palette figures x0 h 1)
     (stroke [Z [x0 (cy x0)]] (:construction palette) 0.012 f)
     (dot Z (:construction palette) f) (label palette Z "z" f {:dx -0.14 :dy 0})
     (svg/layer (a/fade (a/play p 0.45 0.7))
                (svg/polygon (plane/rect x0 (+ x0 h) 0 z) {:fill (:construction palette) :opacity 0.35}))
     (readout palette (a/play p 0.5 0.8) [1.25 2.2] [(str "z = y - x dy/dx = " (fmt z 3))
                                                    "triangle OPQ = z dx / 2"
                                                    "the strip z dx is twice it"])]))

(def ^:private n-strips 8)
(def ^:private strip-states (vec (for [i (range n-strips) c (range 4)] [i c])))

(defmethod scene/draw [tid :strips] [_ _ p {:keys [palette figures]}]
  (let [s (a/play p 0.1 0.8)
        quads (partition 4 (figure/points (:transmute figures) [n-strips s] strip-states))]
    (into [:g (circle-figure palette 1)]
          (concat
           (map-indexed (fn [i q] (svg/polygon q {:fill (if (even? i) (:found palette) (:construction palette))
                                                  :opacity 0.45 :stroke (:ink palette) :width 0.005}))
                        quads)
           [(readout palette (a/play p 0.75 0.95) [1.25 2.2] ["each sector, doubled," "becomes a strip under z:"
                                                              "the figure of z has twice" "the area swept from O"])]))))

(def ^:private z-curve (mapv (fn [i] (let [x (/ (inc i) 50)] [x (cz x)])) (range 50)))

(defmethod scene/draw [tid :rational] [_ _ p {:keys [palette]}]
  (let [f (a/play p 0 0.5)]
    [:g (axes palette 1) (stroke arc (:muted palette) 0.01 1)
     (svg/layer (a/fade f) (svg/polygon (concat [[0 0]] z-curve [[1 0]]) {:fill (:construction palette) :opacity 0.25}))
     (stroke z-curve (:construction palette) 0.02 f)
     (readout palette (a/play p 0.4 0.7) [1.25 2.2] ["the figure of z, x from 0 to 1:" "z^2 = x / (2 - x)" "x = 2 z^2 / (1 + z^2)"
                                                    "a rational figure"])]))

;; data for the series scenes: {:sums [S_1 .. S_40] :quarter-pi q}, the exact
;; partial sums and pi/4 (raster quadrature) computed on the JVM by the view,
;; so the browser does no arithmetic of its own. Without data (JVM scene
;; tests) the scene falls back to the alternating sum and m/pi.

(defn- partial-sum [data n]
  (or (get-in data [:sums (dec n)])
      (reduce + (map (fn [k] (/ (if (even? k) 1.0 -1.0) (inc (* 2 k)))) (range n)))))

(defn- quarter-pi [data] (or (:quarter-pi data) (/ m/pi 4)))

(defn- sums-figure [palette data n-shown f]
  (let [x0 0.1 w 0.16 scale 2.2 q (quarter-pi data)]
    (into [:g (stroke [[0 (* scale q)] [2.3 (* scale q)]] (:construction palette) 0.01 f)
           (svg/layer (a/fade f) (write palette [2.32 (* scale q)] "pi/4" {:anchor "start" :size 0.09}))]
          (for [n (range 1 (inc n-shown))
                :let [s (partial-sum data n) x (+ x0 (* (dec n) w))]]
            [:g (svg/polygon (plane/rect x (+ x (* 0.8 w)) 0 (* scale s))
                             {:fill (if (odd? n) (:found palette) (:muted palette)) :opacity 0.6})
             (write palette [(+ x (* 0.4 w)) -0.12] (str n) {:size 0.07})]))))

(defmethod scene/draw [tid :series] [_ _ p {:keys [palette data]}]
  (let [n (max 1 (int (m/floor (* 13 (a/play p 0 0.85 a/linear)))))]
    [:g (sums-figure palette data n 1)
     (readout palette 1 [0.2 2.5] [(str "S_" n " = " (fmt (partial-sum data n) 5)) (str "pi/4 = " (fmt (quarter-pi data) 5))])]))

(def ^:private qid :leibniz/arithmetical-quadrature)

(defmethod scene/draw [qid :sums] [_ _ p ctx]
  (scene/draw tid :series p ctx))

(defmethod scene/draw [qid :bound] [_ _ p {:keys [palette data]}]
  (let [n (+ 1 (int (m/floor (* 40 (a/play p 0 0.9 a/linear)))))
        err (m/abs (- (quarter-pi data) (partial-sum data n)))]
    [:g (sums-figure palette data 13 1)
     (readout palette 1 [0.2 2.5] [(str "n = " n)
                                   (str "|pi/4 - S_n| = " (fmt err 6))
                                   (str "1/(2n+1)     = " (fmt (/ 1.0 (inc (* 2 n))) 6))])]))


;; ---------------------------------------------------------------------------
;; Nova Methodus

(def ^:private nid :leibniz/nova-methodus)

(defn- bump [x] (+ 0.4 (* 1.2 x) (* -0.5 x x)))
(def ^:private bump-pts (mapv (fn [i] (let [x (/ i 20)] [x (bump x)])) (range 49)))

(defn- tangent-at [x u] [u (+ (bump x) (* (- 1.2 x) (- u x)))])

(defmethod scene/draw [nid :definition] [_ _ p {:keys [palette]}]
  (let [x (+ 0.3 (* 0.6 (a/there-and-back p))) v (bump x) slope (- 1.2 x)
        B [(- x (/ v slope)) 0] dx 0.5]
    [:g (axes palette 1) (stroke bump-pts (:found palette) 0.02 1)
     (stroke [B (tangent-at x (+ x dx))] (:construction palette) 0.01 1)
     (stroke [[x 0] [x v]] (:ink palette) 0.01 1)
     (stroke [[x v] [(+ x dx) v] (tangent-at x (+ x dx))] (:ink palette) 0.012 1)
     (label palette [x v] "V" 1 {:dx -0.12 :dy 0.05}) (label palette B "B" 1 {:dx -0.03 :dy -0.15})
     (readout palette 1 [1.0 2.2] ["dx: a line taken at will" "dv : dx = v : VB"
                                   (str "dv = " (fmt (* slope dx) 3))])]))

(defn- rectangle-growth
  "The rectangle x by v and its growth: the strips x dv, v dx and the
   corner dx dv, which Leibniz drops."
  [palette x v dx dv f]
  [:g (svg/polygon (plane/rect 0 x 0 v) {:fill (:muted palette) :opacity 0.25 :stroke (:ink palette) :width 0.01})
   (svg/layer (a/fade f)
              (svg/polygon (plane/rect 0 x v (+ v dv)) {:fill (:found palette) :opacity 0.5})
              (svg/polygon (plane/rect x (+ x dx) 0 v) {:fill (:construction palette) :opacity 0.5})
              (svg/polygon (plane/rect x (+ x dx) v (+ v dv)) {:fill (:ink palette) :opacity 0.6}))
   (write palette [(/ x 2) (/ v 2)] "xv" {:size 0.12})
   (svg/layer (a/fade f) (write palette [(/ x 2) (+ v (/ dv 2) -0.03)] "x dv" {:size 0.08})
              (write palette [(+ x (/ dx 2)) (/ v 2)] "v dx" {:size 0.08}))])

(defmethod scene/draw [nid :product] [_ _ p {:keys [palette]}]
  (let [d (* 0.35 (- 1 (a/play p 0.45 0.95)))]
    [:g (rectangle-growth palette 1.2 0.9 d (* 0.8 d) (a/play p 0 0.3))
     (readout palette (a/play p 0.1 0.3) [1.7 2.0] ["d(xv) = x dv + v dx + dx dv"
                                                   (str "dx dv = " (fmt (* 0.8 d d) 4))
                                                   "a difference of differences:"
                                                   "it vanishes beside the rest"])]))

(defmethod scene/draw [nid :constant] [_ _ p {:keys [palette]}]
  (let [d (* 0.4 (a/play p 0.1 0.6))]
    [:g (rectangle-growth palette 1.0 0.6 d 0 1)
     (readout palette 1 [1.7 2.0] ["a constant: da = 0" "d(ax) = a dx"])]))

(defmethod scene/draw [nid :addition] [_ _ p {:keys [palette]}]
  (let [f (a/play p 0 0.5)
        segs [[0 0.5] [0.5 1.1] [1.1 1.4]]]
    (into [:g (readout palette 1 [0.1 2.0] ["d(z - y + w + x) = dz - dy + dw + dx" "each part grows by its own d"])]
          (map-indexed (fn [i [a b]] (let [g (* f 0.12 (inc i))]
                                       (svg/polygon (plane/rect (+ a (* i 0.12 f)) (+ b g) 0.5 0.8)
                                                    {:fill (if (even? i) (:found palette) (:construction palette)) :opacity 0.6})))
                       segs))))

(defmethod scene/draw [nid :quotient] [_ _ p {:keys [palette]}]
  (let [d (* 0.3 (- 1 (a/play p 0.3 0.9)))]
    [:g (rectangle-growth palette 1.2 0.9 d (* 0.8 d) 1)
     (readout palette 1 [1.7 2.0] ["z = v/y, so v = zy" "dv = z dy + y dz" "dz = (y dv - v dy)/yy"])]))

(defmethod scene/draw [nid :power] [_ _ p {:keys [palette]}]
  (let [d (* 0.3 (- 1 (a/play p 0.3 0.9)))]
    [:g (rectangle-growth palette 1.0 1.0 d d 1)
     (readout palette 1 [1.7 2.0] ["x x grows by x dx + x dx" "d(x^2) = 2x dx" "d(x^a) = a x^(a-1) dx"])]))

(defmethod scene/draw [nid :maximum] [_ _ p {:keys [palette]}]
  (let [x (+ 0.4 (* 1.6 (a/play p 0 0.8))) slope (- 1.2 x)]
    [:g (axes palette 1) (stroke bump-pts (:found palette) 0.02 1)
     (stroke [(tangent-at x (- x 0.5)) (tangent-at x (+ x 0.5))] (:construction palette) 0.012 1)
     (dot [x (bump x)] (:ink palette) 1)
     (readout palette 1 [1.0 2.2] [(str "dv/dx = " (fmt slope 3))
                                   (if (< (m/abs slope) 0.05) "dv = 0: the ordinate is greatest" "")])]))
