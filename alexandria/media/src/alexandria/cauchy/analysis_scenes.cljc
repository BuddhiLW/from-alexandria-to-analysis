(ns alexandria.cauchy.analysis-scenes
  "The drawings of Cauchy's analysis (alexandria.cauchy.analysis), one
   method per [scene stage] (alexandria.medium.scene/draw), moving in
   Manim's idiom (alexandria.medium.anim).

     :cauchy/continuity   the epsilon-delta game: a band about f(a), a
                          window the player shrinks with it
     :cauchy/series       partial sums of 1/n! and the tail band; the
                          harmonic series escaping every band
     :cauchy/sum-theorem  Abel's series: partial sums (the :abel raster
                          kernel, n continuous) tending to a sawtooth, the
                          jump at pi and the Gibbs overshoot
     :cauchy/integral     Cauchy's sum S as rectangles that refine
     :cauchy/flat         exp(-1/x^2) under a zoom that only flattens it

   ctx :data carries the numbers computed on the JVM (curves as point
   lists, the deltas, the sums); ctx :figures the :abel kernel."
  (:require [alexandria.medium.anim :as a]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.math :as m]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            #?(:cljs [alexandria.medium.player :as player])))

#?(:cljs (def render
           "Clerk's render-fn for these scenes."
           player/render))

;; ---------------------------------------------------------------------------
;; Shared drawing

(defn- fmt [x digits]
  #?(:clj (format (str "%." digits "f") (double x))
     :cljs (.toFixed (js/Number x) digits)))

(defn- write
  ([palette at s] (write palette at s {}))
  ([palette at s opts]
   (svg/text at s (-> (merge {:colour (:ink palette) :size 0.2} opts)))))

(defn- readout
  "Lines of mono text from the point at, faded in by f."
  [palette at f lines]
  (let [[x y] at]
    (svg/layer (a/fade f [0.2 0])
               (into [:g] (map-indexed (fn [i s] (write palette [x (- y (* 0.3 i))] s
                                                        {:anchor "start" :size 0.17 :mono? true}))
                                       lines)))))

(defn- stroke
  "A polyline through ps, created to fraction f."
  [ps colour width f]
  (when (and (pos? f) (seq ps)) (svg/polyline ps {:stroke colour :width width :attrs (a/create f)})))

(defn- axes [palette [x0 x1] [y0 y1]]
  [:g (svg/segment [x0 0] [x1 0] {:stroke (:muted palette) :width 0.012})
   (svg/segment [0 y0] [0 y1] {:stroke (:muted palette) :width 0.012})])

(defn- band
  "The horizontal band y in [lo hi] across [x0 x1]."
  [colour [x0 x1] lo hi opacity]
  (svg/polygon [[x0 lo] [x1 lo] [x1 hi] [x0 hi]] {:fill colour :opacity opacity}))

(defn- window
  "The vertical strip x in [lo hi] across [y0 y1]."
  [colour lo hi [y0 y1] opacity]
  (svg/polygon [[lo y0] [hi y0] [hi y1] [lo y1]] {:fill colour :opacity opacity}))

;; ---------------------------------------------------------------------------
;; Continuity: the epsilon-delta game
;;
;; data: {:curve [[x y] ...] :a a :fa f(a) :rounds [{:eps :delta} ...]
;;        :x [x0 x1] :y [y0 y1] :label "..." :jump {:curve-l :curve-r :a :fa}}

(def ^:private cont :cauchy/continuity)

(defn- delta-at
  "The delta for eps by the rounds' rule, interpolated in eps (log scale)."
  [rounds eps]
  (let [rs (sort-by :eps rounds)
        lo (first rs) hi (last rs)]
    (cond (<= eps (:eps lo)) (* (:delta lo) (/ eps (:eps lo)))
          (>= eps (:eps hi)) (:delta hi)
          :else (let [[r0 r1] (first (filter (fn [[p q]] (<= (:eps p) eps (:eps q))) (partition 2 1 rs)))
                      t (/ (- eps (:eps r0)) (- (:eps r1) (:eps r0)))]
                  (+ (:delta r0) (* t (- (:delta r1) (:delta r0))))))))

(defn- game
  "The graph, the band of half-width eps about f(a), the window of
   half-width delta about a, and the part of the graph inside the window."
  [palette {:keys [curve a fa x y label]} eps delta f-band f-window]
  (let [inside (filter (fn [[px]] (<= (- a delta) px (+ a delta))) curve)]
    [:g (axes palette x y)
     (svg/layer (a/fade f-band) (band (:found palette) x (- fa eps) (+ fa eps) 0.18))
     (svg/layer (a/fade f-window) (window (:construction palette) (- a delta) (+ a delta) y 0.2))
     (stroke curve (:ink palette) 0.03 1)
     (when (pos? f-window)
       (svg/polyline inside {:stroke (:found palette) :width 0.06}))
     (svg/circle [a fa] 0.05 {:fill (:found palette)})
     (write palette [(+ (first x) 0.15) (- (second y) 0.3)] label {:anchor "start" :italic? true})]))

(defmethod scene/draw [cont :graph] [_ _ p {:keys [palette data]}]
  (let [{:keys [curve a fa x y label]} data]
    [:g (axes palette x y)
     (stroke curve (:ink palette) 0.03 (a/play p 0 0.6))
     (svg/layer (a/fade (a/play p 0.5 0.8))
                (svg/circle [a fa] 0.05 {:fill (:found palette)})
                (svg/segment [a 0] [a fa] {:stroke (:muted palette) :width 0.012 :dash "0.05 0.05"})
                (write palette [a -0.3] "a" {:italic? true})
                (write palette [(+ (first x) 0.15) (- (second y) 0.3)] label {:anchor "start" :italic? true}))]))

(defmethod scene/draw [cont :band] [_ _ p {:keys [palette data]}]
  (let [{:keys [rounds x y]} data
        eps (:eps (first rounds))]
    [:g (game palette data eps 0 (a/play p 0 0.5) 0)
     (readout palette [(+ (first x) 0.15) (- (second y) 0.7)] (a/play p 0.4 0.7)
              [(str "eps = " (fmt eps 3)) "the band |f(x) - f(a)| < eps"])]))

(defmethod scene/draw [cont :window] [_ _ p {:keys [palette data]}]
  (let [{:keys [rounds x y]} data
        {:keys [eps delta]} (first rounds)
        d (* delta (+ 2.2 (* -1.2 (a/play p 0.15 0.8))))]
    [:g (game palette data eps d 1 (a/play p 0 0.2))
     (readout palette [(+ (first x) 0.15) (- (second y) 0.7)] (a/play p 0.6 0.85)
              [(str "eps   = " (fmt eps 3)) (str "delta = " (fmt delta 4))
               "|x - a| < delta  =>  inside the band"])]))

(defmethod scene/draw [cont :shrink] [_ _ p {:keys [palette data]}]
  (let [{:keys [rounds x y]} data
        e0 (:eps (apply max-key :eps rounds)) e1 (:eps (apply min-key :eps rounds))
        eps (* e0 (m/pow (/ e1 e0) (a/play p 0.05 0.9 a/smooth)))
        delta (delta-at rounds eps)]
    [:g (game palette data eps delta 1 1)
     (readout palette [(+ (first x) 0.15) (- (second y) 0.7)] 1
              [(str "eps   = " (fmt eps 4)) (str "delta = " (fmt delta 5))
               "every eps has its delta"])]))

(defmethod scene/draw [cont :jump] [_ _ p {:keys [palette data]}]
  (let [{:keys [x y]} data
        {:keys [curve-l curve-r a fa]} (:jump data)
        eps 0.35
        delta (* 0.8 (- 1 (* 0.95 (a/play p 0.15 0.9))))
        right (filter (fn [[px]] (< a px (+ a delta))) curve-r)]
    [:g (axes palette x y)
     (band (:found palette) x (- fa eps) (+ fa eps) 0.18)
     (window (:construction palette) (- a delta) (+ a delta) y 0.2)
     (stroke curve-l (:ink palette) 0.03 1)
     (stroke curve-r (:ink palette) 0.03 1)
     (svg/polyline right {:stroke "#e0605a" :width 0.06})
     (svg/circle [a fa] 0.05 {:fill (:found palette)})
     (readout palette [(+ (first x) 0.15) (- (second y) 0.7)] (a/play p 0 0.2)
              [(str "eps = " (fmt eps 2) ", delta = " (fmt delta 3))
               "right of a: always outside the band"
               "no delta works: not continuous at a"])]))

;; ---------------------------------------------------------------------------
;; Series: the tail band
;;
;; data: {:sums [[n s_n] ...] :limit e :harmonic [[n H_n] ...]}

(def ^:private series :cauchy/series)

(def ^:private sx 0.55)
(defn- at-n [n s y0 sy] [(* sx n) (* sy (- s y0))])

(defn- sum-dots [palette sums upto y0 sy]
  (into [:g] (for [[n s] sums :when (<= n upto)]
               [:g (svg/segment (at-n n y0 y0 sy) (at-n n s y0 sy) {:stroke (:muted palette) :width 0.01})
                (svg/circle (at-n n s y0 sy) 0.055 {:fill (:found palette)})])))

(defmethod scene/draw [series :partials] [_ _ p {:keys [palette data]}]
  (let [{:keys [sums limit]} data
        upto (* (count sums) (a/play p 0 0.85 a/linear))]
    [:g (svg/segment [0 0] [(* sx (inc (count sums))) 0] {:stroke (:muted palette) :width 0.012})
     (sum-dots palette sums upto 1 2.2)
     (svg/layer (a/fade (a/play p 0.8 1))
                (svg/segment (at-n 0 limit 1 2.2) (at-n (inc (count sums)) limit 1 2.2)
                             {:stroke (:construction palette) :width 0.012 :dash "0.08 0.06"})
                (write palette (at-n 6 (+ limit 0.12) 1 2.2) "e = 2.71828..." {:anchor "start"}))
     (readout palette [0.2 -0.35] (a/play p 0.1 0.3)
              ["s_n = 1 + 1/1! + 1/2! + ... + 1/(n-1)!"])]))

(defmethod scene/draw [series :tail-band] [_ _ p {:keys [palette data]}]
  (let [{:keys [sums limit]} data
        n0 (+ 2 (m/floor (* 6 (a/play p 0 0.9 a/linear))))
        s0 (second (nth sums (dec n0)))
        width (- limit s0)
        [x-end] (at-n (inc (count sums)) 0 1 2.2)]
    [:g (svg/segment [0 0] [x-end 0] {:stroke (:muted palette) :width 0.012})
     (svg/polygon [(at-n n0 s0 1 2.2) [x-end (second (at-n 0 s0 1 2.2))]
                   [x-end (second (at-n 0 (+ s0 (* 1.05 width)) 1 2.2))]
                   (at-n n0 (+ s0 (* 1.05 width)) 1 2.2)]
                  {:fill (:construction palette) :opacity 0.25})
     (sum-dots palette sums (count sums) 1 2.2)
     (readout palette [0.2 -0.35] 1
              [(str "from n = " n0 ": every later sum within " (fmt width 6) " of s_n")
               "the tail band narrows as n grows"])]))

(defmethod scene/draw [series :ratio] [_ _ p {:keys [palette data]}]
  (let [{:keys [sums]} data
        k (a/play p 0 0.7 a/linear)
        terms (map-indexed (fn [i [n s]] [n (if (zero? i) s (- s (second (nth sums (dec i)))))]) sums)]
    (into [:g (svg/segment [0 0] [(* sx (inc (count sums))) 0] {:stroke (:muted palette) :width 0.012})
           (readout palette [0.2 -0.35] (a/play p 0.2 0.5)
                    ["u_(n+1) / u_n = 1/(n+1)  ->  0 < 1"
                     "each bar a fraction of the one before"])]
          (for [[n u] terms :when (<= n (* k (count sums) 1.2))]
            (svg/polygon [[(- (* sx n) 0.15) 0] [(+ (* sx n) 0.15) 0]
                          [(+ (* sx n) 0.15) (* 2.2 u)] [(- (* sx n) 0.15) (* 2.2 u)]]
                         {:fill (:found palette) :opacity 0.7})))))

(defmethod scene/draw [series :harmonic] [_ _ p {:keys [palette data]}]
  (let [{:keys [harmonic]} data
        upto (* (count harmonic) (a/play p 0 0.85 a/linear))]
    [:g (svg/segment [0 0] [(* sx (inc (count harmonic))) 0] {:stroke (:muted palette) :width 0.012})
     (into [:g] (for [[n s] harmonic :when (<= n upto)]
                  (svg/circle (at-n n s 0 0.62) 0.05 {:fill "#e0605a"})))
     (into [:g] (for [k (range 1 4) :let [n (m/pow 2 k)] :when (<= (* 2 n) upto)]
                  (svg/segment (at-n n (second (nth harmonic (dec n))) 0 0.62)
                               (at-n (* 2 n) (second (nth harmonic (dec (* 2 n)))) 0 0.62)
                               {:stroke (:construction palette) :width 0.03})))
     (readout palette [0.2 3.1] (a/play p 0.2 0.5)
              ["H_n = 1 + 1/2 + ... + 1/n"
               "1/(n+1) + ... + 1/(2n) >= 1/2, always:"
               "the tail never enters a narrow band"])]))

;; ---------------------------------------------------------------------------
;; The sum theorem: Abel's series
;;
;; data: {:sawtooth [[[x y] ...] ...] (pieces) :xs [x ...] :si-pi n}
;; figures: :abel, params [n], states [[x]]

(def ^:private abel :cauchy/sum-theorem)

(def ^:private xs-abel (mapv (fn [i] [(- (* 2 m/pi (/ i 600)) m/pi)]) (range 601)))
(def ^:private xs-zoom (mapv (fn [i] [(+ 2.2 (* (- m/pi 2.2) (/ i 400)))]) (range 401)))

(defn- abel-frame [palette]
  [:g (svg/segment [(- m/pi) 0] [m/pi 0] {:stroke (:muted palette) :width 0.012})
   (svg/segment [0 -2] [0 2] {:stroke (:muted palette) :width 0.012})
   (svg/segment [m/pi -1.9] [m/pi 1.9] {:stroke (:muted palette) :width 0.008 :dash "0.05 0.05"})
   (write palette [m/pi -2.15] "pi" {:italic? true})
   (write palette [(- m/pi) -2.15] "-pi" {:italic? true})])

(defn- sawtooth-lines [palette data f]
  (into [:g] (for [piece (:sawtooth data)]
               (stroke piece (:construction palette) 0.025 f))))

(defn- partial-curve [figures n]
  (figure/points (:abel figures) [n] xs-abel))

(defmethod scene/draw [abel :terms] [_ _ p {:keys [palette figures]}]
  (let [fs (a/lagged p 3 0.5 0 0.9)]
    (into [:g (abel-frame palette)
           (readout palette [-3.0 2.3] (a/play p 0 0.2)
                    ["sin x,  -sin 2x / 2,  sin 3x / 3: each continuous"])]
          (for [k (range 1 4) :let [f (nth fs (dec k))]]
            (stroke (mapv (fn [[x]] [x (/ (* (if (odd? k) 1 -1) (m/sin (* k x))) k)]) xs-abel)
                    (nth [(:found palette) (:construction palette) (:ink palette)] (dec k)) 0.02 f)))))

(defmethod scene/draw [abel :partial-sums] [_ _ p {:keys [palette data figures]}]
  (let [n (+ 1 (* 23 (a/play p 0.05 0.95 a/rush-into)))]
    [:g (abel-frame palette)
     (sawtooth-lines palette data (a/play p 0 0.2))
     (svg/polyline (partial-curve figures n) {:stroke (:found palette) :width 0.03})
     (readout palette [-3.0 2.3] 1
              [(str "S_n, n = " (fmt n 1) " terms") "the sawtooth x/2 (blue)"])]))

(defmethod scene/draw [abel :jump] [_ _ p {:keys [palette data figures]}]
  (let [f (a/play p 0.1 0.5)]
    [:g (abel-frame palette)
     (sawtooth-lines palette data 1)
     (svg/polyline (partial-curve figures 24) {:stroke (:found palette) :width 0.025 :opacity 0.5})
     (svg/layer (a/fade f)
                (svg/circle [m/pi 0] 0.07 {:fill "#e0605a"})
                (svg/circle [m/pi (/ m/pi 2)] 0.07 {:fill "none" :stroke "#e0605a" :width 0.02})
                (svg/circle [m/pi (- (/ m/pi 2))] 0.07 {:fill "none" :stroke "#e0605a" :width 0.02})
                (svg/segment [m/pi (/ m/pi 2)] [m/pi (- (/ m/pi 2))] {:stroke "#e0605a" :width 0.02}))
     (readout palette [-3.0 2.3] (a/play p 0.4 0.7)
              ["at x = pi every term is 0: the sum is 0"
               "just left: pi/2.  just right: -pi/2"
               "a sum of continuous terms that jumps"])]))

(defmethod scene/draw [abel :gibbs] [_ _ p {:keys [palette data figures]}]
  (let [n (+ 4 (* 44 (a/play p 0 0.9 a/linear)))
        sx-z 3.0 sy-z 2.2
        to-z (fn [[x y]] [(- (* sx-z (- x 2.2)) 1.2) (- (* sy-z (- y 1.0)) 0.4)])
        pts (map to-z (figure/points (:abel figures) [n] xs-zoom))
        peak (reduce max (map second (figure/points (:abel figures) [n] xs-zoom)))]
    [:g (svg/polygon [(to-z [2.2 0.9]) (to-z [m/pi 0.9]) (to-z [m/pi 2.0]) (to-z [2.2 2.0])]
                     {:stroke (:muted palette) :width 0.01})
     (stroke (map to-z [[2.2 1.1] [m/pi (/ m/pi 2)]]) (:construction palette) 0.025 1)
     (svg/segment (to-z [2.2 (:si-pi data)]) (to-z [m/pi (:si-pi data)])
                  {:stroke "#e0605a" :width 0.012 :dash "0.06 0.05"})
     (svg/polyline pts {:stroke (:found palette) :width 0.03})
     (readout palette [-3.0 2.3] 1
              [(str "near pi, n = " (fmt n 0) ": peak " (fmt peak 4))
               (str "pi/2 = 1.5708,  Si(pi) = " (fmt (:si-pi data) 4))
               "the overshoot never goes away"])]))

(defn- terms-of
  "The slider's n (ctx :controls :n, whole terms), or nil without one."
  [ctx]
  (when-let [n (get-in ctx [:controls :n])] (max 1 (m/floor (+ n 0.5)))))

(defmethod scene/draw [abel :uniform] [_ _ p {:keys [palette data figures] :as ctx}]
  (let [n (or (terms-of ctx) (+ 4 (* 40 (a/play p 0 0.9 a/linear))))
        x-n (- m/pi (/ 1 n))
        s-n (second (first (figure/points (:abel figures) [n] [[x-n]])))]
    [:g (abel-frame palette)
     (sawtooth-lines palette data 1)
     (svg/polyline (partial-curve figures n) {:stroke (:found palette) :width 0.025})
     (svg/segment [x-n s-n] [x-n (/ x-n 2)] {:stroke "#e0605a" :width 0.04})
     (readout palette [-3.0 2.3] 1
              [(str "n = " (fmt n 0) ", x = pi - 1/n")
               (str "error |S_n - x/2| = " (fmt (m/abs (- s-n (/ x-n 2))) 3))
               "no single n serves every x: not uniform"])]))

;; ---------------------------------------------------------------------------
;; The definite integral: rectangles refining
;;
;; data: {:curve [[x y] ...] :levels [{:n :rects [[x0 x1 h] ...] :S} ...]
;;        :integral I :label "..."}

(def ^:private integral :cauchy/integral)

(defn- rects [palette rs opacity]
  (into [:g] (for [[x0 x1 h] rs]
               (svg/polygon [[x0 0] [x1 0] [x1 h] [x0 h]]
                            {:fill (:construction palette) :opacity opacity
                             :stroke (:construction palette) :width 0.008}))))

(defn- integral-frame [palette {:keys [curve label]} f]
  [:g (svg/segment [-0.1 0] [3.4 0] {:stroke (:muted palette) :width 0.012})
   (stroke curve (:ink palette) 0.03 f)
   (write palette [0.1 2.6] label {:anchor "start" :italic? true})])

(defmethod scene/draw [integral :curve] [_ _ p {:keys [palette data]}]
  (let [{:keys [curve]} data
        [x0] (first curve) [x1] (last curve)
        f (a/play p 0.3 0.8)]
    [:g (integral-frame palette data (a/play p 0 0.4))
     (into [:g] (for [i (range 7) :let [x (+ x0 (* (/ i 6) (- x1 x0)))]]
                  (svg/layer (a/fade f) (svg/segment [x -0.06] [x 0.06] {:stroke (:ink palette) :width 0.015}))))
     (svg/layer (a/fade f)
                (write palette [x0 -0.3] "x0" {:italic? true})
                (write palette [x1 -0.3] "X" {:italic? true}))]))

(defmethod scene/draw [integral :rectangles] [_ _ p {:keys [palette data]}]
  (let [{:keys [levels]} data
        {:keys [rects S n]} (first levels)
        fs (a/lagged p (count rects) 0.4 0 0.8)]
    [:g (into [:g] (map (fn [[x0 x1 h] f]
                          (svg/polygon [[x0 0] [x1 0] [x1 (* f h)] [x0 (* f h)]]
                                       {:fill (:construction palette) :opacity 0.45
                                        :stroke (:construction palette) :width 0.008}))
                        rects fs))
     (integral-frame palette data 1)
     (readout palette [0.1 2.2] (a/play p 0.7 0.9)
              [(str "n = " n ":  S = " (fmt S 5))])]))

(defmethod scene/draw [integral :refine] [_ _ p {:keys [palette data]}]
  (let [{:keys [levels integral]} data
        k (count levels)
        [i t] [(min (dec k) (long (m/floor (* p k)))) (- (* p k) (m/floor (* p k)))]
        {rs :rects :keys [S n]} (nth levels i)]
    [:g (rects palette rs (+ 0.3 (* 0.2 (a/there-and-back (min 1 (* 2 t))))))
     (integral-frame palette data 1)
     (readout palette [0.1 2.2] 1
              [(str "n = " n ":  S = " (fmt S 5))
               (str "limit:     " (fmt integral 5))
               (str "S - limit = " (fmt (- S integral) 5))])]))

(defmethod scene/draw [integral :mean] [_ _ p {:keys [palette data]}]
  (let [{:keys [curve integral mean]} data
        [x0] (first curve) [x1] (last curve)
        h (/ integral (- x1 x0))
        f (a/play p 0.1 0.6)]
    [:g (svg/polygon [[x0 0] [x1 0] [x1 (* f h)] [x0 (* f h)]]
                     {:fill (:found palette) :opacity 0.3 :stroke (:found palette) :width 0.01})
     (svg/polygon (concat [[x0 0]] curve [[x1 0]]) {:fill (:construction palette) :opacity 0.15})
     (integral-frame palette data 1)
     (svg/layer (a/fade (a/play p 0.6 0.8))
                (svg/circle [mean h] 0.05 {:fill (:found palette)})
                (svg/segment [mean 0] [mean h] {:stroke (:found palette) :width 0.012 :dash "0.05 0.05"}))
     (readout palette [0.1 2.2] (a/play p 0.5 0.8)
              ["S = (X - x0) f(x0 + theta (X - x0))"
               (str "theta = " (fmt (/ (- mean x0) (- x1 x0)) 4))])]))

(defmethod scene/draw [integral :fundamental] [_ _ p {:keys [palette data]}]
  (let [{:keys [curve]} data
        [x0] (first curve) [x1] (last curve)
        x (+ x0 (* (- x1 x0) (+ 0.2 (* 0.75 (a/play p 0 0.9)))))
        under (filter (fn [[px]] (<= px x)) curve)
        [_ fx] (last under)
        area (reduce + (map (fn [[ax ay] [bx _]] (* (- bx ax) ay)) under (rest under)))]
    [:g (svg/polygon (concat [[x0 0]] under [[x 0]]) {:fill (:construction palette) :opacity 0.35})
     (svg/segment [x 0] [x fx] {:stroke (:found palette) :width 0.03})
     (integral-frame palette data 1)
     (readout palette [0.1 2.2] 1
              [(str "F(x) = integral from x0 to x = " (fmt area 4))
               (str "dF/dx = f(x) = " (fmt fx 4))
               "the moving edge adds f(x) per unit"])]))

;; ---------------------------------------------------------------------------
;; The flat function
;;
;; data: {:curves [[[x y] ...] ...] one per zoom level, :zooms [z ...]}

(def ^:private flat :cauchy/flat)

(defn- flat-frame [palette]
  [:g (svg/segment [-3 0] [3 0] {:stroke (:muted palette) :width 0.012})
   (svg/segment [0 -0.2] [0 1.6] {:stroke (:muted palette) :width 0.012})
   (svg/segment [-3 1.2] [3 1.2] {:stroke (:muted palette) :width 0.008 :dash "0.05 0.05"})])

(defmethod scene/draw [flat :flat] [_ _ p {:keys [palette data]}]
  [:g (flat-frame palette)
   (stroke (first (:curves data)) (:found palette) 0.035 (a/play p 0 0.7))
   (readout palette [-2.9 1.9] (a/play p 0.5 0.8)
            ["f(x) = exp(-1/x^2), f(0) = 0" "1 at infinity (dashed, scaled)"])])

(defmethod scene/draw [flat :zoom] [_ _ p {:keys [palette data]}]
  (let [{:keys [curves zooms]} data
        k (count curves)
        i (min (dec k) (long (m/floor (* p k))))
        z (nth zooms i)]
    [:g (flat-frame palette)
     (svg/polyline (nth curves i) {:stroke (:found palette) :width 0.035})
     (readout palette [-2.9 1.9] 1
              [(str "x from -" z " to " z ", height magnified by (1/" z ")^4")
               "zoom in, magnify by a power: flatter still"
               "f(0) = f'(0) = f''(0) = ... = 0"])]))

(defmethod scene/draw [flat :taylor] [_ _ p {:keys [palette data]}]
  [:g (flat-frame palette)
   (svg/polyline (first (:curves data)) {:stroke (:found palette) :width 0.035})
   (stroke [[-3 0] [3 0]] "#e0605a" 0.05 (a/play p 0.1 0.6))
   (readout palette [-2.9 1.9] (a/play p 0.4 0.7)
            ["Taylor at 0: 0 + 0x + 0x^2 + ... = 0" "converges everywhere, to 0, not to f"])])

;; ---------------------------------------------------------------------------
;; Note III: the root search by tenths
;;
;; data: {:rounds [{:x :X :xs [..11] :values [..11] :j :curve [[x y] ..]} ..]
;;        :root a :label "..."}; each round drawn in the same window, its
;; bracket [x X] stretched over [0 5] and its values scaled to fit.

(def ^:private ivt :cauchy/ivt)

(defn- round-frame
  "Maps of round r: bracket onto [0 5], values onto [-1.6 1.6] by the
   round's :scale (its largest |f|, computed by the view)."
  [{:keys [x X scale]}]
  {:at (fn [[px py]] [(* 5 (/ (- px x) (- X x))) (* 1.6 (/ py scale))])})

(defn- round-drawing [palette {:keys [xs values j curve] :as round} f-dots f-pair]
  (let [{:keys [at]} (round-frame round)]
    [:g (svg/segment [-0.1 0] [5.1 0] {:stroke (:muted palette) :width 0.012})
     (stroke (map at curve) (:ink palette) 0.03 1)
     (into [:g] (map-indexed
                 (fn [i [px py]]
                   (svg/layer (a/fade (min 1 (max 0 (- (* 11 f-dots) i))))
                              (svg/circle (at [px py]) 0.06
                                          {:fill (if (neg? py) "#e0605a" (:found palette))})))
                 (map vector xs values)))
     (when (and j (pos? f-pair))
       (let [[ax] (at [(nth xs j) 0]) [bx] (at [(nth xs (inc j)) 0])]
         (svg/layer (a/fade f-pair)
                    (window (:construction palette) ax bx [-1.8 1.8] 0.22))))]))

(defn- ivt-readout [palette round k extra]
  (readout palette [-0.1 2.45] 1
           (into [(str "round " k ":  x = " (fmt (:x round) (inc k)) ",  X = " (fmt (:X round) (inc k)))
                  (str "X - x = 1/10^" k)]
                 extra)))

(defmethod scene/draw [ivt :sign] [_ _ p {:keys [palette data]}]
  (let [r (first (:rounds data))]
    [:g (round-drawing palette r (a/play p 0.2 0.6) 0)
     (readout palette [-0.1 2.45] (a/play p 0 0.3)
              [(:label data) "f(2) = -1 < 0 < 16 = f(3)"])]))

(defmethod scene/draw [ivt :divide] [_ _ p {:keys [palette data]}]
  (let [r (first (:rounds data))]
    [:g (round-drawing palette r (a/play p 0 0.8 a/linear) 0)
     (ivt-readout palette r 0 ["f at x0 + k h/m, k = 0..10 (raster kernel)"])]))

(defmethod scene/draw [ivt :pair] [_ _ p {:keys [palette data]}]
  (let [r (first (:rounds data))]
    [:g (round-drawing palette r 1 (a/play p 0.1 0.5))
     (ivt-readout palette r 0 ["first pair of contrary signs: x1, X'"])]))

(defmethod scene/draw [ivt :refine] [_ _ p {:keys [palette data]}]
  (let [rs (:rounds data)
        n (dec (count rs))
        t (* n (a/play p 0 0.95 a/linear))
        fr (- t (m/floor t))
        k (min n (inc (long (m/floor t))))
        r (nth rs k)]
    [:g (round-drawing palette r (min 1 (* 3 fr)) (if (= k n) 1 (max 0 (- (* 3 fr) 1.5))))
     (ivt-readout palette r k ["the bracket stretched to the same width"])]))

(defmethod scene/draw [ivt :limit] [_ _ p {:keys [palette data]}]
  (let [rs (:rounds data)
        sx (fn [v] (* 5 (- v 2)))]
    [:g (svg/segment [0 0] [5 0] {:stroke (:muted palette) :width 0.012})
     (into [:g] (for [[k {:keys [x X]}] (map-indexed vector rs)
                      :let [y (- 1.8 (* 0.3 k)) f (a/play p (* 0.1 k) (+ 0.2 (* 0.1 k)))]]
                  (svg/layer (a/fade f)
                             (svg/segment [(sx x) y] [(sx X) y] {:stroke (:construction palette) :width 0.05})
                             (svg/circle [(sx x) y] 0.04 {:fill (:found palette)})
                             (svg/circle [(sx X) y] 0.04 {:fill "#e0605a"}))))
     (svg/layer (a/fade (a/play p 0.75 0.95))
                (svg/segment [(sx (:root data)) -0.3] [(sx (:root data)) 2.0]
                             {:stroke (:found palette) :width 0.015 :dash "0.05 0.05"}))
     (readout palette [-0.1 2.45] 1
              ["x0 <= x1 <= x2 ... (green)   X >= X' >= X'' ... (red)"
               "both tend to one limit a, and f(a) = 0"])]))

(defmethod scene/draw [ivt :scholie] [_ _ p {:keys [palette data]}]
  (let [r (peek (:rounds data)) k (dec (count (:rounds data)))]
    [:g (round-drawing palette r 1 1)
     (ivt-readout palette r k [(str "half-sum " (fmt (/ (+ (:x r) (:X r)) 2) 8) ", error < 5e-7")
                               (str "root by raster's Brent: " (fmt (:root data) 10))])]))

;; ---------------------------------------------------------------------------
;; Lesson 3: the ratio of differences, i shrinking
;;
;; data: {:curve [[x y] ..] :x x :fx f(x) :slope f'(x)
;;        :chords [{:i :fxi f(x+i) :q quotient} ..] :label "..."}

(def ^:private deriv :cauchy/derivative)

(defn- chord-at
  "The chord of round r, linearly between rounds by t in [0 1]."
  [chords t]
  (let [k (count chords)
        u (* t (dec k))
        i0 (min (- k 2) (long (m/floor u)))
        s (- u i0)
        lerp (fn [key] (+ (key (nth chords i0)) (* s (- (key (nth chords (inc i0))) (key (nth chords i0))))))]
    {:i (lerp :i) :fxi (lerp :fxi) :q (lerp :q)}))

(defn- chord-drawing [palette {:keys [curve x fx label]} {:keys [i fxi q]} extra]
  [:g (svg/segment [-0.2 0] [3.3 0] {:stroke (:muted palette) :width 0.012})
   (stroke curve (:ink palette) 0.03 1)
   (svg/polyline [[(- x 1.2) (- fx (* 1.2 q))] [(+ x i 1.2) (+ fx (* (+ i 1.2) q))]]
                 {:stroke (:construction palette) :width 0.02})
   (svg/segment [x fx] [(+ x i) fx] {:stroke (:found palette) :width 0.025})
   (svg/segment [(+ x i) fx] [(+ x i) fxi] {:stroke "#e0605a" :width 0.025})
   (svg/circle [x fx] 0.05 {:fill (:found palette)})
   (svg/circle [(+ x i) fxi] 0.05 {:fill "#e0605a"})
   (write palette [(+ x (/ i 2)) (- fx 0.15)] "i" {:italic? true})
   (readout palette [-0.1 1.75] 1 (into [label (str "i = " (fmt i 4) ",  (f(x+i) - f(x))/i = " (fmt q 6))] extra))])

(defmethod scene/draw [deriv :chord] [_ _ p {:keys [palette data]}]
  (chord-drawing palette data (first (:chords data)) ["both terms of the ratio go to 0 with i"]))

(defmethod scene/draw [deriv :shrink] [_ _ p {:keys [palette data]}]
  (chord-drawing palette data (chord-at (:chords data) (a/play p 0 0.9 a/smooth))
                 [(str "the ratio tends to cos 1 = " (fmt (:slope data) 6))]))

(defmethod scene/draw [deriv :power] [_ _ p {:keys [palette data]}]
  (chord-drawing palette data (peek (:chords data))
                 ["x^m: m x^(m-1) + m(m-1)/2 x^(m-2) i + ... + i^(m-1)"
                  "every term but the first carries i"]))

(defmethod scene/draw [deriv :derived] [_ _ p {:keys [palette data]}]
  (chord-drawing palette data (peek (:chords data))
                 ["the limit: the derived function y' = f'(x)"
                  "sin x: [sin(i/2)/(i/2)] cos(x + i/2) -> cos x"]))

;; ---------------------------------------------------------------------------
;; Chapter VI, Theorem 1: u_n below U^n
;;
;; data: {:terms [[n u_n U^n] ..] :N n-from :k-roots [[n root] ..] :U U :k k}
;; heights on a log scale: y = 0.25 * log10 of the value + 3.

(def ^:private root-test :cauchy/root-test)

(defn- lg
  "Height of a value given by its log10 (the view computes the logs)."
  [log10-v]
  (+ 3.2 (* 0.3 log10-v)))

(defn- root-test-bars
  "terms [[n log10 u_n log10 U^n] ..]: bars u_n, dots U^n."
  [palette {:keys [terms N]} f-u f-geo]
  (into [:g (svg/segment [0 0] [6.2 0] {:stroke (:muted palette) :width 0.012})]
        (for [[n lu lg-geo] terms :let [x (* 0.2 n)]]
          [:g (svg/segment [x 0] [x (* f-u (max 0 (lg lu)))]
                           {:stroke (if (>= n N) (:found palette) "#e0605a") :width 0.07})
           (svg/layer (a/fade f-geo)
                      (svg/circle [x (max 0 (lg lg-geo))] 0.035 {:fill (:construction palette)}))])))

(defn- roots-curve
  "k-roots [[log10 n root] ..] onto the plane."
  [k-roots]
  (map (fn [[ln r]] [(* 1.5 ln) (* 3 r)]) k-roots))

(defmethod scene/draw [root-test :roots] [_ _ p {:keys [palette data]}]
  (let [{:keys [k-roots k]} data
        sx nil]
    [:g (svg/segment [0 0] [6.2 0] {:stroke (:muted palette) :width 0.012})
     (svg/segment [0 (* 3 k)] [6.2 (* 3 k)] {:stroke (:construction palette) :width 0.012 :dash "0.05 0.05"})
     (stroke (roots-curve k-roots) (:found palette) 0.03 (a/play p 0 0.8))
     (readout palette [0.1 3.6] 1 ["(u_n)^(1/n) for u_n = n^2/2^n, n from 1 to 10^4 (log scale)"
                                   (str "its limit k = " k)])]))

(defmethod scene/draw [root-test :choose-u] [_ _ p {:keys [palette data]}]
  (let [{:keys [k-roots k U]} data
        sx nil]
    [:g (svg/segment [0 0] [6.2 0] {:stroke (:muted palette) :width 0.012})
     (svg/segment [0 (* 3 k)] [6.2 (* 3 k)] {:stroke (:construction palette) :width 0.012 :dash "0.05 0.05"})
     (svg/layer (a/fade (a/play p 0.1 0.5))
                (svg/segment [0 (* 3 U)] [6.2 (* 3 U)] {:stroke "#e0605a" :width 0.015}))
     (stroke (roots-curve k-roots) (:found palette) 0.03 1)
     (readout palette [0.1 3.6] 1 [(str "k = " k " < U = " U " < 1") "the roots end below U"])]))

(defmethod scene/draw [root-test :compare] [_ _ p {:keys [palette data]}]
  [:g (root-test-bars palette data (a/play p 0 0.6) (a/play p 0.4 0.8))
   (readout palette [0.1 3.6] 1 ["bars u_n, dots U^n (log scale)"
                                 (str "from n = " (:N data) " on, u_n < U^n (green)")])])

(defmethod scene/draw [root-test :progression] [_ _ p {:keys [palette data]}]
  [:g (root-test-bars palette data 1 1)
   (readout palette [0.1 3.6] 1 ["1 + U + U^2 + ... = 1/(1 - U) = 4 converges"
                                 "so the series converges a fortiori: sum = 6"])])

(defmethod scene/draw [root-test :ratio] [_ _ p {:keys [palette data]}]
  [:g (root-test-bars palette data 1 1)
   (readout palette [0.1 3.6] 1 ["Theorem 2: u_(n+1)/u_n = (n+1)^2/(2n^2) -> 1/2 = k"])])

;; ---------------------------------------------------------------------------
;; Lesson 21 equations (3)-(7) and Lesson 26: the steps reuse the integral's
;; drawings (same data shape), registered under their own scene ids.

(def ^:private refinement :cauchy/refinement)
(def ^:private fundamental :cauchy/fundamental)

(defmethod scene/draw [refinement :one] [_ _ p {:keys [palette data]}]
  (let [{:keys [curve]} data
        [x0 y0] (first curve) [x1] (last curve)
        f (a/play p 0 0.6)]
    [:g (svg/polygon [[x0 0] [x1 0] [x1 (* f y0)] [x0 (* f y0)]]
                     {:fill (:construction palette) :opacity 0.4 :stroke (:construction palette) :width 0.01})
     (integral-frame palette data 1)
     (readout palette [0.1 2.2] (a/play p 0.5 0.8) ["(3)  S = (X - x0) f(x0): one element"])]))

(defmethod scene/draw [refinement :mean] [s _ p ctx]
  (scene/draw :cauchy/integral :mean p ctx))

(defmethod scene/draw [refinement :subdivide] [_ _ p {:keys [palette data]}]
  (let [{:keys [levels]} data
        a (nth levels 0) b (nth levels 1)
        f (a/play p 0.1 0.8)]
    [:g (rects palette (:rects a) (* 0.45 (- 1 f)))
     (rects palette (:rects b) (* 0.45 f))
     (integral-frame palette data 1)
     (readout palette [0.1 2.2] 1
              [(str "n = " (:n a) ":  S = " (fmt (:S a) 5))
               (str "each element halved, n = " (:n b) ":  S = " (fmt (:S b) 5))])]))

(defmethod scene/draw [refinement :errors] [_ _ p {:keys [palette data]}]
  (let [{:keys [levels]} data
        k (count levels)
        i (min (- k 2) (long (m/floor (* p (dec k)))))
        a (nth levels i) b (nth levels (inc i))]
    [:g (rects palette (:rects b) 0.4)
     (integral-frame palette data 1)
     (readout palette [0.1 2.2] 1
              [(str "n = " (:n a) " -> " (:n b) ":  |S' - S| = " (fmt (m/abs (- (:S b) (:S a))) 6))
               "= (X - x0) times a mean of the e_k: small with the elements"])]))

(defmethod scene/draw [refinement :third] [_ _ p ctx]
  (scene/draw :cauchy/integral :refine p ctx))

(defmethod scene/draw [fundamental :area] [_ _ p ctx]
  (scene/draw :cauchy/integral :fundamental (* 0.5 p) ctx))

(defmethod scene/draw [fundamental :increment] [_ _ p {:keys [palette data]}]
  (let [{:keys [curve strip]} data
        {:keys [x alpha xt ft]} strip
        under (filter (fn [[px]] (<= x px (+ x alpha))) curve)
        f (a/play p 0 0.5)]
    [:g (svg/polygon (concat [[x 0]] under [[(+ x alpha) 0]]) {:fill (:construction palette) :opacity 0.35})
     (svg/layer (a/fade f)
                (svg/polygon [[x 0] [(+ x alpha) 0] [(+ x alpha) ft] [x ft]]
                             {:fill "none" :stroke (:found palette) :width 0.02})
                (svg/circle [xt ft] 0.05 {:fill (:found palette)}))
     (integral-frame palette data 1)
     (readout palette [0.1 2.2] 1
              ["(3)  F(x + alpha) - F(x) = alpha f(x + theta alpha)"
               (str "alpha = " (fmt alpha 2) ", theta = " (fmt (:theta strip) 4))])]))

(defmethod scene/draw [fundamental :divide] [_ _ p {:keys [palette data]}]
  (let [{:keys [quotients fx]} data
        k (count quotients)
        i (min (dec k) (long (m/floor (* p k))))
        {:keys [alpha quotient theta]} (nth quotients i)]
    [:g (integral-frame palette data 1)
     (readout palette [0.1 2.2] 1
              [(str "alpha = " alpha ":  (F(x+alpha) - F(x))/alpha = " (fmt quotient 6))
               (str "theta = " (fmt theta 4))
               (str "(4)  F'(x) = f(x) = " (fmt fx 6))])]))
