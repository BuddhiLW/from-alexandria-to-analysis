(ns alexandria.riemann.integral-scenes
  "The drawings of Riemann's sections 4 to 6 (alexandria.riemann.integral),
   one method per stage (alexandria.medium.scene/draw), in Manim's idiom
   (alexandria.medium.anim).

     :riemann/integral      f(x) = x^2 on [0 1]: tagged rectangles whose
                            tags wander, partitions refining, upper and
                            lower sums closing on each other, the
                            oscillation strips stacked into one column,
                            the pieces where the oscillation exceeds sigma
     :riemann/pathological  the partial sums S_N of sum (nx)/n^2 (ctx
                            :figures :graph, a raster kernel), the jumps on
                            the even-denominator fractions, the finitely
                            many large ones, and the exact upper and lower
                            sums of S_8 (ctx :data :cells)"
  (:require [alexandria.medium.anim :as a]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.math :as m]
            [alexandria.medium.plane :as plane]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            #?(:cljs [alexandria.medium.player :as player])))

#?(:cljs (def render "Clerk's render-fn for these scenes." player/render))

;; ---------------------------------------------------------------------------
;; Shared drawing

(defn- fmt [x digits]
  #?(:clj (format (str "%." digits "f") (double x))
     :cljs (.toFixed (js/Number x) digits)))

(defn- write
  ([palette at s] (write palette at s {}))
  ([palette at s opts]
   (svg/text at s (merge {:colour (:ink palette) :size 0.075} opts))))

(defn- readout
  "Lines of mono text from `at` downwards, faded in by f."
  [palette at f lines]
  (svg/layer (a/fade f [0.1 0])
             (into [:g] (map-indexed (fn [i s] (write palette (plane/translate at [0 (* -0.1 i)]) s
                                                      {:anchor "start" :size 0.062 :mono? true}))
                                     lines))))

(defn- stroke [ps colour width f]
  (when (pos? f) (svg/polyline ps {:stroke colour :width width :attrs (a/create f)})))

(defn- frac [x] (- x (m/floor x)))

;; ---------------------------------------------------------------------------
;; Section 4 and 5: f(x) = x^2 on [0 1]

(def ^:private integral-id :riemann/integral)

(defn- f2 [x] (* x x))

(def ^:private parabola (mapv (fn [i] (let [x (/ i 100)] [x (f2 x)])) (range 101)))

(defn- axes [palette f]
  [:g (stroke [[-0.03 0] [1.05 0]] (:muted palette) 0.006 f)
   (stroke [[0 -0.03] [0 1.08]] (:muted palette) 0.006 f)
   (svg/layer (a/fade f) (write palette [0 -0.09] "a = 0" {:size 0.055 :colour (:muted palette)})
              (write palette [1 -0.09] "b = 1" {:size 0.055 :colour (:muted palette)}))])

(defn- curve [palette f] (stroke parabola (:found palette) 0.014 f))

(defn- rect [x0 x1 h style] (svg/polygon (plane/rect x0 x1 (min 0 h) (max 0 h)) style))

(defn- golden-eps "A fixed proper fraction per interval." [i] (+ 0.05 (* 0.9 (frac (* 0.618034 (+ i 1))))))

(defn- tagged
  "The rectangles of S over n equal pieces with tags eps(i); faded in by fs."
  [palette n eps fs]
  (into [:g]
        (for [i (range n)
              :let [x0 (/ i n) d (/ 1 n) t (+ x0 (* (eps i) d)) h (f2 t) f (nth fs i 1)]]
          [:g {:opacity f}
           (rect x0 (+ x0 d) h {:fill (:construction palette) :opacity 0.28
                                :stroke (:construction palette) :width 0.005})
           (svg/segment [t 0] [t h] {:stroke (:ink palette) :width 0.003 :dash "0.01 0.01"})
           (svg/circle [t h] 0.011 {:fill (:ink palette)})])))

(defn- s-of [n eps] (reduce + (map (fn [i] (let [d (/ 1 n)] (* d (f2 (+ (* i d) (* (eps i) d)))))) (range n))))

(defn- step-of [p ns] (let [k (count ns) i (min (dec k) (long (m/floor (* p k))))] [i (nth ns i)]))

(defmethod scene/draw [integral-id :tags] [_ _ p {:keys [palette]}]
  (let [n 6
        eps (fn [i] (+ 0.5 (* 0.45 (m/sin (* 2 m/pi (+ (* 1.2 (a/clamp01 (/ (- p 0.35) 0.65))) (* i 0.37)))))))
        fs (a/lagged p n 0.5 0.05 0.35)]
    [:g (axes palette 1) (tagged palette n eps fs) (curve palette (a/play p 0 0.25))
     (readout palette [1.1 1.0] (a/play p 0.3 0.45)
              ["n = 6 pieces, delta_i = 1/6"
               "tag in piece i: x_{i-1} + eps_i delta_i"
               (str "S = " (fmt (s-of n eps) 4))
               "S moves with the tags"])]))

(defmethod scene/draw [integral-id :refine] [_ _ p {:keys [palette]}]
  (let [[_ n] (step-of p [3 6 12 24 48 96])
        s (s-of n golden-eps)]
    [:g (axes palette 1) (tagged palette n golden-eps (repeat 1)) (curve palette 1)
     (readout palette [1.1 1.0] 1
              [(str "n = " n ", delta = 1/" n)
               (str "S = " (fmt s 5))
               (str "|S - 1/3| = " (fmt (m/abs (- s 1/3)) 5))
               "every choice of tags: S -> A = 1/3"])]))

(defn- darboux [n]
  (let [d (/ 1 n)]
    {:upper (reduce + (map #(* d (f2 (* (inc %) d))) (range n)))
     :lower (reduce + (map #(* d (f2 (* % d))) (range n)))}))

(defn- upper-lower-rects [palette n f]
  (into [:g]
        (for [i (range n) :let [x0 (/ i n) x1 (/ (inc i) n)]]
          [:g
           (rect x0 x1 (f2 x1) {:fill (:found palette) :opacity (* 0.2 f) :stroke (:found palette) :width 0.004})
           (rect x0 x1 (f2 x0) {:fill (:construction palette) :opacity (* 0.35 f)
                                :stroke (:construction palette) :width 0.004})])))

(defmethod scene/draw [integral-id :upper-lower] [_ _ p {:keys [palette]}]
  (let [[_ n] (step-of p [4 8 16 32])
        {:keys [upper lower]} (darboux n)]
    [:g (axes palette 1) (upper-lower-rects palette n 1) (curve palette 1)
     (readout palette [1.1 1.0] 1
              [(str "n = " n)
               (str "upper sum U = " (fmt upper 5))
               (str "lower sum L = " (fmt lower 5))
               (str "U - L = " (fmt (- upper lower) 5) " = 1/" n)
               "every S lies between L and U"])]))

(defmethod scene/draw [integral-id :oscillation] [_ _ p {:keys [palette]}]
  (let [n 8 d (/ 1 n)
        slide (a/play p 0.3 0.8)
        column-x 1.25]
    (into [:g (axes palette 1) (curve palette 1)
           (stroke [[column-x 0] [column-x 1]] (:muted palette) 0.004 (a/play p 0.75 0.9))]
          (concat
           (for [i (range n)
                 :let [x0 (* i d) lo (f2 x0) hi (f2 (+ x0 d))
                       x (plane/lerp x0 column-x slide)]]
             (svg/polygon (plane/rect x (+ x d) lo hi)
                          {:fill (:found palette) :opacity 0.55 :stroke (:found palette) :width 0.004}))
           [(readout palette [1.1 0.45] (a/play p 0.75 0.95)
                     ["D_i: greatest minus least"
                      "in piece i, f increasing:"
                      "D_i = f(x_i) - f(x_{i-1})"
                      "stacked: width 1/8,"
                      "height f(1) - f(0) = 1"
                      "sum delta_i D_i = 1/8"])]))))

(defn- g-jump [x] (+ 0.25 (* 0.45 x) (if (>= x 0.5) 0.3 0)))

(defmethod scene/draw [integral-id :sigma] [_ _ p {:keys [palette]}]
  (let [[_ n] (step-of p [4 8 16 32 64])
        sigma 0.2 d (/ 1 n)
        cells (for [i (range n) :let [x0 (* i d) x1 (+ x0 d)
                                      hi (if (and (< x0 0.5) (>= x1 0.5)) (g-jump x1) (g-jump x1))
                                      lo (g-jump x0)
                                      osc (- (if (and (< x0 0.5) (> x1 0.5)) (g-jump x1) hi) lo)]]
                [x0 x1 lo hi osc])
        bad (filter (fn [[_ _ _ _ osc]] (> osc sigma)) cells)
        graph-l (mapv (fn [i] (let [x (* 0.5 (/ i 50))] [x (g-jump x)])) (range 50))
        graph-r (mapv (fn [i] (let [x (+ 0.5 (* 0.5 (/ i 50)))] [x (g-jump x)])) (range 51))]
    (into [:g (axes palette 1)
           (svg/polyline graph-l {:stroke (:found palette) :width 0.014})
           (svg/polyline graph-r {:stroke (:found palette) :width 0.014})]
          (concat
           (for [[x0 x1 lo hi osc] cells]
             (svg/polygon (plane/rect x0 x1 lo hi)
                          {:fill (if (> osc sigma) "#e0675a" (:construction palette))
                           :opacity (if (> osc sigma) 0.6 0.3) :width 0.003 :stroke (:muted palette)}))
           (for [[x0 x1] bad] (svg/segment [x0 -0.04] [x1 -0.04] {:stroke "#e0675a" :width 0.02}))
           [(readout palette [1.1 1.0] 1
                     [(str "n = " n ", sigma = " sigma)
                      "red: oscillation > sigma"
                      (str "their total length s = 1/" n)
                      "sigma s <= sum delta_i D_i"
                      "s -> 0 for every sigma:"
                      "the function is integrable"])]))))

;; ---------------------------------------------------------------------------
;; Section 6: f(x) = sum (nx)/n^2

(def ^:private path-id :riemann/pathological)

(def ^:private sample-count 1201)
(def ^:private samples (mapv (fn [i] [(/ i (dec sample-count))]) (range sample-count)))

(defn- graph-pieces
  "The sampled graph cut where it jumps, as polylines."
  [pts]
  (->> (partition-by identity (map (fn [[_ y0] [_ y1]] (> (m/abs (- y1 y0)) 0.004)) pts (rest pts)))
       (reduce (fn [[acc i] run]
                 (let [k (count run)]
                   [(if (first run) acc (conj acc (subvec pts i (+ i k 1)))) (+ i k)]))
               [[] 0])
       first))

(defn- graph [palette figures n f]
  (into [:g {:opacity f}]
        (map #(svg/polyline % {:stroke (:found palette) :width 0.008})
             (graph-pieces (vec (figure/points (:graph figures) [n] samples))))))

(defn- path-axes [palette]
  [:g (svg/segment [-0.02 0] [1.02 0] {:stroke (:muted palette) :width 0.004})
   (svg/segment [0 -0.7] [0 0.7] {:stroke (:muted palette) :width 0.004})
   (write palette [1 -0.06] "1" {:size 0.045 :colour (:muted palette)})
   (write palette [-0.03 0.5] "1/2" {:size 0.045 :anchor "end" :colour (:muted palette)})])

(defn- top [palette f lines] (readout palette [0.02 0.83] f lines))

(defmethod scene/draw [path-id :bracket] [_ _ p {:keys [palette figures]}]
  [:g (path-axes palette) (graph palette figures 1 (a/play p 0 0.4))
   (top palette (a/play p 0.3 0.6) ["(x): the excess of x over the nearest whole number"
                                    "(x) = 0 at x = 1/2, the mean of 1/2 and -1/2"])])

(defmethod scene/draw [path-id :terms] [_ _ p {:keys [palette figures]}]
  (let [n (+ 1 (* 7 (a/play p 0.05 0.9 a/linear)))]
    [:g (path-axes palette) (graph palette figures n 1)
     (top palette 1 [(str "S_N = (x)/1 + (2x)/4 + ... + (Nx)/N^2,  N = " (fmt n 1))
                     "term n: jumps at odd multiples of 1/(2n), size <= 1/(2n^2)"])]))

(defmethod scene/draw [path-id :jumps] [_ _ p {:keys [palette figures]}]
  (let [f (a/play p 0.2 0.5)]
    [:g (path-axes palette) (graph palette figures 12 1)
     (svg/layer (a/fade f)
                (svg/circle [0.5 0] 0.04 {:stroke "#e0675a" :width 0.008})
                (svg/circle [0.25 0] 0.03 {:stroke "#e0675a" :width 0.006})
                (svg/circle [0.75 0] 0.03 {:stroke "#e0675a" :width 0.006}))
     (top palette 1 ["at x = p/(2n): f(x+0) = f(x) - pi^2/(16 n^2)"
                     "              f(x-0) = f(x) + pi^2/(16 n^2)"
                     "x = 1/2: jump pi^2/8 = 1.234;  x = 1/4, 3/4: pi^2/32"])]))

(defn- jump-ticks
  "Ticks under the axis at p/(2n), n <= n-max, height the jump pi^2/(8 n^2)."
  [n-max]
  (for [n (range 1 (inc n-max)) q (range 1 (* 2 n) 2)
        :when (= 1 (loop [x q y n] (if (zero? y) x (recur y (mod x y)))))]
    [(/ q (* 2 n)) (/ (* m/pi m/pi) (* 8 n n))]))

(defmethod scene/draw [path-id :dense] [_ _ p {:keys [palette figures]}]
  (let [ticks (jump-ticks 24)
        shown (long (* (count ticks) (a/play p 0.05 0.85 a/linear)))]
    (into [:g (path-axes palette) (graph palette figures 12 0.5)
           (top palette 1 ["a jump at every fraction with even denominator:"
                           (str shown " marked, between any two points infinitely many")])]
          (for [[x j] (take shown ticks)]
            (svg/segment [x 0] [x (- (* 0.35 j))] {:stroke "#e0675a" :width 0.003})))))

(defmethod scene/draw [path-id :finite-jumps] [_ _ p {:keys [palette figures]}]
  (let [ticks (jump-ticks 24)
        sigma (- 0.3 (* 0.27 (a/play p 0.1 0.9)))
        big (filter #(>= (second %) sigma) ticks)]
    (into [:g (path-axes palette) (graph palette figures 12 0.35)
           (svg/segment [0 (- (* 0.35 sigma))] [1 (- (* 0.35 sigma))]
                        {:stroke (:construction palette) :width 0.004 :dash "0.02 0.012"})
           (top palette 1 [(str "sigma = " (fmt sigma 3) ": jumps >= sigma at " (count big) " points")
                           "n <= pi / sqrt(8 sigma): always finitely many"])]
          (for [[x j] ticks :let [big? (>= j sigma)]]
            (svg/segment [x 0] [x (- (* 0.35 j))] {:stroke (if big? "#e0675a" (:muted palette))
                                                   :width (if big? 0.007 0.002)})))))

(defmethod scene/draw [path-id :integrable] [_ _ p {:keys [palette figures data]}]
  (let [rows (:cells data)
        [i {:keys [n cells upper lower]}] (step-of p rows)]
    (into [:g (path-axes palette) (graph palette figures 8 1)
           (top palette 1 [(str "S_8 on " n " pieces: U = " (fmt upper 4) ", L = " (fmt lower 4))
                           (str "U - L = " (fmt (- upper lower) 4) "  (exact, in rationals)")])]
          (for [[x0 x1 hi lo] cells]
            (svg/polygon (plane/rect x0 x1 lo hi)
                         {:fill (:construction palette) :opacity 0.28
                          :stroke (:construction palette) :width 0.003})))))
