(ns alexandria.galileo.motion-scenes
  "The drawings of Two New Sciences (alexandria.galileo.motion), one method
   per [scene stage] (alexandria.medium.scene/draw), animated in Manim's
   idiom (alexandria.medium.anim).

     :galileo/third-day-1     speed against time: triangle and rectangle
     :galileo/corollary-1     the odd-number staircase and the gnomons
     :galileo/inclined-plane  the groove, the water clock, the marks
     :galileo/fourth-day-1    the projectile tracing the semi-parabola
     :galileo/fourth-day-7    shots at every elevation; 45 degrees

   ctx :figures carries :roll (the ball on the groove) and :throw (the
   projectile), both raster kernels in the browser; ctx :data the numbers."
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

;; ---------------------------------------------------------------------------
;; Drawing helpers

(defn- fmt [x digits]
  #?(:clj (format (str "%." digits "f") (double x))
     :cljs (.toFixed (js/Number x) digits)))

(defn- write
  ([palette at s] (write palette at s {}))
  ([palette at s opts]
   (svg/text at s (merge {:colour (:ink palette) :size 0.19} opts))))

(defn- label [palette at s f & [{:keys [dx dy] :or {dx 0.08 dy 0.08}}]]
  (svg/layer (a/fade f [0 -0.15])
             (write palette (plane/translate at [dx dy]) s {:italic? true :anchor "start"})))

(defn- stroke [ps colour width f]
  (when (pos? f) (svg/polyline ps {:stroke colour :width width :attrs (a/create f)})))

(defn- dot [at colour r f]
  (when (pos? f) (svg/circle at (* r f) {:fill colour})))

(defn- readout
  "Lines of mono text from `at` down, faded in together."
  [palette f [x y] lines]
  (svg/layer (a/fade f [0.2 0])
             (into [:g] (map-indexed (fn [i s] (write palette [x (- y (* 0.3 i))] s
                                                      {:anchor "start" :size 0.17 :mono? true}))
                                     lines))))

(defn- sample [f a b n] (mapv (fn [i] (f (+ a (* (- b a) (/ i n))))) (range (inc n))))

;; ---------------------------------------------------------------------------
;; Third Day, Theorem I: the mean speed (Fig. 47)

(def ^:private mean-pts
  {:A [0 0] :B [4 0] :E [4 3] :F [4 1.5] :G [0 1.5] :I [2 1.5]})

(defn- speed-axes [palette f]
  (let [{:keys [A B E]} mean-pts]
    [:g (stroke [A [4.6 0]] (:muted palette) 0.02 f)
     (stroke [A [0 3.3]] (:muted palette) 0.02 f)
     (svg/layer (a/fade f)
                (write palette [4.6 -0.35] "time" {:size 0.16 :colour (:muted palette)})
                (write palette [-0.15 3.35] "speed" {:size 0.16 :anchor "end" :colour (:muted palette)}))
     (label palette A "A" f {:dx -0.25 :dy -0.25}) (label palette B "B" f {:dx 0.05 :dy -0.28})]))

(defn- triangle-aeb [palette f fill]
  (let [{:keys [A B E]} mean-pts]
    [:g (when (pos? fill) (svg/polygon [A B E] {:fill (:found palette) :opacity (* 0.25 fill)}))
     (stroke [A E B] (:found palette) 0.035 f)
     (label palette E "E" f)]))

(defn- rectangle-agfb [palette f fill]
  (let [{:keys [A B F G I]} mean-pts]
    [:g (when (pos? fill) (svg/polygon [A B F G] {:fill (:construction palette) :opacity (* 0.25 fill)}))
     (stroke [B F G A] (:construction palette) 0.03 f)
     (label palette F "F" f) (label palette G "G" f {:dx -0.3 :dy 0.05})
     (dot I (:ink palette) 0.05 f) (label palette I "I" f {:dx -0.05 :dy 0.12})]))

(defmethod scene/draw [:galileo/third-day-1 :axes] [_ _ p {:keys [palette]}]
  (let [{:keys [B E]} mean-pts]
    [:g (speed-axes palette (a/play p 0 0.4))
     (stroke [B E] (:found palette) 0.035 (a/play p 0.4 0.7))
     (label palette E "E" (a/play p 0.6 0.8))
     (readout palette (a/play p 0.6 0.9) [4.6 2.8] ["AB: the time" "EB: the last speed"])]))

(defmethod scene/draw [:galileo/third-day-1 :triangle] [_ _ p {:keys [palette]}]
  (let [t (* 4 (a/play p 0.1 0.9 a/linear))]
    [:g (speed-axes palette 1)
     (triangle-aeb palette (a/play p 0 0.2) (a/play p 0.1 0.9 a/linear))
     (stroke [[t 0] [t (* 0.75 t)]] (:ink palette) 0.03 1)
     (dot [t (* 0.75 t)] (:found palette) 0.07 1)
     (readout palette 1 [4.6 2.8] [(str "t = " (fmt t 2)) (str "speed = " (fmt (* 0.75 t) 2))])]))

(defmethod scene/draw [:galileo/third-day-1 :parallels] [_ _ p {:keys [palette]}]
  (let [fs (a/lagged p 24 0.3 0 0.8)]
    (into [:g (speed-axes palette 1) (triangle-aeb palette 1 0.4)
           (readout palette (a/play p 0.7 0.95) [4.6 2.8]
                    ["all the speeds together" "= the distance" "(Oresme, c. 1350)"])]
          (map-indexed (fn [i f] (let [t (* 4 (/ (+ i 0.5) 24))]
                                   (stroke [[t 0] [t (* 0.75 t)]] (:found palette) 0.05 f)))
                       fs))))

(defmethod scene/draw [:galileo/third-day-1 :rectangle] [_ _ p {:keys [palette]}]
  [:g (speed-axes palette 1) (triangle-aeb palette 1 0.6)
   (rectangle-agfb palette (a/play p 0.1 0.7) (a/play p 0.5 0.9))
   (readout palette (a/play p 0.6 0.9) [4.6 2.8] ["BF = FE: half the last speed" "AGFB: uniform motion"])])

(defmethod scene/draw [:galileo/third-day-1 :swap] [_ _ p {:keys [palette]}]
  (let [{:keys [A E F G I]} mean-pts
        s (a/there-and-back-with-pause (a/play p 0.05 0.95 a/linear))
        ;; triangle IEF turned half a turn about I lands on AGI
        moved (map #(plane/rotate-about % I (* m/pi s)) [I E F])]
    [:g (speed-axes palette 1) (triangle-aeb palette 1 0) (rectangle-agfb palette 1 0)
     (svg/polygon [A G I] {:fill (:construction palette) :opacity 0.5})
     (svg/polygon moved {:fill (:found palette) :opacity 0.6 :stroke (:found palette) :width 0.02})
     (readout palette 1 [4.6 2.8] ["AGI: what is lacking" "IEF: what makes it up" "equal triangles"])]))

(defmethod scene/draw [:galileo/third-day-1 :conclusion] [_ _ p {:keys [palette]}]
  [:g (speed-axes palette 1) (triangle-aeb palette 1 1) (rectangle-agfb palette 1 (a/play p 0 0.5))
   (svg/layer (a/fade (a/play p 0.3 0.7) [0 -0.2])
              (write palette [2 3.3] "triangle AEB = rectangle AGFB" {:size 0.24}))
   (readout palette (a/play p 0.5 0.9) [4.6 2.8] ["distance = time x mean speed" "s = T (aT)/2"])])

;; ---------------------------------------------------------------------------
;; Third Day, Corollary I: the odd numbers

;; ---------------------------------------------------------------------------
;; Third Day, Theorem II: the squares of the times (Fig. 48)

(defn- two-times
  "Times AD = 1 and AE = s on the time line, the speed triangles over them."
  [palette s fd fe]
  (let [A [0 0] D [1 0] E [s 0] O [1 0.75] P [s (* 0.75 s)]]
    [:g (speed-axes palette 1)
     (when (pos? fe) (svg/polygon [A E P] {:fill (:found palette) :opacity (* 0.25 fe)}))
     (when (pos? fd) (svg/polygon [A D O] {:fill (:construction palette) :opacity (* 0.5 fd)}))
     (stroke [D O] (:construction palette) 0.03 fd) (stroke [E P] (:found palette) 0.03 fe)
     (label palette D "D" fd {:dx 0 :dy -0.3}) (label palette O "O" fd)
     (label palette E "E" fe {:dx 0 :dy -0.3}) (label palette P "P" fe)]))

(defmethod scene/draw [:galileo/third-day-2 :two-times] [_ _ p {:keys [palette]}]
  (let [s (+ 1 (* 3 (a/play p 0.3 0.9)))]
    [:g (two-times palette s (a/play p 0 0.3) (a/play p 0.2 0.5))
     (readout palette (a/play p 0.3 0.6) [4.6 2.8] [(str "AE : AD = " (fmt s 2)) (str "EP : DO = " (fmt s 2))])]))

(defmethod scene/draw [:galileo/third-day-2 :squares] [_ _ p {:keys [palette]}]
  (let [s (+ 1 (* 3 (a/there-and-back-with-pause (a/play p 0 1 a/linear))))]
    [:g (two-times palette s 1 1)
     (readout palette 1 [4.6 2.8] [(str "AE : AD = " (fmt s 2))
                                   (str "distances: " (fmt (* s s) 2) " : 1")
                                   "the square of the ratio of the times"])]))

(def ^:private unit 0.42)

(defn- fall-column
  "The falling body's marks at the ends of n equal times, from the top."
  [palette n f]
  (let [x 0.3 top 7.2]
    (into [:g (stroke [[x top] [x (- top (* unit 0.25 16))]] (:muted palette) 0.02 f)]
          (for [i (range (inc n)) :let [y (- top (* unit 0.25 i i))]]
            [:g (dot [x y] (:found palette) 0.08 f)
             (svg/layer (a/fade f) (write palette [(- x 0.15) (- y 0.06)] (str (* i i)) {:anchor "end" :size 0.17 :mono? true}))]))))

(defn- steps-bars
  "Bars 1 3 5 7, one per equal time, rising lagged."
  [palette fs]
  (into [:g]
        (map-indexed (fn [i f] (let [h (* unit (+ 1 (* 2 i)) f) x (+ 1.3 (* 1.1 i))]
                                 [:g (svg/polygon (plane/rect x (+ x 0.7) 0.2 (+ 0.2 h))
                                                  {:fill (:found palette) :opacity 0.6})
                                  (svg/layer (a/fade f) (write palette [(+ x 0.35) (+ 0.35 h)] (str (+ 1 (* 2 i))) {:mono? true}))]))
                     fs)))

(def ^:private gnomon-colours [:found :construction :ink :found :construction])

(defn- gnomon-cells
  "The unit squares of the n-th gnomon (n from 0): the L that turns the
   n by n square into the n+1 by n+1 square."
  [n]
  (concat (for [i (range (inc n))] [i n]) (for [j (range n)] [n j])))

(defn- gnomons
  "The square of side k built of gnomons, gnomon i at progress (fs i), with
   its lower-left corner at o and cells of side c."
  [palette [ox oy] c fs]
  (into [:g]
        (for [[n f] (map-indexed vector fs) [i j] (gnomon-cells n) :when (pos? f)]
          (svg/polygon (plane/rect (+ ox (* i c)) (+ ox (* (inc i) c)) (+ oy (* j c)) (+ oy (* (inc j) c)))
                       {:fill ((nth gnomon-colours n) palette) :opacity (* 0.55 f)
                        :stroke (:background palette) :width 0.02}))))

(defmethod scene/draw [:galileo/corollary-1 :staircase] [_ _ p {:keys [palette]}]
  [:g (fall-column palette 4 (a/play p 0 0.35))
   (steps-bars palette (a/lagged p 4 0.6 0.35 0.9))
   (readout palette (a/play p 0.8 1) [1.3 6.6] ["fallen: 1, 4, 9, 16" "in each time: 1, 3, 5, 7"])])

(defmethod scene/draw [:galileo/corollary-1 :gnomons] [_ _ p {:keys [palette]}]
  (let [fs (a/lagged p 4 0.8 0.05 0.85)]
    [:g (steps-bars palette [1 1 1 1])
     (gnomons palette [6 0.2] 0.6 fs)
     (readout palette (a/play p 0.6 0.95) [1.3 6.6]
              ["1 + 3 = 4,  + 5 = 9,  + 7 = 16" "each odd number is a gnomon" "(Pythagoreans; Euclid II)"])]))

(defmethod scene/draw [:galileo/corollary-1 :algebra] [_ _ p {:keys [palette]}]
  (let [n 3]
    [:g (gnomons palette [6 0.2] 0.6 [0.35 0.35 0.35 1])
     (svg/layer (a/fade (a/play p 0.1 0.4))
                (write palette [7.2 2.95] "n" {:italic? true})
                (write palette [8.65 1.0] "n + 1" {:italic? true :anchor "start"}))
     (svg/layer (a/fade (a/play p 0.3 0.6) [0 -0.2])
                (write palette [3 5.6] "(n + 1)² − n² = 2n + 1" {:size 0.3}))
     (readout palette (a/play p 0.6 0.9) [1.3 4.6]
              [(str "n = " n ": 16 - 9 = 7") "Emmy: difference simplifies to 0"])]))

;; ---------------------------------------------------------------------------
;; The inclined plane

(def ^:private groove-len 12)
(def ^:private groove-lift 1.2)

(defn- groove-dir []
  (let [l (m/hypot groove-len groove-lift)] [(/ groove-len l) (/ groove-lift l)]))

(defn- groove [palette f]
  [:g (stroke [[0 1.6] [groove-len (- 1.6 groove-lift)]] (:ink palette) 0.06 f)
   (when (pos? f) (svg/polygon [[0 1.6] [0 0.2] [groove-len 0.2] [groove-len (- 1.6 groove-lift)]]
                               {:fill (:muted palette) :opacity (* 0.25 f)}))])

(defn- ball-at
  "Ball centre at time t (accel a along the groove), by the :roll figure."
  [figures a t]
  (let [[co si] (groove-dir)]
    (first (figure/points (:roll figures) [a co si] [[t]]))))

(def ^:private roll-a
  "The acceleration that brings the ball to the foot in 4 times."
  (/ (* 2 groove-len) 16))

(defn- marks [palette n f]
  (let [[co si] (groove-dir)]
    (into [:g]
          (for [i (range 1 (inc n)) :let [d (* 0.5 roll-a i i) at [(* d co) (- 1.6 (* d si))]]]
            [:g (stroke [(plane/translate at [0 -0.1]) (plane/translate at [0 0.35])] (:found palette) 0.04 f)
             (svg/layer (a/fade f) (write palette (plane/translate at [0 0.45]) (str (* i i)) {:mono? true}))]))))

(defn- water-clock
  "Vessel, jet and glass; the glass filled to fraction w."
  [palette w f]
  (let [x 9.5 y 2.9]
    (svg/layer (a/fade f)
               (svg/polygon (plane/rect x (+ x 1) y (+ y 0.6)) {:fill (:construction palette) :opacity 0.4 :stroke (:construction palette) :width 0.02})
               (when (< 0 w 1) (svg/segment [(+ x 0.5) y] [(+ x 0.5) 2.25] {:stroke (:construction palette) :width 0.02}))
               (svg/polygon (plane/rect (+ x 0.3) (+ x 0.7) 1.75 2.25) {:stroke (:ink palette) :width 0.015})
               (svg/polygon (plane/rect (+ x 0.3) (+ x 0.7) 1.75 (+ 1.75 (* 0.5 w))) {:fill (:construction palette) :opacity 0.7}))))

(defmethod scene/draw [:galileo/inclined-plane :groove] [_ _ p {:keys [palette figures]}]
  [:g (groove palette (a/play p 0 0.5))
   (dot (ball-at figures roll-a 0) (:found palette) 0.15 (a/play p 0.4 0.6))
   (readout palette (a/play p 0.5 0.8) [0.3 3.0] ["12 cubits, lined with parchment" "one end lifted 1 or 2 cubits"])])

(defmethod scene/draw [:galileo/inclined-plane :clock] [_ _ p {:keys [palette figures]}]
  (let [t (* 4 (a/play p 0.15 0.85 a/linear))]
    [:g (groove palette 1)
     (dot (ball-at figures roll-a t) (:found palette) 0.15 1)
     (water-clock palette (/ t 4) (a/play p 0 0.15))
     (readout palette 1 [0.3 3.0] [(str "water weighed: " (fmt (/ t 4) 2) " of the glass") "weights are as the times"])]))

(defmethod scene/draw [:galileo/inclined-plane :roll] [_ _ p {:keys [palette figures]}]
  (let [t (* 4 (a/play p 0.05 0.75 a/linear))
        n (int (m/floor (+ t 1e-9)))]
    [:g (groove palette 1) (marks palette n 1)
     (dot (ball-at figures roll-a t) (:found palette) 0.15 1)
     (water-clock palette (/ t 4) 1)
     (readout palette (a/play p 0.75 0.95) [0.3 3.0] ["marks at 1 : 4 : 9 : 16" "steps 1, 3, 5, 7"])]))

(defmethod scene/draw [:galileo/inclined-plane :quarter] [_ _ p {:keys [palette figures]}]
  (let [t (* 2 (a/play p 0.05 0.6 a/linear))]
    [:g (groove palette 1) (marks palette 4 0.5)
     (dot (ball-at figures roll-a t) (:found palette) 0.15 1)
     (water-clock palette (/ t 4) 1)
     (readout palette (a/play p 0.6 0.9) [0.3 3.0] ["a quarter of the length" "in half the time: (1/4)^(1/2) = 1/2"])]))

;; ---------------------------------------------------------------------------
;; Fourth Day, Theorem I: the semi-parabola (Fig. 108)

(def ^:private u 1.0)
(def ^:private k 0.25)

(defn- path [figures t1]
  (mapv vec (figure/points (:throw figures) [u k] (mapv vector (sample identity 0 t1 48)))))

(defn- ledge [palette f]
  [:g (when (pos? f) (svg/polygon (plane/rect -1.6 0 -0.3 0) {:fill (:muted palette) :opacity (* 0.4 f)}))
   (stroke [[-1.6 0] [0 0]] (:ink palette) 0.03 f)
   (stroke [[0 0] [0 -4.3]] (:muted palette) 0.015 f)
   (label palette [0 0] "b" f {:dx -0.22 :dy 0.08}) (label palette [0 -4.3] "n" f {:dx -0.25 :dy 0})])

(def ^:private letters-top ["c" "d" "e" "f"])
(def ^:private letters-fall ["i" "f" "h" "m"])

(defn- time-ticks [palette fs]
  (into [:g (stroke [[0 0] [4 0]] (:construction palette) 0.02 (first fs))]
        (map-indexed (fn [i f] (let [x (* u (inc i))]
                                 [:g (dot [x 0] (:construction palette) 0.06 f)
                                  (label palette [x 0] (letters-top i) f {:dx -0.05 :dy 0.12})]))
                     fs)))

(defn- falls [palette fs]
  (into [:g]
        (map-indexed (fn [i f] (let [x (* u (inc i)) y (* -1 k (inc i) (inc i))]
                                 [:g (stroke [[x 0] [x y]] (:found palette) 0.025 f)
                                  (dot [x y] (:found palette) 0.07 f)
                                  (label palette [x y] (letters-fall i) f)]))
                     fs)))

(defmethod scene/draw [:galileo/fourth-day-1 :ledge] [_ _ p {:keys [palette]}]
  (let [x (+ -1.5 (* 1.5 (a/play p 0.3 0.9 a/linear)))]
    [:g (ledge palette (a/play p 0 0.3))
     (dot [x 0.12] (:found palette) 0.12 1)
     (readout palette (a/play p 0.5 0.8) [1.5 0.9] ["uniform along the plane ab" "at b: falls as well"])]))

(defmethod scene/draw [:galileo/fourth-day-1 :times] [_ _ p {:keys [palette]}]
  [:g (ledge palette 1) (time-ticks palette (a/lagged p 4 0.5 0.1 0.8))
   (readout palette (a/play p 0.6 0.9) [1.5 0.9] ["bc = cd = de: equal times"])])

(defmethod scene/draw [:galileo/fourth-day-1 :falls] [_ _ p {:keys [palette]}]
  [:g (ledge palette 1) (time-ticks palette [1 1 1 1]) (falls palette (a/lagged p 4 0.6 0.05 0.8))
   (readout palette (a/play p 0.6 0.9) [1.5 0.9] ["ci : df : eh = 1 : 4 : 9" "the squares of the times"])])

(defmethod scene/draw [:galileo/fourth-day-1 :trace] [_ _ p {:keys [palette figures]}]
  (let [t1 (* 4 (a/play p 0.05 0.9 a/linear))
        ps (path figures (max t1 1e-3))]
    [:g (ledge palette 1) (time-ticks palette [1 1 1 1]) (falls palette [0.5 0.5 0.5 0.5])
     (svg/polyline ps {:stroke (:ink palette) :width 0.035})
     (dot (peek ps) (:found palette) 0.12 1)
     (readout palette 1 [1.5 0.9] [(str "t = " (fmt t1 2) ": across " (fmt (* u t1) 2) ", down " (fmt (* k t1 t1) 2))])]))

(defmethod scene/draw [:galileo/fourth-day-1 :symptom] [_ _ p {:keys [palette figures]}]
  (let [t (+ 0.6 (* 3.3 (a/there-and-back-with-pause (a/play p 0 1 a/linear))))
        [x y] (first (figure/points (:throw figures) [u k] [[t]]))
        o [0 y]]
    [:g (ledge palette 1) (svg/polyline (path figures 4) {:stroke (:ink palette) :width 0.035})
     (svg/polygon (plane/rect 0 x y (+ y (* x 0.25))) {:fill (:construction palette) :opacity 0.15})
     (stroke [o [x y]] (:construction palette) 0.04 1)
     (stroke [[0 0] o] (:found palette) 0.06 1)
     (dot [x y] (:found palette) 0.1 1)
     (label palette o "o" 1 {:dx -0.25 :dy -0.05})
     (readout palette 1 [1.5 0.9]
              [(str "ordinate = " (fmt x 3) ", abscissa = " (fmt (- y) 3))
               (str "ordinate² / abscissa = " (fmt (/ (* x x) (- y)) 3))
               "= p, the same everywhere: Conics I.11"])]))

(defmethod scene/draw [:galileo/fourth-day-1 :conclusion] [_ _ p {:keys [palette figures]}]
  [:g (ledge palette 1) (svg/polyline (path figures 4) {:stroke (:found palette) :width 0.05})
   (svg/layer (a/fade (a/play p 0.2 0.6) [0 -0.2])
              (write palette [2.6 -1.2] "x² = p y: a semi-parabola" {:size 0.26}))])

;; ---------------------------------------------------------------------------
;; Fourth Day, Prop. VII: 45 degrees

(def ^:private v2g
  "v^2 / g of every shot: the range at 45 degrees."
  4)

(defn- shot
  "The arc of a shot at elevation deg with v^2/g = 4: 41 points of the
   :shot raster kernel (params [deg v2g], state [u])."
  [figures deg]
  (figure/points (:shot figures) [deg v2g] (mapv (fn [i] [(/ i 40)]) (range 41))))

(defn- ground [palette f] (stroke [[-0.2 0] [4.4 0]] (:muted palette) 0.02 f))

(def ^:private elevations [15 30 45 60 75])

(defmethod scene/draw [:galileo/fourth-day-7 :fan] [_ _ p {:keys [palette figures]}]
  (let [fs (a/lagged p 5 0.5 0.05 0.85)]
    (into [:g (ground palette 1)
           (readout palette (a/play p 0.7 0.95) [0.1 2.6] ["one speed, five elevations" "range = 2cw/g"])]
          (map (fn [deg f] (stroke (shot figures deg) (if (= 45 deg) (:found palette) (:construction palette)) 0.03 f))
               elevations fs))))

(defmethod scene/draw [:galileo/fourth-day-7 :square] [_ _ p {:keys [palette figures]}]
  ;; c, w: the speed's components (v = 1), by the :speed raster kernel
  (let [deg (+ 10 (* 70 (a/there-and-back (a/play p 0 1 a/linear))))
        arc (shot figures deg)
        [[c w]] (figure/points (:speed figures) [deg] [[1]])
        s 1.4]
    [:g (ground palette 1) (svg/polyline arc {:stroke (:construction palette) :width 0.03})
     ;; Euclid II.5 on the line c + w: rectangle c by w under the square on the half
     (svg/polygon (plane/rect 0.2 (+ 0.2 (* s c)) 1.2 (+ 1.2 (* s w))) {:fill (:construction palette) :opacity 0.35})
     (svg/polygon (plane/rect 0.2 (+ 0.2 (* s (/ (+ c w) 2))) 1.2 (+ 1.2 (* s (/ (+ c w) 2)))) {:stroke (:found palette) :width 0.02})
     (readout palette 1 [2.2 2.6] [(str "elevation " (fmt deg 0) " deg")
                                   (str "2cw = " (fmt (* 2 c w) 3))
                                   (str "v² − (c − w)² = " (fmt (- 1 (* (- c w) (- c w))) 3))])]))

(defmethod scene/draw [:galileo/fourth-day-7 :best] [_ _ p {:keys [palette figures]}]
  [:g (ground palette 1)
   (into [:g] (map (fn [deg] (svg/polyline (shot figures deg) {:stroke (:muted palette) :width 0.02})) elevations))
   (stroke (shot figures 45) (:found palette) 0.05 (a/play p 0 0.5))
   (stroke [[2 0] [2 1]] (:ink palette) 0.02 (a/play p 0.4 0.7))
   (dot [4 0] (:found palette) 0.08 (a/play p 0.5 0.7))
   (readout palette (a/play p 0.5 0.9) [0.1 2.6] ["45 degrees: c = w" "amplitude 4, altitude 1" "semi-amplitude 2 = double the altitude" "range v²/g, the longest"])])

(defmethod scene/draw [:galileo/fourth-day-7 :pairs] [_ _ p {:keys [palette figures]}]
  (let [[f1 f2] (a/lagged p 2 0.6 0 0.8)
        land (fn [deg] (peek (shot figures deg)))]
    [:g (ground palette 1)
     (stroke (shot figures 30) (:construction palette) 0.035 f1) (stroke (shot figures 60) (:construction palette) 0.035 f1)
     (stroke (shot figures 15) (:found palette) 0.035 f2) (stroke (shot figures 75) (:found palette) 0.035 f2)
     (dot (land 30) (:construction palette) 0.08 f1)
     (dot (land 15) (:found palette) 0.08 f2)
     (readout palette (a/play p 0.6 0.9) [0.1 2.6] ["45 ± d land together" "30 and 60, 15 and 75"])]))
