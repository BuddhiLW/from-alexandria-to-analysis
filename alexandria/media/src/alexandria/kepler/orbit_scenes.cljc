(ns alexandria.kepler.orbit-scenes
  "The drawings of Kepler's laws (alexandria.kepler.orbit), in Manim's idiom
   (alexandria.medium.anim).

     :kepler/war-with-mars  Mars on its ellipse, the Sun at a focus: the
                            circle that failed by 8', the ellipse, the
                            sectors swept in equal times filling one after
                            another while the planet runs fast near the Sun
                            and slow far from it
     :kepler/harmonice      the six planets as points (log a^3, log T^2) on
                            one line of slope 1

   ctx :figures :planet is the planet kernel: params [e a], state [M] (mean
   anomaly), out [x y], Kepler's equation solved inside the kernel. ctx
   :data carries Mars' e, a, b and the third-law rows."
  (:require [alexandria.medium.anim :as a]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.math :as m]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            #?(:cljs [alexandria.medium.player :as player])))

#?(:cljs (def render "Clerk's render-fn for these scenes." player/render))

(defn- fmt [x digits]
  #?(:clj (format (str "%." digits "f") (double x))
     :cljs (.toFixed (js/Number x) digits)))

(defn- write [palette at s opts]
  (svg/text at s (merge {:colour (:ink palette) :size 0.14 :anchor "start"} opts)))

(defn- readout [palette at f lines]
  (svg/layer (a/fade f [0.2 0])
             (into [:g] (map-indexed (fn [i s] (write palette (update at 1 - (* 0.24 i)) s
                                                      {:mono? true :size 0.12}))
                                     lines))))

;; ---------------------------------------------------------------------------
;; Mars on its ellipse

(def ^:private war :kepler/war-with-mars)
(def ^:private sectors 12)
(def ^:private two-pi (* 2 m/pi))

(defn- positions
  "The planet at mean anomalies ms, one kernel batch."
  [{:keys [figures data]} ms]
  (figure/points (:planet figures) [(:e data) (:a data)] (mapv vector ms)))

(defn- orbit-path [ctx] (positions ctx (map #(* two-pi (/ % 120)) (range 121))))

(defn- sun [{:keys [data]}] [(* (:a data) (:e data)) 0])

(defn- sun-dot [palette ctx f]
  (when (pos? f)
    [:g (svg/circle (sun ctx) (* 0.09 f) {:fill (:found palette)})
     (svg/layer (a/fade f) (write palette (update (sun ctx) 1 - 0.32) "Sun" {:size 0.12 :anchor "middle"
                                                                            :colour (:found palette)}))]))

(defn- planet-dot [palette at]
  (svg/circle at 0.07 {:fill "#d9604c" :stroke (:ink palette) :width 0.01}))

(defn- sector
  "The sector swept from mean anomaly m0 to m1, Sun at the corner."
  [ctx m0 m1 colour opacity]
  (let [arc (positions ctx (map #(+ m0 (* (- m1 m0) (/ % 16))) (range 17)))]
    (svg/polygon (cons (sun ctx) arc) {:fill colour :opacity opacity :stroke colour :width 0.008})))

(defn- ellipse [palette ctx f]
  (svg/polyline (orbit-path ctx) {:stroke (:ink palette) :width 0.02 :attrs (a/create f)}))

(defn- circle-at
  "Points of the circle of radius a at angles ts: the :circle kernel."
  [{:keys [figures data]} ts]
  (figure/points (:circle figures) [(:a data)] (mapv vector ts)))

(defn- circle-path [ctx] (circle-at ctx (map #(* two-pi (/ % 120)) (range 121))))

(defmethod scene/draw [war :circle] [_ _ p {:keys [palette data] :as ctx}]
  (let [{:keys [a]} data
        circle (circle-path ctx)
        equant [(* 2 a (:e data)) 0]]
    [:g (svg/polyline circle {:stroke (:construction palette) :width 0.02 :attrs (a/create (a/play p 0 0.4))})
     (sun-dot palette ctx (a/play p 0.3 0.5))
     (svg/layer (a/fade (a/play p 0.45 0.65))
                (svg/circle equant 0.04 {:fill (:muted palette)})
                (write palette (update equant 1 + 0.12) "equant" {:size 0.1 :colour (:muted palette)}))
     (planet-dot palette (first (circle-at ctx [(* two-pi (a/play p 0.4 1 a/linear))])))
     (readout palette [1.9 1.3] (a/play p 0.5 0.7)
              ["a circle with an equant" "fitted to Tycho's oppositions"])]))

(defmethod scene/draw [war :eight] [_ _ p {:keys [palette] :as ctx}]
  (let [circle (circle-path ctx)
        ;; at 45 degrees the circle and the true place differ by 8': drawn magnified
        t (* 0.25 m/pi)
        on-circle (first (circle-at ctx [t]))
        true-place (first (positions ctx [t]))
        k (a/there-and-back-with-pause p)]
    [:g (svg/polyline circle {:stroke (:construction palette) :width 0.02 :opacity 0.5})
     (ellipse palette ctx (a/play p 0 0.4))
     (sun-dot palette ctx 1)
     (svg/segment (sun ctx) on-circle {:stroke (:construction palette) :width 0.012})
     (svg/segment (sun ctx) true-place {:stroke "#d9604c" :width 0.012})
     (svg/circle on-circle (+ 0.06 (* 0.25 k)) {:stroke (:found palette) :width 0.015})
     (readout palette [1.9 1.3] (a/play p 0.3 0.5)
              ["45 degrees from the apsides:" "circle and sky differ by 8'"
               "Ptolemy's limit: 10'" "Tycho's: finer than 8'"])]))

(defmethod scene/draw [war :ellipse] [_ _ p {:keys [palette data] :as ctx}]
  (let [{:keys [a e]} data
        other [(- (* a e)) 0]
        mm (* two-pi (a/play p 0.2 1 a/linear))
        at (first (positions ctx [mm]))]
    [:g (ellipse palette ctx 1)
     (sun-dot palette ctx 1)
     (svg/circle other 0.04 {:fill (:muted palette)})
     (svg/segment (sun ctx) at {:stroke (:found palette) :width 0.012})
     (svg/segment other at {:stroke (:construction palette) :width 0.012})
     (planet-dot palette at)
     (readout palette [1.9 1.3] (a/play p 0 0.25)
              ["the two focal distances" "always sum to 2a"
               "(Apollonius III.52)" (str "e = " (fmt e 5) ", a = " (fmt a 4))])]))

(defn- swept
  "Sectors 0..n-1 of `sectors` equal times, the k-th filled to f_k."
  [{:keys [palette] :as ctx} fs]
  (into [:g] (map-indexed (fn [k f]
                            (when (pos? f)
                              (let [m0 (* two-pi (/ k sectors))
                                    m1 (+ m0 (* f (/ two-pi sectors)))]
                                (sector ctx m0 m1 (if (even? k) (:found palette) (:construction palette)) 0.35))))
                          fs)))

(defmethod scene/draw [war :sweep] [_ _ p {:keys [palette] :as ctx}]
  (let [f (a/play p 0.1 0.9 a/linear)
        mm (* f (/ two-pi sectors))
        at (first (positions ctx [mm]))]
    [:g (ellipse palette ctx 1) (sun-dot palette ctx 1)
     (swept ctx [f])
     (svg/segment (sun ctx) at {:stroke (:found palette) :width 0.014})
     (planet-dot palette at)
     (readout palette [1.9 1.3] (a/play p 0 0.25)
              ["area from perihelion:" "A = (ab/2)(E - e sin E)"])]))

(defmethod scene/draw [war :equal-areas] [_ _ p {:keys [palette] :as ctx}]
  (let [s (a/play p 0.02 0.98 a/linear)
        fs (mapv (fn [k] (a/clamp01 (- (* s sectors) k))) (range sectors))
        at (first (positions ctx [(* two-pi s)]))]
    [:g (ellipse palette ctx 1) (sun-dot palette ctx 1)
     (swept ctx fs)
     (svg/segment (sun ctx) at {:stroke (:found palette) :width 0.014})
     (planet-dot palette at)
     (readout palette [1.9 1.3] (a/play p 0 0.2)
              ["12 equal times," "12 equal areas:" "A = (ab/2) M"
               "fast near the Sun," "slow far from it"])]))

(defmethod scene/draw [war :solve] [_ _ p {:keys [palette data figures] :as ctx}]
  (let [{:keys [e]} data
        mm (+ 0.3 (* 2.2 (a/there-and-back p)))
        ;; the fixed-point steps E <- M + e sin E, computed by the :steps-k
        ;; kernels (each the pair E_k, E_k+1), shown one by one
        steps (into [] cat (for [k [:steps-0 :steps-2 :steps-4]]
                             (first (figure/points (k figures) [e] [[mm]]))))
        n (int (m/floor (* 5.99 (a/play p 0.1 0.8 a/linear))))
        at (first (positions ctx [mm]))]
    [:g (ellipse palette ctx 1) (sun-dot palette ctx 1)
     (planet-dot palette at)
     (readout palette [1.9 1.3] 1
              (concat [(str "M = " (fmt mm 4))]
                      (map-indexed (fn [i E] (str "E" i " = " (fmt E 8))) (take (inc n) steps))))]))

;; ---------------------------------------------------------------------------
;; T^2 against a^3, log-log

(def ^:private harmonice :kepler/harmonice)

(def ^:private names
  {:mercury "Mercury" :venus "Venus" :earth "Earth" :mars "Mars" :jupiter "Jupiter" :saturn "Saturn"})

(defn- plot-xy
  "Plane point of a row: x = log10 a^3, y = log10 T^2, both computed by a
   raster kernel on the JVM (alexandria.kepler.orbit/third-law), scaled to
   the frame."
  [{:keys [log-a3 log-T2]}]
  [(* 0.9 log-a3) (* 0.9 log-T2)])

(defn- axes [palette f]
  [:g (svg/segment [-1.6 -1.6] [3.2 -1.6] {:stroke (:muted palette) :width 0.012 :attrs (a/create f)})
   (svg/segment [-1.6 -1.6] [-1.6 3.2] {:stroke (:muted palette) :width 0.012 :attrs (a/create f)})
   (svg/layer (a/fade f)
              (write palette [3.2 -1.85] "log a^3" {:size 0.12 :anchor "end" :colour (:muted palette)})
              (write palette [-1.7 3.2] "log T^2" {:size 0.12 :anchor "end" :colour (:muted palette)}))])

(defn- dots [palette rows fs]
  (into [:g] (map (fn [{:keys [planet] :as row} f]
                    (when (pos? f)
                      (let [xy (plot-xy row)]
                        [:g (svg/circle xy (* 0.07 f) {:fill (:found palette)})
                         (svg/layer (a/fade f) (write palette (update (update xy 0 + 0.12) 1 - 0.05)
                                                      (names planet) {:size 0.11}))])))
                  rows fs)))

(defmethod scene/draw [harmonice :table] [_ _ p {:keys [palette data]}]
  (let [rows (:rows data)
        fs (a/lagged p (count rows) 0.4 0 0.8)]
    (into [:g (write palette [-1.6 3.0] "planet    T (years)   a (Earth = 1)   T^2/a^3"
                     {:mono? true :size 0.12 :colour (:muted palette)})]
          (map-indexed (fn [i [{:keys [planet T a k]} f]]
                         (svg/layer (a/fade f [0.3 0])
                                    (write palette [-1.6 (- 2.6 (* 0.36 i))]
                                           (str (subs (str (names planet) "          ") 0 10)
                                                (fmt T 3) "      " (fmt a 4) "          " (fmt k 4))
                                           {:mono? true :size 0.12})))
                       (map vector rows fs)))))

(defmethod scene/draw [harmonice :points] [_ _ p {:keys [palette data]}]
  (let [rows (:rows data)]
    [:g (axes palette (a/play p 0 0.3))
     (dots palette rows (a/lagged p (count rows) 0.5 0.25 0.95))]))

(defmethod scene/draw [harmonice :line] [_ _ p {:keys [palette data]}]
  (let [rows (:rows data)
        f (a/play p 0.1 0.6)
        {:keys [min max]} (:spread data)]
    [:g (axes palette 1)
     (svg/segment [-1.5 -1.5] (let [t (+ -1.5 (* 4.6 f))] [t t]) {:stroke (:construction palette) :width 0.02})
     (dots palette rows (repeat 1))
     (readout palette [0.2 -0.6] (a/play p 0.5 0.8)
              ["one line, slope 1:" "T^2 / a^3 the same for all"
               (str "spread " (fmt min 4) " .. " (fmt max 4))])]))

(defmethod scene/draw [harmonice :newton] [_ _ p {:keys [palette data]}]
  [:g (axes palette 1)
   (svg/segment [-1.5 -1.5] [3.1 3.1] {:stroke (:construction palette) :width 0.02})
   (dots palette (:rows data) (repeat 1))
   (readout palette [0.2 -0.6] (a/play p 0.1 0.4)
            ["Newton, Principia I:" "Prop. 1: equal areas -> central force"
             "Prop. 11: ellipse, focus -> 1/r^2"])])
