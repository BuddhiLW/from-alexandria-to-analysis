(ns alexandria.cauchy.limit-scenes
  "Two definition passages of the Cours d'analyse drawn step by step, one
   sentence per step (widget brief, section 4), in Cauchy's letters:

     :cauchy/limit           Preliminaires: quantite variable, limite, the
                             circle as the limit of inscribed polygons,
                             infiniment petit
     :cauchy/continuity-ch2  ch. II sec. 2: f(x) between two limits, the
                             increment alpha, the difference
                             f(x + alpha) - f(x) at every x, shrinking with
                             alpha; a jump where it does not

   ctx :data carries every number from raster on the JVM
   (alexandria.cauchy.analysis-view/limit-data, continuity-ch2-data); the
   scenes only place and fade them."
  (:require [alexandria.medium.anim :as a]
            [alexandria.medium.math :as m]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            #?(:cljs [alexandria.medium.player :as player])))

#?(:cljs (def render "Clerk's render-fn for these scenes." player/render))

(defn- fmt [x digits]
  #?(:clj (format (str "%." digits "f") (double x))
     :cljs (.toFixed (js/Number x) digits)))

(defn- write [palette at s opts]
  (svg/text at s (merge {:colour (:ink palette) :size 0.2} opts)))

(defn- readout [palette at f lines]
  (let [[x y] at]
    (svg/layer (a/fade f [0.2 0])
               (into [:g] (map-indexed (fn [i s] (write palette [x (- y (* 0.3 i))] s
                                                        {:anchor "start" :size 0.17 :mono? true}))
                                       lines)))))

;; ---------------------------------------------------------------------------
;; Preliminaires: the variable and its limit
;;
;; data: {:xs [[n x_n] ...] :ranks [{:eps :N} ...] :polygons [{:k :area :vertices}] :pi}

(def ^:private lim :cauchy/limit)
(def ^:private sx 0.15)
(def ^:private sy 1.6)
(defn- at [n x] [(* sx n) (* sy x)])

(defn- dots [palette xs upto colour]
  (into [:g] (for [[n x] xs :when (<= n upto)] (svg/circle (at n x) 0.045 {:fill colour}))))

(defn- n-axis [palette]
  [:g (svg/segment [0 0] [(* sx 41) 0] {:stroke (:muted palette) :width 0.012})
   (write palette [(* sx 41) -0.25] "n" {:italic? true :size 0.17})])

(defn- limit-line [palette f]
  (svg/layer (a/fade f)
             (svg/segment (at 0 1) (at 41 1) {:stroke (:construction palette) :width 0.02 :dash "0.08 0.05"})
             (write palette (at 41.3 1.03) "1" {:anchor "start"})))

(defmethod scene/draw [lim :variable] [_ _ p {:keys [palette data]}]
  (let [upto (* 40 (a/play p 0 0.9 a/linear))]
    [:g (n-axis palette)
     (dots palette (:xs data) upto (:found palette))
     (readout palette [0.1 -0.45] (a/play p 0 0.2) ["x_n = 1 + (-1)^n / n:  2, 0.5, 1.333, 0.75, 1.2, ..."])]))

(defmethod scene/draw [lim :fixed] [_ _ p {:keys [palette data]}]
  [:g (n-axis palette) (dots palette (:xs data) 40 (:found palette))
   (limit-line palette (a/play p 0 0.6))
   (readout palette [0.1 -0.45] (a/play p 0.4 0.8) ["une valeur fixe: 1"])])

(defn- band-at [palette eps N f]
  (let [[x0 y0] (at N (- 1 eps)) [x1 y1] (at 41 (+ 1 eps))]
    (svg/layer (a/fade f)
               (svg/polygon [[0 y0] [x1 y0] [x1 y1] [0 y1]] {:fill (:found palette) :opacity 0.15})
               (svg/polygon [[x0 y0] [x1 y0] [x1 y1] [x0 y1]] {:fill (:construction palette) :opacity 0.25})
               (svg/segment [x0 -0.1] [x0 (+ y1 0.1)] {:stroke (:construction palette) :width 0.015}))))

(defmethod scene/draw [lim :as-little] [_ _ p {:keys [palette data]}]
  (let [ranks (:ranks data)
        i (min (dec (count ranks)) (long (m/floor (* p (count ranks)))))
        {:keys [eps N]} (nth ranks i)]
    [:g (n-axis palette) (limit-line palette 1)
     (band-at palette eps N 1)
     (dots palette (:xs data) 40 (:found palette))
     (readout palette [0.1 -0.45] 1 [(str "eps = " eps ": from n = " N " on, |x_n - 1| = 1/n < eps")])]))

(defmethod scene/draw [lim :limit] [_ _ p {:keys [palette data]}]
  [:g (n-axis palette) (limit-line palette 1)
   (dots palette (:xs data) 40 (:found palette))
   (svg/layer (a/fade (a/play p 0.1 0.5))
              (write palette (at 30 1.25) "la limite: 1" {:anchor "start" :italic? true}))])

(defmethod scene/draw [lim :circle] [_ _ p {:keys [palette data]}]
  (let [ps (:polygons data)
        i (min (dec (count ps)) (long (m/floor (* (a/play p 0 0.9 a/linear) (count ps)))))
        {:keys [k area vertices]} (nth ps i)
        c [3.0 1.1] r 1.1
        place (fn [[x y]] [(+ (first c) (* r x)) (+ (second c) (* r y))])]
    [:g (svg/circle c r {:stroke (:muted palette) :width 0.015})
     (svg/polygon (mapv place vertices) {:fill (:found palette) :opacity 0.3 :stroke (:found palette) :width 0.02})
     (readout palette [0.1 -0.45] 1 [(str k " sides: area = " (fmt area 6) "    circle: pi = " (fmt (:pi data) 6))])]))

(defmethod scene/draw [lim :infinitesimal] [_ _ p {:keys [palette data]}]
  (let [ranks (:ranks data)
        q (a/play p 0 0.9 a/linear)
        i (min (dec (count ranks)) (long (m/floor (* q (count ranks)))))
        {:keys [eps N]} (nth ranks i)
        ds (mapv (fn [[n x]] [n (m/abs (- x 1))]) (:xs data))
        [x0 y0] (at N 0) [x1 y1] (at 41 eps)]
    [:g (n-axis palette)
     (svg/polygon [[0 0] [x1 0] [x1 y1] [0 y1]] {:fill (:found palette) :opacity 0.18})
     (svg/segment [x0 -0.1] [x0 (+ y1 0.15)] {:stroke (:construction palette) :width 0.015})
     (dots palette ds 40 "#e0605a")
     (readout palette [0.1 -0.45] 1 [(str "|x_n - 1| = 1/n: below " eps " from n = " N " on; limit 0")])]))

;; ---------------------------------------------------------------------------
;; ch. II sec. 2: continuity between two limits
;;
;; data: {:curve [[x y] ...] :x0 :X :xs [x ...] :fx [f(x) ...]
;;        :alphas [{:alpha :fxa [f(x + alpha) ...] :max}]
;;        :jump {:curve-l :curve-r :x :alphas [{:alpha :diff}]}}

(def ^:private ch2 :cauchy/continuity-ch2)

(defn- frame [palette {:keys [curve x0 X]} f]
  [:g (svg/segment [0 0] [6 0] {:stroke (:muted palette) :width 0.012})
   (svg/polyline curve {:stroke (:ink palette) :width 0.03 :attrs (a/create f)})
   (svg/layer (a/fade f)
              (svg/segment [x0 -0.12] [x0 0.12] {:stroke (:construction palette) :width 0.02})
              (svg/segment [X -0.12] [X 0.12] {:stroke (:construction palette) :width 0.02})
              (write palette [x0 -0.35] "x₀" {:italic? true})
              (write palette [X -0.35] "X" {:italic? true}))])

(defn- difference [palette x fx alpha fxa f]
  (svg/layer (a/fade f)
             (svg/segment [x fx] [(+ x alpha) fx] {:stroke (:construction palette) :width 0.02})
             (svg/segment [(+ x alpha) fx] [(+ x alpha) fxa] {:stroke "#e0605a" :width 0.04})
             (svg/circle [x fx] 0.04 {:fill (:found palette)})))

(defmethod scene/draw [ch2 :limits] [_ _ p {:keys [palette data]}]
  [:g (frame palette data (a/play p 0 0.7))
   (readout palette [0.1 2.6] (a/play p 0.5 0.8) ["f(x) = 1 + sin x, from x₀ = 0.3 to X = 5.5"])])

(defmethod scene/draw [ch2 :increment] [_ _ p {:keys [palette data]}]
  (let [{:keys [xs fx alphas]} data
        j 4 x (nth xs j) {:keys [alpha]} (first alphas)
        f (a/play p 0.1 0.6)]
    [:g (frame palette data 1)
     (svg/layer (a/fade f)
                (svg/segment [x -0.1] [x (nth fx j)] {:stroke (:muted palette) :width 0.012 :dash "0.05 0.05"})
                (svg/segment [(+ x alpha) -0.1] [(+ x alpha) (nth fx j)] {:stroke (:muted palette) :width 0.012 :dash "0.05 0.05"})
                (svg/segment [x -0.05] [(+ x alpha) -0.05] {:stroke (:construction palette) :width 0.04})
                (write palette [x -0.35] "x" {:italic? true})
                (write palette [(+ x alpha) -0.35] "x + α" {:italic? true}))]))

(defmethod scene/draw [ch2 :difference] [_ _ p {:keys [palette data]}]
  (let [{:keys [xs fx alphas]} data
        j 4 {:keys [alpha fxa]} (first alphas)]
    [:g (frame palette data 1)
     (difference palette (nth xs j) (nth fx j) alpha (nth fxa j) (a/play p 0 0.5))
     (readout palette [0.1 2.6] (a/play p 0.4 0.7)
              [(str "f(x + α) - f(x) = " (fmt (- (nth fxa j) (nth fx j)) 4) "  (x = " (fmt (nth xs j) 2) ", α = " alpha ")")])]))

(defmethod scene/draw [ch2 :every-x] [_ _ p {:keys [palette data]}]
  (let [{:keys [xs fx alphas]} data
        {:keys [alpha fxa max]} (first alphas)
        n (count xs)]
    [:g (frame palette data 1)
     (into [:g] (for [j (range n)]
                  (difference palette (nth xs j) (nth fx j) alpha (nth fxa j) (a/play p (* 0.6 (/ j n)) (+ 0.15 (* 0.6 (/ j n)))))))
     (readout palette [0.1 2.6] (a/play p 0.7 0.9) [(str "α = " alpha ": the largest |f(x + α) - f(x)| = " (fmt max 4))])]))

(defmethod scene/draw [ch2 :decrease] [_ _ p {:keys [palette data]}]
  (let [{:keys [xs fx alphas]} data
        i (min (dec (count alphas)) (long (m/floor (* (a/play p 0 0.95 a/linear) (count alphas)))))
        {:keys [alpha fxa max]} (nth alphas i)]
    [:g (frame palette data 1)
     (into [:g] (for [j (range (count xs))] (difference palette (nth xs j) (nth fx j) alpha (nth fxa j) 1)))
     (readout palette [0.1 2.6] 1 [(str "α = " alpha ":  max |f(x + α) - f(x)| = " (fmt max 5))
                                    "décroît indéfiniment avec α"])]))

(defmethod scene/draw [ch2 :jump] [_ _ p {:keys [palette data]}]
  (let [{:keys [curve-l curve-r x alphas]} (:jump data)
        i (min (dec (count alphas)) (long (m/floor (* (a/play p 0 0.95 a/linear) (count alphas)))))
        {:keys [alpha fx fxa]} (nth alphas i)
        xa (- x (* 0.5 alpha))]
    [:g (svg/segment [0 0] [6 0] {:stroke (:muted palette) :width 0.012})
     (svg/polyline curve-l {:stroke (:ink palette) :width 0.03})
     (svg/polyline curve-r {:stroke (:ink palette) :width 0.03})
     (difference palette xa fx alpha fxa 1)
     (readout palette [0.1 2.6] 1 [(str "a jump of 0.8 at x = " (fmt x 1) ";  α = " alpha)
                                    (str "f(x + α) - f(x) = " (fmt (- fxa fx) 4) " at x = 3 - α/2: it does not decrease")])]))
