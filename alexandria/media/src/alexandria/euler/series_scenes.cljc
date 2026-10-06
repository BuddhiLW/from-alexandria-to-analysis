(ns alexandria.euler.series-scenes
  "Euler's infinite polynomials, drawn (alexandria.medium.scene/draw):

     :euler/basel        sin x / x and the partial products
                         prod (1 - x^2/(n^2 pi^2)) closing in on it (the
                         :product figure of alexandria.euler.series), the
                         x^2 coefficients matched, the partial sums climbing
                         to pi^2/6
     :euler/exponential  the terms (ix)^k/k! laid end to end, a spiral of
                         segments (the :spiral figure) converging on the
                         unit circle at angle x; ctx :controls :x"
  (:require [alexandria.euler.draw :as d]
            [alexandria.medium.anim :as a]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.math :as m]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            #?(:cljs [alexandria.medium.player :as player])))

#?(:cljs (def render
           "Clerk's render-fn for these scenes."
           player/render))

;; ---------------------------------------------------------------------------
;; Basel: the curve and its factors

(def ^:private x-max 10)
(def ^:private xs (mapv (fn [i] [(- (* 2 x-max (/ i 240)) x-max)]) (range 241)))

(defn- sinc [x] (if (< (m/abs x) 1e-9) 1 (/ (m/sin x) x)))

(defn- sinc-curve [palette f]
  (d/stroke (mapv (fn [[x]] [x (sinc x)]) xs) (:ink palette) 0.03 f))

(defn- product-curve
  "The partial product with m factors (m may be fractional), from the
   figure, clipped to the window."
  [palette figures mm f]
  (let [ps (figure/points (:product figures) [mm] xs)]
    (d/stroke (mapv (fn [[x y]] [x (max -1.15 (min 2.1 y))]) ps) (:found palette) 0.035 f)))

(defn- roots [palette f]
  (into [:g] (for [k (range -3 4) :when (not (zero? k))
                   :let [x (* k m/pi)]]
               [:g (d/dot [x 0] (:construction palette) 0.09 f)
                (d/label palette [x 0] (str (when (neg? k) "-") (when (not= 1 (m/abs k)) (m/abs k)) "π") f
                         {:dx -0.3 :dy -0.38 :size 0.11 :colour (:construction palette)})])))

(defn- basel-frame [palette f] (d/axes palette [(- x-max) x-max] [-1.1 2.0] f))

(def ^:private basel-id :euler/basel)

(defmethod scene/draw [basel-id :roots] [_ _ p {:keys [palette]}]
  (let [g (fn [x] (* (- 1 (/ x 2)) (+ 1 (/ x 3)) (- 1 (/ x 6)) 0.25))]
    [:g (basel-frame palette 1)
     (d/stroke (d/sample (fn [x] [x (g x)]) -4 7.5 120) (:found palette) 0.035 (a/play p 0 0.5))
     (into [:g] (map (fn [r f] (d/dot [r 0] (:construction palette) 0.1 f)) [2 -3 6] (a/lagged p 3 0.5 0.4 0.7)))
     (d/readout palette (a/play p 0.6 0.85) [-10 1.95]
                ["(1 - x/2)(1 + x/3)(1 - x/6): roots 2, -3, 6, constant term 1"
                 "x coefficient: -(1/2 - 1/3 + 1/6) = -(sum of 1/root)"])]))

(defmethod scene/draw [basel-id :sinc] [_ _ p {:keys [palette]}]
  [:g (basel-frame palette 1) (sinc-curve palette (a/play p 0 0.6)) (roots palette (a/play p 0.5 0.8))
   (d/readout palette (a/play p 0.6 0.85) [-10 1.95] ["sin x / x = 1 - x²/6 + x⁴/120 - ..." "zero at ±π, ±2π, ±3π, ..."])])

(defmethod scene/draw [basel-id :product] [_ _ p {:keys [palette figures]}]
  (let [mm (+ 1 (* 15 (a/play p 0.05 0.9 a/rush-from)))]
    [:g (basel-frame palette 1) (sinc-curve palette 1) (roots palette 1)
     (product-curve palette figures mm 1)
     (d/readout palette 1 [-10 1.95]
                [(str "factors: " (long (m/floor mm)))
                 "(1 - x²/π²)(1 - x²/4π²)(1 - x²/9π²) ..."])]))

(defmethod scene/draw [basel-id :match] [_ _ p {:keys [palette figures]}]
  (let [zoom (a/play p 0 0.4)]
    [:g {:transform (str "scale(" (+ 1 (* 2 zoom)) "," (+ 1 (* 0.6 zoom)) ")")}
     (basel-frame palette 1) (sinc-curve palette 1) (product-curve palette figures 16 1)
     (d/readout palette (a/play p 0.35 0.6) [-3.3 1.15]
                ["x² in sin x / x:   -1/6"
                 "x² in the product: -(1/π² + 1/4π² + 1/9π² + ...)"
                 "so 1 + 1/4 + 1/9 + ... = π²/6"] {:size 0.05 :step 0.12})]))

(defn- bars
  "The partial sums S_1..S_n as bars of width w from x0, height scale h, and
   the line pi^2/6."
  [palette {:keys [partial-sums target]} n f]
  (let [w 0.45 x0 -9.5 h 1.1]
    (into [:g (d/stroke [[x0 (* h target)] [(+ x0 (* w 40)) (* h target)]] (:construction palette) 0.02 f "0.15 0.1")
           (d/label palette [(+ x0 (* w 40)) (* h target)] "π²/6" f {:dx -1.2 :dy 0.12 :size 0.11})]
          (map-indexed (fn [i s] (svg/layer {:opacity (if (< i n) f 0)}
                                            (svg/polygon [[(+ x0 (* i w)) 0] [(+ x0 (* i w) (* 0.8 w)) 0]
                                                          [(+ x0 (* i w) (* 0.8 w)) (* h s)] [(+ x0 (* i w)) (* h s)]]
                                                         {:fill (:found palette) :opacity 0.6})))
                       partial-sums))))

(defmethod scene/draw [basel-id :partial-sums] [_ _ p {:keys [palette data]}]
  (let [n (long (m/floor (+ 1 (* 39 (a/play p 0.05 0.85 a/linear)))))
        s (nth (:partial-sums data) (dec n))]
    [:g (d/axes palette [-9.8 9.5] [-0.1 2] 1) (bars palette data n 1)
     (d/readout palette 1 [-9.5 2.05]
                [(str "N = " n "   S = " (d/fmt s 5))
                 (str "π²/6 - S = " (d/fmt (- (:target data) s) 5) " < 1/N = " (d/fmt (/ 1 n) 5))])]))

(defmethod scene/draw [basel-id :fourth] [_ _ p {:keys [palette figures]}]
  [:g (basel-frame palette 1) (sinc-curve palette 1) (product-curve palette figures 16 0.5)
   (d/readout palette (a/play p 0 0.25) [-10 1.95]
              ["the same move, one coefficient on:"
               "x⁴: 1/120 = sum over pairs 1/(m²n²π⁴)"
               "sum of squares = (sum)² - 2 (pairs)"
               "sum 1/n⁴ = π⁴ (1/36 - 2/120) = π⁴/90"])])

;; ---------------------------------------------------------------------------
;; e^{ix}: the spiral

(def ^:private exp-id :euler/exponential)
(def ^:private terms 16)

(defn- x-of [ctx] (double (get-in ctx [:controls :x] 2.0)))

(defn- vertices
  "The partial sums 0..k of e^{ix} from the :spiral figure (k fractional:
   the last segment drawn to that fraction)."
  [figures x k]
  (let [whole (long (m/floor k))]
    (figure/points (:spiral figures) [x] (conj (mapv vector (range (inc whole))) [k]))))

(defn- unit-circle [palette f]
  (d/stroke (d/sample (fn [t] [(m/cos t) (m/sin t)]) 0 (* 2 m/pi) 120) (:muted palette) 0.012 f))

(defn- spiral-path
  "The segments from 0 to 1 and on through the partial sums, coloured by the
   direction of the term: east-west terms gold (cos), north-south blue
   (sin)."
  [palette ps]
  (let [path (cons [0 0] ps)]
    (into [:g] (map-indexed (fn [k [a b]]
                              (svg/segment a b {:stroke (if (even? k) (:found palette) (:construction palette))
                                                :width (max 0.012 (- 0.04 (* 0.002 k)))}))
                            (map vector path (rest path))))))

(defn- exp-frame [palette f]
  [:g (d/axes palette [-4.2 2.3] [-1.4 3.5] f) (unit-circle palette f)])

(defmethod scene/draw [exp-id :series] [_ _ p {:keys [palette] :as ctx}]
  (let [x (x-of ctx)]
    [:g (exp-frame palette (a/play p 0 0.4))
     (d/readout palette (a/play p 0.2 0.6) [-4.1 3.4]
                ["e^z = 1 + z + z²/2! + z³/3! + ..."
                 (str "z = ix, x = " (d/fmt x 2))])]))

(defmethod scene/draw [exp-id :powers] [_ _ p {:keys [palette]}]
  (let [dirs [[1 0 "1"] [0 1 "i"] [-1 0 "-1"] [0 -1 "-i"]]
        fs (a/lagged p 4 0.6 0.05 0.8)]
    (into [:g (exp-frame palette 1)]
          (map (fn [[dx dy s] f]
                 [:g (d/stroke [[0 0] [dx dy]] (if (zero? dy) (:found palette) (:construction palette)) 0.035 f)
                  (d/label palette [dx dy] s f {:size 0.16})])
               dirs fs))))

(defmethod scene/draw [exp-id :spiral] [_ _ p {:keys [palette figures] :as ctx}]
  (let [x (x-of ctx)
        k (* terms (a/play p 0.02 0.95 a/linear))
        ps (vertices figures x k)
        [ex ey] (peek ps)]
    [:g (exp-frame palette 1) (spiral-path palette ps)
     (d/dot [ex ey] (:ink palette) 0.05 1)
     (d/readout palette 1 [-4.1 3.4]
                [(str "terms: " (long (m/floor k)))
                 (str "sum = " (d/fmt ex 4) " + " (d/fmt ey 4) " i")])]))

(defmethod scene/draw [exp-id :split] [_ _ p {:keys [palette figures] :as ctx}]
  (let [x (x-of ctx)
        ps (vertices figures x terms)
        [ex ey] (peek ps)
        f (a/play p 0.1 0.6)]
    [:g (exp-frame palette 1) (svg/layer {:opacity (- 1 (* 0.6 f))} (spiral-path palette ps))
     (d/stroke [[0 0] [ex 0]] (:found palette) 0.05 f)
     (d/stroke [[ex 0] [ex ey]] (:construction palette) 0.05 f)
     (d/readout palette (a/play p 0.4 0.7) [-4.1 3.4]
                [(str "east-west: 1 - x²/2! + x⁴/4! - ... = cos x = " (d/fmt (m/cos x) 4))
                 (str "north-south: x - x³/3! + ... = sin x = " (d/fmt (m/sin x) 4))])]))

(defmethod scene/draw [exp-id :circle] [_ _ p {:keys [palette figures]}]
  (let [x (+ 0.2 (* 2.94 (a/there-and-back-with-pause p)))
        ps (vertices figures x terms)
        [ex ey] (peek ps)]
    [:g (exp-frame palette 1) (svg/layer {:opacity 0.6} (spiral-path palette ps))
     (d/stroke [[0 0] [ex ey]] (:ink palette) 0.02 1)
     (d/dot [ex ey] (:ink palette) 0.06 1)
     (d/readout palette 1 [-4.1 3.4]
                [(str "x = " (d/fmt x 2) "   |e^{ix}| = " (d/fmt (m/hypot ex ey) 4))])]))

(defmethod scene/draw [exp-id :pi] [_ _ p {:keys [palette figures]}]
  (let [k (* terms (a/play p 0 0.7 a/linear))
        ps (vertices figures m/pi k)
        [ex ey] (peek ps)]
    [:g (exp-frame palette 1) (spiral-path palette ps)
     (d/dot [-1 0] (:found palette) 0.07 (a/play p 0.6 0.8))
     (d/readout palette 1 [-4.1 3.4]
                [(str "x = π: sum = " (d/fmt ex 5) " + " (d/fmt ey 5) " i")])
     (svg/layer (a/fade (a/play p 0.75 0.95) [0 -0.2])
                (d/write palette [-1 -1.25] "e^{iπ} + 1 = 0" {:size 0.2}))]))
