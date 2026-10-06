(ns alexandria.weierstrass.monster-scenes
  "The drawings of Weierstrass' uniform convergence and his function
   without a derivative (alexandria.weierstrass.monster), one method per
   stage (alexandria.medium.scene/draw), in Manim's idiom.

     :weierstrass/uniform  the epsilon tube about the limit: Abel's partial
                           sums (ctx :figures :abel) escape it near pi
                           however late; the geometric partial sums (ctx
                           :figures :geometric) all lie inside from N on
     :weierstrass/monster  the zoom: the window about x0 shrinks by a at
                           every stage while the rise is scaled by
                           w^roughness, and the picture never straightens
                           (ctx :figures :monster, a raster kernel); the
                           points x' x'' and the difference quotients from
                           ctx :data :rows"
  (:require [alexandria.medium.anim :as a]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.math :as m]
            [alexandria.medium.plane :as plane]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            #?(:cljs [alexandria.medium.player :as player])))

#?(:cljs (def render "Clerk's render-fn for these scenes." player/render))

(defn- fmt [x digits]
  #?(:clj (format (str "%." digits "f") (double x))
     :cljs (.toFixed (js/Number x) digits)))

(defn- write [palette at s opts]
  (svg/text at s (merge {:colour (:ink palette) :size 0.16 :anchor "start"} opts)))

(defn- readout [palette at f lines]
  (svg/layer (a/fade f [0.2 0])
             (into [:g] (map-indexed (fn [i s] (write palette (plane/translate at [0 (* -0.22 i)]) s
                                                      {:size 0.13 :mono? true}))
                                     lines))))

(def red "#e0675a")

;; ---------------------------------------------------------------------------
;; Uniform convergence

(def ^:private uniform-id :weierstrass/uniform)

(def ^:private eps 0.3)

(defn- states [lo hi k] (mapv (fn [i] [(+ lo (* (- hi lo) (/ i (dec k))))]) (range k)))

(def ^:private abel-states (states 0 (* m/pi 0.9995) 700))
(def ^:private abel-tail-states (states (* m/pi 1.0005) 3.6 40))

(defn- tube [palette limit-pts f]
  (when (pos? f)
    [:g {:opacity f}
     (svg/polygon (concat (map (fn [[x y]] [x (+ y eps)]) limit-pts)
                          (reverse (map (fn [[x y]] [x (- y eps)]) limit-pts)))
                  {:fill (:construction palette) :opacity 0.18})
     (svg/polyline limit-pts {:stroke (:construction palette) :width 0.02})]))

(def ^:private abel-limit (mapv (fn [[x]] [x (/ x 2)]) abel-states))

(defn- abel-axes [palette]
  [:g (svg/segment [-0.1 0] [3.7 0] {:stroke (:muted palette) :width 0.01})
   (svg/segment [m/pi -0.1] [m/pi 0.1] {:stroke (:muted palette) :width 0.01})
   (write palette [(- m/pi 0.05) -0.3] "pi" {:size 0.13 :italic? true :colour (:muted palette)})
   (svg/circle [m/pi 0] 0.04 {:fill (:construction palette)})])

(defn- abel-curve [palette figures n colour width]
  [:g (svg/polyline (figure/points (:abel figures) [n] abel-states) {:stroke colour :width width})
   (svg/polyline (figure/points (:abel figures) [n] abel-tail-states) {:stroke colour :width width})])

(defn- escapes [figures n]
  (some (fn [[x y]] (> (m/abs (- y (/ x 2))) eps)) (figure/points (:abel figures) [n] abel-states)))

(defmethod scene/draw [uniform-id :cauchy] [_ _ p {:keys [palette figures]}]
  (let [n (+ 1 (* 4 (a/play p 0.1 0.8 a/linear)))]
    [:g (abel-axes palette) (abel-curve palette figures n (:found palette) 0.022)
     (readout palette [0.1 2.35] (a/play p 0 0.2)
              ["Cauchy 1821: a convergent series of continuous"
               "functions has a continuous sum"
               (str "S_N(x) = sin x - sin 2x/2 + ...,  N = " (fmt n 1))])]))

(defmethod scene/draw [uniform-id :abel] [_ _ p {:keys [palette figures]}]
  (let [n (+ 5 (* 35 (a/play p 0.05 0.8 a/linear)))]
    [:g (abel-axes palette)
     (svg/layer (a/fade (a/play p 0.5 0.8))
                (svg/polyline abel-limit {:stroke (:construction palette) :width 0.03})
                (svg/circle [m/pi 0] 0.06 {:fill (:construction palette)}))
     (abel-curve palette figures n (:found palette) 0.016)
     (readout palette [0.1 2.35] 1
              ["Abel 1826: the sum is x/2 on (-pi, pi), 0 at pi"
               (str "N = " (long n) ": every S_N continuous")
               "the limit jumps at pi"])]))

(defmethod scene/draw [uniform-id :tube-abel] [_ _ p {:keys [palette figures]}]
  (let [ns [5 10 20 40]
        [i _] [(min 3 (long (* 4 p))) nil]
        n (nth ns i)]
    [:g (abel-axes palette) (tube palette abel-limit (a/play p 0 0.15))
     (abel-curve palette figures n (if (escapes figures n) red (:found palette)) 0.016)
     (readout palette [0.1 2.35] 1
              [(str "epsilon = " eps ", N = " n)
               (if (escapes figures n) "S_N leaves the tube near pi" "inside")
               "no N serves every x at once: not uniform"])]))

(def ^:private geo-states (states -0.5 0.5 200))
(def ^:private geo-limit (mapv (fn [[x]] [(* 3 x) (/ 1 (- 1 x))]) geo-states))

(defn- geo-curve [palette figures n colour]
  (svg/polyline (mapv (fn [[x y]] [(* 3 x) y]) (figure/points (:geometric figures) [n] geo-states))
                {:stroke colour :width 0.02}))

(defn- geo-axes [palette]
  [:g (svg/segment [-1.7 0] [1.7 0] {:stroke (:muted palette) :width 0.01})
   (write palette [-1.6 -0.25] "-1/2" {:size 0.12 :colour (:muted palette)})
   (write palette [1.4 -0.25] "1/2" {:size 0.12 :colour (:muted palette)})])

(defmethod scene/draw [uniform-id :tube-geometric] [_ _ p {:keys [palette figures]}]
  (let [n (+ 1 (* 7 (a/play p 0.1 0.85 a/linear)))
        err (m/pow 0.5 (m/floor n))
        inside? (< (* 2 err) eps)]
    [:g {:transform "translate(1.75 0)"}
     (geo-axes palette) (tube palette geo-limit (a/play p 0 0.15))
     (geo-curve palette figures n (if inside? (:found palette) red))
     (readout palette [-1.7 2.35] 1
              [(str "1 + x + x^2 + ... on [-1/2, 1/2],  N = " (fmt n 1))
               (str "sup |S_N - 1/(1-x)| = 2^(1-N) = " (fmt (* 2 err) 4))
               (if inside? "inside the tube along its whole length: uniform" "not yet")])]))

(defmethod scene/draw [uniform-id :m-test] [_ _ p {:keys [palette]}]
  (let [k 10
        fs (a/lagged p k 0.4 0.05 0.7)
        w 0.3]
    (into [:g (svg/segment [0 0] [3.4 0] {:stroke (:muted palette) :width 0.01})
           (readout palette [0.1 2.35] (a/play p 0.6 0.9)
                    ["|f_n(x)| = |x|^n <= 2^-n = g_n on [-1/2, 1/2]"
                     "tail after N <= g_N + g_(N+1) + ... = 2^(1-N)"
                     "one bound for every x: uniform"])]
          (for [i (range k) :let [h (* 2 (m/pow 0.5 i)) f (nth fs i)]]
            [:g {:opacity f}
             (svg/polygon (plane/rect (* i w) (+ (* i w) (* 0.85 w)) 0 h)
                          {:fill (if (>= i 4) (:found palette) (:construction palette)) :opacity 0.6})]))))

(defmethod scene/draw [uniform-id :repaired] [_ _ p {:keys [palette]}]
  [:g (readout palette [0.1 1.6] (a/play p 0 0.3)
               ["|f(x) - f(x0)| <= |f - S_N|(x) + |S_N(x) - S_N(x0)| + |S_N - f|(x0)"
                "                <  epsilon/3  +  epsilon/3  +  epsilon/3"
                "N chosen once for all x (uniform), then x near x0"
                "Abel's series is not uniform near pi: the theorem stands"])])

;; ---------------------------------------------------------------------------
;; The monster

(def ^:private monster-id :weierstrass/monster)
(def ^:private window-states (states -1 1 900))

(defn- monster-curve [palette figures params colour]
  (svg/polyline (mapv (fn [[s y]] [(* 2 s) y]) (figure/points (:monster figures) params window-states))
                {:stroke colour :width 0.014}))

(defn- frame [palette]
  [:g (svg/polygon (plane/rect -2 2 -1.6 1.6) {:stroke (:muted palette) :width 0.008})
   (svg/segment [0 -1.6] [0 1.6] {:stroke (:muted palette) :width 0.004 :dash "0.04 0.04"})])

(defmethod scene/draw [monster-id :terms] [_ _ p {:keys [palette figures data]}]
  (let [{:keys [x0]} data
        k (+ 1 (* 6 (a/play p 0.05 0.85 a/linear)))]
    [:g (frame palette) (monster-curve palette figures [x0 1 1.2 k] (:found palette))
     (readout palette [-1.95 2.25] 1
              [(str "f(x) = sum b^n cos(a^n x pi),  a = 13, b = 1/2,  terms: " (fmt k 1))
               "|b^n cos(...)| <= b^n: the M-test makes f continuous"])]))

(defmethod scene/draw [monster-id :zoom] [_ _ p {:keys [palette figures data]}]
  (let [{:keys [x0 roughness a]} data
        z (* 3 (a/play p 0.02 0.98 a/linear))
        w (m/pow a (- z))
        h (* 1.2 (m/pow w roughness))]
    [:g (frame palette) (monster-curve palette figures [x0 w h 9] (:found palette))
     (readout palette [-1.95 2.25] 1
              [(str "window: x0 +- " (fmt w 6) "  (magnified " (fmt (/ 1 w) 0) " times)")
               (str "height scaled by w^(ln 2/ln 13) = " (fmt (/ h 1.2) 4))
               "the same jagged picture at every scale: no tangent"])]))

(defn- row-at [data m'] (first (filter #(= m' (:m %)) (:rows data))))

(defn- chords
  "The window of half-width 2/a^m about x0 with the chords from x0 to x'
   and x'' of stage m."
  [palette figures {:keys [x0 a roughness]} {:keys [m x' x'']} f]
  (let [w (* 2 (m/pow a (- m)))
        s' (/ (- x' x0) w) s'' (/ (- x'' x0) w)
        h (* 1.2 (m/pow w roughness))
        [[_ y'] [_ y'']] (figure/points (:monster figures) [x0 w h 9] [[s'] [s'']])]
    [:g (monster-curve palette figures [x0 w h 9] (:found palette))
     (svg/layer {:opacity f}
                (svg/segment [(* 2 s') y'] [0 0] {:stroke (:construction palette) :width 0.016})
                (svg/segment [0 0] [(* 2 s'') y''] {:stroke red :width 0.016})
                (svg/circle [(* 2 s') y'] 0.04 {:fill (:construction palette)})
                (svg/circle [(* 2 s'') y''] 0.04 {:fill red})
                (svg/circle [0 0] 0.04 {:fill (:ink palette)}))]))

(defmethod scene/draw [monster-id :points] [_ _ p {:keys [palette figures data]}]
  (let [m' (inc (min 2 (long (* 3 p))))
        row (row-at data m')]
    [:g (frame palette) (chords palette figures data row (a/play (- (* 3 p) (dec m')) 0.1 0.5))
     (readout palette [-1.95 2.25] 1
              [(str "m = " m' ": alpha_m = " (:alpha row) ", the whole number nearest a^m x0")
               "x' = (alpha_m - 1)/a^m  <  x0  <  x'' = (alpha_m + 1)/a^m"])]))

(defn- quotient-table [palette data upto f]
  (readout palette [-1.95 -1.85] f
           (cons "m    left quotient    right quotient   bound (ab)^m(2/3 - pi/(ab-1))"
                 (for [{:keys [m left right bound]} (take upto (:rows data))]
                   (str m "    " (fmt left 1) "    " (fmt right 1) "    " (fmt bound 1))))))

(defmethod scene/draw [monster-id :head] [_ _ p {:keys [palette figures data]}]
  [:g (frame palette) (chords palette figures data (row-at data 2) 1)
   (readout palette [-1.95 2.25] (a/play p 0 0.3)
            ["first m terms: |cos A - cos B| = 2|sin((A+B)/2) sin((A-B)/2)|"
             "their share of the quotient < pi (ab)^m / (ab - 1)"])])

(defmethod scene/draw [monster-id :tail] [_ _ p {:keys [palette figures data]}]
  [:g (frame palette) (chords palette figures data (row-at data 2) 1)
   (readout palette [-1.95 2.25] (a/play p 0 0.3)
            ["a odd: cos(a^(m+n) x' pi) = -(-1)^alpha_m, every later term one sign"
             "its first term alone: at least (2/3)(ab)^m"])])

(defmethod scene/draw [monster-id :quotients] [_ _ p {:keys [palette figures data]}]
  (let [upto (max 1 (long (* 6 (a/play p 0.05 0.9 a/linear))))]
    [:g (frame palette) (chords palette figures data (row-at data (min 3 upto)) 1)
     (readout palette [-1.95 2.25] 1 [(str "ab = 13/2 > 1 + 3pi/2 = " (fmt (+ 1 (* 1.5 m/pi)) 3))
                                      "opposite signs, both growing like (ab)^m"])
     (quotient-table palette data upto 1)]))

(defmethod scene/draw [monster-id :no-derivative] [_ _ p {:keys [palette figures data]}]
  [:g (frame palette) (chords palette figures data (row-at data 3) 1)
   (quotient-table palette data 6 1)
   (svg/layer (a/fade (a/play p 0.2 0.6) [0 -0.2])
              (write palette [-1.95 2.25] "no derivative at x0, finite or infinite; and x0 was any point"
                     {:size 0.14}))])
