(ns alexandria.lebesgue.measure-scenes
  "The drawings of Lebesgue's measure zero and Dirichlet's function
   (alexandria.lebesgue.measure), one method per stage, in Manim's idiom:
   intervals of length eps/2^n close over the listed rationals while eps
   shrinks; Riemann's vertical slices of Dirichlet's function stand at
   height 1 by excess and 0 by defect for every division; Lebesgue's
   horizontal slices sort the values first.

   Two scene ids: :lebesgue/measure-zero and :lebesgue/dirichlet. ctx
   :figures :cover gives an end of the n-th interval at eps (state
   [x n e]); ctx :data carries the listed rationals (:rationals)."
  (:require [alexandria.medium.anim :as a]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.math :as m]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            #?(:cljs [alexandria.medium.player :as player])))

#?(:cljs (def render "Clerk's render-fn for these scenes." player/render))

(def ^:private W 5.0)

(defn- X [x] (* W (- x 0.5)))

(defn- fmt [x digits]
  #?(:clj (format (str "%." digits "f") (double x))
     :cljs (.toFixed (js/Number x) digits)))

(defn- write [palette at s & [opts]]
  (svg/text at s (merge {:colour (:ink palette) :size 0.15} opts)))

(defn- caption [palette f lines]
  (svg/layer (a/fade f [0 -0.1])
             (into [:g] (map-indexed (fn [i s] (write palette [(X 0) (- 1.45 (* 0.22 i))] s
                                                     {:anchor "start" :size 0.13 :mono? true}))
                                     lines))))

(defn- unit-axis [palette f]
  [:g (svg/polyline [[(X 0) 0] [(X 1) 0]] {:stroke (:ink palette) :width 0.02 :attrs (a/create f)})
   (svg/layer (a/fade f)
              (write palette [(X 0) -0.25] "0" {:size 0.12})
              (write palette [(X 1) -0.25] "1" {:size 0.12}))])

(defn- rational-ticks [palette xs f]
  (let [fs (a/lagged f (count xs) 0.3 0 1)]
    (into [:g] (map (fn [x g] (svg/circle [(X x) 0] (* 0.035 g) {:fill (:found palette)})) xs fs))))

(defn- intervals
  "The covering intervals of the first k rationals at eps, through the
   :cover figure, each dropped below the line by its index."
  [palette figures xs eps f]
  (let [states (vec (mapcat (fn [x n] [[x n 0] [x n 1]]) xs (range 1 (inc (count xs)))))
        ends (partition 2 (figure/points (:cover figures) [eps] states))
        fs (a/lagged f (count xs) 0.25 0 1)]
    (into [:g]
          (map (fn [[[x0 y] [x1 _]] g]
                 (when (pos? g)
                   (svg/segment [(X x0) (* 2.2 y)] [(X (+ x0 (* g (- x1 x0)))) (* 2.2 y)]
                                {:stroke (:construction palette) :width 0.05})))
               ends fs))))

(defmethod scene/draw [:lebesgue/measure-zero :list] [_ _ p {:keys [palette data]}]
  (let [xs (:rationals data)]
    [:g (unit-axis palette (a/play p 0 0.3))
     (rational-ticks palette xs (a/play p 0.2 0.9 a/linear))
     (caption palette (a/play p 0.5 0.8) ["r₁ = 0, r₂ = 1, r₃ = 1/2, r₄ = 1/3, … (Cantor's count)"])]))

(defmethod scene/draw [:lebesgue/measure-zero :cover] [_ _ p {:keys [palette data figures]}]
  (let [xs (:rationals data)]
    [:g (unit-axis palette 1) (rational-ticks palette xs 1)
     (intervals palette figures xs 0.5 (a/play p 0.05 0.9 a/linear))
     (caption palette (a/play p 0 0.2) ["ε = 1/2: rₙ in an interval of length ε/2ⁿ"])]))

(defmethod scene/draw [:lebesgue/measure-zero :sum] [_ _ p {:keys [palette data figures]}]
  (let [xs (:rationals data)
        k (count xs)
        sweep (a/play p 0.1 0.8 a/linear)
        shown (max 1 (long (m/floor (* sweep k))))
        total (- 1 (m/pow 2 (- shown)))
        bar-y 1.0]
    [:g (unit-axis palette 1) (rational-ticks palette xs 1)
     (intervals palette figures xs 0.5 1)
     (svg/segment [(X 0) bar-y] [(X 0.5) bar-y] {:stroke (:muted palette) :width 0.05})
     (svg/segment [(X 0) bar-y] [(X (* 0.5 total)) bar-y] {:stroke (:construction palette) :width 0.07})
     (write palette [(X 0.55) (- bar-y 0.05)]
            (str "ε/2 + … + ε/2^" shown " = ε(1 − 2^−" shown ") = " (fmt (* 0.5 total) 6) " < ε")
            {:anchor "start" :size 0.12 :mono? true})]))

(defmethod scene/draw [:lebesgue/measure-zero :shrink] [_ _ p {:keys [palette data figures]}]
  (let [xs (:rationals data)
        eps (* 0.5 (m/pow 0.02 (a/play p 0.05 0.85)))]
    [:g (unit-axis palette 1) (rational-ticks palette xs 1)
     (intervals palette figures xs eps 1)
     (caption palette 1 [(str "ε = " (fmt eps 4) ": total length < ε")
                         "for every ε: the rationals have measure 0"])]))

;; ---------------------------------------------------------------------------
;; Dirichlet's function

(def ^:private H 1.6)

(defn- chi-picture
  "Dirichlet's function drawn as the eye sees it: dots at height 1 over the
   listed rationals, a line at height 0 for the irrationals."
  [palette xs f]
  [:g (svg/polyline [[(X 0) 0] [(X 1) 0]] {:stroke (:ink palette) :width 0.02 :attrs (a/create f)})
   (svg/polyline [[(X 0) H] [(X 1) H]] {:stroke (:muted palette) :width 0.008 :dash "0.04 0.06"})
   (into [:g] (map (fn [x] (svg/circle [(X x) H] (* 0.03 f) {:fill (:found palette)})) xs))
   (svg/layer (a/fade f)
              (write palette [(- (X 0) 0.15) (- H 0.05)] "1" {:size 0.12 :anchor "end"})
              (write palette [(- (X 0) 0.15) -0.05] "0" {:size 0.12 :anchor "end"}))])

(defn- slices [palette k height colour f]
  (let [fs (a/lagged f k 0.3 0 1)]
    (into [:g]
          (map (fn [i g]
                 (let [x0 (/ i k) x1 (/ (inc i) k)]
                   (svg/polygon [[(X x0) 0] [(X x1) 0] [(X x1) (* g height)] [(X x0) (* g height)]]
                                {:fill colour :opacity 0.35 :stroke colour :width 0.01})))
               (range k) fs))))

(defn- divisions [p] (nth [4 6 10 16 24 40] (min 5 (long (m/floor (* 6 p))))))

(defmethod scene/draw [:lebesgue/dirichlet :function] [_ _ p {:keys [palette data]}]
  [:g (chi-picture palette (:rationals data) (a/play p 0 0.6))
   (caption palette (a/play p 0.5 0.8) ["χ(x) = 1 on the rationals, 0 on the irrationals"])])

(defmethod scene/draw [:lebesgue/dirichlet :riemann-upper] [_ _ p {:keys [palette data]}]
  (let [k (divisions p)]
    [:g (chi-picture palette (:rationals data) 1)
     (slices palette k H (:found palette) 1)
     (caption palette 1 [(str k " slices: each holds a rational, sup = 1") "S = Σ δᵢ · 1 = 1"])]))

(defmethod scene/draw [:lebesgue/dirichlet :riemann-lower] [_ _ p {:keys [palette data]}]
  (let [k (divisions p)]
    [:g (chi-picture palette (:rationals data) 1)
     (slices palette k H (:found palette) 0.25)
     (into [:g] (map (fn [i] (svg/segment [(X (/ i k)) 0] [(X (/ (inc i) k)) 0]
                                          {:stroke (:construction palette) :width 0.06}))
                     (range k)))
     (caption palette 1 [(str k " slices: each holds an irrational, inf = 0") "s = 0 ≠ 1 = S: no Riemann integral"])]))

(defmethod scene/draw [:lebesgue/dirichlet :lebesgue-slices] [_ _ p {:keys [palette data]}]
  (let [f (a/play p 0.1 0.7)]
    [:g (chi-picture palette (:rationals data) (- 1 (* 0.6 f)))
     (svg/polygon [[(X 0) (- H 0.08)] [(X 1) (- H 0.08)] [(X 1) (+ H 0.08)] [(X 0) (+ H 0.08)]]
                  {:fill (:found palette) :opacity (* 0.2 f)})
     (svg/polygon [[(X 0) -0.08] [(X 1) -0.08] [(X 1) 0.08] [(X 0) 0.08]]
                  {:fill (:construction palette) :opacity (* 0.25 f)})
     (svg/layer (a/fade f)
                (write palette [(+ (X 1) 0.15) (- H 0.05)] "{χ = 1} = Q: measure 0" {:anchor "start" :size 0.12})
                (write palette [(+ (X 1) 0.15) -0.05] "{χ = 0}: measure 1" {:anchor "start" :size 0.12}))]))

(defmethod scene/draw [:lebesgue/dirichlet :lebesgue-sum] [_ _ p {:keys [palette data]}]
  [:g (chi-picture palette (:rationals data) 0.4)
   (svg/layer (a/fade (a/play p 0 0.4) [0 -0.2])
              (write palette [0 0.9] "1 · m(Q) + 0 · m([0,1] − Q)" {:size 0.2}))
   (svg/layer (a/fade (a/play p 0.4 0.8) [0 -0.2])
              (write palette [0 0.5] "= 1 · 0 + 0 · 1 = 0" {:size 0.22 :colour (:found palette)}))])

(def ^:private coins [1 5 1 10 5 1 25 10 1 5 1 25])

(defmethod scene/draw [:lebesgue/dirichlet :coins] [_ _ p {:keys [palette]}]
  (let [sorted (sort coins)
        f (a/play p 0.2 0.8)
        pos-of (fn [i] [(+ -2.5 (* 0.42 i)) 1.0])
        values (vec (distinct sorted))
        sorted-pos (fn [c k] [(+ -2.5 (* 1.3 (.indexOf values c))) (- 0.2 (* 0.3 k))])
        ks (reduce (fn [[acc seen] c] [(conj acc (get seen c 0)) (update seen c (fnil inc 0))]) [[] {}] coins)]
    (into [:g (svg/layer (a/fade (a/play p 0 0.2))
                         (write palette [-2.6 1.45] "Riemann: coin by coin, as they come" {:anchor "start" :size 0.12})
                         (write palette [-2.6 -1.4] "Lebesgue: sort by value, then value × count" {:anchor "start" :size 0.12}))]
          (map-indexed (fn [i c]
                         (let [from (pos-of i) to (sorted-pos c (nth (first ks) i))
                               [x y] [(+ (first from) (* f (- (first to) (first from))))
                                      (+ (second from) (* f (- (second to) (second from))))]]
                           [:g (svg/circle [x y] (+ 0.09 (* 0.004 c)) {:fill (:found palette) :opacity 0.7})
                            (write palette [x (- y 0.04)] (str c) {:size 0.09 :mono? true})]))
                       coins))))
