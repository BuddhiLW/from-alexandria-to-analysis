(ns alexandria.euler.draw
  "The drawing words the Euler scenes share, in Manim's idiom
   (alexandria.medium.anim): text, a readout column, strokes created to a
   fraction, dots, curves sampled from a function. Pure; cljc so the browser
   player and the JVM tests draw the same hiccup."
  (:require [alexandria.medium.anim :as a]
            [alexandria.medium.math :as m]
            [alexandria.medium.plane :as plane]
            [alexandria.medium.svg :as svg]))

(def text-scale 1.45)

(defn fmt
  "x with `digits` decimals."
  [x digits]
  #?(:clj (format (str "%." digits "f") (double x))
     :cljs (.toFixed (js/Number x) digits)))

(defn write
  ([palette at s] (write palette at s {}))
  ([palette at s opts]
   (svg/text at s (-> (merge {:colour (:ink palette) :size 0.13} opts)
                      (update :size * text-scale)))))

(defn label
  "A letter faded in by f with a small upward shift."
  ([palette at s f] (label palette at s f {}))
  ([palette at s f {:keys [dx dy size colour] :or {dx 0.09 dy 0.09 size 0.13}}]
   (svg/layer (a/fade f [0 -0.15])
              (write palette (plane/translate at [dx dy]) s
                     {:italic? true :anchor "start" :size size :colour (or colour (:ink palette))}))))

(defn readout
  "Lines of mono text from `at` downwards, faded in together by f."
  ([palette f at lines] (readout palette f at lines {}))
  ([palette f [x y] lines {:keys [size step] :or {size 0.085 step 0.21}}]
   (svg/layer (a/fade f [0.2 0])
              (into [:g] (map-indexed (fn [i s] (write palette [x (- y (* step i))] s
                                                       {:anchor "start" :size size :mono? true}))
                                      lines)))))

(defn stroke
  "A polyline through ps created to fraction f."
  ([ps colour width f] (stroke ps colour width f nil))
  ([ps colour width f dash]
   (when (pos? f) (svg/polyline ps {:stroke colour :width width :dash dash :attrs (a/create f)}))))

(defn dot [at colour r f]
  (when (pos? f) (svg/circle at (* r f) {:fill colour})))

(defn sample
  "n+1 points (g t) for t from t0 to t1."
  [g t0 t1 n]
  (mapv (fn [i] (g (+ t0 (* (- t1 t0) (/ i n))))) (range (inc n))))

(defn quad
  "Points of the quadratic Bezier curve from p to q bent by `bend` along the
   normal of pq (0 a straight segment)."
  [p q bend]
  (let [[px py] p [qx qy] q
        dx (- qx px) dy (- qy py)
        len (max 1e-9 (m/hypot dx dy))
        c [(+ (/ (+ px qx) 2) (* bend (/ (- dy) len))) (+ (/ (+ py qy) 2) (* bend (/ dx len)))]]
    (sample (fn [t] (let [u (- 1 t)]
                      [(+ (* u u px) (* 2 u t (first c)) (* t t qx))
                       (+ (* u u py) (* 2 u t (second c)) (* t t qy))]))
            0 1 24)))

(defn axes
  "The x axis [x0 x1] at y = 0 and the y axis [y0 y1] at x = 0, muted."
  [palette [x0 x1] [y0 y1] f]
  [:g (stroke [[x0 0] [x1 0]] (:muted palette) 0.01 f)
   (stroke [[0 y0] [0 y1]] (:muted palette) 0.01 f)])
