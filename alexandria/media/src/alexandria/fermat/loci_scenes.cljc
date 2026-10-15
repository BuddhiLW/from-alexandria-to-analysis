(ns alexandria.fermat.loci-scenes
  "The drawings of Fermat's Isagoge (alexandria.fermat.loci), one sentence
   per step, in Manim's idiom (alexandria.medium.anim).

     :setup      the line NZM given in position, N on it, NZ = A, ZI = E at
                 the given (right) angle NZI: the frame every locus uses
     :line       D in A = B in E: I runs along a line through N
     :hyperbola  A in E = Z pl.: I runs along a hyperbola
     :parabola   A q. = D in E: I runs along a parabola, diameter NP
     :circle     B q. - A q. = E q.: I runs along the circle about N, NM = B
     :ellipse    (B q. - A q.) : E q. given: I runs along an ellipse, vertex M
     :descartes  all five at once, the first degree a line, the second a conic

   Every point of every curve is a raster kernel of alexandria.fermat.loci
   (figures :line :hyperbola :parabola :circle :ellipse); the moving I is
   the same kernel at the current u, and A and E are read off it."
  (:require [alexandria.medium.anim :as a]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.plane :as plane]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            #?(:cljs [alexandria.medium.player :as player])))

#?(:cljs (def render "Clerk's render-fn for these scenes." player/render))

(defn- fmt [x digits]
  #?(:clj (format (str "%." digits "f") (double x))
     :cljs (.toFixed (js/Number x) digits)))

(defn- write [palette at s opts]
  (svg/text at s (-> (merge {:colour (:ink palette) :size 0.13} opts) (update :size * 1.45))))

(defn- label [palette at s f & [{:keys [dx dy] :or {dx 0.07 dy 0.08}}]]
  (svg/layer (a/fade f [0 -0.12]) (write palette (plane/translate at [dx dy]) s {:italic? true :anchor "start"})))

(defn- stroke [ps colour width f]
  (when (pos? f) (svg/polyline ps {:stroke colour :width width :attrs (a/create f)})))

(defn- dot [at colour f] (when (pos? f) (svg/circle at (* 0.05 f) {:fill colour})))

(defn- readout [palette at f lines]
  (svg/layer (a/fade f [0.2 0])
             (into [:g] (map-indexed (fn [i s] (write palette (plane/translate at [0 (* -0.3 i)]) s
                                                      {:anchor "start" :size 0.09 :mono? true}))
                                     lines))))

;; ---------------------------------------------------------------------------
;; The parameter u of each locus, the states that draw it, and its colour

(def ^:private B 1.5)

(defn- span [lo hi n] (mapv (fn [i] [(+ lo (* (- hi lo) (/ i n)))]) (range (inc n))))

(def ^:private round
  "u = tan(t/2) round the circle, short of the point at u = infinity."
  (mapv (fn [i] [(Math/tan (/ (- (* 2 Math/PI (/ i 120)) Math/PI) 2.0001))]) (range 121)))

(def curves
  "{locus [states u-range colour-key]}."
  {:line [(span -1.2 3 2) [-1 2.6] :construction]
   :hyperbola [(span 0.36 3 60) [0.4 2.6] :found]
   :parabola [(span -1.7 1.7 60) [-1.4 1.4] :found]
   :circle [round [-2.5 2.5] :ink]
   :ellipse [round [-2.5 2.5] :construction]})

(def ^:private params
  "The figures' parameters (alexandria.fermat.loci/figures): D = 1, B = 2
   for the line, Z = 1, D = 1, B = 1.5, and m = 0.6 for the ellipse."
  {:line [1 2] :hyperbola [1] :parabola [1] :circle [B] :ellipse [B 0.6]})

(defn- points [figures k states] (figure/points (figures k) (params k) states))

(defn- curve [palette figures k f]
  (let [[states _ c] (curves k)] (stroke (points figures k states) (c palette) 0.03 f)))

(defn- frame
  "NZM, N, and the axis of E (dashed, at right angles at N)."
  [palette f]
  [:g (stroke [[-2 0] [3 0]] (:muted palette) 0.012 f)
   (stroke [[0 -2] [0 2.4]] (:muted palette) 0.006 f)
   (dot [0 0] (:ink palette) f) (label palette [0 0] "N" f {:dx -0.2 :dy 0.1})])

(defn- moving-i
  "Z and I at u (the kernel's point), NZ = A along the line, ZI = E, and
   the readout of A, E and Fermat's equation with the values substituted."
  [palette figures k u f eq]
  (let [[[x y]] (points figures k [[u]])]
    [:g (stroke [[0 0] [x 0]] (:found palette) 0.04 f)
     (stroke [[x 0] [x y]] (:construction palette) 0.04 f)
     (dot [x 0] (:ink palette) f) (label palette [x 0] "Z" f {:dy -0.25})
     (dot [x y] (:ink palette) f) (label palette [x y] "I" f)
     (readout palette [-1.9 2.25] f [(str "A = NZ = " (fmt x 3) "   E = ZI = " (fmt y 3)) (eq x y)])]))

(defn- u-of [k p]
  (let [[lo hi] (second (curves k))] (+ lo (* (- hi lo) (a/there-and-back p)))))

;; ---------------------------------------------------------------------------
;; The steps

(defmethod scene/draw [:fermat/loci :setup] [_ _ p {:keys [palette figures]}]
  (let [f1 (a/play p 0 0.25) f2 (a/play p 0.25 0.55) f3 (a/play p 0.55 0.85)]
    [:g (frame palette f1)
     (stroke [[2.4 -0.06] [2.4 0.06]] (:ink palette) 0.012 f1) (label palette [2.4 0] "M" f1 {:dy -0.25})
     (stroke [[0 0] [1.2 0]] (:found palette) 0.04 f2) (dot [1.2 0] (:ink palette) f2)
     (label palette [1.2 0] "Z" f2 {:dy -0.25}) (label palette [0.5 0] "A" f2 {:dy 0.1})
     (stroke [[1.2 0] [1.2 0.9]] (:construction palette) 0.04 f3) (dot [1.2 0.9] (:ink palette) f3)
     (label palette [1.2 0.9] "I" f3) (label palette [1.2 0.4] "E" f3)
     (stroke [[1.08 0] [1.08 0.12] [1.2 0.12]] (:ink palette) 0.01 f3)
     (readout palette [-1.9 2.25] (a/play p 0.85 1) ["NZ = A, ZI = E, the angle NZI given"])]))

(def ^:private equations
  {:line (fn [x y] (str "D A = B E:  1 x " (fmt x 2) " = 2 x " (fmt y 2)))
   :hyperbola (fn [x y] (str "A E = Z:  " (fmt x 2) " x " (fmt y 2) " = " (fmt (* x y) 3)))
   :parabola (fn [x y] (str "A^2 = D E:  " (fmt (* x x) 3) " = 1 x " (fmt y 3)))
   :circle (fn [x y] (str "B^2 - A^2 = E^2:  " (fmt (- (* B B) (* x x)) 3) " = " (fmt (* y y) 3)))
   :ellipse (fn [x y] (str "(B^2 - A^2) : E^2 = " (fmt (/ (- (* B B) (* x x)) (max 1e-9 (* y y))) 3)))})

(defn- locus-step [k p {:keys [palette figures]}]
  (let [f (a/play p 0 0.35)
        shown (take-while #(not= k %) [:line :hyperbola :parabola :circle :ellipse])]
    [:g (frame palette 1)
     (into [:g] (map (fn [j] (svg/layer {:opacity 0.25} (curve palette figures j 1))) shown))
     (curve palette figures k f)
     (when (#{:circle :ellipse} k)
       [:g (dot [B 0] (:ink palette) 1) (label palette [B 0] "M" 1 {:dy -0.25})])
     (moving-i palette figures k (u-of k (a/play p 0.35 1)) (a/play p 0.3 0.4) (equations k))]))

(doseq [k [:line :hyperbola :parabola :circle :ellipse]]
  (defmethod scene/draw [:fermat/loci k] [_ _ p ctx] (locus-step k p ctx)))

(defmethod scene/draw [:fermat/loci :descartes] [_ _ p {:keys [palette figures]}]
  [:g (frame palette 1)
   (into [:g] (map-indexed (fn [i k] (curve palette figures k (a/play p (* 0.15 i) (+ 0.3 (* 0.15 i)))))
                           [:line :hyperbola :parabola :circle :ellipse]))
   (readout palette [-1.9 2.25] (a/play p 0.7 1) ["first degree in A, E: a line"
                                                   "second degree: a conic"])])
