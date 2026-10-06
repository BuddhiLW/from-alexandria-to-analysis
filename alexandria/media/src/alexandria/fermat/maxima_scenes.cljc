(ns alexandria.fermat.maxima-scenes
  "The drawings of Fermat's Methodus (alexandria.fermat.maxima), in Manim's
   idiom (alexandria.medium.anim).

     :fermat/rectangle  the line B = 4 cut at A, the rectangle A (B - A) above
                        it and drawn as a curve; the chord from A to A + E
                        (the :secant kernel) turns into the tangent as E is
                        struck out. ctx :controls :a is the cut A
     :fermat/tangent    the parabola BDN, the tangent BE, O outside the curve,
                        CE = 2 CD"
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
;; :fermat/rectangle. B = 4; the rectangle is drawn at a quarter of its
;; height, the curve y = A (B - A) / 4 above the line (the :rectangle
;; kernel). Fermat's numbers come from the :adequation kernel, the chord
;; from the :secant kernel.

(def ^:private B 4)
(def ^:private k 0.25)
(def ^:private curve-states (mapv (fn [i] [(* B (/ i 80))]) (range 81)))

(defn- a-of [ctx] (double (get-in ctx [:controls :a] 1)))

(defn- height-at [figures x] (second (first (figure/points (:rectangle figures) [B k] [[x]]))))

(defn- numbers
  "Fermat's pair at cut x and excess e: w = 0 [f(A) f(A+E)], 1 the
   compared terms, 2 the quotient before and after striking E."
  [figures x e w]
  (first (figure/points (:adequation figures) [B] [[x e w]])))

(defn- cut-line [palette x f]
  [:g (stroke [[0 0] [x 0]] (:found palette) 0.035 f)
   (stroke [[x 0] [B 0]] (:construction palette) 0.035 f)
   (dot [0 0] (:ink palette) f) (dot [x 0] (:ink palette) f) (dot [B 0] (:ink palette) f)])

(defn- rectangle [palette x f]
  (svg/polygon [[0 -0.1] [x -0.1] [x (- -0.1 (* k (- B x)))] [0 (- -0.1 (* k (- B x)))]]
               {:fill (:found palette) :opacity (* 0.3 f) :stroke (:found palette) :width 0.01}))

(defn- rect-curve [palette figures f]
  (stroke (figure/points (:rectangle figures) [B k] curve-states) (:muted palette) 0.02 f))

(defn- secant [palette figures x e f]
  (let [[[x0 y0] [x1 y1]] (figure/points (:secant figures) [B x e] [[-0.6] [1.6]])]
    [:g (stroke [[x0 (* k y0)] [x1 (* k y1)]] (:construction palette) 0.02 f)
     (dot [x (height-at figures x)] (:ink palette) f)
     (dot [(+ x e) (height-at figures (+ x e))] (:construction palette) f)]))

(defmethod scene/draw [:fermat/rectangle :problem] [_ _ p {:keys [palette figures]}]
  (let [x (+ 0.6 (* 2.8 (a/there-and-back p)))
        [fa] (numbers figures x 0 0)]
    [:g (cut-line palette x (a/play p 0 0.2)) (rectangle palette x (a/play p 0.1 0.3))
     (label palette [0 0] "A" 1) (label palette [x 0] "E" 1) (label palette [B 0] "C" 1)
     (readout palette [0 1.9] 1 [(str "AE x EC = " (fmt fa 3))])]))

(defmethod scene/draw [:fermat/rectangle :name-a] [_ _ p {:keys [palette figures] :as ctx}]
  (let [x (a-of ctx) [fa] (numbers figures x 0 0)]
    [:g (cut-line palette x 1) (rectangle palette x 1) (rect-curve palette figures (a/play p 0.2 0.9))
     (readout palette [0 1.9] (a/play p 0 0.3) [(str "A = " (fmt x 2) "   B - A = " (fmt (- B x) 2))
                                               (str "B A - A^2 = " (fmt fa 3))])]))

(defmethod scene/draw [:fermat/rectangle :name-ae] [_ _ p {:keys [palette figures] :as ctx}]
  (let [x (a-of ctx) e 0.8 [fa fae] (numbers figures x e 0)]
    [:g (cut-line palette x 1) (rect-curve palette figures 1) (secant palette figures x e (a/play p 0.2 0.6))
     (readout palette [0 1.9] (a/play p 0.3 0.6)
              [(str "f(A)     = " (fmt fa 3)) (str "f(A + E) = " (fmt fae 3))])]))

(defmethod scene/draw [:fermat/rectangle :common] [_ _ p {:keys [palette figures] :as ctx}]
  (let [x (a-of ctx) e 0.8 [be rest] (numbers figures x e 1)]
    [:g (cut-line palette x 1) (rect-curve palette figures 1) (secant palette figures x e 1)
     (readout palette [0 1.9] (a/play p 0 0.4)
              ["B E  ~  2 A E + E^2" (str (fmt be 3) "  ~  " (fmt rest 3))])]))

(defn- shrinking [palette figures x e]
  (let [[q q0] (numbers figures x e 2)]
    [:g (secant palette figures x e 1)
     (readout palette [0 1.9] 1 [(str "E = " (fmt e 3)) (str "B - 2A - E = " (fmt q 3)) (str "B - 2A     = " (fmt q0 3))])]))

(defmethod scene/draw [:fermat/rectangle :divide] [_ _ p {:keys [palette figures] :as ctx}]
  (let [x (a-of ctx)]
    [:g (cut-line palette x 1) (rect-curve palette figures 1) (shrinking palette figures x (- 0.8 (* 0.5 (a/smooth p))))]))

(defmethod scene/draw [:fermat/rectangle :strike] [_ _ p {:keys [palette figures] :as ctx}]
  (let [x (a-of ctx) e (* 0.3 (- 1 (a/smooth p)))]
    [:g (cut-line palette x 1) (rect-curve palette figures 1) (shrinking palette figures x (max e 1e-4))
     (svg/layer (a/fade (a/play p 0.7 1)) (write palette [2 (+ 1.1 (* k 4))] "B = 2A: the half" {:size 0.11}))]))

(defmethod scene/draw [:fermat/rectangle :derivative] [_ _ p {:keys [palette figures]}]
  (let [x (+ 2 (* 1.4 (- 1 (a/smooth p))))
        [_ slope] (numbers figures x 0 2)]
    [:g (cut-line palette x 1) (rectangle palette x 1) (rect-curve palette figures 1)
     (secant palette figures x 1e-4 1)
     (readout palette [0 1.9] 1 [(str "A = " (fmt x 3)) (str "f'(A) = B - 2A = " (fmt slope 3))])]))

;; ---------------------------------------------------------------------------
;; :fermat/tangent. The parabola x = y^2 (the :parabola kernel; vertex D at
;; the origin, diameter along x), B = (1, 1), C = (1, 0), E = (-1, 0); the
;; tangent is the :tangent kernel, O's ratios the :outside kernel.

(def ^:private parabola-states (mapv (fn [i] [(- (* 3 (/ i 60)) 1.5)]) (range 61)))
(def ^:private pts {:D [0 0] :B [1 1] :C [1 0] :E [-1 0]})

(defn- tangent-figure [palette figures f]
  (let [line (figure/points (:tangent figures) [1 1] [[-0.1] [1.6]])]
    [:g (stroke [[-1.4 0] [2.4 0]] (:muted palette) 0.01 1)
     (stroke (figure/points (:parabola figures) [1] parabola-states) (:found palette) 0.03 1)
     (stroke line (:construction palette) 0.02 f)
     (stroke [(:B pts) (:C pts)] (:ink palette) 0.014 f)
     (into [:g] (map (fn [[n at]] [:g (dot at (:ink palette) 1) (label palette at (name n) 1)]) pts))]))

(defmethod scene/draw [:fermat/tangent :parabola] [_ _ p {:keys [palette figures]}]
  (tangent-figure palette figures (a/play p 0.3 0.9)))

(defmethod scene/draw [:fermat/tangent :outside] [_ _ p {:keys [palette figures]}]
  (let [s (+ 0.35 (* 0.5 (a/there-and-back p)))
        [[ox oy]] (figure/points (:tangent figures) [1 1] [[s]])
        on-curve (first (figure/points (:parabola figures) [1] [[oy]]))
        [cd-di bc-oi] (first (figure/points (:outside figures) [] [[s]]))]
    [:g (tangent-figure palette figures 1)
     (dot [ox oy] (:construction palette) 1) (label palette [ox oy] "O" 1)
     (stroke [[ox 0] [ox oy]] (:construction palette) 0.014 1)
     (dot on-curve (:found palette) 1)
     (readout palette [-1.3 1.6] 1 [(str "CD : DI = " (fmt cd-di 3))
                                    (str "BC^2 : OI^2 = " (fmt bc-oi 3))])]))

(defmethod scene/draw [:fermat/tangent :name] [_ _ p {:keys [palette figures]}]
  [:g (tangent-figure palette figures 1)
   (readout palette [-1.3 1.6] (a/play p 0 0.4) ["CD = D (given)" "CE = A (sought)" "CI = E"])])

(defmethod scene/draw [:fermat/tangent :adequate] [_ _ p {:keys [palette figures]}]
  [:g (tangent-figure palette figures 1)
   (readout palette [-1.3 1.6] (a/play p 0 0.4) ["D E^2 - 2 D A E  ~  - A^2 E"])])

(defmethod scene/draw [:fermat/tangent :strike] [_ _ p {:keys [palette figures]}]
  [:g (tangent-figure palette figures 1)
   (readout palette [-1.3 1.6] (a/play p 0 0.4) ["D E + A^2  ~  2 D A" "strike E:  A = 2 D"])])

(defmethod scene/draw [:fermat/tangent :subtangent] [_ _ p {:keys [palette figures]}]
  (let [f (a/play p 0 0.6)]
    [:g (tangent-figure palette figures 1)
     (stroke [[-1 -0.12] [0 -0.12]] (:construction palette) 0.04 f)
     (stroke [[0 -0.12] [1 -0.12]] (:found palette) 0.04 f)
     (readout palette [-1.3 1.6] (a/play p 0.4 0.8) ["CE = 2 CD = 2" "ED = DC"])]))
