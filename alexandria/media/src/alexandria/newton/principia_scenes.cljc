(ns alexandria.newton.principia-scenes
  "The drawings of Principia Book I (alexandria.newton.principia), one
   method per stage (alexandria.medium.scene/draw), in Manim's idiom.

     :newton/lemma-1   inscribed and circumscribed bars squeezing the curve
     :newton/prop-1    the polygon of impulses: A, B, c, C built step by
                       step, the equal triangles filling, then finer
                       polygons converging to the orbit
     :newton/prop-11   the ellipse about its focus S, the body moving with
                       equal areas, the force arrow scaled as 1/SP^2

   ctx :data carries the polygons (alexandria.newton.principia/polygon-data);
   ctx :figures the :orbit kernel (body and force arrow)."
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
  (svg/text at s (merge {:colour (:ink palette) :size 0.11} opts)))

(defn- label [palette at s f & [{:keys [dx dy] :or {dx 0.05 dy 0.06}}]]
  (svg/layer (a/fade f [0 -0.1]) (write palette (plane/translate at [dx dy]) s {:italic? true :anchor "start"})))

(defn- stroke [ps colour width f & [dash]]
  (when (pos? f) (svg/polyline ps {:stroke colour :width width :dash dash :attrs (a/create f)})))

(defn- dot [at colour f] (when (pos? f) (svg/circle at (* 0.035 f) {:fill colour})))

(defn- readout [palette f at lines]
  (svg/layer (a/fade f [0.1 0])
             (into [:g] (map-indexed (fn [i s] (write palette (plane/translate at [0 (* -0.17 i)]) s
                                                      {:anchor "start" :size 0.085 :mono? true}))
                                     lines))))

(defn- tri [palette ps colour f]
  (svg/layer (a/fade f) (svg/polygon ps {:fill colour :opacity 0.3 :stroke colour :width 0.008})))

;; ---------------------------------------------------------------------------
;; Lemma I (by Lemma II): bars under y = 1 - x^2

(def ^:private lid :newton/lemma-1)

(defn- curve [x] (- 1 (* x x)))
(def ^:private curve-pts (mapv (fn [i] (let [x (/ i 40)] [(* 2 x) (* 2 (curve x))])) (range 41)))

(defn- bars [palette n f]
  (into [:g]
        (for [i (range n)
              :let [x0 (/ i n) x1 (/ (inc i) n)]]
          [:g (svg/polygon (plane/rect (* 2 x0) (* 2 x1) 0 (* 2 (curve x0)))
                           {:fill (:construction palette) :opacity (* 0.25 f) :stroke (:construction palette) :width 0.006})
           (svg/polygon (plane/rect (* 2 x0) (* 2 x1) 0 (* 2 (curve x1)))
                        {:fill (:found palette) :opacity (* 0.45 f)})])))

(defmethod scene/draw [lid :bars] [_ _ p {:keys [palette]}]
  [:g (bars palette 5 (a/play p 0.2 0.7)) (stroke curve-pts (:ink palette) 0.02 (a/play p 0 0.3))
   (readout palette (a/play p 0.5 0.8) [2.3 2.0] ["inscribed (filled) and" "circumscribed (outlined)"])])

(defmethod scene/draw [lid :difference] [_ _ p {:keys [palette]}]
  (let [f (a/play p 0.2 0.8)]
    [:g (bars palette 5 1) (stroke curve-pts (:ink palette) 0.02 1)
     (svg/layer (a/fade f) (svg/polygon (plane/rect 2.0 2.4 0 2) {:fill (:construction palette) :opacity 0.5}))
     (readout palette f [2.6 2.0] ["the little excesses, slid over," "fill one rectangle:" "base 1/n by height 1"])]))

(defmethod scene/draw [lid :limit] [_ _ p {:keys [palette]}]
  (let [n (int (+ 2 (m/floor (* 60 (a/play p 0 0.9 a/rush-from)))))]
    [:g (bars palette n 1) (stroke curve-pts (:ink palette) 0.02 1)
     (readout palette 1 [2.3 2.0] [(str "n = " n) (str "difference = 1/" n " = " (fmt (/ 1.0 n) 4))
                                   "less than any given D:" "ultimately equal"])]))

;; ---------------------------------------------------------------------------
;; Proposition I: the polygon of impulses

(def ^:private pid :newton/prop-1)
(def ^:private S [0 0])

(defn- coarse [data] (first (:polygons data)))

(defn- v+ [[a b] [c d]] [(+ a c) (+ b d)])
(defn- v- [[a b] [c d]] [(- a c) (- b d)])

(defn- centre [palette f]
  [:g (dot S (:ink palette) (max f 0.001)) (label palette S "S" f {:dx -0.14 :dy -0.12})])

(defn- step-figure
  "The step at B = ps[i]: the unhindered c, the impulse cC parallel to SB."
  [palette ps i {:keys [c-f impulse-f tri-f]}]
  (let [A (ps (dec i)) B (ps i) C (ps (inc i)) c (v+ B (v- B A))]
    [:g
     (tri palette [S A B] (:found palette) tri-f)
     (tri palette [S B c] (:muted palette) (* tri-f c-f))
     (tri palette [S B C] (:construction palette) (* tri-f impulse-f))
     (stroke [B c] (:muted palette) 0.012 c-f "0.04 0.03")
     (dot c (:muted palette) c-f) (label palette c "c" c-f)
     (stroke [c C] (:construction palette) 0.014 impulse-f)
     (stroke [S B] (:muted palette) 0.008 impulse-f "0.03 0.03")
     (stroke [B C] (:ink palette) 0.018 impulse-f)
     (dot C (:ink palette) impulse-f) (label palette C "C" impulse-f)]))

(defn- first-segment [palette ps f]
  (let [[A B] ps]
    [:g (stroke [A B] (:ink palette) 0.018 f)
     (dot A (:ink palette) 1) (label palette A "A" 1) (dot B (:ink palette) f) (label palette B "B" f)]))

(defmethod scene/draw [pid :first-moment] [_ _ p {:keys [palette data]}]
  (let [ps (coarse data)]
    [:g (centre palette (a/play p 0 0.3)) (first-segment palette ps (a/play p 0.2 0.8))]))

(defmethod scene/draw [pid :unhindered] [_ _ p {:keys [palette data]}]
  (let [ps (coarse data)]
    [:g (centre palette 1) (first-segment palette ps 1)
     (step-figure palette ps 1 {:c-f (a/play p 0 0.4) :impulse-f 0 :tri-f (a/play p 0.4 0.8)})
     (readout palette (a/play p 0.5 0.8) [1.4 1.8] ["Bc = AB, by Law I" "SAB = SBc: equal bases," "one vertex (Euclid I.38)"])]))

(defmethod scene/draw [pid :impulse] [_ _ p {:keys [palette data]}]
  (let [ps (coarse data)]
    [:g (centre palette 1) (first-segment palette ps 1)
     (step-figure palette ps 1 {:c-f 1 :impulse-f (a/play p 0.1 0.6) :tri-f 0})
     (readout palette (a/play p 0.5 0.8) [1.4 1.8] ["an impulse toward S at B:" "cC parallel to SB"])]))

(defmethod scene/draw [pid :parallels] [_ _ p {:keys [palette data]}]
  (let [ps (coarse data)
        A (ps 0) B (ps 1) C (ps 2) c (v+ B (v- B A))
        slide (a/there-and-back p)
        X (plane/lerp-point c C slide)]
    [:g (centre palette 1) (first-segment palette ps 1)
     (step-figure palette ps 1 {:c-f 1 :impulse-f 1 :tri-f 1})
     (tri palette [S B X] (:construction palette) 1)
     (readout palette 1 [1.4 1.8] ["a vertex sliding along cC" "keeps the area of SBX:"
                                   (str "SBX = " (fmt (/ (m/abs (- (* (first B) (second X)) (* (second B) (first X)))) 2) 4))
                                   (str "SAB = " (fmt (/ (m/abs (- (* (first A) (second B)) (* (second A) (first B)))) 2) 4))
                                   "Euclid I.37"])]))

(defn- polygon-triangles [palette ps k f]
  (into [:g]
        (for [i (range k) :let [a (ps i) b (ps (inc i))]]
          (tri palette [S a b] (if (even? i) (:found palette) (:construction palette)) f))))

(defmethod scene/draw [pid :polygon] [_ _ p {:keys [palette data]}]
  (let [ps (coarse data)
        k (int (m/floor (* (dec (count ps)) (a/play p 0 0.9 a/linear))))]
    [:g (centre palette 1)
     (polygon-triangles palette ps (max 1 k) 1)
     (stroke (subvec ps 0 (inc (max 1 k))) (:ink palette) 0.016 1)
     (readout palette 1 [1.4 1.8] [(str (max 1 k) " moments, " (max 1 k) " equal triangles")
                                   "SAB = SBC = SCD = ..."])]))

(defmethod scene/draw [pid :limit] [_ _ p {:keys [palette data]}]
  (let [{:keys [polygons counts]} data
        j (min (dec (count polygons)) (int (m/floor (* (count polygons) (a/play p 0 0.95 a/linear)))))
        ps (polygons j)]
    [:g (centre palette 1)
     (polygon-triangles palette ps (dec (count ps)) 0.8)
     (stroke (last polygons) (:muted palette) 0.008 1)
     (stroke ps (:ink palette) 0.014 1)
     (readout palette 1 [1.4 1.8] [(str (counts j) " impulses") "the perimeter tends to a curve:" "the force acts continually,"
                                   "areas stay as the times"])]))

;; ---------------------------------------------------------------------------
;; Proposition XI: the ellipse about a focus
;;
;; Every point on the ellipse comes from the :orbit kernel (state [theta 0]
;; the body, [theta 1] the arrow head); the angle reached at a fraction of
;; the period is ctx :data :thetas, Kepler's equation solved by raster on the
;; JVM (alexandria.newton.principia/prop-11-data). No orbit arithmetic here.

(def ^:private eid :newton/prop-11)

(defn- on-orbit
  "The body at each angle of ths, one kernel batch."
  [figures params ths]
  (figure/points (:orbit figures) params (mapv (fn [th] [th 0]) ths)))

(defn- ellipse-pts [figures params]
  (on-orbit figures params (mapv (fn [i] (* 2 m/pi (/ i 120))) (range 121))))

(defn- theta-at
  "The angle after fraction t of a period: the raster table, interpolated."
  [{:keys [thetas]} t]
  (let [n (dec (count thetas))
        x (* (max 0 (min 1 t)) n)
        i (min (dec n) (int (m/floor x)))
        a (thetas i) b (thetas (inc i))
        b (if (< b a) (+ b (* 2 m/pi)) b)]
    (+ a (* (- x i) (- b a)))))

(defn- ellipse [palette figures params f]
  [:g (stroke (ellipse-pts figures params) (:found palette) 0.016 f)
   (dot S (:ink palette) (max 0.001 f)) (label palette S "S" f {:dx -0.05 :dy -0.15})])

(defn- body [palette figures params th f]
  (let [[P head] (figure/points (:orbit figures) params [[th 0] [th 1]])]
    [:g (stroke [P head] (:construction palette) 0.025 f)
     (dot head (:construction palette) (* 0.7 f))
     (dot P (:ink palette) f) (label palette P "P" f)]))

(defn- focal
  "[SP PH] at theta, by the :focal raster kernel."
  [figures ps th]
  (first (figure/points (:focal figures) (subvec (vec ps) 0 2) [[th]])))

(defmethod scene/draw [eid :ellipse] [_ _ p {:keys [palette figures data]}]
  (let [ps (:params data)]
    [:g (ellipse palette figures ps (a/play p 0 0.6)) (body palette figures ps 1.0 (a/play p 0.5 0.8))]))

(defmethod scene/draw [eid :focus] [_ _ p {:keys [palette figures data]}]
  (let [ps (:params data) H (:H data)
        [P] (on-orbit figures ps [1.0])
        f (a/play p 0.1 0.6)]
    [:g (ellipse palette figures ps 1) (body palette figures ps 1.0 1)
     (stroke [S P H] (:construction palette) 0.01 f)
     (dot H (:muted palette) f) (label palette H "H" f {:dx -0.05 :dy -0.15})
     (readout palette (a/play p 0.5 0.8) [0.6 1.5] [(str "SP + PH = " (fmt (reduce + (focal figures ps 1.0)) 3))
                                                    "= 2AC, the whole axis"])]))

(defmethod scene/draw [eid :qr] [_ _ p {:keys [palette figures data]}]
  (let [ps (:params data) th 1.0 dth (* 0.5 (- 1 (a/play p 0.1 0.9)))
        [P Q] (on-orbit figures ps [th (+ th dth)])]
    [:g (ellipse palette figures ps 1) (body palette figures ps th 1)
     (tri palette [S P Q] (:found palette) 1)
     (dot Q (:ink palette) 1) (label palette Q "Q" 1)
     (readout palette 1 [0.6 1.5] [(str "angle PSQ = " (fmt dth 3)) "Q runs into P:" "L x QR = QT^2 ultimately"])]))

(defmethod scene/draw [eid :force] [_ _ p {:keys [palette figures data]}]
  (let [ps (:params data)
        th (theta-at data (a/play p 0 1 a/linear))
        [r] (focal figures ps th)]
    [:g (ellipse palette figures ps 1) (body palette figures ps th 1)
     (readout palette 1 [0.6 1.5] [(str "SP = " (fmt r 3))
                                   (str "force x SP^2 = " (fmt (* (/ 1.0 (* r r)) r r) 3))
                                   "the arrow: as 1/SP^2"])]))

(defmethod scene/draw [eid :around] [_ _ p {:keys [palette figures data]}]
  (let [ps (:params data)
        t (a/play p 0 1 a/linear)
        ths (mapv #(theta-at data (/ % 12)) (range 13))
        k (int (m/floor (* 12 t)))]
    (into [:g (ellipse palette figures ps 1) (body palette figures ps (theta-at data t) 1)]
          (concat
           (for [i (range k) :let [a (nth ths i) b (nth ths (inc i))
                                   b (if (< b a) (+ b (* 2 m/pi)) b)
                                   arc (on-orbit figures ps (mapv (fn [j] (+ a (* (- b a) (/ j 8)))) (range 9)))]]
             (svg/polygon (cons S arc) {:fill (if (even? i) (:found palette) (:construction palette)) :opacity 0.3}))
           [(readout palette 1 [0.6 1.5] ["equal times, equal areas" "u'' + u = 1/p on the ellipse"])]))))
