(ns alexandria.ptolemy.chords-scenes
  "The drawings of Ptolemy's two constructions (Almagest I.10, Halma 1813
   pp. 31-33), one sentence of his per step, his letters.

     :ptolemy/half  the chord of half an arc: semicircle ABG on AG, D the
                    midpoint of arc GB, AE = AB, DZ perpendicular to AG,
                    GD^2 = AG.GZ
     :ptolemy/sum   the chord of the sum of two arcs: circle ABGD about Z,
                    the diameter BZE, the quadrilateral BGDE and the lemma

   ctx :figures :half is alexandria.ptolemy.chords/half-figure (state
   [beta], 14 outputs), :sum is sum-figure (state [alpha beta], 15
   outputs); every coordinate and every number drawn comes out of those
   kernels. ctx :controls carries the reader's beta (and alpha).
   The figure is drawn in Ptolemy's parts (radius 60)."
  (:require [alexandria.medium.anim :as a]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            #?(:cljs [alexandria.medium.player :as player])))

#?(:cljs (def render "Clerk's render-fn for these scenes." player/render))

(def R 60)

(defn- fmt [x digits]
  #?(:clj (format (str "%." digits "f") (double x))
     :cljs (.toFixed (js/Number x) digits)))

(defn sexa
  "x parts as p m′ s″ (Ptolemy's sexagesimal), rounded to the second."
  [x]
  (let [total (Math/round (* 3600.0 (double x)))
        p (quot total 3600) m (quot (rem total 3600) 60) s (rem total 60)]
    (str p "ᵖ " m "′ " s "″")))

(defn- write [palette at s opts]
  (svg/text at s (merge {:colour (:ink palette) :size 5.5 :anchor "start"} opts)))

(defn- letter [palette at s dx dy]
  (svg/text [(+ (first at) dx) (+ (second at) dy)] s {:colour (:ink palette) :size 6.5 :italic? true}))

(defn- readout [palette at f lines]
  (svg/layer (a/fade f [0 0])
             (into [:g] (map-indexed (fn [i s] (write palette (update at 1 - (* 7 i)) s {:mono? true :size 4.6}))
                                     lines))))

(defn- dot [at colour] (svg/circle at 1.3 {:fill colour}))

(defn- arc-points
  "The circle of radius R from angle t0 to t1 (radians), n steps."
  [t0 t1 n]
  (mapv (fn [i] (let [t (+ t0 (* (- t1 t0) (/ i n)))] [(* R (Math/cos t)) (* R (Math/sin t))])) (range (inc n))))

;; ---------------------------------------------------------------------------
;; The half arc

(defn- half-state [{:keys [figures controls]}]
  (let [beta (get controls :beta 72)
        [bx by dx dy ex zx AB GB GD GZ half-diff AGGZ GD2 crd] (first (figure/points (:half figures) [R] [[beta]]))]
    {:beta beta :A [(- R) 0] :G [R 0] :B [bx by] :D [dx dy] :E [ex 0] :Z [zx 0]
     :AB AB :GB GB :GD GD :GZ GZ :half-diff half-diff :AGGZ AGGZ :GD2 GD2 :crd crd}))

(defn- half-base [palette {:keys [A G B D]} f]
  [:g (svg/polyline (arc-points 0 Math/PI 90) {:stroke (:ink palette) :width 0.6 :attrs (a/create f)})
   (svg/segment A G {:stroke (:ink palette) :width 0.6})
   (svg/segment G B {:stroke "#C0392B" :width 1.1})
   (svg/layer (a/fade f) (dot B "#C0392B") (dot D "#1F6FB4")
              (letter palette A "A" -4 -6) (letter palette G "G" 4 -6)
              (letter palette B "B" 1 4) (letter palette D "D" 4 3))])

(defmethod scene/draw [:ptolemy/half :setup] [_ _ p {:keys [palette] :as ctx}]
  (let [{:keys [beta GB] :as s} (half-state ctx)]
    [:g (half-base palette s (a/play p 0 0.5))
     (readout palette [-75 -14] (a/play p 0.4 0.7)
              [(str "arc GB = " (fmt beta 1) "°") (str "GB = crd " (fmt beta 1) "° = " (sexa GB))])]))

(defmethod scene/draw [:ptolemy/half :ae] [_ _ p {:keys [palette] :as ctx}]
  (let [{:keys [A B D E AB] :as s} (half-state ctx)
        f (a/play p 0 0.6)]
    [:g (half-base palette s 1)
     (svg/segment A B {:stroke "#7B5EA7" :width 0.8})
     (svg/segment A D {:stroke (:muted palette) :width 0.5})
     (svg/layer (a/fade f) (svg/segment B D {:stroke "#1F6FB4" :width 0.8})
                (svg/segment D E {:stroke "#1F6FB4" :width 0.8}) (dot E "#7B5EA7")
                (letter palette E "E" 0 -7))
     (readout palette [-75 -14] (a/play p 0.5 0.8)
              [(str "AE = AB = " (sexa AB)) "triangles ABD, AED: BD = DE = DG"])]))

(defmethod scene/draw [:ptolemy/half :dz] [_ _ p {:keys [palette] :as ctx}]
  (let [{:keys [A B D E Z G GZ half-diff AB] :as s} (half-state ctx)
        f (a/play p 0 0.6)]
    [:g (half-base palette s 1)
     (svg/segment A B {:stroke "#7B5EA7" :width 0.8})
     (svg/segment D E {:stroke "#1F6FB4" :width 0.6}) (svg/segment D G {:stroke "#1F6FB4" :width 0.8})
     (dot E "#7B5EA7") (letter palette E "E" 0 -7)
     (svg/layer (a/fade f) (svg/segment D Z {:stroke "#E67E22" :width 0.8 :dash "2 1.5"})
                (dot Z "#E67E22") (letter palette Z "Z" 2 -7)
                (svg/segment E G {:stroke "#2E8B57" :width 2}))
     (readout palette [-75 -14] (a/play p 0.5 0.8)
              [(str "EZ = ZG = " (sexa GZ)) (str "(AG − AB)/2 = (120ᵖ − " (sexa AB) ")/2 = " (sexa half-diff))])]))

(defmethod scene/draw [:ptolemy/half :result] [_ _ p {:keys [palette] :as ctx}]
  (let [{:keys [A D Z G GD GZ AGGZ GD2 crd beta] :as s} (half-state ctx)]
    [:g (half-base palette s 1)
     (svg/polygon [A D G] {:fill "#1F6FB4" :opacity (* 0.15 (a/play p 0 0.4)) :stroke "#1F6FB4" :width 0.5})
     (svg/segment D Z {:stroke "#E67E22" :width 0.8 :dash "2 1.5"}) (dot Z "#E67E22") (letter palette Z "Z" 2 -7)
     (svg/segment D G {:stroke "#C0392B" :width 2})
     (readout palette [-75 -14] (a/play p 0.3 0.7)
              [(str "AG·GZ = 120 × " (fmt GZ 4) " = " (fmt AGGZ 3))
               (str "GD² = " (fmt GD2 3))
               (str "GD = crd " (fmt (/ beta 2) 2) "° = " (sexa GD))
               (str "120 sin(" (fmt (/ beta 4) 3) "°) = " (sexa crd))])]))

;; ---------------------------------------------------------------------------
;; The sum of two arcs

(defn- sum-state [{:keys [figures controls]}]
  (let [alpha (get controls :alpha 72) beta (get controls :beta2 60)
        [bx by gx gy ex ey AB BG BD GE DE BE GD AG crd] (first (figure/points (:sum figures) [R] [[alpha beta]]))]
    {:alpha alpha :beta beta :A [(- R) 0] :D [R 0] :Z [0 0] :B [bx by] :G [gx gy] :E [ex ey]
     :AB AB :BG BG :BD BD :GE GE :DE DE :BE BE :GD GD :AG AG :crd crd}))

(defn- sum-base [palette {:keys [A D Z B G]} f]
  [:g (svg/polyline (arc-points 0 (* 2 Math/PI) 120) {:stroke (:ink palette) :width 0.6 :attrs (a/create f)})
   (svg/segment A D {:stroke (:ink palette) :width 0.5})
   (svg/segment A B {:stroke "#7B5EA7" :width 1.1}) (svg/segment B G {:stroke "#C0392B" :width 1.1})
   (svg/layer (a/fade f) (dot B "#7B5EA7") (dot G "#C0392B") (dot Z (:ink palette))
              (letter palette A "A" -5 -2) (letter palette D "D" 5 -2) (letter palette Z "Z" 2 -6)
              (letter palette B "B" -3 4) (letter palette G "G" 2 4))])

(defmethod scene/draw [:ptolemy/sum :setup] [_ _ p {:keys [palette] :as ctx}]
  (let [{:keys [alpha beta AB BG] :as s} (sum-state ctx)]
    [:g (sum-base palette s (a/play p 0 0.5))
     (readout palette [-75 -68] (a/play p 0.4 0.7)
              [(str "AB = crd " (fmt alpha 1) "° = " (sexa AB) ",  BG = crd " (fmt beta 1) "° = " (sexa BG))])]))

(defmethod scene/draw [:ptolemy/sum :diameter] [_ _ p {:keys [palette] :as ctx}]
  (let [{:keys [B D G E BD GE DE] :as s} (sum-state ctx)
        f (a/play p 0 0.6)]
    [:g (sum-base palette s 1)
     (svg/layer (a/fade f) (svg/segment B E {:stroke (:muted palette) :width 0.6 :dash "2 1.5"})
                (dot E (:ink palette)) (letter palette E "E" 3 -5)
                (svg/segment B D {:stroke "#1F6FB4" :width 0.8}) (svg/segment G E {:stroke "#1F6FB4" :width 0.8})
                (svg/segment D E {:stroke "#2E8B57" :width 0.8}) (svg/segment G D {:stroke "#E67E22" :width 0.6 :dash "1.5 1.5"}))
     (readout palette [-75 -68] (a/play p 0.5 0.8)
              [(str "BD = " (sexa BD) ", GE = " (sexa GE) ", DE = AB = " (sexa DE))])]))

(defmethod scene/draw [:ptolemy/sum :quad] [_ _ p {:keys [palette] :as ctx}]
  (let [{:keys [B D G E BD GE BG DE BE GD] :as s} (sum-state ctx)
        f (a/play p 0 0.5)]
    [:g (sum-base palette s 1)
     (svg/polygon [B G D E] {:fill "#E6A700" :opacity (* 0.18 f) :stroke "#E6A700" :width 0.6})
     (svg/segment B D {:stroke "#1F6FB4" :width 0.8}) (svg/segment G E {:stroke "#1F6FB4" :width 0.8})
     (svg/segment B E {:stroke (:muted palette) :width 0.6 :dash "2 1.5"}) (dot E (:ink palette)) (letter palette E "E" 3 -5)
     (svg/segment G D {:stroke "#E67E22" :width 1.6})
     (readout palette [-75 -61] (a/play p 0.3 0.7)
              [(str "BD·GE = " (fmt (* BD GE) 2) " = BG·DE + BE·GD")
               (str "GD = (BD·GE − BG·DE)/BE = " (sexa GD))])]))

(defmethod scene/draw [:ptolemy/sum :result] [_ _ p {:keys [palette] :as ctx}]
  (let [{:keys [A G D GD AG crd alpha beta] :as s} (sum-state ctx)
        f (a/play p 0 0.5)]
    [:g (sum-base palette s 1)
     (svg/segment G D {:stroke "#E67E22" :width 1})
     (svg/layer (a/fade f) (svg/segment A G {:stroke "#2E8B57" :width 2}))
     (readout palette [-75 -61] (a/play p 0.3 0.7)
              [(str "AG = √(120² − GD²) = " (sexa AG) " = crd " (fmt (+ alpha beta) 1) "°")
               (str "120 sin(" (fmt (/ (+ alpha beta) 2) 2) "°) = " (sexa crd))])]))
