(ns alexandria.archimedes.method-scenes
  "The drawings of The Method, Proposition 1 (alexandria.archimedes.method),
   one method per stage (alexandria.medium.scene/draw), animated in Manim's
   idiom (alexandria.medium.anim): outlines are created stroke by stroke,
   labels fade in with a shift, the bar rocks until it is balanced.

   ctx :data carries Heath's points; ctx :figures the :hang figure, which
   carries every slice of the segment to H."
  (:require [alexandria.medium.anim :as a]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.math :as m]
            [alexandria.medium.plane :as plane]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            #?(:cljs [alexandria.medium.player :as player])))

#?(:cljs (def render
           "Clerk's render-fn for these scenes."
           player/render))

(def ^:private scene-id :archimedes/method-1)

;; ---------------------------------------------------------------------------
;; The figure, in plain numbers (Heath's points come in ctx :data :points)

(defn- curve [t] (- 1 (* t t)))
(def ^:private arc (mapv (fn [i] (let [t (- (/ i 40) 1)] [t (curve t)])) (range 81)))
(def ^:private slice-count 61)
(def ^:private slice-states
  (vec (for [i (range slice-count) e [0 1]] [(- (* 2 (/ i (dec slice-count))) 1) e])))

(defn- slice-at
  "M, N, O, P of the line through x = t."
  [t]
  {:O [t 0] :P [t (curve t)] :N [t (- 1 t)] :M [t (- 2 (* 2 t))]})

(defn- fmt [x digits]
  #?(:clj (format (str "%." digits "f") (double x))
     :cljs (.toFixed (js/Number x) digits)))

;; ---------------------------------------------------------------------------
;; Drawing in Manim's idiom

(def ^:private text-scale 1.45)

(defn- write
  ([palette at s] (write palette at s {}))
  ([palette at s opts]
   (svg/text at s (-> (merge {:colour (:ink palette) :size 0.13} opts)
                      (update :size * text-scale)))))

(defn- label
  "A point's letter, faded in by f with a small upward shift."
  [palette at s f & [{:keys [dx dy] :or {dx 0.09 dy 0.09}}]]
  (svg/layer (a/fade f [0 -0.15])
             (write palette (plane/translate at [dx dy]) s {:italic? true :anchor "start"})))

(defn- stroke
  "A polyline through ps created to fraction f."
  [ps colour width f]
  (when (pos? f) (svg/polyline ps {:stroke colour :width width :attrs (a/create f)})))

(defn- dot [at colour f]
  (when (pos? f) (svg/circle at (* 0.045 f) {:fill colour})))

(defn- rocking-bar
  "The bar CH turned by deg about K, with the fulcrum below K."
  [palette {:keys [C H K]} deg f]
  (let [[kx ky] K]
    [:g
     [:g {:transform (str "rotate(" (- deg) " " kx " " (- ky) ")")}
      (stroke [C H] (:ink palette) 0.04 f)]
     (svg/layer (a/fade f)
                (svg/polygon [[kx ky] [(- kx 0.14) (- ky 0.24)] [(+ kx 0.14) (- ky 0.24)]]
                             {:fill (:muted palette) :stroke (:ink palette) :width 0.012}))]))

(defn- segment-figure
  "The segment ABC: curve, chord, fill, triangle ABC, DB; each part at its
   own progress in `fs` (0 hidden .. 1 drawn)."
  [palette {:keys [A B C D]} {:keys [curve-f fill-f chord-f tri-f]}]
  [:g
   (when (pos? fill-f) (svg/polygon arc {:fill (:found palette) :opacity (* 0.22 fill-f)}))
   (stroke arc (:found palette) 0.03 curve-f)
   (stroke [A C] (:ink palette) 0.02 chord-f)
   (stroke [A B C] (:ink palette) 0.016 tri-f)
   (stroke [D B] (:muted palette) 0.012 tri-f)])

(defn- construction
  "AKF, BE, the tangent CF, CK and KH, each created at its progress."
  [palette {:keys [A B C E F K H]} [f-af f-be f-cf f-ck f-kh]]
  (let [blue (:construction palette)]
    [:g
     (stroke [A F] blue 0.016 f-af)
     (stroke [B E] blue 0.016 f-be)
     (stroke [C F] blue 0.02 f-cf)
     (stroke [C K] blue 0.016 f-ck)
     (stroke [K H] blue 0.016 f-kh)]))

(defn- letters [palette pts names fs]
  (into [:g] (map (fn [n f] (label palette (pts n) (name n) f)) names fs)))

(defn- whole-figure
  "Everything the first two steps built, fully drawn."
  [palette pts]
  [:g (segment-figure palette pts {:curve-f 1 :fill-f 1 :chord-f 1 :tri-f 1})
   (construction palette pts [1 1 1 1 1])
   (letters palette pts [:A :B :C :D :E :F :K :H] (repeat 1))])

(defn- readout
  "Lines of mono text at the right of the figure, faded in together."
  [palette f lines]
  (svg/layer (a/fade f [0.2 0])
             (into [:g] (map-indexed (fn [i s] (write palette [1.45 (- 4.5 (* 0.32 i))] s
                                                      {:anchor "start" :size 0.1 :mono? true}))
                                     lines))))

(defn- slice-lines
  "MO (construction) and OP (found) at x = t, with M N O P dotted."
  [palette t f]
  (let [{:keys [O P N M]} (slice-at t)]
    [:g
     (stroke [O M] (:construction palette) 0.022 f)
     (stroke [O P] (:found palette) 0.04 f)
     (dot M (:construction palette) f) (dot N (:ink palette) f)
     (dot O (:ink palette) f) (dot P (:found palette) f)
     (label palette M "M" f) (label palette N "N" f {:dx -0.22 :dy 0.05})
     (label palette O "O" f {:dx 0.05 :dy -0.24}) (label palette P "P" f)]))

;; ---------------------------------------------------------------------------
;; The stages

(defmethod scene/draw [scene-id :segment] [_ _ p {:keys [palette data]}]
  (let [pts (:points data)
        [l-a l-b l-c l-d] (a/lagged p 4 0.4 0.55 0.95)]
    [:g
     (segment-figure palette pts {:curve-f (a/play p 0 0.4) :fill-f (a/play p 0.35 0.65)
                                  :chord-f (a/play p 0.3 0.5) :tri-f (a/play p 0.55 0.85)})
     (letters palette pts [:A :B :C :D] [l-a l-b l-c l-d])]))

(defmethod scene/draw [scene-id :construct] [_ _ p {:keys [palette data]}]
  (let [pts (:points data)
        fs (a/lagged p 5 0.6 0 0.85)
        [l-e l-f l-k l-h] (a/lagged p 4 0.5 0.2 1)]
    [:g (segment-figure palette pts {:curve-f 1 :fill-f 1 :chord-f 1 :tri-f 1})
     (construction palette pts fs)
     (letters palette pts [:A :B :C :D] (repeat 1))
     (letters palette pts [:E :F :K :H] [l-e l-f l-k l-h])]))

(defmethod scene/draw [scene-id :balance] [_ _ p {:keys [palette data]}]
  (let [pts (:points data)]
    [:g (whole-figure palette pts)
     (rocking-bar palette pts (* 9 (a/wiggle (a/clamp01 (/ (- p 0.3) 0.7)))) (a/play p 0 0.35))
     (readout palette (a/play p 0.4 0.7) ["the bar CH" "fulcrum K, its middle" "HK = KC"])]))

(defmethod scene/draw [scene-id :slice] [_ _ p {:keys [palette data]}]
  (let [pts (:points data)
        t (+ -0.75 (* 1.35 (a/there-and-back-with-pause p)))
        {:keys [O P M]} (slice-at t)
        mo (second M) op (second P) ao (+ t 1)]
    [:g (whole-figure palette pts)
     (rocking-bar palette pts 0 1)
     (slice-lines palette t (a/play p 0 0.15))
     (readout palette (a/play p 0.05 0.25)
              [(str "MO : OP = " (fmt (/ mo op) 3))
               (str "CA : AO = " (fmt (/ 2 ao) 3))
               (str "HK : KN = " (fmt (/ 2 ao) 3))
               "the same ratio, for every line"])]))

(defn- weighed
  "One slice at x = t: MO in place, its copy TG carried to H by f, and the
   bar tilted by what is still unbalanced."
  [palette pts t f p]
  (let [{:keys [O P]} (slice-at t)
        len (second P)
        [hx hy] (:H pts)
        from [(first O) (/ len 2)]
        to [hx hy]
        [cx cy] (plane/lerp-point from to f)
        arc-lift (* 0.9 (m/sin (* m/pi f)))
        tilt (* 8 (- 1 f) (if (< f 1) 1 0))
        settle (* 3 (a/wiggle (a/clamp01 (/ (- p 0.72) 0.28))))]
    [:g
     (rocking-bar palette pts (+ tilt settle) 1)
     (slice-lines palette t 1)
     (svg/polyline [[cx (+ cy arc-lift (- (/ len 2)))] [cx (+ cy arc-lift (/ len 2))]]
                   {:stroke (:found palette) :width 0.05})
     (svg/layer (a/fade (a/play p 0.55 0.75))
                (write palette [(- hx 0.12) (+ hy (/ len 2) 0.12)] "TG" {:italic? true :anchor "end"}))]))

(defmethod scene/draw [scene-id :weigh] [_ _ p {:keys [palette data]}]
  (let [pts (:points data)
        t 0.3
        {:keys [P M]} (slice-at t)
        f (a/play p 0.1 0.6)]
    [:g (whole-figure palette pts)
     (weighed palette pts t f p)
     (readout palette (a/play p 0.6 0.8)
              [(str "MO x KN = " (fmt (* (second M) (+ t 1)) 3))
               (str "TG x HK = " (fmt (* (second P) 2) 3))
               "equal: in equilibrium about K"])]))

(defmethod scene/draw [scene-id :all-slices] [_ _ p {:keys [palette data figures]}]
  (let [pts (:points data)
        s (a/play p 0.05 0.95 a/linear)
        ends (partition 2 (figure/points (:hang figures) [s] slice-states))
        carried (count (filter (fn [[[x _]]] (< x -1.5)) ends))]
    (into [:g (whole-figure palette pts)
           (rocking-bar palette pts 0 1)]
          (concat
           (for [i (range slice-count)
                 :let [t (- (* 2 (/ i (dec slice-count))) 1) {:keys [O M]} (slice-at t)]]
             (svg/segment O M {:stroke (:construction palette) :width 0.008}))
           (for [[foot head] ends] (svg/segment foot head {:stroke (:found palette) :width 0.018}))
           [(readout palette (a/play p 0 0.2)
                     [(str "lines of the segment at H: " carried " of " slice-count)
                      "every pair balances about K,"
                      "so the bar stays level"])]))))

(defn- hung-pile
  "The outline of all the segment's lines hung at H as the :hang figure
   leaves them: each line of length OP centred at H, spread sideways."
  [[hx hy]]
  (let [ts (map #(- (/ % 20) 1) (range 41))
        x (fn [t] (+ hx (* 0.22 t)))]
    (concat (map (fn [t] [(x t) (+ hy (/ (curve t) 2))]) ts)
            (map (fn [t] [(x t) (- hy (/ (curve t) 2))]) (reverse ts)))))

(defn- medians [palette {:keys [A C F D E K]} fs]
  (let [ink (:muted palette)]
    [:g (stroke [A E] ink 0.012 (nth fs 0))
     (stroke [C K] ink 0.012 (nth fs 1))
     (stroke [F D] ink 0.012 (nth fs 2))]))

(defmethod scene/draw [scene-id :centres] [_ _ p {:keys [palette data]}]
  (let [{:keys [A C F W H] :as pts} (:points data)
        shrink (a/play p 0.6 1)
        tri (map #(plane/lerp-point % W shrink) [A C F])
        seg (map (fn [v] (plane/lerp-point v H shrink)) (hung-pile H))]
    [:g (rocking-bar palette pts 0 1)
     (svg/polygon tri {:fill (:construction palette) :opacity 0.3 :stroke (:construction palette) :width 0.016})
     (svg/polygon seg {:fill (:found palette) :opacity 0.6 :stroke (:found palette) :width 0.012})
     (medians palette pts (a/lagged p 3 0.5 0 0.5))
     (dot W (:ink palette) (a/play p 0.45 0.6))
     (label palette W "W" (a/play p 0.45 0.6))
     (letters palette pts [:A :C :F :K :H] (repeat (- 1 (* 0.7 shrink))))
     (readout palette (a/play p 0.4 0.6) ["W: where the medians meet" "CK = 3 KW"])]))

(defmethod scene/draw [scene-id :ratio] [_ _ p {:keys [palette data]}]
  (let [{:keys [A C F D E K W H] :as pts} (:points data)
        quarters [[A D K] [D C E] [K E F] [D E K]]
        fs (a/lagged p 4 0.5 0.35 0.85)]
    (into [:g (rocking-bar palette pts 0 1)
           (dot W (:construction palette) 1) (dot H (:found palette) 1)
           (label palette W "ACF" 1) (label palette H "segment" 1 {:dx -0.9 :dy 0.12})
           (readout palette (a/play p 0 0.25)
                    ["ACF x KW = segment x HK" "HK = 3 KW" "so segment = ACF / 3"])]
          (concat
           (map (fn [q f] (svg/polygon q {:stroke (:construction palette) :width 0.012
                                          :fill (:construction palette) :opacity (* 0.25 f)}))
                quarters fs)
           [(svg/layer (a/fade (a/play p 0.85 1))
                       (write palette [-0.3 4.55] "ACF = 4 ABC" {:size 0.13}))]))))

(defmethod scene/draw [scene-id :conclusion] [_ _ p {:keys [palette data]}]
  (let [pts (:points data)]
    [:g (segment-figure palette pts {:curve-f 1 :fill-f (+ 1 (* 2 (a/play p 0 0.5))) :chord-f 1 :tri-f 1})
     (letters palette pts [:A :B :C] (repeat 1))
     (svg/layer (a/fade (a/play p 0.2 0.5) [0 -0.2])
                (write palette [0 2.6] "segment ABC = 4/3 ABC" {:size 0.2}))
     (svg/layer (a/fade (a/play p 0.55 0.85))
                (write palette [0 2.05] "a sort of indication, not yet a demonstration"
                       {:size 0.1 :italic? true :colour (:muted palette)}))]))
