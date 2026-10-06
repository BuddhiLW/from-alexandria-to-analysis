(ns alexandria.dedekind.cuts-scenes
  "The drawings of Dedekind's cut of sqrt 2 (alexandria.dedekind.cuts), one
   method per stage (alexandria.medium.scene/draw), in Manim's idiom: the
   rationals split onto the two sides of the line and the camera zooms on
   sqrt 2, where no rational ever lands.

   ctx :figures :line places the rational m/n (state [m n]) on the line
   magnified about sqrt 2, A1 above, A2 below; ctx :data carries the
   rationals (:states), Dedekind's y-map climbs (:climb, :fall) and the
   descent pairs (:descent)."
  (:require [alexandria.medium.anim :as a]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.math :as m]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            #?(:cljs [alexandria.medium.player :as player])))

#?(:cljs (def render "Clerk's render-fn for these scenes." player/render))

(def ^:private scene-id :dedekind/cut-sqrt-2)
(def ^:private sqrt2 (m/sqrt 2))
(def ^:private half-width 3.2)

(defn- fmt [x digits]
  #?(:clj (format (str "%." digits "f") (double x))
     :cljs (.toFixed (js/Number x) digits)))

(defn- write [palette at s & [opts]]
  (svg/text at s (merge {:colour (:ink palette) :size 0.17} opts)))

(defn- caption [palette f lines]
  (svg/layer (a/fade f [0 -0.1])
             (into [:g] (map-indexed (fn [i s] (write palette [(- half-width) (- 1.15 (* 0.24 i))] s
                                                     {:anchor "start" :size 0.13 :mono? true}))
                                     lines))))

(defn- zoom-of
  "The magnification at progress z in [0 1]: 2.2 (the line from 0 to 2)
   to 2.2 * 400."
  [z]
  (* 2.2 (m/pow 400 z)))

(defn- axis [palette zoom f]
  (let [tick (fn [v] (let [x (* zoom (- v sqrt2))]
                       (when (< (m/abs x) half-width)
                         [:g (svg/segment [x -0.06] [x 0.06] {:stroke (:muted palette) :width 0.012})
                          (write palette [x -0.3] (str v) {:size 0.12 :colour (:muted palette)})])))]
    (into [:g (svg/polyline [[(- half-width) 0] [half-width 0]]
                            {:stroke (:ink palette) :width 0.02 :attrs (a/create f)})]
          (keep tick [0 1 2 1.4 1.41 1.42 1.414 1.415]))))

(defn- marker [palette f]
  (svg/layer (a/fade f)
             (svg/segment [0 -0.55] [0 0.55] {:stroke (:ink palette) :width 0.012 :dash "0.05 0.05"})
             (write palette [0 0.68] "√2" {:size 0.18})))

(defn- dots
  "Every rational of the data, placed by the :line figure at zoom; f fades
   them in, lift separates the classes."
  [palette {:keys [figures data]} zoom lift f]
  (let [states (:states data)
        pts (figure/points (:line figures) [sqrt2 zoom lift] states)]
    (into [:g]
          (keep (fn [[x y]]
                  (when (< (m/abs x) half-width)
                    (svg/circle [x y] (* 0.035 f) {:fill (if (pos? y) (:construction palette) (:found palette))}))))
          pts)))

(defn- classes-legend [palette f]
  (svg/layer (a/fade f)
             (write palette [(- half-width) 0.55] "A1: r ≤ 0 or r² < 2" {:anchor "start" :size 0.13 :colour (:construction palette)})
             (write palette [(- half-width) -0.6] "A2: r > 0 and r² > 2" {:anchor "start" :size 0.13 :colour (:found palette)})))

(defmethod scene/draw [scene-id :line] [_ _ p {:keys [palette] :as ctx}]
  [:g (axis palette (zoom-of 0) (a/play p 0 0.4))
   (dots palette ctx (zoom-of 0) 0 (a/play p 0.3 0.9))
   (caption palette (a/play p 0.5 0.8) ["the rationals m/n, n ≤ 24, on the line"])])

(defmethod scene/draw [scene-id :gap] [_ _ p {:keys [palette] :as ctx}]
  (let [z (a/play p 0.15 0.9)]
    [:g (axis palette (zoom-of z) 1)
     (dots palette ctx (zoom-of z) 0 1)
     (marker palette (a/play p 0 0.2))
     (caption palette (a/play p 0.2 0.4) [(str "zoom ×" (fmt (/ (zoom-of z) 2.2) 0))
                                          "dense everywhere, yet none lands on √2"])]))

(defmethod scene/draw [scene-id :cut] [_ _ p {:keys [palette] :as ctx}]
  (let [lift (* 0.22 (a/play p 0.1 0.6))]
    [:g (axis palette (zoom-of 0.35) 1)
     (dots palette ctx (zoom-of 0.35) lift 1)
     (marker palette 1)
     (classes-legend palette (a/play p 0.4 0.7))]))

(defmethod scene/draw [scene-id :no-square] [_ _ p {:keys [palette data]}]
  (let [pairs (:descent data)
        k (count pairs)
        shown (a/lagged p k 0.6 0.05 0.85)]
    (into [:g (write palette [0 1.1] "t² − 2u² = 0 would descend for ever:" {:size 0.15})]
          (map-indexed (fn [i [[t u] f]]
                         (svg/layer (a/fade f [0.2 0])
                                    (write palette [(- (* 0.95 i) 2.4) 0.2] (str t "/" u)
                                           {:size 0.2 :colour (:found palette)})
                                    (write palette [(- (* 0.95 i) 2.4) -0.2] (str "u = " u)
                                           {:size 0.11 :mono? true :colour (:muted palette)})))
                       (map vector pairs shown)))))

(defn- climb-dots
  "The successive y-map values xs at height y, each placed and labelled in
   turn as f runs from 0 to 1."
  [palette zoom xs f colour y]
  (into [:g]
        (map-indexed (fn [i x]
                       (let [g (a/play f (* i 0.25) (+ 0.25 (* i 0.25)))
                             px (* zoom (- x sqrt2))]
                         (when (< (m/abs px) half-width)
                           [:g (svg/circle [px y] (* 0.06 g) {:fill colour})
                            (svg/layer (a/fade g) (write palette [px (+ y (if (pos? y) 0.15 -0.25) (* 0.16 (mod i 2) (if (pos? y) 1 -1)))] (fmt x 6)
                                                         {:size 0.1 :mono? true}))])))
                     xs)))

(defmethod scene/draw [scene-id :no-greatest] [_ _ p {:keys [palette data] :as ctx}]
  (let [zoom (zoom-of 0.55)]
    [:g (axis palette zoom 1)
     (dots palette ctx zoom 0.22 0.6)
     (marker palette 1)
     (climb-dots palette zoom (:climb data) (a/play p 0 0.5 a/linear) (:construction palette) 0.3)
     (climb-dots palette zoom (:fall data) (a/play p 0.5 1 a/linear) (:found palette) -0.3)
     (caption palette (a/play p 0 0.2) ["y = x(x² + 6)/(3x² + 2): a better one, every time"])]))

(defmethod scene/draw [scene-id :create] [_ _ p {:keys [palette] :as ctx}]
  [:g (axis palette (zoom-of 0.35) 1)
   (dots palette ctx (zoom-of 0.35) 0.22 1)
   (svg/circle [0 0] (* 0.09 (a/play p 0.2 0.6)) {:fill (:ink palette)})
   (marker palette 1)
   (caption palette (a/play p 0.4 0.7) ["the cut itself is the new number √2"])])

(defmethod scene/draw [scene-id :eudoxus] [_ _ p {:keys [palette data]}]
  (let [rows (:eudoxus data)
        fs (a/lagged p (count rows) 0.5 0.05 0.9)]
    (into [:g (write palette [0 1.15] "Elements V Def. 5: n·diagonal against m·side" {:size 0.15})]
          (map-indexed (fn [i [{:keys [m n verdict]} f]]
                         (svg/layer (a/fade f [0.2 0])
                                    (write palette [-1.6 (- 0.75 (* 0.26 i))] (str m "/" n)
                                           {:size 0.14 :mono? true})
                                    (write palette [-0.2 (- 0.75 (* 0.26 i))]
                                           (str n "d " (if (= :exceeds verdict) ">" "<") " " m "s")
                                           {:size 0.14 :mono? true})
                                    (write palette [1.6 (- 0.75 (* 0.26 i))]
                                           (if (= :exceeds verdict) "A1" "A2")
                                           {:size 0.14 :mono? true
                                            :colour (if (= :exceeds verdict) (:construction palette) (:found palette))})))
                       (map vector rows fs)))))

(defmethod scene/draw [scene-id :complete] [_ _ p {:keys [palette] :as ctx}]
  (let [z (- 1 (a/play p 0 0.7))]
    [:g (axis palette (zoom-of (* 0.35 z)) 1)
     (dots palette ctx (zoom-of (* 0.35 z)) (* 0.22 z) 1)
     (svg/polyline [[(- half-width) 0] [half-width 0]]
                   {:stroke (:found palette) :width 0.035 :attrs (a/create (a/play p 0.5 1))})
     (caption palette (a/play p 0.6 0.9) ["every cut of the reals is made by one real:" "no gaps left"])]))
