(ns alexandria.galileo.shot-widget
  "Galileo's projectile and falling body as tied controls (widget brief,
   section 5; the bar is the Apollonius conics playground).

   The shot: one state {:v :theta :R :H}, every key a Leva control, tied by
   the equations alexandria.galileo.shot derives with Emmy (g = 9.81):

     R = 2 v^2 sin(theta) cos(theta) / g     H = v^2 sin^2(theta) / (2 g)

     v moves       theta fixed: R and H forward
     theta moves   v fixed: R and H forward
     R moves       theta fixed: v = sqrt(g R / (2 sin cos)), then H
     H moves       theta fixed: v = sqrt(2 g H) / sin(theta), then R

   v is clamped to its slider and R and H are clamped to the values v's
   edges give, so no key leaves its range. The path is drawn by Mafs on
   the raster backend; the page names the curve live as Apollonius did
   (a parabola, παραβολή, with its latus rectum 2 v^2 cos^2(theta) / g) and
   names 45 degrees, the greatest range for this v, when theta is there.

   The fall: a drop height D and its time T = sqrt(2 D / g) tied, n equal
   times of T/n, and the distance fallen in each, in units of the first,
   from a raster kernel: 1 : 3 : 5 : 7 ..."
  (:require [alexandria.galileo.shot :as shot]
            [alexandria.medium.tie :as tie]
            [alexandria.raster :as raster]
            [clojure.walk :as walk]
            [emmy.clerk :as ec]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [nextjournal.clerk :as clerk]))

(def g shot/g)

(def tied [:v :theta :R :H])

(def v-range [2 30 0.01])

(def ranges
  "[min max step]; R's and H's maxima are those of v's maximum (R at 45
   degrees, H at 89)."
  {:v v-range
   :theta [1 89 0.1]
   :R [0 (/ (* 30 30) g) 0.01]
   :H [0 (/ (* 30 30) (* 2 g)) 0.01]})

(def labels {:v "v (m/s)" :theta "θ elevation °" :R "R range (m)" :H "H height (m)"})

(defn- lo [k] (first (ranges k)))
(defn- hi [k] (second (ranges k)))

(defn- forward [v deg]
  (let [th (tie/rad deg)] [v deg (shot/range-of v th g) (shot/height-of v th g)]))

(defn from-v [v deg _R _H] (forward (tie/clamp v (lo :v) (hi :v)) deg))
(defn from-theta [v deg _R _H] (forward v (tie/clamp deg (lo :theta) (hi :theta))))

(defn from-R
  "theta fixed: R held between the ranges of v's edges, then v."
  [_v deg R _H]
  (let [th (tie/rad deg)
        R' (tie/clamp R (shot/range-of (lo :v) th g) (shot/range-of (hi :v) th g))]
    (forward (shot/speed-of-range R' th g) deg)))

(defn from-H
  "theta fixed: H held between the heights of v's edges, then v."
  [_v deg _R H]
  (let [th (tie/rad deg)
        H' (tie/clamp H (shot/height-of (lo :v) th g) (shot/height-of (hi :v) th g))]
    (forward (shot/speed-of-height H' th g) deg)))

(defn tie
  "The tie kernel: 16 numbers, the solved state per source (order of `tied`)."
  [v deg R H]
  (fn [_] (vec (mapcat #(% v deg R H) [from-v from-theta from-R from-H]))))

(defn solve
  "The tie on the JVM by raster: {source [v theta R H]}."
  [[v deg R H]]
  (let [out (vec (binding [raster/*simplify?* false]
                   (raster/value (fn [v d R H] ((tie v d R H) 0)) v deg R H)))]
    (into {} (map (fn [[k o]] [k (subvec out o (+ o 4))]) (tie/source-block tied)))))

(def readout-index {:R 0 :H 1 :t 2 :p 3 :Rmax 4 :ratio 5 :c 6 :w 7})

(defn readout
  "[R H flight-time latus-rectum R-at-45 4H/R c w] by raster."
  [v deg _R _H]
  (fn [_]
    (let [th (tie/rad deg) c (e/* v (e/cos th)) w (e/* v (e/sin th))]
      [(shot/range-of v th g) (shot/height-of v th g) (e// (e/* 2 w) g)
       (e// (e/* 2 (e/square c)) g) (e// (e/square v) g)
       (e// (e/* 4 (shot/height-of v th g)) (shot/range-of v th g)) c w])))

(defn path
  "The shot (x(u), y(u)), u in [0 1] from the gun to the landing point."
  [v deg]
  (fn [u]
    (let [th (tie/rad deg)
          x (e/* u (shot/range-of v th g))]
      [x ((shot/trajectory v th g) x)])))

(defn path-45
  "The 45-degree shot of the same v, for comparison."
  [v]
  (path v 45))

(defn fall-readout
  "[T s1 s2 ... s8 / s1] for the drop D: T = sqrt(2D/g), then the distance
   in each of 8 equal times, in units of the first, by raster."
  [D]
  (fn [_]
    (let [T (shot/fall-time D g) tau (e// T 8)
          s (fn [k] (shot/fall-distance g (e/* k tau)))]
      (into [T] (map (fn [k] (e// (e/- (s (inc k)) (s k)) (s 1))) (range 8))))))

(defn init []
  (zipmap tied (binding [raster/*simplify?* false] (raster/value forward 18.0 30.0))))

(defn- template [subs form]
  (walk/postwalk (fn [x] (if (and (symbol? x) (contains? subs x)) (subs x) x)) form))

(def caption
  (str "Every slider is tied: move v or θ and the range R and the height H follow; move R or H and v is "
       "solved back with θ held. The blue curve is the shot, a parabola drawn by Mafs on the raster backend; "
       "the dashed grey one is the 45° shot of the same speed, the longest. Below, a body falls from rest "
       "through D: the bars are the distances fallen in eight equal times, from a raster kernel."))

(defn- body [{:keys [tie-b ro-b fall-b !p !s !f]}]
  (let [[tsym tform] tie-b [rsym rform] ro-b [fsym fform] fall-b
        n (fn [k d] (tie/fmt (list 'aget 'r (readout-index k)) d))
        opts {:tie-sym tsym :p !p :s !s :keys tied :extra [] :counter "__galileoSolves"
              :ranges ranges :watch-key ::tie}]
    (template
     {'?ro rsym '?fall fsym '?s !s '?f !f
      '?n-R (n :R 2) '?n-H (n :H 2) '?n-t (n :t 2) '?n-p (n :p 2) '?n-Rmax (n :Rmax 2) '?n-ratio (n :ratio 3)
      '?n-c (n :c 2) '?n-w (n :w 2) '?n-v (tie/fmt 'v 2) '?n-th (tie/fmt 'th 1) '?n-T (tie/fmt '(aget q 0) 3)
      '?leva (leva/sub-panel {:fill true :flat true :title-bar false}
                             (leva/controls {:atom !p :schema (tie/schema tied ranges labels)}))
      '?leva-f (leva/sub-panel {:fill true :flat true :title-bar false}
                               (leva/controls {:atom !f :schema {:D {:min 1 :max 100 :step 0.5 :label "D drop (m)"}}}))
      '?mafs (mafs/mafs {:view-box {:x [-2 95] :y [-3 48]} :height 380 :zoom true}
                        (mafs/cartesian {:subdivisions 1})
                        (mafs/parametric {:xy (ev/with-params {:atom !s :params [:v]} path-45) :t [0 1]
                                          :color "#9AA5B1" :style "dashed"})
                        (mafs/parametric {:xy (ev/with-params {:atom !s :params [:v :theta]} path) :t [0 1]
                                          :color "#1F6FB4" :weight 3})
                        'marks)
      '?caption caption
      '?watch-off (tie/unwatch-form !p)}
     (list 'reagent.core/with-let
           (into [tsym tform rsym rform fsym fform] (tie/locals opts))
           '(let [st @?s v (get st :v) th (get st :theta)
                  r (?ro (array 0) (array v th (get st :R) (get st :H)))
                  q (?fall (array 0) (array (get @?f :D)))
                  at45 (< (js/Math.abs (- th 45)) 0.05)
                  marks [:<>
                         [mafs.core/Point {:x (* 0.5 (aget r 0)) :y (aget r 1) :color "#C0392B"}]
                         [mafs.core/Point {:x (aget r 0) :y 0 :color "#111"}]]
                  ratios (mapv (fn [k] (aget q (inc k))) (range 8))]
              [:div {:data-figure "galileo-shot"
                     :data-ratios (js/JSON.stringify (clj->js ratios))
                     :data-R (str (aget r 0)) :data-H (str (aget r 1))}
               [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                [:div {:style {:flex "1 1 460px" :min-width "320px"}} ?mafs]
                [:div {:style {:flex "0 1 320px" :min-width "270px"}}
                 ?leva
                 [:div {:style {:font-size "1.2em" :font-weight 600 :margin "0.3em 0"}}
                  [:span {:data-conic-name "parabola"} "parabola"] " (e = 1): παραβολή"]
                 [:div {:data-max-name (if at45 "max" "")
                        :style {:font-weight 600 :color (if at45 "#2f8a3e" "#666")}}
                  (if at45
                    "θ = 45°: the greatest range for this speed, R = v²/g (Prop. VII)"
                    (str "45° would reach v²/g = " ?n-Rmax " m"))]
                 (let [tex (str "R = \\frac{2v^2\\sin\\theta\\cos\\theta}{g} = \\frac{2 \\cdot " ?n-v "^2 \\sin " ?n-th "^\\circ \\cos " ?n-th "^\\circ}{9.81} = " ?n-R)]
                   [:div {:data-formula tex} [:f> nextjournal.clerk.render/render-katex tex {:inline? false}]])
                 (let [tex (str "H = \\frac{v^2\\sin^2\\theta}{2g} = " ?n-H ",\\quad \\frac{4H}{R} = \\tan\\theta = " ?n-ratio)]
                   [:div [:f> nextjournal.clerk.render/render-katex tex {:inline? false}]])
                 [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.88em" :line-height 1.5}}
                  [:div "c = v cos θ = " ?n-c ",  w = v sin θ = " ?n-w]
                  [:div "flight time 2w/g = " ?n-t " s"]
                  [:div "latus rectum 2c²/g = " ?n-p " m"]]]]
               [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-end" :margin-top "0.8em"}}
                [:div {:style {:flex "0 1 300px"}} ?leva-f
                 [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.88em"}}
                  "T = √(2D/g) = " ?n-T " s, cut into 8 equal times"]]
                [:div {:data-fall "1" :style {:display "flex" :gap "6px" :align-items "flex-end" :height "170px"}}
                 (for [[k x] (map-indexed vector ratios)]
                   ^{:key k}
                   [:div {:style {:text-align "center" :font-size "0.8em"}}
                    [:div {:style {:width "26px" :height (str (* 10 x) "px") :background "#2E86C1"}}]
                    [:div (.toFixed x 3)]])]]
               [:p {:style {:font-size "0.9em" :opacity 0.85}} ?caption]])
           '(finally ?watch-off)))))

(defn widget
  ([] (widget (init)))
  ([start]
   (binding [vc/*backend* :raster]
     (let [s (mapv start tied)]
       (ev/with-let [!p start !s start !f {:D 50}]
         (body {:tie-b (tie/kernel-binding "tie" tie s)
                :ro-b (tie/kernel-binding "readout" readout s)
                :fall-b (tie/kernel-binding "fall" fall-readout [50])
                :!p !p :!s !s :!f !f}))))))

(defn derivation []
  (apply clerk/col
         (concat
          (for [{:keys [label eqs]} (shot/equations)]
            (apply clerk/col
                   (clerk/html [:div {:style {:font-size "0.85em" :opacity 0.7 :margin-top "0.5em"}} label])
                   (for [q eqs] (ec/->TeX q :simplify? true))))
          [(clerk/html
            (into [:ul {:style {:font-size "0.9em"}}]
                  (for [{:keys [label grade]} (shot/grades)]
                    [:li {:data-grade (name grade)} [:code (name grade)] " " label])))])))
