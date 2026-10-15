(ns alexandria.alhazen.widgets
  "The widgets of the Ibn al-Haytham notebook, emmy-viewers fragments on
   the raster backend; every number from a kernel of
   alexandria.alhazen.mirror.

   `problem`   Alhazen's problem on a convex spherical mirror (Risner
               V.39) as a tied widget: eye distance a, visible point at
               distance b and angle al, reflection point ph. Moving a, b or
               al solves ph (bisection on the browser kernel of the
               equal-angles residual g, between the tangent limits);
               moving ph solves b back (the inverse kernel source-for).
               The figure: circle, eye, object, the two rays and the normal,
               the two equal angles numbered live.
   `parabola`  the parabolic burning mirror: a pencil of rays parallel to
               the axis, each reflected by the kernel parabola-hit, all
               through the focus; f and the number of rays as controls."
  (:require [alexandria.alhazen.mirror :as mirror]
            [alexandria.medium.flow :as flow]
            [alexandria.medium.tie :as tie]
            [emmy.leva :as leva]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]))

(def keys* [:a :b :al :ph])
(def ranges {:a [1.05 6 0.01] :b [1.05 8 0.01] :al [0.2 3.0 0.01] :ph [0.0 3.0 0.001]})
(def labels {:a "a, eye from the centre" :b "b, object from the centre"
             :al "angle of the object at the centre" :ph "reflection point (angle)"})
(def start {:a 2.0 :b 3.0 :al 1.5 :ph 0.6680715563582877})

(defn- kf [f] (fn [& args] (fn [_] (apply f args))))

(def ^:private S 70)
(defn- px [x] (list '+ 230 (list '* S x)))
(defn- py [y] (list '- 200 (list '* S y)))

(defn- solve-ph-form
  "Browser form: ph in the visible arc by 50 bisection steps on ?g."
  [a b al]
  (list 'let ['lo (list 'max 0 (list '- al (list 'js/Math.acos (list '/ 1 b))))
              'hi (list 'min al (list 'js/Math.acos (list '/ 1 a)))
              'gv (list 'fn ['x] (list 'aget (list '?g '(array 0) (list 'array a b al 'x)) 0))]
        (list 'if '(>= lo hi) ##NaN
              '(loop [lo (+ lo 1e-12) hi (- hi 1e-12) glo (gv (+ lo 1e-12)) i 0]
                 (if (= i 50) (/ (+ lo hi) 2)
                     (let [m (/ (+ lo hi) 2) gm (gv m)]
                       (if (pos? (* glo gm)) (recur m hi gm (inc i)) (recur lo m glo (inc i)))))))))

(defn problem
  "Section 3: Alhazen's problem, tied."
  []
  (binding [vc/*backend* :raster]
    (ev/with-let [!p start]
      (let [[gsym gform] (flow/kernel "g" (kf mirror/g) [2 3 1.5 0.6] [0])
            [ssym sform] (flow/kernel "src" (kf mirror/source-for) [2 1.5 0.6] [0])]
        (flow/template
         {'?p !p '?g gsym '?s ssym}
         (list 'reagent.core/with-let
               [gsym gform ssym sform
                'last '(atom @?p)
                '_ (list 'add-watch '?p ::tie
                         (list 'fn '[_ _ old new]
                               (list 'when-not '(= new @last)
                                     (list 'let ['src '(first (filter #(not= (get old %) (get new %)) [:a :b :al :ph]))
                                                 'st (list 'if '(= src :ph)
                                                           '(assoc new :b (min 8 (max 1.05 (aget (?s (array 0) (array (:a new) (:al new) (:ph new))) 0))))
                                                           (list 'assoc 'new :ph (solve-ph-form '(:a new) '(:b new) '(:al new))))]
                                           '(reset! last st)
                                           '(js/setTimeout (fn [] (reset! ?p st)) 0)))))]
               (list 'let ['{:keys [a b al ph]} '@?p
                           'Px '(js/Math.cos ph) 'Py '(js/Math.sin ph)
                           'Bx '(* b (js/Math.cos al)) 'By '(* b (js/Math.sin al))
                           'ang '(fn [qx qy] (let [ux (- qx Px) uy (- qy Py)]
                                               (* (/ 180 js/Math.PI) (js/Math.acos (/ (+ (* ux Px) (* uy Py)) (js/Math.hypot ux uy))))))
                           'res '(aget (?g (array 0) (array a b al ph)) 0)]
                     [:div {:data-figure "alhazen-problem" :data-ph '(str ph) :data-residual '(str res)
                            :style {:display "flex" :flex-wrap "wrap" :gap "1em"}}
                      [:svg {:viewBox "0 0 460 400" :style {:flex "1 1 420px" :max-width "560px"}}
                       [:circle {:cx 230 :cy 200 :r S :fill "#8A9BB022" :stroke "#555" :stroke-width 2}]
                       [:line {:x1 230 :y1 200 :x2 (px '(* 1.6 Px)) :y2 (py '(* 1.6 Py)) :stroke "#999" :stroke-dasharray "4 3"}]
                       [:line {:x1 (px 'a) :y1 (py 0) :x2 (px 'Px) :y2 (py 'Py) :stroke "#1F6FB4" :stroke-width 2}]
                       [:line {:x1 (px 'Bx) :y1 (py 'By) :x2 (px 'Px) :y2 (py 'Py) :stroke "#E39B2D" :stroke-width 2}]
                       [:circle {:cx (px 'a) :cy (py 0) :r 6 :fill "#1F6FB4"}]
                       [:text {:x (px 'a) :y (py -0.3) :font-size 14} "eye A"]
                       [:circle {:cx (px 'Bx) :cy (py 'By) :r 6 :fill "#E39B2D"}]
                       [:text {:x (px 'Bx) :y (py '(+ By 0.2)) :font-size 14} "object B"]
                       [:circle {:cx (px 'Px) :cy (py 'Py) :r 5 :fill "#C0392B"}]
                       [:text {:x (px '(* 1.15 Px)) :y (py '(* 1.15 Py)) :font-size 14} "P"]]
                      [:div {:style {:flex "0 1 330px"}}
                       (leva/sub-panel {:fill true :flat true :title-bar false}
                                       (leva/controls {:atom !p :schema (tie/schema keys* ranges labels)}))
                       [:div {:data-angles true :style {:font-family "ui-monospace, monospace" :margin "0.5em 0"}}
                        "angle AP to the normal: " (tie/fmt '(ang a 0) 3) "°" [:br]
                        "angle BP to the normal: " (tie/fmt '(ang Bx By) 3) "°" [:br]
                        "residual g: " (tie/fmt 'res 12)]
                       [:div {:style {:font-size "0.85em" :opacity 0.8}}
                        "Move the eye or the object and the reflection point is solved (bisection on the kernel of the equal-angles law); move P and the object's distance is solved back along its ray."]]])
               '(finally (remove-watch ?p ::tie))))))))

(defn parabola
  "Section 4: rays parallel to the axis on the mirror y^2 = 4 f x, each
   reflected by the kernel parabola-hit (third number: where it crosses
   the axis, minus f)."
  []
  (binding [vc/*backend* :raster]
    (ev/with-let [!q {:f 1.0 :rays 9}]
      (let [[hsym hform] (flow/kernel "hit" (kf mirror/parabola-hit) [1 1] [0])]
        (flow/template
         {'?q !q '?h hsym}
         (list 'reagent.core/with-let [hsym hform]
               (list 'let ['{:keys [f rays]} '@?q
                           'n '(js/Math.round rays)
                           'hs '(for [i (range n)] (* 3.2 (- (/ (+ i 0.5) n) 0.5) 2))
                           'hits '(vec (for [h hs] (let [o (?h (array 0) (array f h))] [(aget o 0) (aget o 1) (aget o 2)])))
                           'X '(fn [x] (+ 60 (* 60 x))) 'Y '(fn [y] (- 200 (* 50 y)))
                           'gap '(apply max (map #(js/Math.abs (nth % 2)) hits))]
                     [:div {:data-figure "alhazen-parabola" :data-gap '(str gap)
                            :style {:display "flex" :flex-wrap "wrap" :gap "1em"}}
                      [:svg {:viewBox "0 0 460 400" :style {:flex "1 1 420px" :max-width "560px"}}
                       [:polyline {:fill "none" :stroke "#555" :stroke-width 3
                                   :points '(apply str (for [i (range 81)] (let [y (- (* i 0.1) 4)] (str ((fn [x] (+ 60 (* 60 x))) (/ (* y y) (* 4 f))) "," (- 200 (* 50 y)) " "))))}]
                       [:line {:x1 0 :y1 200 :x2 460 :y2 200 :stroke "#999" :stroke-dasharray "4 3"}]
                       '(for [[i [x y]] (map-indexed vector hits)]
                          ^{:key i}
                          [:g [:line {:x1 460 :y1 (Y y) :x2 (X x) :y2 (Y y) :stroke "#E39B2D" :stroke-width 1.5}]
                           [:line {:x1 (X x) :y1 (Y y) :x2 (X f) :y2 (Y 0) :stroke "#C0392B" :stroke-width 1.2}]])
                       [:circle {:cx '(X f) :cy 200 :r 6 :fill "#C0392B"}]
                       [:text {:x '(+ 8 (X f)) :y 190 :font-size 14} "focus"]]
                      [:div {:style {:flex "0 1 300px"}}
                       (leva/sub-panel {:fill true :flat true :title-bar false}
                                       (leva/controls {:atom !q :schema {:f {:min 0.3 :max 3 :step 0.01 :label "f, focus from the vertex"}
                                                                         :rays {:min 1 :max 25 :step 1 :label "rays from the Sun"}}}))
                       [:div {:style {:font-family "ui-monospace, monospace" :margin "0.5em 0"}}
                        "largest miss of the focus: " (tie/fmt 'gap 12)]
                       [:div {:style {:font-size "0.85em" :opacity 0.8}}
                        "Each ray parallel to the axis is reflected by the kernel (equal angles with the tangent) and crosses the axis at the focus, at distance f from the vertex."]]])))))))
