;; # Conics playground
^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns conics-playground
  {:history/year -200
   :history/title "Conics playground"
   :history/era "Greek geometry"
   :nextjournal.clerk/visibility {:code :hide :result :show}}
  (:refer-clojure :exclude [+ - * / = zero? compare numerator denominator ref partial
                            infinite? abs])
  (:require [emmy.env :refer [+ - * / cos sin]]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.mathbox.plot :as plot]
            [emmy.viewer :as ev]
            [history-of-math.page :as page]
            [history-of-math.widgets :as hom]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:result :hide}}
(hom/install!)

;; Apollonius cut one cone with one plane and named the three sections by
;; how a square compares with a rectangle: *ellipsis* (falling short),
;; *parabolē* (equal), *hyperbolē* (exceeding). In polar form about a focus
;; the three are one curve, $r = \dfrac{\ell}{1 + e\cos t}$, and the
;; eccentricity $e$ decides the name: $e < 1$ ellipse, $e = 1$ parabola,
;; $e > 1$ hyperbola. Drag $\ell$ and $e$ in the panel.

^{::clerk/visibility {:result :hide}}
(defn conic-name
  "Browser code naming the section of eccentricity (:e @!p)."
  [!p]
  (list 'let ['e (ev/get !p :e)]
        '(cond (< (js/Math.abs (- e 1)) 0.005) "parabola"
               (< e 1) "ellipse"
               :else "hyperbola")))

(ev/with-let [!p {:l 2 :e 0.5}]
  [:<>
   (leva/controls {:folder {:name "Focus and eccentricity"}
                   :atom !p
                   :schema {:l {:min 0.5 :max 4 :step 0.01}
                            :e {:min 0 :max 2 :step 0.01}}})
   [:p {:style {:font-size "1.1em"}}
    "e = " (ev/get !p :e) ": "
    [:strong {:data-conic-name "true"} (conic-name !p)]]
   (mafs/mafs {:view-box {:x [-8 8] :y [-6 6]} :height 360}
              (mafs/cartesian)
              (mafs/parametric
               {:xy (ev/with-params {:atom !p :params [:l :e]}
                      (fn [l e]
                        (fn [t]
                          (let [r (/ l (+ 1 (* e (cos t))))]
                            [(* r (cos t)) (* r (sin t))]))))
                :t [(- 0.01 Math/PI) (- Math/PI 0.01)]
                :color :blue}))])

;; The same sections on the cone. The double cone is $(u\cos v,\ u\sin v,\ u)$;
;; the plane through $(0, 0, h)$ turns about a horizontal line by the angle
;; $\theta$. Below the cone's slope ($\theta < 45^\circ$) it cuts an ellipse,
;; at the slope a parabola, beyond it a hyperbola.

(ev/with-let [!c {:tilt 0.4 :h 0.6}]
  (plot/scene
   {:range [[-3 3] [-3 3] [-3 3]]}
   (leva/controls {:folder {:name "Cutting plane"}
                   :atom !c
                   :schema {:tilt {:min 0 :max 1.4 :step 0.01}
                            :h {:min -1.5 :max 1.5 :step 0.01}}})
   (plot/parametric-surface
    {:f (fn [[u v]] [(* u (cos v)) (* u (sin v)) u])
     :u [-2 2] :v [0 (* 2 Math/PI)]
     :u-samples 32 :v-samples 48
     :color "#d9a441" :opacity 0.6})
   (plot/parametric-surface
    {:f (ev/with-params {:atom !c :params [:tilt :h]}
          (fn [tilt h]
            (fn [[u v]]
              [(* u (cos tilt)) v (+ h (* u (sin tilt)))])))
     :u [-2.5 2.5] :v [-2.5 2.5]
     :u-samples 8 :v-samples 8
     :color "#3090ff" :opacity 0.35})))

(page/prev-next "notebooks/conics_playground.clj")
