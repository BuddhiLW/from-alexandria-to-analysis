(ns alexandria.fermat.playgrounds
  (:require [emmy.env :as e] [emmy.leva :as leva] [emmy.mafs :as mafs]
            [emmy.viewer :as ev] [emmy.viewer.compile :as vc] [emmy.viewer.raster]
            [clojure.walk :as walk]))

(defn- kernel [f xs] (vc/compiled-fn f xs [0] {:simplify? false}))
(defn- fmt [x] (list 'let ['v x] '(if (js/isFinite v) (.toFixed v 3) "undefined")))
(defn- bind
  "A quoted browser form with the symbol !s replaced by the state atom. Left
   bare, !s is a Clojure local the browser's SCI cannot resolve."
  [form !s]
  (walk/postwalk-replace {'!s !s} form))

(defn- button [!s title values]
  [:button {:type "button" :style {:margin "0.25em" :padding "0.35em" :cursor "pointer"}
            :on-click (list 'fn [] (list 'reset! !s values))} title])
(defn- frame [id figure controls]
  [:div {:data-playground id :style {:display "flex" :flex-wrap "wrap" :gap "1em"}}
   [:div {:style {:flex "1 1 470px" :min-width "300px"}} figure]
   [:div {:style {:flex "1 1 320px" :min-width "280px"}} controls]])

(defn normal-values [p c h d v]
  (fn [_]
    (let [f (fn [x] (e/+ (e/* p x) (e/* c (e/square x)) (e/* h (e/expt x 3))))
          a h b (e/+ 1 c (e/* h d))
          z (e/+ p (e/* -2 v) (e/* (e/+ 1 c) d) (e/* h (e/square d)))
          other (e// (e/- 0 z) (e/+ b (e/* a d) 1e-9))
          disc (e/- (e/square b) (e/* 4 a z))
          sq (e/sqrt (e/* 1/2 (e/+ disc (e/abs disc))))
          safe-a (e/+ a 1e-8)
          target (e/+ d (e/* 1/2 (e/+ p (e/* 2 c d) (e/* 3 h (e/square d)))))
          root1 (e// (e/+ (e/- 0 b) sq) (e/* 2 safe-a))
          root2 (e// (e/- (e/- 0 b) sq) (e/* 2 safe-a))]
      [(e/sqrt (e/* 1/2 (e/+ (f d) (e/abs (f d))))) target
       (e/- v target) other disc root1 root2 (f d)
       (e/+ (e/square (e/- v d)) (f d))])))

(defn normal-curve [p c h]
  (fn [x] (let [v (e/+ (e/* p x) (e/* c (e/square x)) (e/* h (e/expt x 3)))]
            (e/sqrt (e/* 1/2 (e/+ v (e/abs v)))))))

(def normal-init {:p 2 :c -0.5 :h 0 :d 1 :v 1.1})
(def normal-schema {:p {:min 0.1 :max 4 :step 0.01 :label "latus rectum p"}
                    :c {:min -1.5 :max 1.5 :step 0.01 :label "quadratic c"}
                    :h {:min -0.3 :max 0.3 :step 0.01 :label "cubic h"}
                    :d {:min 0.05 :max 3 :step 0.01 :label "C: abscissa d"}
                    :v {:min -1 :max 4 :step 0.01 :label "P: centre v"}})

(defn normal-playground []
  (binding [vc/*backend* :raster]
    (ev/with-let [!s normal-init]
      (let [[ksym k] [(gensym "normal") (kernel normal-values (mapv normal-init [:p :c :h :d :v]))]
            upper (ev/with-params {:atom !s :params [:p :c :h]} normal-curve)
            lower (ev/with-params {:atom !s :params [:p :c :h]}
                    (fn [p c h] (fn [x] (e/- 0 ((normal-curve p c h) x)))))]
        (list 'reagent.core/with-let [ksym k]
          (list 'let ['r (list ksym '(array 0) (list 'array (ev/get !s :p) (ev/get !s :c) (ev/get !s :h) (ev/get !s :d) (ev/get !s :v)))
                      'd (ev/get !s :d) 'v (ev/get !s :v)
                      'touch '(<= (js/Math.abs (aget r 2)) 0.012)
                      'cubic (list '> (list 'js/Math.abs (ev/get !s :h)) 0.005)]
              (frame "descartes-normal"
                (mafs/mafs {:view-box {:x [-1 4.5] :y [-2.8 2.8]} :height 400}
                  (mafs/cartesian)
                  (mafs/of-x {:y upper :domain [0 4.5] :color "#1F6FB4"})
                  (mafs/of-x {:y lower :domain [0 4.5] :color "#1F6FB4"})
                  (mafs/circle {:center ['v 0] :radius '(js/Math.sqrt (aget r 8)) :color "#7B5EA7"})
                  (mafs/segment {:point1 ['v 0] :point2 ['d '(aget r 0)] :color "#2E8B57"})
                  (mafs/movable-point {:point ['d '(aget r 0)] :color "#C0392B"
                    :constrain (bind '(fn [[x _]] (let [x (max 0.05 (min 3 x))]
                                 [x (js/Math.sqrt (max 0 (+ (* (:p @!s) x) (* (:c @!s) x x) (* (:h @!s) x x x))))])) !s)
                    :on-move (list 'fn '[[x _]] (list 'swap! !s 'assoc :d 'x))})
                  (mafs/movable-point {:point ['v 0] :color "#7B5EA7"
                    :constrain '(fn [[x _]] [(max -1 (min 4 x)) 0])
                    :on-move (list 'fn '[[x _]] (list 'swap! !s 'assoc :v 'x))})
                  (mafs/text "C" {:x 'd :y '(aget r 0) :attach "ne"})
                  (mafs/text "P" {:x 'v :y 0 :attach "s"}))
                [:div (leva/sub-panel {:fill true :flat true :title-bar false}
                        (leva/controls {:atom !s :schema normal-schema}))
                 [:strong {:data-case true} '(if touch "double root: the circle touches, PC is the normal"
                                                 "two meets: secant circle")]
                 [:div "Curve: y² = p x + c x² + h x³; C = (d, √f(d)), P = (v, 0)."]
                 [:div "C ordinate = " (fmt '(aget r 0)) "; radius² = " (fmt '(aget r 8))]
                 [:div "P for a double root: d + f′(d)/2 = " (fmt '(aget r 1)) "; v − target = " (fmt '(aget r 2))]
                 [:div "Intersection equation: (x − d)[h x² + (1+c+hd)x + p−2v+(1+c)d+hd²] = 0"]
                 [:div "roots: d = " (fmt 'd) ", other = " (fmt '(aget r 3))
                  (list 'when 'cubic [:span ", third = " (fmt '(aget r 5)) ", " (fmt '(aget r 6))
                                       "; quadratic discriminant = " (fmt '(aget r 4))])]
                 [:div {:data-hypothesis true :style {:color '(if (<= (aget r 7) 0) "#C0392B" "#2E8B57")}}
                  '(if (<= (aget r 7) 0) "C is outside the real curve: the hypothesis fails" "C lies on the real curve")]
                 [:p "Try this:"]
                 (button !s "Merge the two roots" {:p 2 :c -0.5 :h 0 :d 1 :v 1.5})
                 (button !s "Make it a cubic: does it still work?" {:p 2 :c -0.4 :h 0.12 :d 1 :v 1.78})
                 (button !s "Parabola" {:p 1 :c 0 :h 0 :d 1 :v 1.5})
                 (button !s "Reset: Descartes’ ellipse" normal-init)])))))))

(defn adequate-curve [kind B]
  (fn [x]
    (let [w0 (e/* -1/6 (e/- kind 1) (e/- kind 2) (e/- kind 3))
          w1 (e/* 1/2 kind (e/- kind 2) (e/- kind 3))
          w2 (e/* -1/2 kind (e/- kind 1) (e/- kind 3))
          w3 (e/* 1/6 kind (e/- kind 1) (e/- kind 2))]
      (e/+ (e/* w0 (e/- (e/* B x) (e/square x)))
           (e/* w1 (e/- (e/* B x) (e/expt x 3)))
           (e/* w2 (e/+ (e/square x) (e/* B x)))
           (e/* w3 (e/abs (e/- x B)))))))

(defn adequate-values [kind B a ex]
  (fn [_]
    (let [f (adequate-curve kind B)
          w0 (e/* -1/6 (e/- kind 1) (e/- kind 2) (e/- kind 3))
          w1 (e/* 1/2 kind (e/- kind 2) (e/- kind 3))
          w2 (e/* -1/2 kind (e/- kind 1) (e/- kind 3))
          w3 (e/* 1/6 kind (e/- kind 1) (e/- kind 2))
          derivative (e/+ (e/* w0 (e/- B (e/* 2 a)))
                          (e/* w1 (e/- B (e/* 3 (e/square a))))
                          (e/* w2 (e/+ B (e/* 2 a)))
                          (e/* w3 (e// (e/- a B) (e/+ (e/abs (e/- a B)) 1/10000000))))
          quotient (e// (e/- (f (e/+ a ex)) (f a)) (e/+ ex 1/10000000))]
      [(f a) (f (e/+ a ex)) quotient derivative (e/- quotient derivative)])))

(def adequate-init {:kind 0 :B 4 :a 1 :ex 0.6})

(def adequate-schema {:kind {:label "f" :options {"rectangle B·a − a²" 0 "cubic B·a − a³" 1
                                                  "quadratic a² + B·a" 2 "corner |a − B|" 3}}
                      :B {:min 0.2 :max 5 :step 0.01 :label "B"}
                      :a {:min -2 :max 5 :step 0.01 :label "A: point"}
                      :ex {:min -1.5 :max 1.5 :step 0.01 :label "E: increment"}})

(defn adequality-playground []
  (binding [vc/*backend* :raster]
    (ev/with-let [!s adequate-init]
      (let [[ksym k] [(gensym "adequate") (kernel adequate-values (mapv adequate-init [:kind :B :a :ex]))]
            curve (ev/with-params {:atom !s :params [:kind :B]} adequate-curve)]
        (list 'reagent.core/with-let [ksym k]
          (list 'let ['r (list ksym '(array 0) (list 'array (ev/get !s :kind) (ev/get !s :B) (ev/get !s :a) (ev/get !s :ex)))
                    'a (ev/get !s :a) 'ex (ev/get !s :ex)
                    'corner (list '= (list 'int (ev/get !s :kind)) 3)
                    'B (ev/get !s :B)]
          (frame "fermat-adequality"
            (mafs/mafs {:view-box {:x [-2 5] :y [-3 7]} :height 390}
              (mafs/cartesian) (mafs/of-x {:y curve :domain [-2 5] :color "#1F6FB4"})
              (mafs/segment {:point1 ['a '(aget r 0)] :point2 ['(+ a ex) '(aget r 1)] :color "#E67E22"})
              (mafs/movable-point {:point ['a '(aget r 0)] :color "#C0392B"
                :constrain (bind '(fn [[x _]] (let [x (max -2 (min 5 x))]
                                      [x ((fn [kind B x] (case (int kind)
                                           0 (- (* B x) (* x x)) 1 (- (* B x) (* x x x))
                                           2 (+ (* x x) (* B x)) (js/Math.abs (- x B))))
                                           (:kind @!s) (:B @!s) x)])) !s)
                :on-move (list 'fn '[[x _]] (list 'swap! !s 'assoc :a 'x))})
              (mafs/text "A" {:x 'a :y '(aget r 0) :attach "nw"}))
            [:div (leva/sub-panel {:fill true :flat true :title-bar false}
                    (leva/controls {:atom !s :schema adequate-schema}))
             [:strong {:data-case true} '(if corner
                 (if (< (js/Math.abs (- a B)) 0.02) "corner: no single derivative at A = B"
                     "corner: away from B the slope exists")
                 (if (< (js/Math.abs (aget r 3)) 0.02)
                   "adequality gives a stationary point" "not at a maximum or minimum yet"))]
             [:div "f(A) = " (fmt '(aget r 0)) "; f(A+E) = " (fmt '(aget r 1))]
             [:div "[f(A+E) − f(A)]/E = " [:span {:data-quotient true} (fmt '(aget r 2))]]
             [:div "E → 0: " [:span {:data-limit true} (fmt '(aget r 3))]
              "; difference = " (fmt '(aget r 4))]
             [:div "adequality gives A = " (bind '(case (int (:kind @!s))
                 0 (.toFixed (/ B 2) 3) 1 (.toFixed (js/Math.sqrt (/ B 3)) 3)
                 2 (.toFixed (/ (- B) 2) 3) "B (corner: striking E is invalid)") !s)]
             [:div {:data-hypothesis true :style {:color '(if corner "#C0392B" "#2E8B57")}}
              '(if corner "Differentiability hypothesis broken at the corner"
                 "Smooth polynomial: striking E is valid after division")]
             [:p "Try this:"]
             (button !s "Cut Fermat’s line in half" {:kind 0 :B 4 :a 2 :ex 0.2})
             (button !s "Shrink E toward zero" {:kind 0 :B 4 :a 2 :ex 0.01})
             (button !s "Break it: a corner" {:kind 3 :B 2 :a 2 :ex 0.3})
             (button !s "Reset" adequate-init)])))))))

(def loci-presets
  [{:label "Fermat: line" :al 0 :be 0 :ga 0 :de 1 :ep -1 :ze 0}
   {:label "Fermat: rectangular hyperbola" :al 0 :be 1 :ga 0 :de 0 :ep 0 :ze -1}
   {:label "Fermat: parabola" :al 1 :be 0 :ga 0 :de 0 :ep -1 :ze 0}
   {:label "Fermat: circle" :al 1 :be 0 :ga 1 :de 0 :ep 0 :ze -2.25}
   {:label "Fermat: ellipse" :al 1 :be 0 :ga 2 :de 0 :ep 0 :ze -2.25}
   {:label "Fermat: second hyperbola" :al 1 :be 0 :ga -2 :de 0 :ep 0 :ze 2.25}
   {:label "degenerate: pair of lines" :al 1 :be 0 :ga -1 :de 0 :ep 0 :ze 0}])

(defn locus-values [al be ga de ep ze A E]
  (fn [_]
    (let [disc (e/- (e/square be) (e/* 4 al ga))
          det (e/- (e/+ (e/* 4 al ga ze) (e/* be de ep))
                   (e/* al (e/square ep)) (e/* ga (e/square de)) (e/* ze (e/square be)))
          residual (e/+ (e/* al (e/square A)) (e/* be A E) (e/* ga (e/square E))
                        (e/* de A) (e/* ep E) ze)]
      [disc det residual (e/+ (e/square al) (e/square be) (e/square ga))
       (e/- al ga) (e/+ (e/square de) (e/square ep))])))

(defn locus-branch [sgn al be ga de ep ze]
  (fn [A]
    (let [b (e/+ (e/* be A) ep)
          c (e/+ (e/* al (e/square A)) (e/* de A) ze)
          disc (e/- (e/square b) (e/* 4 ga c))
          root (e// (e/+ (e/- 0 b) (e/* sgn (e/sqrt (e/* 1/2 (e/+ disc (e/abs disc))))))
                     (e/* 2 (e/+ ga 1e-8)))
          linear (e// (e/- 0 c) (e/+ b 1e-8))]
      (e/+ (e/* (e// (e/square ga) (e/+ (e/square ga) 1e-8)) root)
           (e/* (e// 1e-8 (e/+ (e/square ga) 1e-8)) linear)))))

(def loci-schema
  (into {} (for [k [:al :be :ga :de :ep :ze :A :E]]
             [k {:min -4 :max 4 :step 0.01
                 :label ({:al "α (A²)" :be "β (AE)" :ga "γ (E²)" :de "δ (A)"
                          :ep "ε (E)" :ze "ζ (constant)" :A "NZ = A" :E "ZI = E"} k)}])))

(defn loci-playground []
  (binding [vc/*backend* :raster]
    (ev/with-let [!s (assoc (dissoc (nth loci-presets 4) :label) :A 1 :E 0.5)]
      (let [params [:al :be :ga :de :ep :ze]
            f (fn [sgn] (ev/with-params {:atom !s :params params} (partial locus-branch sgn)))
            plus (f 1) minus (f -1)
            ksym (gensym "locus")
            k (kernel locus-values (mapv {:al 1 :be 0 :ga 2 :de 0 :ep 0 :ze -2.25 :A 1 :E 0.5}
                                         (concat params [:A :E])))]
        (list 'reagent.core/with-let [ksym k]
          (list 'let ['r (list ksym '(array 0) (list* 'array (map (fn [key] (ev/get !s key)) (concat params [:A :E]))))
                    'quad '(aget r 3) 'disc '(aget r 0) 'det '(aget r 1) 'be (ev/get !s :be)
                    'nm '(cond (and (< quad 1e-8) (< (aget r 5) 1e-8)) "whole plane or empty locus (all coefficients vanish)"
                               (< quad 1e-8) "line (εὐθεῖα)"
                               (< (js/Math.abs det) 1e-5) "degenerate pair of lines"
                               (< (js/Math.abs disc) 1e-5) "parabola (παραβολή, exact application)"
                               (pos? disc) "hyperbola (ὑπερβολή, excess)"
                               (and (< (js/Math.abs (aget r 4)) 1e-5)
                                    (< (js/Math.abs be) 1e-5)) "circle (κύκλος)"
                               :else "ellipse (ἔλλειψις, falling short)")]
          (frame "fermat-isagoge"
            (mafs/mafs {:view-box {:x [-4 4] :y [-3 3]} :height 410}
              (mafs/cartesian)
              (mafs/of-x {:y plus :domain [-4 4] :color "#1F6FB4"})
              (mafs/of-x {:y minus :domain [-4 4] :color "#1F6FB4"})
              (mafs/movable-point {:point [(ev/get !s :A) (ev/get !s :E)] :color "#C0392B"
                :on-move (list 'fn '[[a e]] (list 'swap! !s 'assoc :A 'a :E 'e))})
              (mafs/text "I" {:x (ev/get !s :A) :y (ev/get !s :E) :attach "ne"}))
            [:div (leva/sub-panel {:fill true :flat true :title-bar false}
                    (leva/controls {:atom !s :schema loci-schema}))
             [:strong {:data-locus-name true} 'nm]
             [:div "αA² + βAE + γE² + δA + εE + ζ = 0"]
             [:div "β² − 4αγ = " (fmt '(aget r 0)) " (" (fmt (ev/get !s :be)) "² − 4·"
              (fmt (ev/get !s :al)) "·" (fmt (ev/get !s :ga)) ")"]
             [:div "I = (A,E); equation at I = " [:span {:data-residual true} (fmt '(aget r 2))]]
             [:div {:data-hypothesis true :style {:color '(if (< (js/Math.abs (aget r 2)) 0.01) "#2E8B57" "#C0392B")}}
              '(if (< (js/Math.abs (aget r 2)) 0.01) "I lies on Fermat’s locus" "I is off the locus: equation not satisfied")]
             [:p "Try this: Fermat’s loci and a degenerate boundary"]
             (into [:div] (map (fn [{:keys [label] :as preset}]
                                 (button !s label (assoc (dissoc preset :label) :A 1 :E 0.5))) loci-presets))
             (button !s "Reset: ellipse" (assoc (dissoc (nth loci-presets 4) :label) :A 1 :E 0.5))])))))))
