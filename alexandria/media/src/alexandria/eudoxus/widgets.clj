(ns alexandria.eudoxus.widgets
  "The Eudoxus notebook's tied-controls widgets (alexandria.medium.tie):

     def-5    Elements V, Def. 5: A, B, C, D tied by C : D = A : B, and the
              equimultiples m, n; mA against nB and mC against nD named
              live (alike exceed, alike equal, alike fall short)
     xii-2    Elements XII.2: the inscribed 2^k-gon and what it leaves of
              the circle, k <-> the leftover eps (Newton on Emmy's D)"
  (:require [alexandria.eudoxus.ties :as et]
            [alexandria.medium.tied :as tie]
            [emmy.env :as e]
            [emmy.mafs :as mafs]
            [emmy.viewer :as ev]))

(def ^:private PI Math/PI)

(defn- katex [tex-form]
  [:div {:data-formula tex-form} [:f> 'nextjournal.clerk.render/render-katex tex-form {:inline? false}]])

;; ---------------------------------------------------------------------------
;; Def. 5

(def def5-keys [:A :B :C :D :m :n])
(def def5-ranges {:A [0.2 3 0.01] :B [0.2 3 0.01] :C [0.2 3 0.01] :D [0.05 45 0.0001] :m [1 12 1] :n [1 12 1]})

(def def5-sources
  "A, B or C moves: the other two held, D the fourth proportional. D
   moves: A, B held, C = A D / B. m or n moves: nothing else moves."
  {:A (fn [A B C _ m n] [A B C (et/fourth A B C) m n])
   :B (fn [A B C _ m n] [A B C (et/fourth A B C) m n])
   :C (fn [A B C _ m n] [A B C (et/fourth A B C) m n])
   :D (fn [A B _ D m n] [A B (et/third A B D) D m n])
   :m (fn [A B C D m n] [A B C D m n])
   :n (fn [A B C D m n] [A B C D m n])})

(defn def5-readout "[mA nB mC nD (mA - nB) (mC - nD) C/A]." [A B C D m n]
  (fn [_] [(e/* m A) (e/* n B) (e/* m C) (e/* n D) (e/- (e/* m A) (e/* n B)) (e/- (e/* m C) (e/* n D)) (e// C A)]))

(def def5-start {:A 1.41 :B 1 :C 2.12 :D (/ 2.12 1.41) :m 5 :n 7})

(defn- bar [y len colour label]
  [:<>
   (mafs/segment {:point1 [0 y] :point2 [len y] :color colour :weight 6})
   (mafs/text label {:x len :y y :attach "e" :size 14})])

(defn- sign-name [x]
  (list 'let ['v x] '(cond (> v 1e-9) "exceeds" (< v -1e-9) "falls short of" :else "equals")))

(defn- def5-body [_ r]
  (let [n (fn [i] (tie/fmt (list 'aget r i) 3))
        same (list '= (sign-name (list 'aget r 4)) (sign-name (list 'aget r 5)))]
    [:div
     (mafs/mafs
      {:view-box {:x [-0.5 40] :y [-0.5 4.5]} :height 220 :preserve-aspect-ratio false}
      (bar 4 (list 'aget r 0) :blue "mA")
      (bar 3 (list 'aget r 1) :blue "nB")
      (bar 1.5 (list 'aget r 2) :red "mC")
      (bar 0.5 (list 'aget r 3) :red "nD"))
     [:p {:style {:font-size "1.1em"}}
      "mA " [:strong {:data-first (sign-name (list 'aget r 4))} (sign-name (list 'aget r 4))] " nB, and mC "
      [:strong {:data-second (sign-name (list 'aget r 5))} (sign-name (list 'aget r 5))] " nD: "
      [:strong {:data-alike (list 'if same "alike" "not alike")} (list 'if same "alike" "NOT alike")]]
     (katex (list 'str "mC - nD = " (n 5) " = \\frac{C}{A}(mA - nB) = " (n 6) "\\cdot(" (n 4) ")"))
     [:p {:style {:font-size "0.9em" :opacity 0.85}}
      "Move A, B or C and D follows as the fourth proportional (C : D = A : B); move D and C follows. "
      "m and n are free: whatever they are, the two comparisons agree, which is what Definition 5 asks of the same ratio."]]))

(defn def-5 []
  (tie/widget {:id :def-5 :keys def5-keys :start def5-start :ranges def5-ranges
               :labels {:m "m (multiple of A, C)" :n "n (multiple of B, D)"}
               :sources def5-sources :readout def5-readout :body def5-body}))

;; ---------------------------------------------------------------------------
;; XII.2

(def xii2-keys [:k :eps])
(def xii2-ranges {:k [2 10 1] :eps [0.00001 0.37 0.00001]})

(defn- sides [k] (e/exp (e/* k (Math/log 2))))

(def xii2-sources
  "k moves: eps = leftover(2^k). eps moves: k = log2 n, n by Newton."
  {:k (fn [k _] [k (et/leftover (sides k))])
   :eps (fn [_ eps] [(et/k-of-leftover eps) eps])})

(defn xii2-readout "[k eps n share]." [k eps]
  (fn [_] [k eps (sides k) (et/share (sides k))]))

(def xii2-start {:k 3 :eps (- 1 (/ (* 4 (Math/sin (/ PI 4))) PI))})

(defn- polygon-curve
  "The inscribed regular n-gon of the unit circle (apothem cos(pi/n)); see
   alexandria.archimedes.widgets/polygon-curve for the fold."
  [n]
  (fn [t]
    (let [s (e// (e/acos (e/* 0.9999999999 (e/cos (e/* n t)))) n)
          rho (e// (e/cos (e// PI n)) (e/cos s))]
      [(e/* rho (e/cos t)) (e/* rho (e/sin t))])))

(defn- xii2-body [!s r]
  (let [n (fn [i] (tie/fmt (list 'aget r i) 5))]
    [:div
     (mafs/mafs
      {:view-box {:x [-1.2 1.2] :y [-1.1 1.1]} :height 320}
      (mafs/parametric {:xy (fn [t] [(e/cos t) (e/sin t)]) :t [0 (* 2 PI)] :color :blue})
      (mafs/parametric {:xy (ev/with-params {:atom !s :params [:k]} (fn [k] (polygon-curve (sides k))))
                        :t [0 (* 2 PI)] :color :red}))
     (katex (list 'str "n = 2^k = " (n 2) ",\\quad \\frac{\\text{polygon}}{\\text{circle}} = \\frac{n \\sin(2\\pi/n)}{2\\pi} = " (n 3)
                  ",\\quad \\varepsilon = " (n 1)))
     [:p {:style {:font-size "0.9em" :opacity 0.85}}
      "Each doubling takes away more than half of what was left (X.1), so any ε is reached: move ε and k follows "
      "(the smallest whole k that reaches it is the next one up)."]]))

(defn xii-2 []
  (tie/widget {:id :xii-2 :keys xii2-keys :start xii2-start :ranges xii2-ranges
               :labels {:k "k (sides 2ᵏ)" :eps "ε (circle left over)"}
               :sources xii2-sources :readout xii2-readout :body xii2-body}))
