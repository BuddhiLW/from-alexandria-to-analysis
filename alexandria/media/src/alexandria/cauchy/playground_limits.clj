(ns alexandria.cauchy.playground-limits
  "Playgrounds for the four limit passages of Cauchy's Cours d'analyse
   (Preliminaires, ch. II sec. 2, the intermediate value theorem of
   Note III, the difference quotient of the Resume). Each section has its
   own Mafs figure: the eps band and the rank N(eps), the increment
   f(x + alpha) - f(x) with alpha halved, the secant and the tangent, the
   bracketing intervals refined round after round. Every placeable object
   is a MovablePoint whose atom is a reagent cursor onto the section state,
   so dragging it, the Leva sub-panel, the presets and reset all write one
   atom. Every number is a precompiled raster kernel evaluated in the
   browser."
  (:require [alexandria.cauchy.playground :as base]
            [clojure.walk :as walk]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]))

(defn- kernel [f]
  (vc/compiled-fn (fn [x] (fn [_] [(f x)])) [1] [0] {:simplify? false}))

(def derivative-functions
  "f' of each base/functions entry, for the tangent of the derivative
   playground (the jump and the flat function guarded by epsilons)."
  {"1 + sin x" e/cos
   "x²" (fn [x] (e/* 2 x))
   "sin x" e/cos
   "x³ − 2x − 5" (fn [x] (e/- (e/* 3 (e/square x)) 2))
   "jump at 1" (fn [x] (e/divide 1e-12 (e/square (e/+ (e/abs (e/- x 1)) 1e-12))))
   "flat e^(−1/x²)" (fn [x] (e/* (e/divide (e/* 2 x) (e/square (e/+ (e/square x) 1e-20)))
                                  (e/exp (e/- (e/divide 1 (e/+ (e/square x) 1e-20))))))})

(def presets
  "Try-this experiments per section: [label patch]. The last of each breaks
   the hypothesis Cauchy's statement needs."
  {:limit [["Shrink ε to 0.01" {:i 0.01}]
           ["Monotone 1 + 1/n" {:f "1 + 1/n" :i 0.05}]
           ["Break it: 1 + (−1)ⁿ" {:f "1 + (−1)ⁿ (does not converge)" :i 0.1}]]
   :continuity-ch2 [["Shrink α to 0.01" {:i 0.01}]
                    ["Steep x³ − 2x − 5 at 2" {:f "x³ − 2x − 5" :x 2 :i 0.3}]
                    ["Break it: the jump at 1" {:f "jump at 1" :x 0.95 :i 0.1}]]
   :derivative [["Let i shrink to 0.001" {:i 0.001}]
                ["Power x² at 1.5" {:f "x²" :x 1.5 :i 0.4}]
                ["Break it: the jump at 1" {:f "jump at 1" :x 0.95 :i 0.1}]]
   :ivt [["Cauchy's x³ − 2x − 5 on [2, 3]" {:f "x³ − 2x − 5" :x0 2 :X 3 :n 10}]
         ["Finer: m = 32" {:n 32}]
         ["Same signs: x² on [0.5, 2]" {:f "x²" :x0 0.5 :X 2}]
         ["Break it: the jump on [0, 2]" {:f "jump at 1" :x0 0 :X 2}]]})

;; ## Figures, one per passage. Free symbols are bound by `page`.

(defn limit-figure
  "Preliminaires: the values x_n as points against rank n, the band
   1 - eps < x < 1 + eps, the cut at N(eps). Drag the band's upper edge
   (eps) and the rank marker (n)."
  []
  '[mafs.core/Mafs {:view-box {:x [0 62] :y [-0.3 2.4]} :height 340 :preserve-aspect-ratio false}
    [mafs.coordinates/Cartesian {}]
    [mafs.core/Polygon {:points [[0 (- 1 i)] [62 (- 1 i)] [62 (+ 1 i)] [0 (+ 1 i)]]
                        :color "#e9ac39" :fill-opacity 0.22 :weight 0}]
    [mafs.line/Segment {:point1 [0 1] :point2 [62 1] :color "#888" :style "dashed"}]
    (when N [mafs.core/Polygon {:points [[N -0.3] [62 -0.3] [62 2.4] [N 2.4]]
                                :color "#25894d" :fill-opacity 0.06 :weight 0}])
    (when N [mafs.line/Segment {:point1 [N -0.3] :point2 [N 2.4] :color "#c0392b" :style "dashed"}])
    (into [:<>] (for [j (range 1 61)]
                  [mafs.core/Point {:x j :y (val j)
                                    :color (if (< (js/Math.abs (- (val j) 1)) i) "#25894d" "#c0392b")}]))
    [mafs.core/Text {:x 1 :y (+ 1 i) :attach "ne" :size 15} "1 + ε"]
    [mafs.core/Text {:x 1 :y (- 1 i) :attach "se" :size 15} "1 − ε"]
    (when N [mafs.core/Text {:x N :y 2.25 :attach "e" :color "#c0392b" :size 15} "N(ε)"])
    [mafs.core/Text {:x n :y (val n) :attach "n" :size 15} "xₙ"]
    [mafs.core/MovablePoint {:atom BAND :color "#d58d19"
                             :constrain (fn [[_ y]] [5 (max 1.001 (min 2.2 y))])}]
    [mafs.core/MovablePoint {:atom RANK :color "#246b9c"
                             :constrain (fn [[t _]] (let [k (max 1 (min 60 (js/Math.round t)))] [k (val k)]))}]])

(defn continuity-figure
  "Ch. II sec. 2: the curve, the point x and x + alpha on it, the increment
   f(x + alpha) - f(x) as a red rise, and the same increment for alpha/2,
   alpha/4, ... in fading strokes: 'decreases indefinitely with alpha'."
  []
  '[mafs.core/Mafs {:view-box {:x [-2 4] :y [-4 6]} :height 340 :preserve-aspect-ratio false}
    [mafs.coordinates/Cartesian {}]
    [mafs.core/Polyline {:points points :color "#246b9c" :weight 2}]
    (into [:<>] (for [[k [t v]] (map-indexed vector halvings)]
                  [mafs.line/Segment {:point1 [t y] :point2 [t v] :color "#cb5730"
                                      :opacity (/ 1 (+ 1.5 k)) :weight 2}]))
    [mafs.line/Segment {:point1 [x y] :point2 [(+ x i) y] :color "#858585" :style "dashed"}]
    [mafs.line/Segment {:point1 [(+ x i) y] :point2 [(+ x i) yi] :color "#cb5730" :weight 4}]
    [mafs.core/Text {:x (+ x i) :y (/ (+ y yi) 2) :attach "e" :size 15} "f(x+α) − f(x)"]
    [mafs.core/Text {:x (+ x (/ i 2)) :y y :attach "s" :size 15} "α"]
    [mafs.core/MovablePoint {:atom PX :color "#c0392b"
                             :constrain (fn [[t _]] (let [t (max -2 (min 3 t))] [t (val t)]))}]
    [mafs.core/MovablePoint {:atom PXI :color "#7b5ea7"
                             :constrain (fn [[t _]] (let [t (max -2 (min 4 t))] [t (val t)]))}]])

(defn derivative-figure
  "Resume, 3e lecon: the secant through x and x + i, the run i and the rise
   f(x + i) - f(x), and the tangent of slope f'(x) it approaches."
  []
  '[mafs.core/Mafs {:view-box {:x [-2 4] :y [-4 6]} :height 340 :preserve-aspect-ratio false}
    [mafs.coordinates/Cartesian {}]
    [mafs.core/Polyline {:points points :color "#246b9c" :weight 2}]
    (when (> (js/Math.abs i) 0.0001)
      [mafs.line/ThroughPoints {:point1 [x y] :point2 [(+ x i) yi] :color "#c0392b"}])
    [mafs.line/Segment {:point1 [(- x 1.5) (- y (* 1.5 slope))]
                        :point2 [(+ x 1.5) (+ y (* 1.5 slope))]
                        :color "#289453" :style "dashed" :weight 3}]
    [mafs.line/Segment {:point1 [x y] :point2 [(+ x i) y] :color "#888"}]
    [mafs.line/Segment {:point1 [(+ x i) y] :point2 [(+ x i) yi] :color "#c0392b"}]
    [mafs.core/Text {:x (+ x (/ i 2)) :y y :attach "s" :size 15} "i"]
    [mafs.core/Text {:x (+ x 1.5) :y (+ y (* 1.5 slope)) :attach "e" :color "#289453" :size 15} "tangent"]
    [mafs.core/Text {:x (+ x i) :y yi :attach "ne" :color "#c0392b" :size 15} "secant"]
    [mafs.core/MovablePoint {:atom PX :color "#c0392b"
                             :constrain (fn [[t _]] (let [t (max -2 (min 3 t))] [t (val t)]))}]
    [mafs.core/MovablePoint {:atom PXI :color "#7b5ea7"
                             :constrain (fn [[t _]] (let [t (max -2 (min 4 t))] [t (val t)]))}]])

(defn ivt-figure
  "Note III: the curve between x0 and X, the m subdivision points marked
   by the sign of f, the bracketing intervals where the sign changes, and
   below the axis the bracket refined round after round (each round cut
   into m parts again). Drag x0 and X along the axis."
  []
  '[mafs.core/Mafs {:view-box {:x [-2.5 4.5] :y [-4 6]} :height 360 :preserve-aspect-ratio false}
    [mafs.coordinates/Cartesian {}]
    [mafs.core/Polyline {:points points :color "#246b9c" :weight 2}]
    (into [:<>] (for [[l r] brackets]
                  [mafs.line/Segment {:point1 [l 0] :point2 [r 0] :color "#dc9b22" :weight 8}]))
    (into [:<>] (for [t nodes]
                  [mafs.line/Segment {:point1 [t -0.15] :point2 [t 0.15]
                                      :color (if (neg? (val t)) "#c0392b" "#25894d") :weight 3}]))
    (into [:<>] (for [[k [l r]] (map-indexed vector levels)]
                  [:<> [mafs.line/Segment {:point1 [l (* -0.6 (+ k 1))] :point2 [r (* -0.6 (+ k 1))]
                                           :color "#8b53ad" :weight 6}]
                   [mafs.core/Text {:x r :y (* -0.6 (+ k 1)) :attach "e" :size 13}
                    (str "round " (+ k 2) ": width " (.toExponential (- r l) 1))]]))
    (when root [mafs.core/Point {:x root :y 0 :color "#8b53ad"}])
    [mafs.core/Text {:x x0 :y 0 :attach "nw" :size 15} "x₀"]
    [mafs.core/Text {:x X :y 0 :attach "ne" :size 15} "X"]
    [mafs.core/MovablePoint {:atom PX0 :color "#c0392b"
                             :constrain (fn [[t _]] [(max -2 (min 3.9 t)) 0])}]
    [mafs.core/MovablePoint {:atom PXX :color "#7b5ea7"
                             :constrain (fn [[t _]] [(max -1.9 (min 4 t)) 0])}]])

(defn- schema [id]
  (merge {:f {:options (vec (keys (if (= id :limit) base/sequences base/functions)))
              :label (if (= id :limit) "sequence xₙ" "f(x)")}}
         (case id
           :limit {:i {:min 0.001 :max 1.2 :step 0.001 :label "ε"}
                   :n {:min 1 :max 60 :step 1 :label "rank n"}}
           :ivt {:x0 {:min -2 :max 3.9 :step 0.01 :label "x₀"}
                 :X {:min -1.9 :max 4 :step 0.01 :label "X"}
                 :n {:min 2 :max 60 :step 1 :label "m subdivisions"}}
           {:x {:min -2 :max 3 :step 0.01 :label "x"}
            :i {:min -1 :max 1 :step 0.001 :label (if (= id :derivative) "i" "α")}})))

(def page
  '(let [{:keys [f x i n x0 X]} @!P
         fnk (get KERNELS f)
         ;; val reads the function from the state, not from this render's
         ;; closure: MovablePoint keeps the first constraint and cursor it is
         ;; given, and those must follow the sequence or curve picked later.
         val (fn [t] (let [g (:f @!P)]
                       (if (and (= g "flat e^(−1/x²)") (< (js/Math.abs t) 1e-9))
                         0 (aget ((get KERNELS g) (array 0) (array t)) 0))))
         fmt (fn [v] (if (and (number? v) (js/isFinite v)) (.toFixed v 5) "undefined"))
         ;; cursors: the draggable points read and write the one state atom
         cur (fn [read write] (reagent.core/cursor (fn ([_] (read @!P)) ([_ p] (swap! !P write p))) []))
         BAND (cur (fn [s] [5 (+ 1 (:i s))]) (fn [s p] (assoc s :i (- (nth p 1) 1))))
         RANK (cur (fn [s] [(:n s) (val (:n s))]) (fn [s p] (assoc s :n (nth p 0))))
         PX (cur (fn [s] [(:x s) (val (:x s))]) (fn [s p] (assoc s :x (nth p 0))))
         PXI (cur (fn [s] [(+ (:x s) (:i s)) (val (+ (:x s) (:i s)))]) (fn [s p] (assoc s :i (- (nth p 0) (:x s)))))
         PX0 (cur (fn [s] [(:x0 s) 0]) (fn [s p] (assoc s :x0 (nth p 0))))
         PXX (cur (fn [s] [(:X s) 0]) (fn [s p] (assoc s :X (nth p 0))))
         y (when x (val x)) yi (when x (val (+ x i)))
         points (when (not= ID :limit)
                  (mapv (fn [j] (let [t (+ -2.5 (* j (/ 7 280)))] [t (val t)])) (range 281)))
         halvings (when (= ID :continuity-ch2)
                    (mapv (fn [k] (let [t (+ x (/ i (js/Math.pow 2 k)))] [t (val t)])) (range 1 7)))
         nodes (when (= ID :ivt) (mapv (fn [j] (+ x0 (* j (/ (- X x0) n)))) (range (inc n))))
         bracket-of (fn [a b] (let [xs (mapv (fn [j] (+ a (* j (/ (- b a) n)))) (range (inc n)))]
                                (filterv (fn [[l r]] (<= (* (val l) (val r)) 0)) (map vector xs (rest xs)))))
         brackets (when (and (= ID :ivt) (< x0 X)) (bracket-of x0 X))
         levels (when (seq brackets)
                  (loop [[a b] (first brackets) k 0 acc []]
                    (let [nb (first (bracket-of a b))]
                      (if (or (= k 4) (nil? nb)) acc (recur nb (inc k) (conj acc nb))))))
         ;; a safeguarded bisection on the raster kernel; a jump is never called a root
         root (when (and (seq brackets) (not= f "jump at 1"))
                (let [[a b] (first brackets)]
                  (loop [lo a hi b k 0]
                    (if (= k 44) (/ (+ lo hi) 2)
                        (let [mid (/ (+ lo hi) 2)]
                          (if (<= (* (val lo) (val mid)) 0) (recur lo mid (inc k)) (recur mid hi (inc k))))))))
         N (when (= ID :limit)
             (first (for [j (range 1 401)
                          :when (every? (fn [k] (< (js/Math.abs (- (val k) 1)) i)) (range j 401))] j)))
         slope (when (= ID :derivative) (aget ((get DERIVATIVES f) (array 0) (array x)) 0))
         jump? (= f "jump at 1")
         crossing? (and x (<= (min x (+ x i)) 1 (max x (+ x i))))
         holds? (case ID
                  :limit (some? N)
                  :ivt (and (< x0 X) (seq brackets) (not jump?))
                  (not (and jump? crossing?)))
         case-name (case ID
                     :limit (if N "convergent: from rank N(ε) on, every xₙ is inside the band"
                                "does not converge: no rank N(ε) exists")
                     :continuity-ch2 (cond (and jump? crossing?) "discontinuous: the increment stays near 1 however small α"
                                           (< (js/Math.abs (- yi y)) 0.01) "continuous: the increment is already below 0.01"
                                           :else "continuous: the increment decreases with α")
                     :derivative (cond (and jump? crossing?) "jump: the ratio grows without limit, no derivative"
                                       (< (js/Math.abs i) 0.0001) "i = 0: the ratio is 0/0, undefined"
                                       :else "the secant approaches the tangent")
                     :ivt (cond (>= x0 X) "x₀ ≥ X: no interval"
                                (empty? brackets) "f(x₀) and f(X) have the same sign: nothing to bracket"
                                jump? "f is not continuous: the signs change but no root"
                                :else "continuous, opposite signs: a root is bracketed"))]
     [:div {:data-playground (name ID) :data-case case-name :data-holds (str (boolean holds?))
            :data-value (str (case ID :limit N :ivt root :derivative (/ (- yi y) i) (- yi y)))
            :style {:border "1px solid #ddd" :border-radius "6px" :padding "0.6em"}}
      [:h4 TITLE]
      [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
       [:div {:style {:flex "1 1 460px" :min-width "320px"}} FIGURE]
       [:div {:style {:flex "0 1 300px" :min-width "260px"}}
        LEVA
        [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.88em" :line-height 1.6 :margin-top "0.4em"}}
         (case ID
           :limit [:<> [:div "|xₙ − 1| < ε = " (fmt i)]
                   [:div "N(ε) = " (or N "none within 400 ranks")]
                   [:div "x" n " = " (fmt (val n)) ", |x" n " − 1| = " (fmt (js/Math.abs (- (val n) 1)))
                    (if (and N (>= n N)) " (inside: n ≥ N)" " ")]]
           :continuity-ch2 [:<> [:div "x = " (fmt x) ", α = " (fmt i)]
                            [:div "f(x + α) − f(x) = " (fmt yi) " − " (fmt y) " = " (fmt (- yi y))]
                            [:div "with α/64: " (fmt (- (nth (last halvings) 1) y))]]
           :derivative [:<> [:div "x = " (fmt x) ", i = " (fmt i)]
                        [:div "[f(x + i) − f(x)]/i = " (if (> (js/Math.abs i) 0.0001) (fmt (/ (- yi y) i)) "0/0")]
                        [:div "f′(x) = " (fmt slope)]
                        [:div "difference = " (if (> (js/Math.abs i) 0.0001) (fmt (- (/ (- yi y) i) slope)) "—")]]
           :ivt [:<> [:div "f(x₀) = " (fmt (val x0)) ", f(X) = " (fmt (val X))]
                 [:div "m = " n ", brackets: " (count brackets)]
                 [:div "after " (inc (count levels)) " rounds, width " (if (seq levels) (fmt (let [[l r] (last levels)] (- r l))) "—")]
                 [:div "root ≈ " (if root (fmt root) "none") (when root (str ", f(root) = " (.toExponential (val root) 1)))]])]
        [:div {:data-verdict (str (boolean holds?))
               :style {:font-weight 600 :margin "0.4em 0" :padding "0.2em 0.5em" :border-radius "4px"
                       :color "white" :background (if holds? "#25894d" "#c0392b")}}
         case-name]]]
      [:div {:style {:margin-top "0.4em"}} "Try this: "
       (for [[label patch] PRESETS]
         ^{:key label} [:button {:type "button" :style {:margin "0.2em"} :on-click #(swap! !P merge patch)} label])]
      [:button {:type "button" :on-click #(reset! !P START)} "Reset to Cauchy’s setup"]]))

(def figures
  {:limit limit-figure :continuity-ch2 continuity-figure
   :derivative derivative-figure :ivt ivt-figure})

(defn widget
  "The playground for section id (:limit :continuity-ch2 :derivative :ivt)."
  [id]
  (let [{:keys [title] :as setup} (base/setups id)
        initial (dissoc setup :title :presets)
        fs (if (= id :limit) base/sequences base/functions)
        ks (into {} (for [[label _] fs] [label (symbol (str "cauchy-limit-k-" (Math/abs (hash label))))]))
        ds (into {} (for [[label _] derivative-functions] [label (symbol (str "cauchy-derivative-k-" (Math/abs (hash label))))]))]
    (binding [vc/*backend* :raster]
      (ev/with-let [!p initial]
        (let [bindings (vec (concat
                             (mapcat (fn [[label f]] [(ks label) (kernel f)]) fs)
                             (when (= id :derivative)
                               (mapcat (fn [[label f]] [(ds label) (kernel f)]) derivative-functions))))
              leva (leva/sub-panel {:fill true :titleBar {:drag false :title title}}
                                   (leva/controls {:atom !p :schema (schema id)}))
              ;; the browser's SCI resolves every symbol of the form, dead
              ;; branches included, and the derivative kernels are bound only
              ;; in the derivative section: elsewhere DERIVATIVES is empty
              replacements {'!P !p 'ID id 'TITLE title 'PRESETS (presets id) 'START initial
                            'LEVA leva 'KERNELS ks 'DERIVATIVES (if (= id :derivative) ds {})}
              figure (walk/postwalk-replace replacements ((figures id)))]
          (list 'reagent.core/with-let bindings
                (walk/postwalk-replace (assoc replacements 'FIGURE figure) page)))))))
