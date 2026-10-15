(ns alexandria.cauchy.playground
  "Open experiments, numbers evaluated by precompiled raster kernels."
  (:require [alexandria.cauchy.ratio :as ratio]
            [alexandria.cauchy.ratio-widget :as ratio-widget]
            [clojure.walk :as walk]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]))

(def functions
  {"1 + sin x" (fn [x] (e/+ 1 (e/sin x)))
   "x²" e/square
   "sin x" e/sin
   "x³ − 2x − 5" (fn [x] (e/- (e/- (e/expt x 3) (e/* 2 x)) 5))
   "jump at 1" (fn [x] (e/+ 0.2 (e/divide (e/- x 1) (e/+ (e/abs (e/- x 1)) 1e-12))))
   "flat e^(−1/x²)" (fn [x] (e/exp (e/- (e/divide 1 (e/+ (e/square x) 1e-20)))))})

(def sequences
  {"1 + (−1)ⁿ/n" (fn [n] (e/+ 1 (e/divide (e/cos (e/* Math/PI n)) n)))
   "1 + 1/n" (fn [n] (e/+ 1 (e/divide 1 n)))
   "1 + (−1)ⁿ (does not converge)" (fn [n] (e/+ 1 (e/cos (e/* Math/PI n))))})

(def setups
  {:limit {:title "Limit · Préliminaires" :f "1 + (−1)ⁿ/n" :x 1 :i 0.1 :n 20 :x0 1 :X 3 :mode "equal"
           :presets [["Does not converge" {:f "1 + (−1)ⁿ (does not converge)"}] ["Cauchy's sequence" {:f "1 + (−1)ⁿ/n"}]]}
   :continuity-ch2 {:title "Continuity · chapter II" :f "1 + sin x" :x 1 :i 0.2 :n 12 :x0 0 :X 3 :mode "equal"
                    :presets [["Break at the jump" {:f "jump at 1" :x 0.95 :i 0.1}] ["Shrink α" {:i 0.01}]]}
   :ivt {:title "A root between two signs" :f "x³ − 2x − 5" :x 2 :i 0.2 :n 10 :x0 2 :X 3 :mode "equal"
         :presets [["Discontinuous: theorem fails" {:f "jump at 1" :x0 0 :X 2}] ["More subdivisions" {:n 32}]]}
   :series {:title "The sum of a series" :series "1/n!" :n 10 :presets [["Harmonic: no sum" {:series "1/n"}] ["Square summable" {:series "1/n²"}]]}
   :root-test {:title "The root and ratio tests" :series "n²/2ⁿ" :n 13 :presets [["Inconclusive at k = 1" {:series "1/n"}] ["Divergence at k = 2" {:series "2ⁿ/n²"}]]}
   :derivative {:title "Difference quotient" :f "sin x" :x 1 :i 0.2 :n 12 :x0 0 :X 3 :mode "equal"
                :presets [["Power x²" {:f "x²"}] ["Near the jump" {:f "jump at 1" :x 0.95 :i 0.1}]]}
   :integral {:title "Cauchy's finite sum" :f "1 + sin x" :x 1 :i 0.2 :n 8 :x0 0 :X 3 :mode "equal"
              :presets [["Geometric division" {:mode "geometric"}] ["Refine to 40" {:n 40}]]}
   :refinement {:title "Refine the division" :f "1 + sin x" :x 1 :i 0.2 :n 8 :x0 0 :X 3 :mode "equal"
                :presets [["Geometric division" {:mode "geometric"}] ["Break continuity" {:f "jump at 1"}]]}
   :fundamental {:title "Move the upper limit" :f "1 + sin x" :x 1 :i 0.2 :n 16 :x0 0 :X 3 :mode "equal"
                 :presets [["Short strip" {:i 0.01}] ["Power x²" {:f "x²"}]]}
   :flat {:title "Flat at the origin" :f "flat e^(−1/x²)" :x 0.4 :i 0.1 :n 24 :x0 0 :X 1 :mode "equal"
          :presets [["Approach zero" {:x 0.08 :i 0.01}] ["Away from zero" {:x 0.8}]]}})

(defn- kernel [f] (vc/compiled-fn (fn [x] (fn [_] [(f x)])) [1] [0] {:simplify? false}))

(defn- schema [id]
  (if (#{:series :root-test} id)
    {:series {:options (mapv #(get-in ratio/series [% :label]) ratio/order) :label "series uₙ"}
     :n {:min 1 :max 60 :step 1 :label "n"}}
    (merge {:f {:options (vec (keys (if (= id :limit) sequences functions))) :label (if (= id :limit) "sequence xₙ" "f(x)")}
            :x {:min -2 :max 3 :step 0.01 :label "x"}
            :i {:min 0.001 :max 1 :step 0.001 :label (case id :limit "ε" :continuity-ch2 "α" :derivative "i" "α")}}
           (when (#{:ivt :integral :refinement :fundamental :flat} id)
             {:x0 {:min -2 :max 2 :step 0.01 :label "x₀"}
              :X {:min -1 :max 4 :step 0.01 :label "X"}
              :n {:min 2 :max 60 :step 1 :label (if (= id :ivt) "m subdivisions" "n partitions")}})
           (when (#{:integral :refinement :fundamental} id)
             {:mode {:options ["equal" "geometric"] :label "division mode"}}))))

(def series-page
  '(let [{:keys [series n]} @!P
         {:keys [u root ratio s k]} (get TABLE series)
         idx (dec n)
         verdict (cond (< k 1) "convergent (root test)" (> k 1) "divergent (root test)" :else "inconclusive: k = 1")
         fmt (fn [v] (if (js/isFinite v) (.toFixed v 6) "∞"))]
     [:div {:data-playground (name ID) :data-case verdict :data-value (str (nth s idx))}
      [:h4 TITLE] LEVA
      [:div "sₙ = Σ uⱼ = " (fmt (nth s idx))]
      [:div "uₙ = " (fmt (nth u idx)) "; (uₙ)^(1/n) = " (fmt (nth root idx))]
      [:div "uₙ₊₁/uₙ = " (fmt (nth ratio idx))]
      [:strong verdict]
      [:div "Try this: " (for [[label patch] PRESETS]
                          ^{:key label} [:button {:type "button" :on-click #(swap! !P merge patch)} label])]
      [:button {:type "button" :on-click #(reset! !P START)} "Reset to Cauchy's setup"]]))

(def page
  '(let [{:keys [f x i n x0 X mode]} @!P
         fnk (get KERNELS f)
         val (fn [t] (if (and (= f "flat e^(−1/x²)") (< (js/Math.abs t) 1e-9))
                        0 (aget (fnk (array 0) (array t)) 0)))
         fmt (fn [v] (if (js/isFinite v) (.toFixed v 6) "undefined"))
         y (val x) yi (val (+ x i))
         width (- X x0)
         nodes (fn [count]
                 (mapv (fn [j] (+ x0 (* width (if (= mode "geometric")
                                                    (- (js/Math.pow 2 (/ j count)) 1)
                                                    (/ j count))))) (range (inc count))))
         sum (fn [count] (let [xs (nodes count)]
                           (reduce + (map (fn [a b] (* (- b a) (val a))) xs (rest xs)))))
         s (when n (sum n))
         sr (when n (sum (* 2 n)))
         a (val x0) b (val X)
         brackets (when (= ID :ivt)
                    (let [xs (nodes n)]
                      (filterv (fn [[l r]] (<= (* (val l) (val r)) 0)) (map vector xs (rest xs)))))
         limit? (when (= ID :limit)
                  (let [tail (mapv (fn [j] (js/Math.abs (- (val j) 1))) (range 1 401))]
                    (first (for [j (range 1 400) :when (every? #(< % i) (subvec tail (dec j)))] j))))
         continuous? (not= f "jump at 1")
         case-name (case ID
                     :limit (if limit? "convergent: eventually inside ε" "does not converge: no N(ε)")
                     :continuity-ch2 (if (and (not continuous?) (<= x 1 (+ x i))) "discontinuous at the jump" "continuous at x")
                     :ivt (cond (empty? brackets) "no bracket: hypothesis fails" (not continuous?) "discontinuous: a bracket does not guarantee a root" :else "continuous: root bracketed")
                     :derivative (if (and (not continuous?) (<= x 1 (+ x i))) "jump: no derivative" "difference quotient approaches derivative")
                     :flat "all derivatives at zero = 0; Taylor series = 0 ≠ f(x) for x ≠ 0"
                     (if continuous? "continuous: sums approach the integral" "continuity hypothesis broken"))
         xmin (if (#{:ivt :integral :refinement :fundamental :flat} ID) (min x0 X) -2)
         xmax (if (#{:ivt :integral :refinement :fundamental :flat} ID) (max (+ x0 0.1) X) 4)
         points (when-not (= ID :limit)
                  (mapv (fn [j] (let [t (+ xmin (* (/ j 90) (- xmax xmin)))] [t (val t)])) (range 91)))
         f-area (when (= ID :fundamental)
                  (let [count (* 4 n)
                        f-at-x (fn [end] (let [h (/ (- end x0) count)]
                                           (reduce + (map (fn [j] (* h (val (+ x0 (* j h))))) (range count)))))]
                    (/ (- (f-at-x (+ x i)) (f-at-x x)) i)))]
     [:div {:data-playground (name ID) :data-case case-name :data-value (str (if (= ID :limit) limit? (or s (- yi y))))}
      [:h4 TITLE] LEVA
      (when points
        [mafs.core/Mafs {:view-box {:x [xmin xmax] :y [-4 6]} :height 250 :preserve-aspect-ratio false}
         [mafs.coordinates/Cartesian {}]
         [mafs.core/Polyline {:points points :color "#246b9c" :weight 2}]
         (when (not= ID :ivt) [mafs.core/Point {:x x :y y :color "#c0392b"}])])
      (case ID
        :limit [:div "|xₙ − 1| < ε = " (fmt i) " eventually: N(ε) = " (or limit? "none within 400 ranks")
                "; xₙ = " (fmt (val n)) " at n = " n]
        :continuity-ch2 [:div "f(x + α) − f(x) = " (fmt yi) " − " (fmt y) " = " (fmt (- yi y))]
        :ivt [:div "f(x₀) = " (fmt a) "; f(X) = " (fmt b) "; m = " n
              "; brackets: " (if (seq brackets) (mapv (fn [[l r]] (str "[" (fmt l) ", " (fmt r) "]")) brackets) "none")]
        :derivative [:div "[f(x + i) − f(x)] / i = [" (fmt yi) " − " (fmt y) "] / " (fmt i) " = " (fmt (/ (- yi y) i))]
        :flat [:div "f(x) = " (fmt y) "; f(0) = f'(0) = f''(0) = ⋯ = 0; Taylor at 0 = 0"]
        [:div "Sₙ = Σ (xⱼ₊₁ − xⱼ) f(xⱼ) = " (fmt s) "; S₂ₙ = " (fmt sr)
         "; S₂ₙ − Sₙ = " (fmt (- sr s))
         (when (= ID :fundamental) (str "; [F(x+α)−F(x)]/α = " (fmt f-area) "; f(x) = " (fmt y)))])
      [:strong {:data-verdict (str (or continuous? (#{:limit :series :root-test :flat} ID)))} case-name]
      [:div "Try this: " (for [[label patch] PRESETS]
                          ^{:key label} [:button {:type "button" :on-click #(swap! !P merge patch)} label])]
      [:button {:type "button" :on-click #(reset! !P START)} "Reset to Cauchy's setup"]]))

(defn widget [id]
  (let [{:keys [title presets] :as start} (setups id)
        initial (apply dissoc start [:title :presets])
        series? (#{:series :root-test} id)
        fs (if (= id :limit) sequences functions)
        ks (into {} (for [[label _] fs] [label (symbol (str "cauchy-k-" (Math/abs (hash label))))]))]
    (binding [vc/*backend* :raster]
      (ev/with-let [!p initial]
        (let [bindings (vec (mapcat (fn [[label f]] [(ks label) (kernel f)]) (if series? {} fs)))
              m {'!P !p 'ID id 'TITLE title 'PRESETS presets 'START initial
                 'LEVA (leva/controls {:atom !p :schema (schema id)})
                 'KERNELS ks 'TABLE (when series? (ratio-widget/data))}]
          (list 'reagent.core/with-let bindings
                (walk/postwalk-replace m (if series? series-page page))))))))
