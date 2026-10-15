(ns alexandria.gauss.widgets
  "The 17-gon widgets of the Gauss notebook (Disquisitiones arithmeticae,
   art. 354 and 365), emmy-viewers fragments on the raster backend.

     periods-player  the tower for p = 17 stepped, one sentence per step:
                     the sixteen roots ordered by the powers of 3, split
                     into periods of 8, 4 and 2, each split a quadratic
                     whose roots are printed from raster; the last step
                     draws cos(2 pi/17) = w1/2 on the circle and the 17-gon
     n-gon           the playground: n free (slider, or drag the vertex P1
                     along the circle), the n-gon drawn by a raster kernel,
                     2 cos(2 pi/n) read off it, n factored, the case named
                     live (constructible / not, the obstruction, Gauss 365-366
                     and Wantzel 1837), and the period tower of the prime
                     that decides it: one chip per level with its degree,
                     red where the degree is not 2; the circle coloured by
                     the periods of the level picked. Try-this presets and
                     reset.

   The towers and the classification are plain data from
   alexandria.gauss.towers (integers, and period values from raster)."
  (:require [alexandria.gauss.heptadecagon :as h]
            [alexandria.gauss.towers :as towers]
            [clojure.string :as str]
            [emmy.clerk :as ec]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [nextjournal.clerk :as clerk]))

;; ---------------------------------------------------------------------------
;; Shared

(def palette ["#C0392B" "#1F6FB4" "#2E8B57" "#E67E22" "#7B5EA7" "#16A085" "#B7950B" "#D35400"])

(defn- kernel [f xs] (vc/compiled-fn f xs [0] {:simplify? false}))

(defn- fmt
  ([x] (fmt x 4))
  ([x n] (list 'let ['v x] (list 'if '(js/isFinite v) (list '.replace (list '.toFixed 'v n) "-" "−") "∞"))))

(defn- num
  "x to n places, for TeX (a plain hyphen-minus: KaTeX sets it as a minus)."
  [x n]
  (format (str "%." n "f") (double x)))

(defn- button [attrs title on-click]
  [:button (merge {:type "button"
                   :style {:margin "0.25em" :padding "0.3em 0.6em" :cursor "pointer" :border-radius "0.3em"
                           :border "1px solid currentColor" :background "transparent" :color "inherit"}
                   :on-click on-click}
                  attrs)
   title])

(defn vertex
  "The kernel of the playground: for n and k, [cos(2 pi k/n) sin(2 pi k/n)
   2 cos(2 pi/n)]."
  [n k]
  (fn [_]
    (let [a (e/divide (e/* 2 Math/PI k) n)]
      [(e/cos a) (e/sin a) (e/* 2 (e/cos (e/divide (* 2 Math/PI) n)))])))

;; ---------------------------------------------------------------------------
;; 1. The tower of 17, stepped

(defn player-steps
  "The steps of the 17-gon player, each {:text :level :eq}; every number in
   them from raster (heptadecagon/nested-values and tower-values)."
  []
  (let [v (h/nested-values)
        n #(num (v %) 6)]
    [{:level 0 :text "Write the sixteen roots r, r², …, r¹⁶ of x¹⁷ = 1 in the order of the powers of 3 (art. 353): 3 is a primitive root of 17, so 3⁰, 3¹, …, 3¹⁵ run through every exponent once."
      :eq "r^{1}, r^{3}, r^{9}, r^{10}, r^{13}, r^{5}, r^{15}, r^{11}, r^{16}, r^{14}, r^{8}, r^{7}, r^{4}, r^{12}, r^{2}, r^{6}"}
     {:level 1 :text "Take every second root of that row: two periods of eight. Their sum is −1 and their product −4, so they are the roots of a quadratic."
      :eq (str "y^2 + y - 4 = 0:\\quad y_1 = \\tfrac{-1+\\sqrt{17}}{2} = " (n :y1) ",\\quad y_2 = " (n :y2))}
     {:level 2 :text "Take every fourth: four periods of four. The two inside y₁ have sum y₁ and product −1; the two inside y₂ have sum y₂ and product −1."
      :eq (str "z^2 - y_1 z - 1 = 0:\\ z_1 = " (n :z1) ",\\ z_2 = " (n :z2)
               ";\\quad z^2 - y_2 z - 1 = 0:\\ z_3 = " (n :z3) ",\\ z_4 = " (n :z4))}
     {:level 3 :text "Take every eighth: eight periods of two, r^k + r^{−k} = 2 cos(2πk/17). The two inside z₁ have sum z₁ and product z₃."
      :eq (str "w^2 - z_1 w + z_3 = 0:\\quad w_1 = 2\\cos\\tfrac{2\\pi}{17} = " (n :w1) ",\\quad w_2 = " (n :w2))}
     {:level 3 :text "Four square roots, nested, give cos(2π/17) = w₁/2: the perpendicular at that abscissa cuts the circle at the first vertex, and the chord steps round seventeen times."
      :eq (str "\\cos\\tfrac{2\\pi}{17} = \\tfrac{w_1}{2} = " (n :cos))
      :polygon true}]))

(defn periods-player
  "The tower of 17 stepped (prev / next / play), the circle coloured by the
   periods of the step's level."
  []
  (binding [vc/*backend* :raster]
    (let [{:keys [levels]} (towers/tower 17)
          members (mapv :member levels)
          steps (player-steps)
          xy (fn [k] (let [a (/ (* 2 Math/PI k) 17)] [(Math/cos a) (Math/sin a)]))
          cos17 (:cos (h/nested-values))
          kv (kernel vertex [17 1])
          ksym (gensym "vertex")]
      (ev/with-let [!s {:step 0}]
        (list 'reagent.core/with-let [ksym kv]
              (list 'let ['i (list 'max 0 (list 'min (dec (count steps)) (ev/get !s :step)))
                          'st (list 'nth steps 'i)
                          'lvl '(:level st)
                          'mem (list 'nth members 'lvl)
                          'P (list 'fn '[k] (list 'let ['q (list ksym '(array 0) '(array 17 k))] '[(aget q 0) (aget q 1)]))]
                    [:div {:data-figure "gauss-periods" :data-step '(str i)}
                     [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em"}}
                      [:div {:style {:flex "1 1 380px" :min-width "300px"}}
                       (mafs/mafs {:height 380 :view-box {:x [-1.3 1.3] :y [-1.3 1.3]}}
                                  (mafs/cartesian {:x-axis {:lines 0.5} :y-axis {:lines 0.5}})
                                  (mafs/circle {:center [0 0] :radius 1 :color "#8A9BB0" :fill-opacity 0})
                                  (list 'when '(:polygon st)
                                        [:<>
                                         (mafs/segment {:point1 [cos17 -1.2] :point2 [cos17 1.2] :color "#7B5EA7" :style "dashed"})
                                         (mafs/polygon {:points (list 'mapv 'P '(range 17)) :color "#C0392B" :fill-opacity 0.08})])
                                  (into [:<>]
                                        (for [k (range 1 17) :let [[x y] (xy k)]]
                                          [:<>
                                           (mafs/point {:x x :y y :color (list 'nth palette (list 'mod (list 'nth 'mem (dec k)) 8))})
                                           (mafs/text (str k) {:x (* 1.13 x) :y (* 1.13 y) :size 12})]))
                                  (mafs/point {:x 1 :y 0 :color "#111111"})
                                  (mafs/text "1" {:x 1.13 :y 0 :size 12}))]
                      [:div {:style {:flex "1 1 320px" :min-width "260px" :font-size "0.95em" :line-height 1.55}}
                       [:div {:style {:opacity 0.7}} "step " '(inc i) " of " (count steps)]
                       [:p {:data-step-text true} '(:text st)]
                       [:div {:data-formula "period-step"}
                        [:f> 'nextjournal.clerk.render/render-katex '(:eq st) {:inline? false}]]
                       [:div
                        (button {:data-act "prev"} "◀ prev" (list 'fn [] (list 'swap! !s 'update :step '(fn [j] (max 0 (dec j))))))
                        (button {:data-act "next"} "next ▶" (list 'fn [] (list 'swap! !s 'update :step (list 'fn '[j] (list 'min (dec (count steps)) '(inc j))))))
                        (button {:data-act "reset"} "from the start" (list 'fn [] (list 'reset! !s {:step 0})))]
                       [:div {:style {:font-size "0.85em" :opacity 0.75 :margin-top "0.4em"}}
                        "The dots are the roots r^k = cos(2πk/17) + i sin(2πk/17), numbered by k; one colour per period of the step."]]]])
              '(finally nil))))))

;; ---------------------------------------------------------------------------
;; 2. The playground over n

(def n-max "The largest n the playground offers." 100)

(defn- factor-str [fs]
  (str/join " · " (for [[p a] fs] (if (= a 1) (str p) (str p "^" a)))))

(defn- case-name
  "The case of the n-gon in words."
  [{:keys [n constructible? factors fermat obstruction culprit degree]}]
  (cond
    (and constructible? (empty? fermat))
    (str "constructible: n = " (factor-str factors) " is a power of 2; bisect the angle again and again")
    constructible?
    (str "constructible: n = " (factor-str factors) ", "
         (if (= 1 (count fermat))
           (str (first fermat) " is a Fermat prime 2^(2^ν) + 1")
           (str "the odd primes " (str/join ", " fermat) " are distinct Fermat primes 2^(2^ν) + 1"))
         " (Gauss, art. 365)")
    (= obstruction :odd-prime-power)
    (str "not constructible: " culprit "² divides n = " (factor-str factors) "; the division into " (* culprit culprit)
         " parts needs an equation of degree " culprit " (art. 366); Wantzel (1837) proved no ruler and compass can do it")
    :else
    (str "not constructible: " culprit " is prime but " culprit " − 1 = " (dec culprit) " has the odd factor " degree
         "; the tower needs an equation of degree " degree " (art. 365); Wantzel (1837) proved no ruler and compass can do it")))

(defn- tower-prime
  "The prime whose tower decides n: the culprit, else the largest odd prime
   factor, else nil (a power of 2)."
  [{:keys [culprit factors]}]
  (or culprit (last (remove #{2} (keys factors)))))

(defn n-data
  "{n {:name :ok :prime :p :fs}} for 3..n-max, and {p [{:e :degree :values
   :member}]} for the primes that decide them."
  []
  (let [cls (towers/classes n-max)
        info (into (sorted-map)
                   (for [[n c] cls]
                     [n {:name (case-name c) :ok (:constructible? c) :prime (h/prime? n)
                         :p (tower-prime c) :fs (factor-str (:factors c))
                         :obstruction (some-> (:obstruction c) name)}]))
        ps (set (keep :p (vals info)))
        tw (into (sorted-map)
                 (for [p ps]
                   [p (mapv #(select-keys % [:e :f :degree :values :member]) (:levels (towers/tower p)))]))]
    {:info info :towers tw}))

(def n-init {:n 17 :level 3})

(defn n-gon
  "The playground over n."
  []
  (binding [vc/*backend* :raster]
    (let [{:keys [info towers]} (n-data)
          kv (kernel vertex [17 1])
          ksym (gensym "vertex")]
      (ev/with-let [!s n-init]
        (list 'reagent.core/with-let [ksym kv]
              (list 'let ['n (list 'max 3 (list 'min n-max (list 'js/Math.round (ev/get !s :n))))
                          'I (list 'get info 'n)
                          'tw (list 'get towers '(:p I))
                          'lv (list 'max 0 (list 'min '(dec (count tw)) (list 'js/Math.round (ev/get !s :level))))
                          'L '(when tw (nth tw lv))
                          'colour? '(and (:prime I) L)
                          'P (list 'fn '[k] (list 'let ['q (list ksym '(array 0) '(array n k))] '[(aget q 0) (aget q 1)]))
                          'q1 (list ksym '(array 0) '(array n 1))]
                    [:div {:data-figure "gauss-ngon" :data-n '(str n) :data-case '(:name I)
                           :data-constructible '(str (:ok I)) :data-twocos '(str (aget q1 2))}
                     [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                      [:div {:style {:flex "1 1 400px" :min-width "300px"}}
                       (mafs/mafs {:height 400 :view-box {:x [-1.3 1.3] :y [-1.3 1.3]}}
                                  (mafs/cartesian {:x-axis {:lines 0.5} :y-axis {:lines 0.5}})
                                  (mafs/circle {:center [0 0] :radius 1 :color "#8A9BB0" :fill-opacity 0})
                                  (mafs/polygon {:points '(mapv P (range n))
                                                 :color '(if (:ok I) "#2E8B57" "#C0392B") :fill-opacity 0.07})
                                  (mafs/segment {:point1 ['(aget q1 0) -1.2] :point2 ['(aget q1 0) 1.2] :color "#7B5EA7" :style "dashed"})
                                  (list 'into [:<>]
                                        (list 'for '[k (range 1 n)]
                                              (list 'let '[[x y] (P k)]
                                                    (mafs/point {:x 'x :y 'y
                                                                 :color (list 'if 'colour?
                                                                              (list 'nth palette '(mod (nth (:member L) (dec k)) 8))
                                                                              "#3C4650")}))))
                                  (mafs/point {:x 1 :y 0 :color "#111111"})
                                  (mafs/text "P₀" {:x 1.12 :y -0.08 :size 13})
                                  (mafs/movable-point {:point ['(aget q1 0) '(aget q1 1)] :color "#7B5EA7"
                                                       :constrain '(fn [[x y]] (let [r (js/Math.hypot x y) r (if (> r 1e-9) r 1)] [(/ x r) (/ y r)]))
                                                       :on-move (list 'fn '[[x y]]
                                                                      (list 'let ['a '(js/Math.atan2 y x)
                                                                                  'a '(if (> a 1e-3) a 1e-3)]
                                                                            (list 'swap! !s 'assoc :n (list 'max 3 (list 'min n-max '(js/Math.round (/ (* 2 js/Math.PI) a)))))))})
                                  (mafs/text "P₁" {:x '(* 1.14 (aget q1 0)) :y '(* 1.14 (aget q1 1)) :size 13}))
                       [:div {:style {:font-size "0.85em" :opacity 0.75}}
                        "drag P₁ along the circle (the angle P₀OP₁ = 2π/n), or move n; the dashed line is x = cos(2π/n). For a prime n the vertices are coloured by Gauss' periods of the level you pick."]]
                      [:div {:style {:flex "1 1 320px" :min-width "280px" :font-size "0.93em" :line-height 1.55}}
                       (leva/sub-panel {:fill true :flat true :title-bar false}
                                       (leva/controls {:atom !s :schema {:n {:min 3 :max n-max :step 1 :label "n (sides)"}
                                                                          :level {:min 0 :max 7 :step 1 :label "tower level"}}}))
                       [:div {:data-case true :style {:font-weight 600 :font-size "1.08em" :margin "0.3em 0"
                                                      :color '(if (:ok I) "#2E8B57" "#C0392B")}}
                        '(:name I)]
                       [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.92em"}}
                        [:div "n = " 'n " = " '(:fs I)]
                        [:div "2 cos(2π/n) = " [:span {:data-twocos true} (fmt '(aget q1 2) 6)]
                         ", the side 2 sin(π/n) = " (fmt '(js/Math.hypot (- 1 (aget q1 0)) (aget q1 1)) 6)]]
                       (list 'if 'tw
                             [:div {:style {:margin-top "0.5em"}}
                              [:div "Gauss' tower for p = " '(:p I) " (p − 1 = " '(dec (:p I)) "):"]
                              (list 'into [:div {:data-tower true :style {:display "flex" :flex-wrap "wrap" :gap "0.3em" :margin "0.3em 0"}}]
                                    (list 'for '[[j T] (map-indexed vector tw)]
                                          [:span {:data-level 'j
                                                  :style {:padding "0.1em 0.5em" :border-radius "0.8em"
                                                          :border (list 'str "1px solid " '(if (> (:degree T) 2) "#C0392B" "#2E8B57"))
                                                          :background '(if (= j lv) "#7B5EA722" "transparent")
                                                          :color '(if (> (:degree T) 2) "#C0392B" "inherit")}}
                                           '(if (zero? j) "1 period" (str "×" (:degree T) " → " (:e T) " periods"))]))
                              (list 'when 'L
                                    [:div {:style {:font-size "0.88em"}}
                                     "level " 'lv ": " '(:e L) " periods of " '(* 2 (:f L)) " roots"
                                     (list 'when '(<= (:e L) 8)
                                           (list 'str ", values " '(clojure.string/join ", " (map (fn [v] (.replace (.toFixed v 4) "-" "−")) (:values L)))))])
                              [:div {:style {:font-size "0.85em" :opacity 0.8}}
                               '(if (every? (fn [T] (<= (:degree T) 2)) tw)
                                  "every step a quadratic: square roots suffice"
                                  "a step of degree above 2: Gauss says it can be neither avoided nor reduced")]]
                             [:div {:style {:margin-top "0.5em" :font-size "0.9em"}} "no odd prime: only bisections, no tower."])
                       [:p {:style {:margin "0.6em 0 0.2em"}} [:strong "Try this:"]]
                       (button {:data-preset "17"} "n = 17: Gauss' polygon, a tower of four quadratics" (list 'fn [] (list 'reset! !s {:n 17 :level 3})))
                       (button {:data-preset "7"} "n = 7: the heptagon needs a cubic" (list 'fn [] (list 'reset! !s {:n 7 :level 1})))
                       (button {:data-preset "9"} "n = 9: 3 twice, the trisection again" (list 'fn [] (list 'reset! !s {:n 9 :level 1})))
                       (button {:data-preset "51"} "n = 51 = 3 · 17: two Fermat primes at once" (list 'fn [] (list 'reset! !s {:n 51 :level 3})))
                       (button {:data-preset "reset"} "Reset: n = 17" (list 'fn [] (list 'reset! !s n-init)))]]])
              '(finally nil))))))

;; ---------------------------------------------------------------------------
;; The derivation, for the page

(defn derivation
  "Emmy's radicals for cos(2 pi/17) beside Gauss' printed one, and the
   grades."
  []
  (let [{:keys [cos]} (h/nested)]
    (clerk/col
     (clerk/html [:div {:style {:font-size "0.85em" :opacity 0.7}} "Emmy, by the tower (w₁/2):"])
     (ec/->TeX cos :simplify? false)
     (clerk/html [:div {:style {:font-size "0.85em" :opacity 0.7}} "Gauss, art. 365:"])
     (ec/->TeX (h/gauss-365) :simplify? false))))
