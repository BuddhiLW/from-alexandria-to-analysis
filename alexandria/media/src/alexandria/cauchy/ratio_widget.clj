(ns alexandria.cauchy.ratio-widget
  "Chapter VI, Theorems I and II side by side: the reader picks a series,
   drives n, and watches the root (u_n)^(1/n) and the ratio u_(n+1)/u_n
   close on the same k (alexandria.cauchy.ratio).

   State {:series :n :U :N}, every key a live Leva control. U and N are
   tied by Cauchy's argument (widget brief, section 5):

     U moves        the series fixed, N re-solved: the first n from which
                    every root stays below U (above U when k > 1)
     N moves        U re-solved: the greatest root from N on (the least,
                    for k > 1), the U that just works from N
     series moves   U kept if it still lies between k and 1, else brought
                    inside; N re-solved

   Only the touched key is a source; pushes never re-fire the solver
   (origin guard, and an echo within half a slider step is ignored).

   Every number is raster's: the table of u_n, roots, ratios and partial
   sums (alexandria.ratio/tables, raster kernels of Emmy's log u_n) ships
   as data; the browser only selects in it (the tie) and draws. Emmy's
   ratio in closed form is shown as TeX beside the plot, with n
   substituted from the table."
  (:require [alexandria.cauchy.ratio :as ra]
            [clojure.walk :as walk]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.viewer :as ev]
            [nextjournal.clerk :as clerk]))

(def colours {:root "#C0392B" :ratio "#1F6FB4" :k "#2F8A3E" :U "#E67E22" :one "#666666" :n "#7B5EA7"})

(def view {:x [0 (+ ra/shown 1)] :y [-0.15 2.3]})

(defn- label [id] (get-in ra/series [id :label]))

(defn data
  "What the browser reads, per series label: k, the TeX of u_n and of
   Emmy's ratio, the sum when known, and the raster table to the horizon
   as parallel vectors."
  []
  (into {}
        (for [id ra/order
              :let [{:keys [k tex]} (ra/series id)
                    rows (@ra/tables id)]]
          [(label id)
           {:k (double k) :tex tex
            :ratio-tex (e/->TeX (ra/ratio-expr id 'n))
            :side (ra/side k) :urange (ra/u-range k)
            :sum (ra/sums-known id)
            :u (mapv :u rows) :root (mapv :root rows) :ratio (mapv :ratio rows) :s (mapv :s rows)}])))

(def ranges {:n [1 ra/shown 1] :U [0.02 2.0 0.005] :N [1 ra/horizon 1]})

(defn init []
  (let [rows (@ra/tables :quad)]
    {:series (label :quad) :n 10 :U 0.75 :N (ra/first-from rows 1/2 3/4)}))

(defn- schema []
  {:series {:options (mapv label ra/order) :label "series"}
   :n {:min 1 :max ra/shown :step 1 :label "n"}
   :U {:min 0.02 :max 2.0 :step 0.005 :label "U (Cauchy's)"}
   :N {:min 1 :max ra/horizon :step 1 :label "N: from here on"}})

(def tie-code
  '[first-from (fn [{:keys [root side]} U]
                 (let [ok? (if (= 1 side) (fn [r] (<= r U)) (fn [r] (>= r U)))]
                   (loop [n (count root) found nil]
                     (if (or (zero? n) (not (ok? (nth root (dec n))))) found (recur (dec n) n)))))
    greatest-from (fn [{:keys [root side]} N]
                    (reduce (if (= 1 side) max min) (drop (dec N) root)))
    clampU (fn [{:keys [urange]} U] (let [[lo hi] urange] (min hi (max lo U))))
    pushing (atom false)
    pushed (atom {})
    tie (fn [src]
          (js/setTimeout
           (fn []
             (let [cur (assoc @!S src (get @!P src))
                   d (get DATA (:series cur))
                   solved (cond
                            (nil? (:side d)) cur
                            (= src :N) (let [N (max 1 (min HORIZON (js/Math.round (:N cur))))]
                                         (assoc cur :N N :U (greatest-from d N)))
                            :else (let [U (clampU d (:U cur))]
                                    (assoc cur :U U :N (or (first-from d U) HORIZON))))
                   shown @!P
                   push (into {} (filter (fn [[k v]] (and (number? v) (> (js/Math.abs (- v (get shown k))) 1e-12))) solved))]
               (set! (.-__ratioSolves js/globalThis) (inc (or (.-__ratioSolves js/globalThis) 0)))
               (reset! !S solved)
               (when (seq push)
                 (reset! pushing true)
                 (swap! !P merge push)
                 (reset! pushing false)
                 (swap! pushed merge push)
                 (js/setTimeout (fn [] (swap! pushed (fn [m] (apply dissoc m (keys push))))) 600))))
           0))
    _ (add-watch !P :cauchy/ratio-tie
                 (fn [_ _ old new]
                   (when-not @pushing
                     (let [src (first (filter (fn [k] (not= (get old k) (get new k))) [:series :n :U :N]))
                           echo? (and src (contains? @pushed src) (number? (get new src))
                                      (<= (js/Math.abs (- (get new src) (get @pushed src)))
                                          (+ 1e-9 (get HALF src 0))))]
                       (cond (= src :n) (swap! !S assoc :n (get new :n))
                             (and src (not echo?)) (tie src))))))])

(def template
  '(let [{:keys [series n U N]} @!S
         {:keys [k tex ratio-tex side sum u root ratio s]} (get DATA series)
         i (dec n)
         fmt (fn [x d] (if (js/isFinite x) (.replace (.toFixed x d) "-" "−") "∞"))
         sci (fn [x] (if (< (js/Math.abs x) 1e-3) (.replace (.toExponential x 3) "-" "−") (fmt x 5)))
         shown (fn [v] (map-indexed (fn [j y] [(inc j) (min 2.25 y)]) (take SHOWN v)))
         verdict (case side 1 "converges" -1 "diverges" "open")
         [x0 x1] (:x VIEW)
         subst (.replace ratio-tex (js/RegExp. "\\bn\\b" "g") (str n))]
     [:div {:data-figure "cauchy-ratio-root" :data-series series :data-n (str n) :data-u (str U) :data-nn (str N)
            :data-root (str (nth root i)) :data-ratio (str (nth ratio i)) :data-verdict verdict}
      [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
       [:div {:style {:flex "1 1 440px" :min-width "300px"}}
        [mafs.core/Mafs {:view-box VIEW :height 400 :preserve-aspect-ratio false}
         [mafs.coordinates/Cartesian {:x-axis {:lines 10} :y-axis {:lines 0.5}}]
         [mafs.line/Segment {:point1 [x0 1] :point2 [x1 1] :color (:one COLOURS) :style "dashed"}]
         [mafs.line/Segment {:point1 [x0 k] :point2 [x1 k] :color (:k COLOURS) :weight 2}]
         (when side
           [:<>
            [mafs.line/Segment {:point1 [x0 U] :point2 [x1 U] :color (:U COLOURS) :weight 3}]
            (when (<= N SHOWN)
              [mafs.core/Polygon {:points [[N -0.15] [x1 -0.15] [x1 2.3] [N 2.3]] :color (:U COLOURS) :fill-opacity 0.08 :weight 0}])])
         [mafs.core/Polyline {:points (shown root) :color (:root COLOURS) :weight 2}]
         [mafs.core/Polyline {:points (shown ratio) :color (:ratio COLOURS) :weight 2}]
         [mafs.line/Segment {:point1 [n -0.15] :point2 [n 2.3] :color (:n COLOURS) :opacity 0.6}]
         [mafs.core/Point {:x n :y (min 2.25 (nth root i)) :color (:root COLOURS)}]
         [mafs.core/Point {:x n :y (min 2.25 (nth ratio i)) :color (:ratio COLOURS)}]]
        [:div {:style {:font-size "0.85em" :opacity 0.8}}
         [:span {:style {:color (:root COLOURS)}} "● root (uₙ)^(1/n)  "]
         [:span {:style {:color (:ratio COLOURS)}} "● ratio uₙ₊₁/uₙ  "]
         [:span {:style {:color (:k COLOURS)}} "— k  "]
         [:span {:style {:color (:U COLOURS)}} "— U, shaded from N on  "]
         [:span "- - 1"]]]
       [:div {:style {:flex "0 1 320px" :min-width "270px"}}
        LEVA
        [:div {:data-formula (str tex) :style {:margin-top "0.4em"}}
         [:f> nextjournal.clerk.render/render-katex (str tex ",\\qquad \\frac{u_{n+1}}{u_n} = " ratio-tex) {:inline? false}]
         [:f> nextjournal.clerk.render/render-katex (str "n = " n ":\\quad \\frac{u_{" (inc n) "}}{u_{" n "}} = " subst " = " (fmt (nth ratio i) 5)) {:inline? false}]]
        [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.88em" :line-height 1.5}}
         [:div "uₙ = " (sci (nth u i))]
         [:div "root (uₙ)^(1/n) = " [:span {:data-root-shown "1"} (fmt (nth root i) 5)]]
         [:div "ratio uₙ₊₁/uₙ = " [:span {:data-ratio-shown "1"} (fmt (nth ratio i) 5)] "   (k = " (fmt k 3) ")"]
         [:div "sₙ = " (fmt (nth s i) 6) (when sum (str "   (sum " (fmt sum 6) ")"))]
         (when side
           [:div (if (= 1 side)
                   (str "U = " (fmt U 4) ": from N = " N " on every root < U, so uₙ < Uⁿ")
                   (str "U = " (fmt U 4) ": from N = " N " on every root > U > 1, so uₙ > Uⁿ"))])]
        [:div {:data-verdict-shown verdict
               :style {:font-size "1.15em" :font-weight 600 :margin "0.4em 0"
                       :color (case side 1 (:k COLOURS) -1 (:root COLOURS) (:one COLOURS))}}
         (case side
           1 (str "k = " (fmt k 3) " < 1: the series converges")
           -1 (str "k = " (fmt k 3) " > 1: the series diverges")
           (str "k = 1: Theorems I and II leave it open (" (if (= series "1/n") "this one diverges" "this one converges") ")"))]]]]))

(defn widget
  ([] (widget (init)))
  ([start]
   (let [d (data)]
     (ev/with-let [!p start !s start]
       (let [m {'!P !p '!S !s 'DATA d 'HORIZON ra/horizon 'SHOWN ra/shown 'VIEW view 'COLOURS colours
                'HALF (into {} (for [[k [_ _ s]] ranges] [k (/ s 2)]))
                'LEVA (leva/controls {:atom !p :schema (schema)})}]
         (list 'reagent.core/with-let
               (walk/postwalk-replace m tie-code)
               (walk/postwalk-replace m template)
               (list 'finally (list 'remove-watch !p :cauchy/ratio-tie))))))))

(defn grades-list
  "The ratio-test grades as a Clerk list."
  []
  (clerk/html
   (into [:ul {:style {:font-size "0.9em"}}]
         (for [{:keys [label grade]} (ra/grades)]
           [:li {:data-grade (name grade)} [:code (name grade)] " " label]))))
