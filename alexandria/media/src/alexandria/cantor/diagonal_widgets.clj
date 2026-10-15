(ns alexandria.cantor.diagonal-widgets
  "The widgets of the Cantor 1891 notebook.

     table-widget    an 8 x 8 table of m and w as the claimed list E1, E2,
                     ...: click a cell to change it. The diagonal a(n,n) and
                     its flip b are computed live; the cell of row n where
                     the new row b differs from row n (place n) is
                     highlighted, and the page says that b is in no row.
     theorem-widget  Cantor's theorem on a finite set X = {0 .. n-1}, n <= 3:
                     choose f : X -> P(X) from a table, see D = {x : x not
                     in f(x)} and that no f(x) equals D; a button runs every
                     map for n <= 3 and reports how many miss D.

   The table starts from the first eight rationals of [0, 1) written in
   binary (alexandria.cantor.sets/binary-rows), m = 0, w = 1. Every count
   of the second widget is computed on the JVM by
   alexandria.structure.cardinality/cantor-report; the browser flips cells
   and recomputes D, which is one line. No randomness."
  (:require [alexandria.cantor.sets :as sets]
            [alexandria.structure.cardinality :as card]
            [nextjournal.clerk :as clerk]))

(def width 8)

(defn table-data
  "{:rows [[\"m\" \"w\" ...] ...]}: the starting table as strings."
  []
  {:rows (mapv (fn [r] (mapv name r)) (map :row (sets/binary-rows width)))})

(defn theorem-data
  "{:report [{:n :size :power-size :maps :missed} ...]} for n = 1, 2, 3."
  []
  {:report (mapv card/cantor-report [1 2 3])})

(defn valid-table?
  "True when the data is a width x width table of \"m\" and \"w\"."
  [{:keys [rows]}]
  (and (= width (count rows))
       (every? #(and (= width (count %)) (every? #{"m" "w"} %)) rows)))

(defn valid-report?
  "True when every case misses D in every map and the counts are (2^n)^n."
  [{:keys [report]}]
  (and (= [1 2 3] (mapv :n report))
       (every? (fn [{:keys [n maps missed size power-size]}]
                 (and (= missed maps) (= maps (long (Math/pow (Math/pow 2 n) n)))
                      (= size n) (= power-size (long (Math/pow 2 n)))))
               report)))

(def ^:private table-render
  '(fn [{:keys [rows]}]
     (reagent.core/with-let [t (reagent.core/atom (vec (map vec rows)))]
       (let [n 8
             flip (fn [c] (if (= c "m") "w" "m"))
             b (vec (map (fn [i] (flip (get-in @t [i i]))) (range n)))
             in-no-row (every? (fn [i] (not= (nth b i) (get-in @t [i i]))) (range n))
             equal-rows (filter (fn [i] (= b (nth @t i))) (range n))
             cell {:width "2em" :height "2em" :text-align "center" :border "1px solid #8886"
                   :font-family "ui-monospace, monospace"}
             tog (fn [i j] (swap! t update-in [i j] flip))]
         [:div {:data-figure "cantor-table" :data-diagonal (apply str (map (fn [i] (get-in @t [i i])) (range n)))
                :data-flip (apply str b) :data-in-no-row (str in-no-row)
                :style {:margin "1em 0" :overflow-x "auto"}}
          [:p {:style {:font-size "0.9em"}}
           "Claimed list. Click a letter to change it. Row E" [:sub "n"] " is read left to right as (a" [:sub "n,1"] ", a" [:sub "n,2"] ", ...)."]
          [:table {:style {:border-collapse "collapse"}}
           [:tbody
            (for [i (range n)]
              ^{:key i}
              [:tr [:th {:style {:padding-right "0.6em" :font-weight 400}} (str "E" (inc i))]
               (for [j (range n)]
                 ^{:key j}
                 [:td {:style (merge cell (when (= i j) {:background "#E39B2D55"}))}
                  [:button {:type "button" :data-cell (str i "-" j) :on-click (fn [] (tog i j))
                            :style {:width "100%" :height "100%" :cursor "pointer" :border 0
                                    :background "transparent" :color "inherit" :font "inherit"}}
                   (get-in @t [i j])]])])
            [:tr [:th {:style {:padding-right "0.6em" :font-weight 600}} "E0"]
             (for [j (range n)]
               ^{:key j}
               [:td {:data-b (str j) :style (merge cell {:background "#C0392B33" :font-weight 700})} (nth b j)])]]]
          [:p {:data-verdict true :style {:font-weight 600 :color (if in-no-row "#2E8B57" "#C0392B")}}
           (if in-no-row
             "E0 = (b1, b2, ...) differs from every row En at place n (the shaded cell of row n against the same place of E0), so E0 is in none of the rows."
             "something is wrong: E0 agrees with a row on its diagonal")]
          [:p {:style {:font-size "0.9em"}}
           "Diagonal a(n,n): " [:code (apply str (map (fn [i] (get-in @t [i i])) (range n)))]
           ". Flipped, b: " [:code (apply str b)]
           (if (empty? equal-rows) ". Equal to no row of the table." (str ". Equal to row(s) " (pr-str equal-rows)))]
          [:button {:type "button" :data-act "reset" :on-click (fn [] (reset! t (vec (map vec rows))))
                    :style {:padding "0.3em 0.6em" :cursor "pointer"}} "Cantor-style start (binary rationals)"]
          " "
          [:button {:type "button" :data-act "all-m" :on-click (fn [] (reset! t (vec (repeat n (vec (repeat n "m"))))))
                    :style {:padding "0.3em 0.6em" :cursor "pointer"}} "every letter m"]])))) 

(def ^:private theorem-render
  '(fn [{:keys [report]}]
     (reagent.core/with-let [n (reagent.core/atom 3)
                             f (reagent.core/atom [[true false true] [false false true] [true true false]])
                             all? (reagent.core/atom false)]
       (let [xs (range @n)
             fx (fn [x] (set (filter (fn [y] (get-in @f [x y])) xs)))
             d (set (filter (fn [x] (not (get-in @f [x x]))) xs))
             hits (filter (fn [x] (= d (fx x))) xs)
             missed (empty? hits)
             show (fn [s] (str "{" (apply str (interpose ", " (sort s))) "}"))
             btn {:margin "0.2em" :padding "0.3em 0.6em" :cursor "pointer"}]
         [:div {:data-figure "cantor-theorem" :data-n (str @n) :data-d (show d) :data-missed (str missed)
                :style {:margin "1em 0"}}
          [:div "X = {0 .. n-1}, n = "
           (for [k [1 2 3]]
             ^{:key k}
             [:button {:type "button" :data-n-btn (str k) :on-click (fn [] (reset! n k))
                       :style (merge btn (when (= k @n) {:background "#1F6FB433"}))} k])]
          [:p {:style {:font-size "0.9em"}} "Choose f(x), a subset of X, for each x: tick the members. "
           [:b "|X| = " @n ", |P(X)| = " (Math/pow 2 @n) "."]]
          [:table {:style {:border-collapse "collapse"}}
           [:thead [:tr [:th "x"] (for [y xs] ^{:key y} [:th {:style {:padding "0 0.5em"}} (str "∈f(x)? " y)])
                    [:th "f(x)"] [:th "x ∈ f(x)?"]]]
           [:tbody
            (for [x xs]
              ^{:key x}
              [:tr [:td x]
               (for [y xs]
                 ^{:key y}
                 [:td {:style {:text-align "center" :background (when (= x y) "#E39B2D55")}}
                  [:input {:type "checkbox" :data-cell (str x "-" y) :checked (boolean (get-in @f [x y]))
                           :on-change (fn [] (swap! f update-in [x y] not))}]])
               [:td {:style {:font-family "ui-monospace, monospace"}} (show (fx x))]
               [:td (if (contains? (fx x) x) "yes" "no: x goes into D")]])]]
          [:p "D = {x : x ∉ f(x)} = " [:code {:data-d-text true} (show d)]]
          [:p {:data-verdict true :style {:font-weight 600 :color (if missed "#2E8B57" "#C0392B")}}
           (if missed
             "D is not f(x) for any x: for each x, D and f(x) disagree about x itself. f is not onto P(X)."
             (str "D equals f(x) for x in " (pr-str hits) ": impossible, see below"))]
          [:button {:type "button" :data-act "run-all" :on-click (fn [] (swap! all? not)) :style btn}
           (if @all? "hide the run" "run every map for n ≤ 3")]
          (when @all?
            [:table {:data-report true :style {:margin-top "0.6em" :font-family "ui-monospace, monospace" :font-size "0.9em"}}
             [:thead [:tr [:th "n"] [:th "|P(X)|"] [:th "maps f : X → P(X)"] [:th "D missed"] [:th ""]]]
             [:tbody
              (for [{:keys [n size power-size maps missed]} report]
                ^{:key n}
                [:tr {:data-row (str n)} [:td n] [:td power-size] [:td maps] [:td missed]
                 [:td {:style {:color (if (= maps missed) "#2E8B57" "#C0392B")}}
                  (if (= maps missed) "missed in every case" "COUNTEREXAMPLE")]])]])]))))

(defn table-widget
  "The 8 x 8 m/w table with its diagonal, flip and highlighted places."
  []
  (let [d (table-data)]
    (assert (valid-table? d) "the starting table is not 8 x 8 of m and w")
    (clerk/with-viewer {:render-fn table-render :transform-fn clerk/mark-presented} d)))

(defn theorem-widget
  "Cantor's theorem on X = {0 .. n-1}, n <= 3, with the run over every map."
  []
  (let [d (theorem-data)]
    (assert (valid-report? d) "a map on a set of up to three elements hits its diagonal set")
    (clerk/with-viewer {:render-fn theorem-render :transform-fn clerk/mark-presented} d)))
