(ns alexandria.galois.widgets
  "The widgets of the Galois notebook (1831, printed 1846).

     roots-widget  the four roots of x^4 - 2 on the complex plane (numbers
                   from alexandria.galois.quartic, raster). The reader
                   applies sigma (multiply by i) and tau (conjugate) or any
                   of the 24 permutations of S_4 from the list; the roots
                   are relabelled live, and the page names whether the
                   permutation keeps the relations r0 + r2 = 0, r1 + r3 = 0,
                   that is whether it lies in the group of the equation (8
                   of 24, the dihedral group D_4).
     group-table   the 24 permutations with their verdicts (JVM, raster)."
  (:require [alexandria.galois.quartic :as q]
            [emmy.mafs :as mafs]
            [emmy.viewer :as ev]
            [nextjournal.clerk :as clerk]
            [alexandria.galois.solvable :as s]))

(def colours {:root "#1F6FB4" :in "#2E8B57" :out "#C0392B" :rel "#E39B2D"})

(defn- compose-form
  "Quoted (fn [s t] s after t) on permutation vectors."
  []
  '(fn [s t] (mapv (fn [k] (nth s (nth t k))) (range 4))))

(defn roots-widget
  "Permute the roots of x^4 - 2 and see whether the permutation is in the
   group of the equation."
  []
  (let [rts (q/roots)
        tbl (q/table)
        in (into #{} (map :perm) (filter :keeps? tbl))
        names (into {} (map (juxt :perm :cycles)) tbl)
        btn {:margin "0.2em" :padding "0.3em 0.6em" :cursor "pointer"}]
    (ev/with-let [!s {:perm [0 1 2 3]}]
      (list 'let ['rts rts 'in in 'names names 'compose (compose-form)
                  'p (list :perm (list 'deref !s))
                  'ok '(contains? in p)
                  'pos '(fn [k] (nth rts (nth p k)))
                  'go! (list 'fn '[v] (list 'swap! !s 'assoc :perm 'v))
                  'apply! (list 'fn '[g] (list 'swap! !s 'update :perm '(fn [cur] (compose g cur))))]
            [:div {:data-figure "galois-roots" :data-perm '(pr-str p) :data-in-group '(str ok)}
             [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
              [:div {:style {:flex "1 1 360px" :min-width "300px"}}
               (mafs/mafs {:view-box {:x [-1.6 1.6] :y [-1.6 1.6]} :height 360}
                          (mafs/cartesian {})
                          (mafs/circle {:center [0 0] :radius (Math/pow 2 0.25) :color "#8A9BB0" :fill-opacity 0})
                          (mafs/polyline {:points ['(pos 0) '(pos 2)], :color (colours :rel), :style "dashed"})
                          (mafs/polyline {:points ['(pos 1) '(pos 3)], :color (colours :rel), :style "dashed"})
                          (into [:<>] (for [k (range 4)]
                                        [:<> (mafs/point {:x (list 'first (list 'pos k)) :y (list 'second (list 'pos k)) :color (colours :root)})
                                         (mafs/text (str "r" k) {:x (list 'first (list 'pos k)) :y (list 'second (list 'pos k))
                                                                 :attach "ne" :attach-distance 14})])))]
              [:div {:style {:flex "1 1 280px" :min-width "260px" :font-size "0.95em" :line-height 1.5}}
               [:div "permutation: " [:strong {:data-cycles true} '(get names p)]
                " (the label r" [:sub "k"] " now sits where r" [:sub "p(k)"] " was)"]
               [:div {:data-verdict true
                      :style {:font-weight 600 :margin "0.4em 0" :color (list 'if 'ok (colours :in) (colours :out))}}
                '(if ok "in the group of x⁴ − 2: both dashed pairs still sum to 0"
                     "not in the group: a dashed pair no longer sums to 0, a rational relation is broken")]
               [:div
                [:button {:type "button" :data-act "sigma" :style btn :on-click (list 'fn [] (list 'apply! q/sigma))} "σ: multiply by i"]
                [:button {:type "button" :data-act "tau" :style btn :on-click (list 'fn [] (list 'apply! q/tau))} "τ: conjugate"]
                [:button {:type "button" :data-act "reset" :style btn :on-click (list 'fn [] (list 'go! [0 1 2 3]))} "identity"]]
               [:div {:style {:margin-top "0.5em" :font-size "0.85em"}} "all 24 permutations of S₄ (green: in the group):"]
               (into [:div {:style {:display "flex" :flex-wrap "wrap" :gap "0.2em" :font-family "ui-monospace, monospace" :font-size "0.8em"}}]
                     (for [{:keys [perm cycles keeps?]} tbl]
                       [:button {:type "button" :data-perm (pr-str perm)
                                 :style {:padding "0.1em 0.4em" :border-radius "0.3em" :cursor "pointer"
                                         :border (str "1px solid " (colours (if keeps? :in :out)))
                                         :color (colours (if keeps? :in :out)) :background "transparent"}
                                 :on-click (list 'fn [] (list 'go! perm))}
                        cycles]))]]]))))

(defn group-table
  "The table of S_4 against the group of x^4 - 2 (raster on the JVM)."
  []
  (let [tbl (q/table)]
    (clerk/html
     [:div
      [:p {:style {:font-size "0.9em"}}
       (count (filter :keeps? tbl)) " of " (count tbl) " permutations keep both relations; the closure of σ and τ in S₄ has "
       (count (:ok (q/galois-group))) " elements."]
      [:table {:style {:font-family "ui-monospace, monospace" :font-size "0.8em"}}
       [:thead [:tr [:th "permutation"] [:th "|r_s(0) + r_s(2)|"] [:th "|r_s(1) + r_s(3)|"] [:th ""]]]
       (into [:tbody]
             (for [{:keys [cycles keeps? residuals]} tbl]
               [:tr [:td cycles] [:td (format "%.3g" (first residuals))] [:td (format "%.3g" (second residuals))]
                [:td {:style {:color (colours (if keeps? :in :out))}} (if keeps? "in the group" "not")]]))]])))

(defn chain-table
  "The chains S_4 > A_4 > V_4 > Z_2 > 1 and S_5 > A_5 > 1, each step with
   its orders, index, normality and whether the quotient is abelian, all
   computed on the permutations (alexandria.galois.solvable)."
  []
  (let [row (fn [names {:keys [orders index normal? abelian?]}]
              [:tr [:td names] [:td (str (first orders) " → " (second orders))] [:td index]
               [:td (if normal? "yes" "no")]
               [:td {:style {:color (colours (if (and abelian? (#{2 3 5} index)) :in :out))}}
                (if abelian? (str "abelian, order " index) (str "not abelian, order " index))]])
        s4 (s/chain-report 4 (s/chain-S4)) s5 (s/chain-report 5 (s/chain-S5))]
    (clerk/html
     [:table {:data-figure "galois-chains" :style {:font-family "ui-monospace, monospace" :font-size "0.85em"}}
      [:thead [:tr [:th "step"] [:th "orders"] [:th "index"] [:th "normal"] [:th "quotient"]]]
      (into [:tbody]
            (concat (map row ["S₄ ⊳ A₄" "A₄ ⊳ V₄" "V₄ ⊳ Z₂" "Z₂ ⊳ 1"] s4)
                    (map row ["S₅ ⊳ A₅" "A₅ ⊳ 1"] s5)))])))
