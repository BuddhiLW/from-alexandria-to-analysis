(ns alexandria.boole.widgets
  "The widgets of the Boole notebook (1854).

     syllogism-widget   choose the figure and the forms (A, E, I, O) of two
                        premises; the premises are shown as Boole's
                        equations and in words; for two universals the
                        middle term is eliminated (f(1) f(0) = 0), the
                        residue developed in the constituents of s and p
                        and read as classes that do not exist; the 2^3 truth
                        table verifies the conclusion; a button checks all
                        256 moods, flagging those valid only with existential
                        import.
     image of the data  `syllogism-data` is the whole table of 64 premise
                        pairs, computed on the JVM from alexandria.boole.logic
                        (alexandria.structure.boolring); the browser only
                        draws and switches."
  (:require [alexandria.boole.logic :as l]
            [nextjournal.clerk :as clerk]))

(defn- pair-row [figure f1 f2]
  (let [ms (mapv #(l/mood figure f1 f2 %) l/forms)
        el (l/eliminate-middle figure f1 f2)
        uni (first (filter #(and (#{:A :E} (-> % :conclusion :form)) (:valid? %)) ms))
        [[a1 b1] [a2 b2]] (l/premise-terms figure)]
    {:figure figure :f1 (name f1) :f2 (name f2)
     :premises [{:eq (l/equation-text f1 a1 b1) :words (l/words f1 a1 b1)}
                {:eq (l/equation-text f2 a2 b2) :words (l/words f2 a2 b2)}]
     :eliminated (when el {:empty (:empty el) :conclusions (:conclusions el)
                           :residual (mapv #(update % :coef (fn [c] (if (seq c) 1 0))) (:residual el))})
     :moods (mapv (fn [m] {:form (name (-> m :conclusion :form)) :words (-> m :conclusion :words)
                           :valid (:valid? m) :import (:import? m)}) ms)
     :table (when uni
              (mapv #(select-keys % [:bits :premise-1 :premise-2 :conclusion :ok?])
                    (l/truth-table figure f1 f2 (-> uni :conclusion :form))))
     :table-conclusion (when uni (-> uni :conclusion :words))}))

(defn syllogism-data
  "{:pairs [row ...] :tally {...} :moods [{name valid import} ...]}: 64 rows,
   one per figure and pair of premise forms, and the 256 moods."
  []
  (let [ms (l/all-moods)]
    {:pairs (vec (for [fig [1 2 3 4] f1 l/forms f2 l/forms] (pair-row fig f1 f2)))
     :tally (l/tally ms)
     :moods (mapv #(select-keys % [:name :figure :valid? :import?]) ms)}))

(defn finite-syllogism-data?
  "64 pairs, each with two premises and four mood verdicts; the 256 moods
   tally to 15 valid, 9 with import; every table has 8 rows."
  [{:keys [pairs tally moods]}]
  (boolean
   (and (= 64 (count pairs)) (= 256 (count moods))
        (every? #(and (= 2 (count (:premises %))) (= 4 (count (:moods %)))) pairs)
        (every? #(or (nil? (:table %)) (= 8 (count (:table %)))) pairs)
        (= 256 (reduce + (vals tally))))))

(def ^:private syllogism-render
  '(fn [{:keys [pairs tally moods]}]
     (reagent.core/with-let [fig (reagent.core/atom 1) f1 (reagent.core/atom "A") f2 (reagent.core/atom "A") all? (reagent.core/atom false)]
       (let [r (first (filter #(and (= @fig (:figure %)) (= @f1 (:f1 %)) (= @f2 (:f2 %))) pairs))
             el (:eliminated r)
             btn (fn [label act on-click on?]
                   [:button {:type "button" :data-act act :on-click on-click
                             :style {:margin "0.15em" :padding "0.25em 0.55em" :border-radius "0.3em" :cursor "pointer"
                                     :border "1px solid currentColor" :color "inherit"
                                     :background (if on? "#1F6FB433" "transparent")}} label])
             cells [["11" "s p" [1 1]] ["10" "s(1 - p)" [1 0]] ["01" "(1 - s)p" [0 1]] ["00" "(1 - s)(1 - p)" [0 0]]]
             empty? (set (map vec (:empty el)))
             good (filter :valid (:moods r))
             imp (filter :import (:moods r))]
         [:div {:data-figure "boole-syllogism" :data-fig (str @fig) :data-mood (str @f1 @f2)
                :data-eliminated (str (boolean el)) :style {:margin "1em 0"}}
          [:div "figure " (for [x [1 2 3 4]] ^{:key x} (btn (str x) (str "fig-" x) #(reset! fig x) (= x @fig)))
           "  major " (for [x ["A" "E" "I" "O"]] ^{:key x} (btn x (str "f1-" x) #(reset! f1 x) (= x @f1)))
           "  minor " (for [x ["A" "E" "I" "O"]] ^{:key x} (btn x (str "f2-" x) #(reset! f2 x) (= x @f2)))]
          [:div {:style {:margin "0.5em 0" :line-height 1.6}}
           (for [[i p] (map-indexed vector (:premises r))]
             ^{:key i} [:div (str (if (zero? i) "major: " "minor: ") (:words p) "   ")
                        [:code {:data-premise (str i)} (:eq p)]])
           [:div "s = minor term, p = major term, m = middle term"]]
          (if el
            [:div
             [:div {:data-conclusion true :style {:font-weight 600}}
              (if (seq (:conclusions el))
                (str "eliminating m: " (clojure.string/join "; " (:conclusions el)))
                "eliminating m: nothing follows about s and p")]
             [:svg {:width 330 :height 130 :viewBox "0 0 330 130" :style {:max-width "100%"}}
              (for [[i [k lab bits]] (map-indexed vector cells)]
                ^{:key k} [:g {:data-region k :data-empty (str (boolean (empty? bits)))}
                           [:rect {:x (+ 5 (* 160 (mod i 2))) :y (+ 5 (* 60 (quot i 2))) :width 155 :height 55
                                   :fill (if (empty? bits) "#8A9BB0" "#1F6FB4") :fill-opacity (if (empty? bits) 0.55 0.2)
                                   :stroke "#223"}]
                           [:text {:x (+ 12 (* 160 (mod i 2))) :y (+ 38 (* 60 (quot i 2))) :font-size 13 :fill "currentColor"}
                            (str lab (when (empty? bits) " = 0: no such objects"))]])]]
            [:div {:data-conclusion true :style {:font-weight 600}}
             "a particular premise is not an equation: tested against models instead"])
          [:div "conclusions that follow in this figure: "
           [:strong {:data-valid true} (if (seq good) (clojure.string/join "; " (map :words good)) "none")]
           (when (seq imp)
             [:span {:data-import true :style {:color "#C0392B"}}
              (str "   (only if the classes are not empty: " (clojure.string/join "; " (map :words imp)) ")")])]
          (when (:table r)
            [:table {:data-table true :style {:margin "0.5em 0" :border-collapse "collapse"}}
             [:caption (str "truth table, 2^3 assignments; conclusion: " (:table-conclusion r))]
             [:thead [:tr (for [h ["s" "p" "m" "premise 1" "premise 2" "conclusion" "ok"]] ^{:key h} [:th {:style {:padding "0 0.6em"}} h])]]
             [:tbody (for [[i row] (map-indexed vector (:table r))]
                       ^{:key i} [:tr {:data-row-ok (str (:ok? row))}
                                  (for [[j x] (map-indexed vector (concat (:bits row) [(:premise-1 row) (:premise-2 row) (:conclusion row) (if (:ok? row) "\u2713" "\u2717")]))]
                                    ^{:key j} [:td {:style {:padding "0 0.6em" :text-align "center"}} (str x)])])]])
          [:div (btn (if @all? "hide the 256 moods" "check all 256 moods") "all-moods" #(swap! all? not) @all?)]
          (when @all?
            [:div {:data-all-moods true :data-valid-count (str (:valid tally)) :data-import-count (str (:import tally))}
             (str (:valid tally) " moods are valid outright, " (:import tally) " only with existential import, "
                  (:invalid tally) " are invalid. Aristotle's 24 valid moods are the first two kinds together.")
             [:div (for [fg [1 2 3 4]]
                     ^{:key fg} [:div (str "figure " fg ": ")
                                 (clojure.string/join " " (map (fn [m] (str (:name m) (when-not (:valid? m) "*")))
                                                               (filter #(and (= fg (:figure %)) (or (:valid? %) (:import? %))) moods)))])
              [:div "* valid only if the classes are not empty"]]])]))))

(defn syllogism-widget
  "Premises as Boole's equations, elimination of the middle term, truth table, all moods."
  []
  (let [d (syllogism-data)]
    (assert (finite-syllogism-data? d) "the syllogism data are incomplete")
    (clerk/with-viewer {:render-fn syllogism-render :transform-fn clerk/mark-presented} d)))
