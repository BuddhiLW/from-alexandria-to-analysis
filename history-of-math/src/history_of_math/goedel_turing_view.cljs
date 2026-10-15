(ns history-of-math.goedel-turing-view
  (:require [clojure.string :as str]
            [history-of-math.goedel-turing :as gt]
            [reagent.core :as r]))

(defn- value [event] (.. event -target -value))

(defn numbering []
  (r/with-let [formula (r/atom "x+1")]
    (let [result (gt/numbering @formula)]
      [:section {:class "goedel-numbering"}
       [:label "Enter one to four signs (0, 1, x, +, =): "
        [:input {:type "text" :value @formula :max-length 4
                 :on-change (fn [event] (reset! formula (value event)))}]]
       (if result
         [:div
          [:p "Prime powers: " (str/join " · "
                                         (map (fn [[p e]] (str p "^" e)) (:factors result)))]
          [:p "Encoded integer: " (:number result)]
          [:p "Decoded by division: " (:decoded result)]
          [:p "Round-trip: " (if (= @formula (:decoded result)) "yes" "no")]]
         [:p "Use only the five signs above, with no spaces."])])))

(defn- bits-of-four? [s] (and (= 4 (count s)) (every? #{\0 \1} s)))

(defn diagonal []
  (r/with-let [rows (r/atom ["0011" "1010" "1100" "0101"])]
    (let [bits @rows
          opposite (apply str (map-indexed
                               (fn [i row] (if (= \0 (nth row i)) \1 \0)) bits))]
      [:section
       [:p "Edit each four-bit row. The new sequence disagrees with row i at position i."]
       [:table [:thead [:tr [:th "i"] [:th "Sequence i"] [:th "Diagonal bit"] [:th "Opposite bit"]]]
        [:tbody (for [i (range 4)]
                  ^{:key i}
                  [:tr [:td (inc i)]
                   [:td [:input {:value (nth bits i) :max-length 4
                                 :on-change (fn [event]
                                              (let [s (value event)]
                                                (when (bits-of-four? s)
                                                  (swap! rows assoc i s))))}]]
                   [:td (nth (nth bits i) i)]
                   [:td (nth opposite i)]])]]
       [:p "Anti-diagonal: " [:strong opposite]
        ". No row in this finite list is equal to it."]])))

(defn- binary? [s] (and (seq s) (every? #{\0 \1} s)))

(defn turing-machine []
  (r/with-let [input (r/atom "1011")
               m (r/atom (gt/machine "1011"))]
    (let [{:keys [tape head state steps] :as cur} @m
          ks (keys tape)
          cells (range (dec (apply min head ks)) (+ 2 (apply max head ks)))]
      [:section {:class "turing-machine"}
       [:label "Binary input: "
        [:input {:value @input :max-length 12
                 :on-change (fn [event]
                              (let [s (value event)]
                                (reset! input s)
                                (when (binary? s) (reset! m (gt/machine s)))))}]]
       [:table [:tbody
                [:tr (for [i cells]
                       ^{:key i}
                       [:td {:style {:border "1px solid #888" :width "1.6em"
                                     :text-align "center"
                                     :background (when (= i head) "#ffe08a")}}
                        (str (get tape i "_"))])]
                [:tr (for [i cells]
                       ^{:key i} [:td {:style {:text-align "center"}}
                                  (when (= i head) "▲")])]]]
       [:p "State: " [:strong (name state)] ". Steps: " steps
        ". Tape: " (gt/tape-string cur)]
       [:button {:on-click (fn [_] (swap! m gt/step)) :disabled (gt/halted? cur)} "Step"]
       " "
       [:button {:on-click (fn [_] (swap! m gt/run)) :disabled (gt/halted? cur)} "Run to halt"]
       " "
       [:button {:on-click (fn [_] (when (binary? @input) (reset! m (gt/machine @input))))}
        "Reset"]])))

(defn halting []
  (let [label {:halts "halts" :loops "loops"}]
    [:section
     [:table
      [:thead [:tr [:th "Program"]
               (for [j (range 4)] ^{:key j} [:th (str "on P" (inc j))])
               [:th "D on itself"]]]
      [:tbody
       (for [[i {:keys [program row d-on-self]}] (map-indexed vector (gt/halting-rows))]
         ^{:key program}
         [:tr [:td program]
          (for [j (range 4)]
            ^{:key j}
            [:td {:style (when (= i j) {:font-weight "bold" :background "#ffe08a"})}
             (label (nth row j))])
          [:td (label d-on-self)]])]]
     [:p "Bold cells are H's verdicts on the diagonal. D does the opposite of each,"
      " so D differs from every listed program on that program's own description."]]))
