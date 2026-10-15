(ns alexandria.pascal.widgets
  "The widgets of the problem-of-points notebook (Pascal and Fermat 1654,
   Pascal's Traite du triangle arithmetique 1665, Huygens 1657),
   emmy-viewers fragments on the raster backend. One shared game state for
   sections 1-4 (a, b, s; `game`), so moving a or b in any of them moves
   all four; every number comes from a raster kernel of
   alexandria.pascal.points.

   `stake-tie`        section 1: a, b, s and the first player's share as
                      tied Leva controls (alexandria.medium.tie); the 2s on
                      the table as a bar split live.
   `fermat-table`     section 2: the 2^n outcomes of the n = a + b - 1
                      rounds as rows of a/b letters lit by the winner, one
                      outcome kernel call per row; count and fraction live.
   `pascal-grid`      section 3: the positions (i, j) in a grid, filled by
                      the mean kernel cell by cell (step, or all); a click
                      on a cell shows the two it depends on and the mean
                      with its numbers.
   `triangle-row`     section 4: the arithmetic triangle, the base of a + b
                      cells lit (last b for the first player, first a for
                      the second), and the three methods side by side with
                      an agree badge.
   `stepped-triangle` the definition of the triangle, one sentence of
                      Pascal per step.
   `huygens`          section 5: a, b, p, q and the price x tied (Prop.
                      III and its inverse)."
  (:require [alexandria.medium.flow :as flow]
            [alexandria.medium.tie :as tie]
            [alexandria.pascal.points :as pp]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [nextjournal.clerk :as clerk]))

(def colours {:first "#1F6FB4" :second "#C0392B" :undecided "#8A9BB0" :dep "#E39B2D" :done "#2E8B57"
              :ok "#2E8B57" :bad "#C0392B"})

(defn- button [attrs title on-click]
  [:button (merge {:type "button" :on-click on-click
                   :style {:margin "0.2em" :padding "0.3em 0.6em" :cursor "pointer" :border-radius "0.3em"
                           :border "1px solid currentColor" :background "transparent" :color "inherit"}}
                  attrs)
   title])

(defn- katex [tex-form attr]
  (list 'let ['tex tex-form] [:div {attr 'tex} [:f> 'nextjournal.clerk.render/render-katex 'tex {:inline? false}]]))

(def start {:a 2 :b 3 :s 32 :share 44})

(def game
  "Browser form of the page's one game atom ({:a :b :s :share}), shared by
   sections 1-4 (each is its own Clerk result, so the atom lives on
   globalThis): pass it as !g to every widget."
  (list 'or '(aget js/globalThis "__pointsGame")
        (list 'let ['at (list 'reagent.core/atom start)] '(aset js/globalThis "__pointsGame" at) 'at)))

(defn- kf
  "f of numbers as a kernel body: (fn [& params] (fn [state] (f params...)))."
  [f]
  (fn [& args] (fn [_] (apply f args))))

(defn- k [nm f init] (flow/kernel nm (kf f) init [0]))

(defn- ab-buttons
  "Buttons that set a and b in the shared game atom (browser symbol ?g)."
  [key]
  (into [:span {:style {:margin-right "0.6em"}} [:strong (str key " = ")]]
        (for [v (range 1 (inc pp/max-missing))]
          (button {(keyword (str "data-set-" key)) (str v)
                   :style {:margin "0.1em" :padding "0.1em 0.5em" :cursor "pointer" :border-radius "0.3em"
                           :border "1px solid currentColor" :color "inherit"
                           :background (list 'if (list '= v (list (keyword key) '@?g)) "#1F6FB433" "transparent")}}
                  (str v)
                  (list 'fn [] (list 'swap! '?g 'assoc (keyword key) v))))))

;; ---------------------------------------------------------------------------
;; 1. The stake, tied

(defn stake-tie
  "Section 1. !g the shared game atom (a browser form, `game`); it is also
   the Leva panel's atom, the exact state !x."
  [!g]
  (binding [vc/*backend* :raster]
    (ev/with-let [!x start]
      (let [init (mapv start pp/stake-keys)
            [tsym tform] (tie/kernel-binding "stake" (kf pp/stake-tie) init)
            [rsym rform] (k "readout" pp/stake-readout [2 3 32])
            opts {:tie-sym tsym :p !g :s !x :keys pp/stake-keys :ranges pp/stake-ranges :counter "__pointsSolves"}
            n (fn [i d] (tie/fmt (list 'aget 'r i) d))]
        (flow/template
         {'?g !g '?x !x '?r rsym}
         (list 'reagent.core/with-let (into [tsym tform rsym rform] (tie/locals opts))
               (list 'let '[g @?g
                            r (?r (array 0) (array (:a g) (:b g) (:s @?x)))
                            frac (aget r 0)
                            pct (* 100 frac)]
                     [:div {:data-figure "points-stake" :data-a '(str (:a g)) :data-b '(str (:b g))
                            :data-share '(str (aget r 1)) :data-frac '(str frac)}
                      [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                       [:div {:style {:flex "1 1 420px" :min-width "300px"}}
                        [:div {:style {:font-size "0.9em" :margin-bottom "0.3em"}}
                         "The " (n 3 0) " pistoles on the table (2s), split:"]
                        [:div {:data-bar true :style {:display "flex" :height "3.2em" :border-radius "0.4em" :overflow "hidden"
                                                      :font-family "ui-monospace, monospace" :color "white" :font-weight 600}}
                         [:div {:data-bar-first true
                                :style {:width '(str pct "%") :background (colours :first) :display "flex"
                                        :align-items "center" :justify-content "center" :transition "width 0.3s"}}
                          "first: " (n 1 2)]
                         [:div {:style {:flex "1" :background (colours :second) :display "flex"
                                        :align-items "center" :justify-content "center"}}
                          "second: " (n 2 2)]]
                        [:div {:style {:margin "0.6em 0"}} (ab-buttons "a") (ab-buttons "b")]
                        (katex (list 'str "E(" '(:a g) "," '(:b g) ") = \\frac{" (n 4 0) "}{" (n 4 0) " + " (n 5 0) "} = "
                                     (n 0 5) ",\\quad \\text{share} = 2s\\,E = 2 \\cdot " (tie/fmt '(:s @?x) 2) " \\cdot " (n 0 5)
                                     " = " (n 1 3))
                               :data-formula)
                        [:div {:data-case-name true :style {:font-weight 600 :font-size "1.05em" :margin "0.4em 0"}}
                         '(cond (= (:a g) (:b g)) "even game: equal shares, each takes back his own stake"
                                (< (:a g) (:b g)) (str "the first player is ahead (" (:a g) " points short against " (:b g) "): more than his stake")
                                :else (str "the first player is behind (" (:a g) " points short against " (:b g) "): less than his stake"))]]
                       [:div {:style {:flex "0 1 330px" :min-width "260px"}}
                        (leva/sub-panel {:fill true :flat true :title-bar false}
                                        (leva/controls {:atom !g :schema (tie/schema pp/stake-keys pp/stake-ranges pp/stake-labels)}))
                        [:div {:style {:font-size "0.85em" :opacity 0.8}}
                         "Move a, b or the stake s and the share is re-solved; type a share and the stake that gives it is solved back (s = share / 2E)."]
                        [:p [:strong "Try this: "]]
                        (button {:data-preset "pascal-1"} "Pascal's first case: 1 point against 2"
                                (list 'fn [] (list 'swap! !g 'assoc :a 1 :b 2 :s 32)))
                        (button {:data-preset "pascal-2"} "1 against 3: 56 of 64"
                                (list 'fn [] (list 'swap! !g 'assoc :a 1 :b 3 :s 32)))
                        (button {:data-preset "letter"} "2 against 3, the letter of 24 August"
                                (list 'fn [] (list 'swap! !g 'assoc :a 2 :b 3 :s 32)))
                        (button {:data-preset "traite"} "2 against 4, the Traité's example"
                                (list 'fn [] (list 'swap! !g 'assoc :a 2 :b 4 :s 32)))]]])
               (list 'finally (tie/unwatch-form !g))))))))

;; ---------------------------------------------------------------------------
;; 2. Fermat's enumeration

(defn fermat-table
  "Section 2: every outcome of the n = a + b - 1 rounds, lit by its winner."
  [!g]
  (binding [vc/*backend* :raster] (ev/with-let [!unused {}] (let [[osym oform] (k "outcome" pp/outcome [2 3 0])
          [csym cform] (k "count" pp/outcome-count [2 3])
          [ssym sform] (k "split" pp/split [11 5 32])] (flow/template {'?g !g '?o osym '?c csym '?sp ssym} (list 'reagent.core/with-let [osym oform csym cform ssym sform]
             (list 'let ['g '@?g
                         'c '(?c (array 0) (array (:a g) (:b g)))
                         'n '(js/Math.round (aget c 0))
                         'total '(js/Math.round (aget c 1))
                         'rows '(vec (for [kk (range total)]
                                       (let [o (?o (array 0) (array (:a g) (:b g) kk))]
                                         {:w (aget o 0) :wa (aget o 1)
                                          :letters (apply str (for [j (range n)] (if (> (aget o (+ 2 j)) 0.5) "a" "b")))})))
                         'fa '(reduce + (map :w rows))
                         'sp '(?sp (array 0) (array fa (- total fa) (:s g)))]
                   [:div {:data-figure "points-fermat" :data-favourable '(str fa) :data-total '(str total)
                          :data-frac '(str (aget sp 0))}
                    [:div {:style {:margin "0.3em 0"}} (ab-buttons "a") (ab-buttons "b")]
                    [:div {:style {:font-size "0.9em"}}
                     "The game is decided within n = a + b − 1 = " [:strong 'n] " rounds. Fermat lets all " 'n
                     " be played, whatever happens, and writes down every way they can fall: 2" [:sup 'n] " = " 'total
                     " outcomes, each as likely as any other. Blue: the first player wins (at least " '(:a g)
                     " a's); red: the second (at least " '(:b g) " b's)."]
                    [:div {:data-rows true
                           :style {:display "grid" :grid-template-columns "repeat(auto-fill, minmax(4.6em, 1fr))" :gap "0.25em"
                                   :font-family "ui-monospace, monospace" :margin "0.6em 0" :max-height "22em" :overflow-y "auto"}}
                     '(for [[kk {:keys [w letters]}] (map-indexed vector rows)]
                        ^{:key kk}
                        [:span {:data-outcome letters :data-winner (if (> w 0.5) "first" "second")
                                :style {:padding "0.15em 0.3em" :border-radius "0.3em" :text-align "center" :color "white"
                                        :background (if (> w 0.5) "#1F6FB4" "#C0392B")}}
                         letters])]
                    (katex (list 'str "\\frac{\\#\\{\\text{first wins}\\}}{2^{" 'n "}} = \\frac{" 'fa "}{" 'total "} = "
                                 (tie/fmt '(aget sp 0) 5) ",\\qquad \\text{split } " 'fa " : " '(- total fa)
                                 " = " (tie/fmt '(aget sp 1) 2) " : " (tie/fmt '(aget sp 2) 2) "\\text{ of } " '(* 2 (:s g)))
                           :data-formula)
                    [:div {:data-case-name true :style {:font-weight 600 :margin "0.3em 0"}}
                     '(str fa " outcomes of " total " for the first player, " (- total fa) " for the second: they share the stakes as "
                           fa " to " (- total fa))]])))))))

;; ---------------------------------------------------------------------------
;; 3. Pascal's recursion

(def grid-order "Pascal's order of the cells, [[i j] ...]." pp/cells)

(defn pascal-grid
  "Section 3: the grid of positions (i, j), i points the first lacks, j the
   second; the edges i = 0 (he has won: 1) and j = 0 (he has lost: 0);
   each inner cell the mean of the cell above (i - 1, j: he wins the next
   round) and to the left (i, j - 1: he loses it). Stepped in Pascal's
   order, one mean kernel call per filled cell."
  [!g]
  (binding [vc/*backend* :raster]
    (ev/with-let [!p {:step 0 :sel nil}]
      (let [[msym mform] (k "mean" pp/mean [1 0.5])
            [ssym sform] (k "split" pp/split [11 5 32])
            m pp/max-missing
            order (vec grid-order)]
        (flow/template
         {'?g !g '?p !p '?m msym '?sp ssym}
         (list 'reagent.core/with-let [msym mform ssym sform
                                       'order order
                                       'fill '(fn [nsteps]
                                                (reduce (fn [acc [i j]]
                                                          (let [E (fn [i j] (cond (zero? i) 1 (zero? j) 0 :else (get acc [i j])))
                                                                o (?m (array 0) (array (E (dec i) j) (E i (dec j))))]
                                                            (assoc acc [i j] (aget o 0))))
                                                        {} (take nsteps order)))]
               (list 'let ['g '@?g 'p '@?p
                           'step '(:step p)
                           'vals '(fill step)
                           'full '(fill (count order))
                           'target [(list :a 'g) (list :b 'g)]
                           'sel '(or (:sel p) (when (pos? step) (nth order (dec step))) target)
                           'E '(fn [vs i j] (cond (zero? i) 1 (zero? j) 0 :else (get vs [i j])))
                           'sv '(get full target)
                           'sp '(?sp (array 0) (array sv (- 1 sv) (:s g)))
                           '[si sj] 'sel
                           'u '(E full (dec si) sj)
                           'v '(E full si (dec sj))
                           'mo '(?m (array 0) (array u v))]
                     [:div {:data-figure "points-pascal" :data-step '(str step) :data-value '(str sv)
                            :data-frac '(str (aget sp 0)) :data-sel '(str si "," sj)}
                      [:div {:style {:margin "0.3em 0"}} (ab-buttons "a") (ab-buttons "b")]
                      [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1.2em" :align-items "flex-start"}}
                       [:table {:style {:border-collapse "collapse" :font-family "ui-monospace, monospace" :font-size "0.85em"}}
                        [:thead [:tr [:th {:style {:padding "0.2em 0.4em" :font-size "0.8em" :opacity 0.7}} "i \\ j"]
                                 (list 'for ['j (vec (range (inc m)))] '^{:key j} [:th {:style {:padding "0.2em 0.5em"}} j])]]
                        [:tbody
                         (list 'for ['i (vec (range (inc m)))]
                               (list 'with-meta
                                     [:tr [:th {:style {:padding "0.2em 0.5em"}} 'i]
                                      (list 'for ['j (vec (range (inc m)))]
                                            '(let [edge? (or (zero? i) (zero? j))
                                                   v (cond (and (zero? i) (zero? j)) nil (zero? i) 1 (zero? j) 0 :else (get vals [i j]))
                                                   dep? (or (= [i j] [(dec si) sj]) (= [i j] [si (dec sj)]))
                                                   cur? (= [i j] [si sj])
                                                   tgt? (= [i j] target)]
                                               ^{:key j}
                                               [:td {:data-cell (str i "," j) :data-cell-value (if (nil? v) "" (str v))
                                                     :on-click (fn [] (when (and (pos? i) (pos? j)) (swap! ?p assoc :sel [i j])))
                                                     :style {:padding "0.35em 0.5em" :text-align "center" :min-width "4.2em"
                                                             :cursor (if edge? "default" "pointer")
                                                             :border (cond cur? "2px solid #2E8B57" tgt? "2px solid #1F6FB4" :else "1px solid #8884")
                                                             :background (cond dep? "#E39B2D55" edge? "#8A9BB022" (some? v) "#2E8B5722" :else "transparent")}}
                                                (if (nil? v) "" (.toFixed v 4))]))]
                                     '{:key i}))]]
                       [:div {:style {:flex "1 1 300px" :min-width "260px" :font-size "0.92em" :line-height 1.5}}
                        [:div "step " [:strong 'step] " of " '(count order) ": "
                         '(if (zero? step) "only the edges are known" (let [[i j] (nth order (dec step))] (str "cell (" i ", " j ") filled")))]
                        [:div {:data-dependency true :style {:margin "0.4em 0"}}
                         "Position (" 'si ", " 'sj "): if the first player wins the next round he is at (" '(dec si) ", " 'sj
                         "), worth " (tie/fmt 'u 4) "; if he loses, at (" 'si ", " '(dec sj) "), worth " (tie/fmt 'v 4) "."]
                        (katex (list 'str "E(" 'si "," 'sj ") = \\tfrac12\\big(E(" '(dec si) "," 'sj ") + E(" 'si "," '(dec sj) ")\\big) = \\tfrac12("
                                     (tie/fmt 'u 4) " + " (tie/fmt 'v 4) ") = " (tie/fmt 'v 4) " + \\tfrac12(" (tie/fmt 'u 4) " - " (tie/fmt 'v 4) ") = "
                                     (tie/fmt '(aget mo 0) 5))
                               :data-formula)
                        [:div {:style {:font-size "0.85em" :opacity 0.8}}
                         "Pascal's way of saying it: what he gets even if he loses (" (tie/fmt 'v 4)
                         "), plus half of what is still in doubt (½ · " (tie/fmt '(* 2 (aget mo 1)) 4) ")."]
                        [:div {:data-case-name true :style {:font-weight 600 :margin "0.4em 0"}}
                         "Your game (" '(:a g) ", " '(:b g) "): E = " (tie/fmt 'sv 5) ", the first player takes "
                         (tie/fmt '(aget sp 1) 2) " of " '(* 2 (:s g))]
                        [:div
                         (button {:data-act "prev"} "◀ back" (list 'fn [] (list 'swap! '?p '(fn [p] (assoc p :step (max 0 (dec (:step p))) :sel nil)))))
                         (button {:data-act "next"} "next cell ▶" (list 'fn [] (list 'swap! '?p '(fn [p] (assoc p :step (min (count order) (inc (:step p))) :sel nil)))))
                         (button {:data-act "all"} "fill all" (list 'fn [] (list 'swap! '?p 'assoc :step '(count order) :sel nil)))
                         (button {:data-act "reset"} "edges only" (list 'fn [] (list 'reset! '?p {:step 0 :sel nil})))]
                        [:div {:style {:font-size "0.8em" :opacity 0.75}}
                         "Orange: the two positions the selected one depends on. Green border: the selected cell; blue: your game. Click any inner cell."]]]])))))))

;; ---------------------------------------------------------------------------
;; 4. The arithmetic triangle, and the three methods together

(defn triangle-row
  "Section 4: the triangle in Pascal's square layout (cell (i, j) =
   C(i + j, i)); the base of a + b cells lit; the three methods side by side."
  [!g]
  (binding [vc/*backend* :raster] (ev/with-let [!unused {}] (let [[csym cform] (k "cell" pp/triangle-cell [1 1])
          [tsym tform] (k "triangle" pp/triangle [2 3])
          [osym oform] (k "outcome" pp/outcome [2 3 0])
          [nsym nform] (k "count" pp/outcome-count [2 3])
          [msym mform] (k "mean" pp/mean [1 0.5])
          [ssym sform] (k "split" pp/split [11 5 32])
          m (inc pp/max-rounds)] (flow/template {'?g !g '?c csym '?t tsym '?o osym '?n nsym '?m msym '?sp ssym} (list 'reagent.core/with-let [csym cform tsym tform osym oform nsym nform msym mform ssym sform
                                     'order (vec grid-order)]
             (list 'let ['g '@?g 'a '(:a g) 'b '(:b g) 'n '(+ a b -1)
                         't '(?t (array 0) (array a b))
                         'sa '(aget t 0) 'sb '(aget t 1)
                         'tri '(/ sa (+ sa sb))
                         'cnt '(?n (array 0) (array a b))
                         'total '(js/Math.round (aget cnt 1))
                         'fa '(reduce + (for [kk (range total)] (aget (?o (array 0) (array a b kk)) 0)))
                         'fer '(/ fa total)
                         'pas '(get (reduce (fn [acc [i j]]
                                              (let [E (fn [i j] (cond (zero? i) 1 (zero? j) 0 :else (get acc [i j])))]
                                                (assoc acc [i j] (aget (?m (array 0) (array (E (dec i) j) (E i (dec j)))) 0))))
                                            {} order) [a b])
                         'agree '(and (< (js/Math.abs (- fer pas)) 1e-12) (< (js/Math.abs (- pas tri)) 1e-12))
                         'sp '(?sp (array 0) (array sa sb (:s g)))]
                   [:div {:data-figure "points-triangle" :data-fermat '(str fer) :data-pascal '(str pas)
                          :data-triangle '(str tri) :data-agree '(if agree "yes" "no")}
                    [:div {:style {:margin "0.3em 0"}} (ab-buttons "a") (ab-buttons "b")]
                    [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1.2em" :align-items "flex-start"}}
                     [:table {:style {:border-collapse "collapse" :font-family "ui-monospace, monospace" :font-size "0.82em"}}
                      [:tbody
                       (list 'for ['i (vec (range m))]
                             (list 'with-meta
                                   [:tr (list 'for ['j (vec (range m))]
                                              '(let [on-base (= (+ i j) n)
                                                     place j
                                                     first? (and on-base (>= place a))
                                                     second? (and on-base (< place a))
                                                     shown (<= (+ i j) (dec (max 6 (inc n))))
                                                     v (when shown (js/Math.round (?c (array 0) (array i j))))]
                                                 ^{:key j}
                                                 [:td {:data-tri-cell (str i "," j) :data-lit (cond first? "first" second? "second" :else "")
                                                       :style {:padding "0.25em 0.4em" :text-align "center" :min-width "2.4em"
                                                               :border (if shown "1px solid #8884" "none")
                                                               :color (if (or first? second?) "white" "inherit")
                                                               :background (cond first? "#1F6FB4" second? "#C0392B" :else "transparent")}}
                                                  (when shown (str v))]))]
                                   '{:key i}))]]
                     [:div {:style {:flex "1 1 300px" :min-width "260px" :font-size "0.92em" :line-height 1.5}}
                      [:div "The base of " [:strong '(+ a b)] " cells (a + b, the points the two lack together) runs diagonally. "
                       "Pascal gives the first player the cells counted from the far end, as many as the second lacks (" 'b
                       ", blue), and the second as many as the first lacks (" 'a ", red)."]
                      (katex (list 'str "\\text{blue} = " '(js/Math.round sa) ",\\ \\text{red} = " '(js/Math.round sb)
                                   ",\\quad E = \\frac{" '(js/Math.round sa) "}{" '(js/Math.round (+ sa sb)) "} = " (tie/fmt 'tri 5))
                             :data-formula)
                      [:table {:data-three true :style {:font-family "ui-monospace, monospace" :font-size "0.9em" :margin "0.4em 0"}}
                       [:tbody
                        [:tr [:td "Fermat, counting outcomes"] [:td {:data-method "fermat" :style {:padding-left "1em"}} (tie/fmt 'fer 6)]]
                        [:tr [:td "Pascal, the recursion"] [:td {:data-method "pascal" :style {:padding-left "1em"}} (tie/fmt 'pas 6)]]
                        [:tr [:td "Pascal, the triangle"] [:td {:data-method "triangle" :style {:padding-left "1em"}} (tie/fmt 'tri 6)]]]]
                      [:div {:data-invariant '(if agree "pass" "fail")
                             :style {:display "inline-block" :padding "0.15em 0.6em" :border-radius "0.6em" :color "white"
                                     :background '(if agree "#2E8B57" "#C0392B")}}
                       '(if agree "✓ the three methods give the same split" "✗ the methods disagree")]
                      [:div {:data-case-name true :style {:font-weight 600 :margin "0.4em 0"}}
                       "The first player takes " (tie/fmt '(aget sp 1) 2) ", the second " (tie/fmt '(aget sp 2) 2) " of " '(* 2 (:s g))]]]])))))))

;; ---------------------------------------------------------------------------
;; The triangle's definition, stepped

(def triangle-steps
  [{:upto 0 :fr "Le nombre de la première cellule qui est à l'angle droit est arbitraire; mais celui-là étant placé, tous les autres sont forcés, et pour cette raison il s'appelle le générateur du triangle."
    :en "The number of the first cell, at the right angle, is arbitrary; but once it is placed all the others are forced, and for this reason it is called the generator of the triangle."}
   {:upto 1 :fr "Et chacun des autres est spécifié par cette seule règle."
    :en "And each of the others is specified by this single rule."}
   {:upto 2 :fr "Le nombre de chaque cellule est égal à celui de la cellule qui la précède dans son rang perpendiculaire, plus à celui de la cellule qui la précède dans son rang parallèle."
    :en "The number of each cell equals that of the cell before it in its perpendicular rank, plus that of the cell before it in its parallel rank."}
   {:upto 4 :fr "Ainsi la cellule F, c'est-à-dire le nombre de la cellule F, égale la cellule C plus la cellule E, et ainsi des autres."
    :en "Thus the cell F, that is the number of the cell F, equals the cell C plus the cell E, and so for the others."}
   {:upto 6 :fr "Et celles qu'une même base traverse diagonalement sont dites cellules d'une même base."
    :en "And the cells that one base crosses diagonally are called cells of one base."}])

(defn stepped-triangle
  "Pascal's definition, one sentence per step; the triangle fills base by
   base (cells from the triangle-cell kernel)."
  []
  (binding [vc/*backend* :raster]
    (ev/with-let [!s {:step 0}]
      (let [[csym cform] (k "cell" pp/triangle-cell [1 1])
            steps triangle-steps
            n 7]
        (flow/template
         {'?s !s '?c csym}
         (list 'reagent.core/with-let [csym cform 'steps steps]
               (list 'let ['i '(:step @?s) 'st '(nth steps i) 'upto '(:upto st)]
                     [:div {:data-figure "triangle-steps" :data-step '(str i)}
                      [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1.2em" :align-items "flex-start"}}
                       [:table {:style {:border-collapse "collapse" :font-family "ui-monospace, monospace"}}
                        [:tbody
                         (list 'for ['r (vec (range n))]
                               (list 'with-meta
                                     [:tr (list 'for ['c (vec (range n))]
                                                '(let [b (+ r c)
                                                       shown (<= b upto)
                                                       hl (and (= i 3) (or (= [r c] [1 1]) (= [r c] [0 1]) (= [r c] [1 0])))
                                                       base? (and (= i 4) (= b 5))]
                                                   ^{:key c}
                                                   [:td {:style {:padding "0.3em 0.5em" :min-width "2.2em" :text-align "center"
                                                                 :border (if (< b 7) "1px solid #8884" "none")
                                                                 :background (cond hl "#E39B2D55" base? "#1F6FB433" :else "transparent")}}
                                                    (when shown (str (js/Math.round (?c (array 0) (array r c)))))]))]
                                     '{:key r}))]]
                       [:div {:style {:flex "1 1 320px" :min-width "260px" :line-height 1.5}}
                        [:div {:style {:opacity 0.7 :font-size "0.85em"}} "step " '(inc i) " of " (count steps)]
                        [:blockquote {:data-step-text true :style {:font-style "italic" :margin "0.4em 0"}} '(:fr st)]
                        [:div {:style {:font-size "0.9em"}} [:span {:style {:opacity 0.7}} "Translation: "] '(:en st)]
                        [:div {:style {:margin-top "0.4em"}}
                         (button {:data-act "prev"} "◀" (list 'fn [] (list 'swap! '?s 'update :step '(fn [j] (max 0 (dec j))))))
                         (button {:data-act "next"} "▶" (list 'fn [] (list 'swap! '?s 'update :step (list 'fn '[j] (list 'min (dec (count steps)) '(inc j))))))]
                        [:div {:style {:font-size "0.8em" :opacity 0.7}}
                         "Here the generator is 1; in step 4 the cell of the second row and column (Pascal's F) is the sum of the cell above it and the cell before it; in step 5 one base is shaded."]]]])))))))

;; ---------------------------------------------------------------------------
;; 5. Huygens

(def huygens-start {:a 3 :b 7 :p 1 :q 1 :x 5})

(defn- huygens-preset
  "Browser form: a preset as one edit per key, 40 ms apart, so the tie
   watch (one source key per edit, solved on the next tick) re-solves the
   exact state after each; the last key is a prize, so the price is solved
   from all the preset's values."
  [!p kvs]
  (list* 'fn []
         (map-indexed (fn [i [kk v]] (list 'js/setTimeout (list 'fn [] (list 'swap! !p 'assoc kk v)) (* 40 i))) kvs)))

(defn huygens
  "Section 5: Huygens' Prop. III tied both ways."
  ([] (huygens huygens-start))
  ([start]
   (binding [vc/*backend* :raster]
     (ev/with-let [!p start !s start]
       (let [init (mapv start pp/huygens-keys)
             [tsym tform] (tie/kernel-binding "huygens" (kf pp/huygens-blocks) init)
             opts {:tie-sym tsym :p !p :s !s :keys pp/huygens-keys :ranges pp/huygens-ranges :counter "__huygensSolves"}
             f (fn [kk d] (tie/fmt (list 'get '@?s kk) d))]
         (flow/template
          {'?s !s}
          (list 'reagent.core/with-let (into [tsym tform] (tie/locals opts))
                (list 'let '[s @?s
                             frac (/ (:p s) (+ (:p s) (:q s)))]
                      [:div {:data-figure "huygens" :data-x '(str (:x s)) :data-q '(str (:q s))}
                       [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                        [:div {:style {:flex "1 1 400px" :min-width "300px"}}
                         [:div {:style {:font-size "0.9em" :margin-bottom "0.3em"}}
                          "The chances: p cases give a, q cases give b, all equally likely."]
                         [:div {:style {:display "flex" :height "2.6em" :border-radius "0.4em" :overflow "hidden" :color "white"
                                        :font-family "ui-monospace, monospace"}}
                          [:div {:style {:width '(str (* 100 frac) "%") :background (colours :first) :display "flex"
                                         :align-items "center" :justify-content "center"}} "p → " (f :a 0)]
                          [:div {:style {:flex "1" :background (colours :second) :display "flex"
                                         :align-items "center" :justify-content "center"}} "q → " (f :b 0)]]
                         (katex (list 'str "x = \\frac{pa + qb}{p + q} = \\frac{" (f :p 2) "\\cdot" (f :a 0) " + " (f :q 2) "\\cdot" (f :b 0)
                                      "}{" (f :p 2) " + " (f :q 2) "} = " (f :x 3))
                                :data-formula)
                         [:div {:data-case-name true :style {:font-weight 600 :font-size "1.05em" :margin "0.4em 0"}}
                          '(cond (< (js/Math.abs (- (:p s) (:q s))) 1e-9) (str "equal chances (Prop. I): the price is the midpoint, " (.toFixed (:x s) 2))
                                 (> (:p s) (:q s)) "a is the likelier prize: the price leans toward a"
                                 :else "b is the likelier prize: the price leans toward b")]
                         [:div {:style {:font-size "0.85em" :opacity 0.8}}
                          "Huygens' test of a fair price: holding x, I can play a fair game against other players that gives me exactly these chances of a and of b."]]
                        [:div {:style {:flex "0 1 330px" :min-width "260px"}}
                         (leva/sub-panel {:fill true :flat true :title-bar false}
                                         (leva/controls {:atom !p :schema (tie/schema pp/huygens-keys pp/huygens-ranges pp/huygens-labels)}))
                         [:div {:style {:font-size "0.85em" :opacity 0.8}}
                          "Move a prize or the chances and the price follows (Prop. III); type a price and the chances q that make it fair are solved back (q = p (a − x)/(x − b))."]
                         [:p [:strong "Try this: "]]
                         (button {:data-preset "prop1"} "Prop. I: 3 or 7 shillings, even chance"
                                 (huygens-preset !p [[:b 7] [:p 1] [:q 1] [:a 3]]))
                         (button {:data-preset "points"} "The problem of points: 64 in 11 cases, 0 in 5"
                                 (huygens-preset !p [[:b 0] [:p 11] [:q 5] [:a 64]]))
                         (button {:data-preset "prop3"} "Prop. III: 3 cases of 13, 2 of 8"
                                 (huygens-preset !p [[:b 8] [:p 3] [:q 2] [:a 13]]))]]])
                (list 'finally (tie/unwatch-form !p)))))))))

;; ---------------------------------------------------------------------------
;; Derivations for the page

(defn derivation
  "Pascal's rule and its solutions for a = 1 and a = 2, by Emmy."
  []
  (clerk/col
   (clerk/tex "E(a,b) = \\tfrac12\\big(E(a-1,b) + E(a,b-1)\\big),\\qquad E(0,b) = 1,\\ E(a,0) = 0")
   (clerk/tex (str "\\text{Emmy, Corollaire premier: } A + \\tfrac{B}{2} - \\tfrac12\\big(A + (A+B)\\big) = "
                   (e/->TeX (e/simplify (pp/lemma-difference)))))
   (clerk/tex (str "E(1,b) = " (e/->TeX (e/simplify (pp/E1 'b (e/expt 2 (e/- 'b))))) ":\\quad E(1,b) - \\tfrac12\\big(1 + E(1,b-1)\\big) = "
                   (e/->TeX (e/simplify (pp/recursion-difference-1)))))
   (clerk/tex (str "E(2,b) = " (e/->TeX (e/simplify (pp/E2 'b (e/expt 2 (e/- 'b))))) ":\\quad E(2,b) - \\tfrac12\\big(E(1,b) + E(2,b-1)\\big) = "
                   (e/->TeX (e/simplify (pp/recursion-difference-2)))))))

(defn derivation-huygens
  "Prop. I and III with their inverse, by Emmy."
  []
  (let [[x res] (pp/huygens-prop1)]
    (clerk/col
     (clerk/tex (str "\\text{Prop. I: } 2x - a = b \\;\\Rightarrow\\; x = " (e/->TeX (e/simplify x))
                     ",\\quad 2x - a - b = " (e/->TeX (e/simplify res))))
     (clerk/tex (str "\\text{Prop. III: } x = " (e/->TeX (pp/expectation 'a 'b 'p 'q))
                     ",\\quad q = " (e/->TeX (pp/chances-for-price 'a 'b 'p 'x))))
     (clerk/tex (str "q\\big(x(p,q)\\big) - q = " (e/->TeX (e/simplify (pp/huygens-round-trip)))))
     (clerk/tex (str "\\text{points, } a = 2,\\ b = 3:\\quad \\frac{11\\cdot 2s + 5 \\cdot 0}{16} - \\frac{11 s}{8} = "
                     (e/->TeX (e/simplify (pp/huygens-points))))))))

(defn agreement-table
  "All games in range, the three methods by raster on the JVM."
  []
  (let [agree @pp/agreement-cache
        m pp/max-missing]
    (clerk/html
     [:table {:data-table "points-agreement" :style {:font-family "ui-monospace, monospace" :font-size "0.8em" :border-collapse "collapse"}}
      (into [:thead [:tr [:th "a \\ b"]]] (for [b (range 1 (inc m))] [:th {:style {:padding "0 0.6em"}} b]))
      (into [:tbody]
            (for [a (range 1 (inc m))]
              (into [:tr [:th a]]
                    (for [b (range 1 (inc m))
                          :let [[f p t] (agree [a b]) r (rationalize p)]]
                      [:td {:style {:padding "0.2em 0.6em" :text-align "center"
                                    :color (if (== f p t) (colours :ok) (colours :bad))}}
                       (str (numerator r) "/" (denominator r))]))))])))
