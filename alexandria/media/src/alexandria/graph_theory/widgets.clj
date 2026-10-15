(ns alexandria.graph-theory.widgets
  "The graph-theory notebook's widgets (widget brief sections 4 and 5).
   Every one mounts a special case of alexandria.structure.graph; the
   numbers come from raster kernels compiled from its readouts.

     multigraph   E53's four regions, draggable (the euler playground's
                  drawing, alexandria.euler.playgrounds/kb-figure), bridges
                  added and removed per pair. mode :euler names Euler's
                  rule live (structure.graph/euler-cases through
                  structure.tie/case-form) and animates the witness walk;
                  mode :kirchhoff shows the reduced Laplacian, its
                  determinant (the tree count), E - V + 1, and lists the
                  spanning trees, each drawable.
     cayley       n <-> E <-> cycle rank of K_n tied (structure.tie
                  :pivot through medium.tie's watch), Kirchhoff's
                  determinant against n^(n-2), and the k-th labelled tree
                  from its Pruefer code.
     icosian      the dodecahedron: click a path through the twenty
                  vertices, named live (path, Hamiltonian path, Hamilton's
                  cycle, stuck), against Euler's question on the same graph.
     graph-steps  a stepped figure: one sentence per step beside the graph
                  it changes (Euler's notation, Kempe's chain swap).
     colour-map   Kempe's map coloured by clicking; clashes counted.
     derivation   the graded claims with their TeX."
  (:require [alexandria.euler.playgrounds :as kb]
            [alexandria.graph-theory.bridges :as b]
            [alexandria.medium.flow :as flow]
            [alexandria.medium.tie :as mtie]
            [alexandria.structure.graph :as gr]
            [alexandria.structure.tie :as tie]
            [clojure.string :as str]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [nextjournal.clerk :as clerk]))

(def ^:private pair-keys [:AB :AC :AD :BC :BD :CD])

(defn- katex [tex-form attr]
  (list 'let ['tex tex-form] [:div {attr 'tex} [:f> 'nextjournal.clerk.render/render-katex 'tex {:inline? false}]]))

(defn- btn [attrs label on-click]
  [:button (merge {:type "button" :on-click on-click
                   :style {:margin "0.15em" :padding "0.2em 0.55em" :cursor "pointer" :border-radius "0.3em"
                           :border "1px solid currentColor" :background "transparent" :color "inherit"}}
                  attrs)
   label])

(defn- frame [id figure controls]
  [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
   [:style (str "[data-widget=\"" id "\"] .MafsView text { pointer-events: none; }")]
   [:div {:style {:flex "1 1 460px" :min-width "300px"}} figure]
   [:div {:style {:flex "1 1 320px" :min-width "260px" :font-size "0.92em" :line-height 1.55}} controls]])

;; ---------------------------------------------------------------------------
;; 1-2. The editable multigraph: Euler and Kirchhoff

(def ^:private edges-form
  "Browser form: the edges [[u v [p j]] ...] of the counts cs, in Euler's
   lettering order."
  (list 'vec (list 'for ['p [0 1 2 4 5 3] 'j '(range (nth cs p))]
                   (list 'let ['[u v] (list 'nth (gr/pairs 4) 'p)] '[u v [p j]]))))

(defn multigraph
  "The four regions and their bridges, editable; mode :euler or :kirchhoff."
  [mode]
  (binding [vc/*backend* :raster]
    (let [pk [:ax :ay :bx :by :cx :cy :dx :dy]
          init (zipmap pair-keys b/counts-init)
          [dsym dform] (mtie/kernel-binding "kbdraw" kb/kb-values (vec (concat (map kb/kb-places pk) (map double b/counts-init))))
          [qsym qform] (mtie/kernel-binding "graph" (tie/readout-kernel b/readout) (mapv double b/counts-init))
          ix (tie/index b/readout)
          q (fn [k] (list 'aget 'q (ix k)))
          case-form (tie/case-form b/bridge-cases q)
          letters "abcdefghijklmnopqrstuvwx"
          id (str "multigraph-" (name mode))]
      (ev/with-let [!s init !p kb/kb-places !w {:k 0 :tree 0}]
        (flow/template
         {'?s !s '?p !p '?w !w '?d dsym '?q qsym}
         (list 'reagent.core/with-let [dsym dform qsym qform
                                       'timer (when (= mode :euler) '(js/setInterval (fn [] (swap! ?w update :k inc)) 700))
                                       'hierholzer b/walk-form 'spanning b/trees-form]
               (list 'let ['cs (vec (for [k pair-keys] (list 'js/Math.round (list 'get '@?s k))))
                           'r (list '?d '(array 0) (list* 'array (concat (for [k pk] (list 'get '@?p k)) (for [i (range 6)] (list 'nth 'cs i)))))
                           'q '(?q (array 0) (apply array cs))
                           'case-name case-form
                           'edges edges-form
                           'labels (list 'into {} (list 'map-indexed (list 'fn '[i [_ _ pj]] ['pj (list 'subs letters 'i '(inc i))]) 'edges))
                           'deg (vec (for [i (range 4)] (list 'js/Math.round (q (keyword "deg" (str i))))))
                           'start '(or (first (filter (fn [i] (odd? (nth deg i))) (range 4)))
                                       (first (filter (fn [i] (pos? (nth deg i))) (range 4))))
                           'walk '(if (contains? #{"circuit" "path"} case-name) (hierholzer edges start) [])
                           'crossings '(vec (keep (fn [[_ e]] (when e (nth (nth edges e) 2))) walk))
                           'kk '(mod (get @?w :k) (+ 3 (count crossings)))
                           'record '(apply str (map (fn [[v e]] (str (when e (get labels (nth (nth edges e) 2))) (nth ["A" "B" "C" "D"] v))) walk))
                           'trees (if (= mode :kirchhoff) '(spanning edges 4 200) [])
                           'tree-ix '(min (get @?w :tree) (max 0 (dec (count trees))))
                           'order (if (= mode :euler)
                                    '(into {} (map-indexed (fn [n t] [t (inc n)]) (take kk crossings)))
                                    {})
                           'walked (if (= mode :euler)
                                     '(set (take kk crossings))
                                     '(set (map (fn [i] (nth (nth edges i) 2)) (get trees tree-ix []))))]
                     [:div {:data-widget id :data-case 'case-name :data-odd (list 'str (q :odd)) :data-bridges (list 'str (q :E))
                            :data-trees (list 'str (q :trees)) :data-rank (list 'str (q :rank)) :data-walk 'record
                            :data-listed '(str (count trees))}
                      (frame id
                             [:div (kb/kb-figure !p)
                              [:div {:style {:font-size "0.85em" :opacity 0.75}}
                               (if (= mode :euler)
                                 "drag the regions · red: an odd number of bridges lead in · blue: the walk, numbered as it is crossed"
                                 "drag the regions · blue: the spanning tree chosen in the list")]]
                             [:div
                              [:table {:style {:border-collapse "collapse"}}
                               (into [:tbody]
                                     (for [[i k] (map-indexed vector pair-keys)
                                           :let [nm (str (subs (name k) 0 1) "–" (subs (name k) 1))]]
                                       [:tr [:td "bridges " nm] [:td {:style {:min-width "2em" :text-align "center"}} (list 'nth 'cs i)]
                                        [:td (btn {:data-remove (name k)} "−" (list 'fn [] (list 'swap! '?s 'update k '(fn [c] (max 0 (dec c))))))
                                         (btn {:data-add (name k)} "+" (list 'fn [] (list 'swap! '?s 'update k '(fn [c] (min 4 (inc c))))))]]))]
                              [:div "degrees: A " (q (keyword "deg" "0")) ", B " (q (keyword "deg" "1")) ", C " (q (keyword "deg" "2")) ", D " (q (keyword "deg" "3"))
                               "; odd regions " (mtie/fmt (q :odd) 0) "; bridges " (mtie/fmt (q :E) 0)]
                              (if (= mode :euler)
                                [:div
                                 [:div {:style {:font-size "1.15em" :font-weight 600 :margin "0.4em 0"
                                                :color '(if (contains? #{"circuit" "path"} case-name) "#2E8B57" "#C0392B")}}
                                  [:span {:data-case-name true} 'case-name] ": "
                                  (list 'get b/case-sentences 'case-name)
                                  (list 'when '(= case-name "path")
                                        '(str " Here from " (nth ["A" "B" "C" "D"] start) " to "
                                              (nth ["A" "B" "C" "D"] (last (filter (fn [i] (odd? (nth deg i))) (range 4)))) "."))]
                                 '(when (seq record) [:div "the walk, in Euler's letters: " [:code {:data-record true} record]])]
                                [:div
                                 (katex (list 'str "L_{\\hat A} = \\begin{pmatrix}"
                                              (cons 'str (interpose " \\\\ " (for [i (range 3)] (cons 'str (interpose " & " (for [j (range 3)] (mtie/fmt (q (keyword "L" (str i "-" j))) 0)))))))
                                              "\\end{pmatrix},\\quad \\det L_{\\hat A} = " (mtie/fmt (q :trees) 0))
                                        :data-matrix)
                                 [:div "rows and columns B, C, D: the bridge count on the diagonal, minus the bridges between two regions off it (Kirchhoff's determinant with A struck out)."]
                                 [:div {:style {:font-weight 600 :margin "0.3em 0"}}
                                  "spanning trees: " [:span {:data-tree-count true} (mtie/fmt (q :trees) 0)]
                                  "; independent circuits E − V + c = " (mtie/fmt (q :E) 0) " − 4 + " (mtie/fmt (q :components) 0) " = "
                                  [:span {:data-rank true} (mtie/fmt (q :rank) 0)]]
                                 (list 'if (list '> (q :pieces) 1.5) [:div {:style {:color "#C0392B"}} "the network falls apart: no tree spans it, the determinant is 0"]
                                       [:div {:style {:font-size "0.9em"}} "listed (" '(count trees) "): "
                                        '(into [:span] (map-indexed (fn [i t] [:button {:type "button" :data-tree i
                                                                                          :style {:margin "0.1em" :font-family "monospace"
                                                                                                  :background (if (= i tree-ix) "#1F6FB433" "transparent")
                                                                                                  :border "1px solid #888" :border-radius "0.2em" :color "inherit"}
                                                                                          :on-click (fn [] (swap! ?w assoc :tree i))}
                                                                                 (apply str (map (fn [e] (get labels (nth (nth edges e) 2))) t))])
                                                                    trees))])])
                              [:p {:style {:margin "0.6em 0 0.2em"}} [:strong "Try this:"]]
                              (flow/button !s "Remove bridge g (C–D): A and B stay odd" (assoc init :CD 0))
                              (flow/button !s "Build a bridge B–C" (assoc init :BC 1))
                              (flow/button !s "Cross each bridge twice (§18)" (into {} (map (fn [[k v]] [k (* 2 v)]) init)))
                              (flow/button !s "One bridge per pair" (zipmap pair-keys (repeat 1)))
                              (flow/button !s "Reset: the seven bridges" init)])])
               (list 'finally '(when timer (js/clearInterval timer)))))))))

;; ---------------------------------------------------------------------------
;; 3. Cayley: n^(n-2)

(def ^:private cayley-labels {:n "points n" :E "edges of Kₙ" :rank "circuits E − n + 1"})

(defn cayley
  "n, E and the cycle rank of K_n tied; Kirchhoff's determinant against
   n^(n-2); the k-th tree from its Pruefer code."
  []
  (binding [vc/*backend* :raster]
    (let [t b/cayley-tie
          start (zipmap (:keys t) (tie/start t :n 4))
          [tsym tform] (mtie/kernel-binding "ctie" (tie/kernel t) (mapv start (:keys t)))
          [qsym qform] (mtie/kernel-binding "cq" (tie/readout-kernel b/cayley-readout) [4.0])
          ix (tie/index b/cayley-readout)
          q (fn [k] (list 'aget 'q (ix k)))
          opts {:tie-sym tsym :p '?p :s '?s :keys (:keys t) :ranges (:ranges t) :counter "__cayleySolves"}
          pts (vec (for [i (range 6)] [(* 2 (Math/sin (* i (/ Math/PI 3)))) (* 2 (Math/cos (* i (/ Math/PI 3))))]))]
      (ev/with-let [!p start !s start !k {:k 0}]
        (flow/template
         {'?p !p '?s !s '?k !k '?q qsym}
         (list 'reagent.core/with-let (into [tsym tform qsym qform 'pruefer b/prufer-form] (mtie/locals opts))
               (list 'let ['n '(js/Math.round (get @?s :n))
                           'q '(?q (array 0) (array n))
                           'total '(js/Math.round (aget q 0))
                           'k '(mod (get @?k :k) (max 1 total))
                           'code '(if (< n 3) [] (vec (reverse (take (- n 2) (map (fn [x] (mod x n)) (iterate (fn [x] (quot x n)) k))))))
                           'tree '(pruefer code n)
                           'pts pts
                           'name (list 'if (list '< (list 'js/Math.abs (list '- (q :trees) (q :cayley))) 1e-6)
                                       "Kirchhoff's determinant = Cayley's nⁿ⁻²" "mismatch")]
                     [:div {:data-widget "cayley" :data-n '(str n) :data-trees (list 'str (q :trees)) :data-cayley (list 'str (q :cayley))}
                      (frame "cayley"
                             [:div
                              (mafs/mafs {:view-box {:x [-2.6 2.6] :y [-2.6 2.6]} :height 340}
                                         '(into [:<>] (for [i (range n) j (range (inc i) n)]
                                                        [mafs.line/Segment {:point1 (nth pts i) :point2 (nth pts j) :color "#BBBBBB" :weight 1}]))
                                         '(into [:<>] (for [[u v] tree] [mafs.line/Segment {:point1 (nth pts u) :point2 (nth pts v) :color "#2E8B57" :weight 4}]))
                                         '(into [:<>] (for [i (range n)] [mafs.core/Point {:x (first (nth pts i)) :y (second (nth pts i)) :color "#111111"}]))
                                         '(into [:<>] (for [i (range n)] [mafs.core/Text {:x (first (nth pts i)) :y (second (nth pts i)) :attach "ne" :size 16} (str (inc i))])))
                              [:div {:style {:font-size "0.85em" :opacity 0.75}} "grey: every pair joined (Kₙ) · green: one labelled tree, from its code"]]
                             [:div
                              (leva/sub-panel {:fill true :flat true :title-bar false}
                                              (leva/controls {:atom !p :schema (mtie/schema (:keys t) (:ranges t) cayley-labels)}))
                              [:div "E = n(n−1)/2 = " (mtie/fmt '(get @?s :E) 0) ", circuits = (n−1)(n−2)/2 = " (mtie/fmt '(get @?s :rank) 0)]
                              (katex (list 'str "\\det\\begin{pmatrix}"
                                           (list 'apply 'str (list 'interpose " \\\\ "
                                                                   (list 'for ['i '(range (dec n))]
                                                                         (list 'apply 'str (list 'interpose " & "
                                                                                                 (list 'for ['j '(range (dec n))]
                                                                                                       (list 'js/Math.round (list 'aget 'q (list '+ (ix (keyword "L" "0-0")) '(* 5 i) 'j)))))))))
                                           "\\end{pmatrix} = " (mtie/fmt (q :trees) 0) " = " '(str n) "^{" '(str (- n 2)) "} = " (mtie/fmt (q :cayley) 0))
                                     :data-det)
                              [:div {:style {:font-weight 600 :color "#2E8B57"} :data-name true} 'name]
                              [:div "tree " [:span {:data-k true} '(inc k)] " of " '(str total) ", code (" '(apply str (interpose ", " (map inc code))) ")"]
                              (btn {:data-next-tree true} "next tree" '(fn [] (swap! ?k update :k inc)))
                              (btn {} "previous" '(fn [] (swap! ?k update :k dec)))
                              [:p {:style {:font-size "0.9em"}} "The matrix is Kirchhoff's for Kₙ with one point struck out: n − 1 on the diagonal, −1 elsewhere. The code: strike the lowest leaf, write down its neighbour, n − 2 times; every code of n − 2 digits from 1..n gives one tree, which is why there are nⁿ⁻² of them (Prüfer, 1918)."]])])
               (list 'finally (mtie/unwatch-form !p))))))))

;; ---------------------------------------------------------------------------
;; 4. Hamilton's icosian game

(defn- flip [[x y]] [x (- y)])

(defn icosian
  "Click a path through the dodecahedron's twenty vertices."
  []
  (let [D b/icosian
        at (into {} (map (fn [[k p]] [k (flip p)])) (:at D))
        edges (mapv :ends (:edges D))
        nbrs (into {} (map (fn [[k xs]] [k (vec (distinct (map second xs)))])) (gr/neighbours D))
        solution (gr/hamiltonian-cycle D)]
    (ev/with-let [!h {:path []}]
      (flow/template
       {'?h !h}
       (list 'let ['path '(get @?h :path)
                   'nbrs nbrs 'at at
                   'last-v '(peek path)
                   'on-path '(set path)
                   'free '(if last-v (remove on-path (get nbrs last-v)) [])
                   'closed '(and (= 20 (count path)) (some #{(first path)} (get nbrs last-v)))
                   'case-name '(cond (empty? path) "start anywhere"
                                     closed "Hamilton's cycle: every vertex once, and back to the start"
                                     (= 20 (count path)) "a Hamiltonian path: every vertex once, but not back to the start"
                                     (empty? free) (str "stuck after " (count path) " vertices: no unvisited neighbour")
                                     :else (str (count path) " of 20 vertices"))
                   'letters b/consonants]
             [:div {:data-widget "icosian" :data-visited '(str (count path)) :data-closed '(str (boolean closed))}
              (frame "icosian"
                     (into [:svg {:viewBox "-3.4 -3.4 6.8 6.8" :style {:width "100%" :max-width "460px"}}]
                           (concat
                            (for [[u v] edges]
                              [:line {:x1 (first (at u)) :y1 (second (at u)) :x2 (first (at v)) :y2 (second (at v))
                                      :stroke "#999" :stroke-width 0.04}])
                            ['(into [:g] (for [[u v] (partition 2 1 (if closed (conj path (first path)) path))]
                                           [:line {:x1 (first (get at u)) :y1 (second (get at u)) :x2 (first (get at v)) :y2 (second (get at v))
                                                   :stroke "#C0392B" :stroke-width 0.11 :stroke-linecap "round"}]))]
                            (for [v (range 20)]
                              [:g {:data-vertex v :style {:cursor "pointer"}
                                   :on-click (list 'fn [] (list 'swap! '?h 'update :path
                                                                (list 'fn '[p]
                                                                      (list 'cond '(empty? p) [v]
                                                                            (list '= v '(peek p)) '(pop p)
                                                                            (list 'and (list 'not (list 'contains? '(set p) v)) (list 'some #{v} (list 'get 'nbrs '(peek p)))) (list 'conj 'p v)
                                                                            :else 'p))))}
                               [:circle {:cx (first (at v)) :cy (second (at v)) :r 0.2
                                         :fill (list 'cond (list '= v 'last-v) "#C0392B" (list 'contains? 'on-path v) "#F5B7B1"
                                                     (list 'some #{v} 'free) "#ABEBC6" :else "white")
                                         :stroke "#333" :stroke-width 0.04}]
                               [:text {:x (first (at v)) :y (+ 0.07 (second (at v))) :text-anchor "middle" :font-size 0.2
                                       :style {:pointer-events "none"}} (nth b/consonants v)]])))
                     [:div
                      [:div {:style {:font-size "1.1em" :font-weight 600 :margin "0.3em 0"
                                     :color '(if closed "#2E8B57" "inherit")} :data-case-name true} 'case-name]
                      [:div "path: " [:code '(apply str (map (fn [v] (nth letters v)) path))]]
                      [:div {:style {:margin "0.4em 0"}}
                       "Click a vertex to start, then a green neighbour to go on; click the last vertex to step back."]
                      [:div {:style {:font-size "0.92em"}}
                       "Euler's question on the same board fails at once: all 20 vertices have 3 edges, an odd number, so no walk crosses the 30 edges once each. Hamilton asks for every " [:em "vertex"] " once, Euler for every " [:em "edge"] " once; Euler's has a rule that decides it by counting, Hamilton's has none known that is fast."]
                      (btn {:data-solution true} "Show a solution" (list 'fn [] (list 'reset! '?h {:path solution})))
                      (btn {:data-clear true} "Clear" '(fn [] (reset! ?h {:path []})))])])))))

;; ---------------------------------------------------------------------------
;; Stepped figures and the colouring map

(defn- bow [[x1 y1] [x2 y2] off]
  (let [mx (/ (+ x1 x2) 2) my (/ (+ y1 y2) 2) dx (- x2 x1) dy (- y2 y1)
        len (max 1e-6 (Math/sqrt (+ (* dx dx) (* dy dy))))
        cx (+ mx (* 2 off (/ (- dy) len))) cy (+ my (* 2 off (/ dx len)))]
    {:d (format "M %.3f %.3f Q %.3f %.3f %.3f %.3f" (double x1) (double y1) cx cy (double x2) (double y2))
     :mid [(+ (* 0.5 mx) (* 0.5 cx)) (+ (* 0.5 my) (* 0.5 cy))]}))

(defn- graph-svg
  "Hiccup SVG of G (its :at), parallel edges bowed; vertex-name, the
   per-vertex fill (a browser form over v allowed), edge colour fns."
  [G {:keys [vertex-name fill edge-stroke edge-label view r] :or {view "-3 -3 6 6" r 0.32}}]
  (let [at (into {} (map (fn [[k p]] [k (flip p)])) (:at G))
        groups (group-by (comp set :ends) (:edges G))
        off (into {} (mapcat (fn [[_ es]] (map-indexed (fn [i {:keys [id]}] [id (* 0.35 (- i (/ (dec (count es)) 2.0)))]) es)) groups))]
    (into [:svg {:viewBox view :style {:width "100%" :max-width "420px"}}]
          (concat
           (for [{:keys [id] [u v] :ends} (:edges G)
                 :let [{:keys [d mid]} (bow (at u) (at v) (off id))]]
             [:g [:path {:d d :fill "none" :stroke (edge-stroke id) :stroke-width 0.07}]
              (when edge-label [:text {:x (first mid) :y (second mid) :font-size 0.28 :fill "#1F6FB4"} (edge-label id)])])
           (for [v (:vertices G)]
             [:g {:data-vertex (str v)}
              [:circle {:cx (first (at v)) :cy (second (at v)) :r r :fill (fill v) :stroke "#222" :stroke-width 0.04}]
              [:text {:x (first (at v)) :y (+ 0.1 (second (at v))) :text-anchor "middle" :font-size 0.3 :font-weight 600
                      :style {:pointer-events "none"}} (vertex-name v)]])))))

(defn graph-steps
  "A stepped figure: steps [{:who :text :fill {v colour} :lit #{edge-ids}
   :mark #{vertices}}], one figure per step (computed on the JVM), one
   sentence highlighted; previous/next."
  [id G steps opts]
  (let [figs (vec (for [{:keys [fill lit mark]} steps]
                    (graph-svg G (assoc opts
                                        :fill (fn [v] (cond (contains? (or mark #{}) v) "#E6A700" :else (get fill v "white")))
                                        :edge-stroke (fn [e] (if (contains? (or lit #{}) e) "#1F6FB4" "#888"))))))
        n (count steps)]
    (ev/with-let [!i {:i 0}]
      (flow/template
       {'?i !i}
       (list 'let ['i '(get @?i :i) 'figs figs 'steps (mapv #(select-keys % [:who :text]) steps)]
             [:div {:data-widget id :data-step '(str i)}
              (frame id '(nth figs i)
                     [:div
                      '(into [:ol] (for [[j {:keys [who text]}] (map-indexed vector steps)]
                                     [:li {:data-step-sentence j
                                           :style {:opacity (if (<= j i) 1 0.35) :font-weight (if (= j i) 600 400)
                                                   :background (if (= j i) "#E6A70022" "transparent") :padding "0.2em"}}
                                      text (when who [:br]) (when who [:small {:style {:opacity 0.7}} who])]))
                      [:div
                       (btn {:data-step-prev true} "previous" '(fn [] (swap! ?i update :i (fn [k] (max 0 (dec k))))))
                       (btn {:data-step-next true} "next" (list 'fn [] (list 'swap! '?i 'update :i (list 'fn '[k] (list 'min (dec n) '(inc k))))))]])])))))

(defn euler-notation
  "E53 par. 4-8 stepped on Koenigsberg's graph."
  []
  (let [K b/koenigsberg
        at-a (set (map :id (filter #(some #{:A} (:ends %)) (:edges K))))]
    (graph-steps "euler-notation" K
                 [{:text "Each land mass gets a capital, A the island, B and C the banks, D the land between the branches; each bridge a small letter."
                   :who "E53 par. 2"}
                  {:text "A crossing from A to B by bridge a or b is written AB: only which regions a bridge joins matters, not where."
                   :who "E53 par. 4" :lit #{:a} :mark #{:A :B}}
                  {:text "A walk over n bridges is written with n + 1 capitals: ABDC records three crossings, a, f, g."
                   :who "E53 par. 5" :lit #{:a :f :g} :mark #{:A :B :D :C}}
                  {:text "Five bridges lead into A, an odd number: in, out, in, out, in. A must appear (5 + 1)/2 = 3 times."
                   :who "E53 par. 8" :lit at-a :mark #{:A}}
                  {:text "B, C, D have three bridges each: 2 times each. 3 + 2 + 2 + 2 = 9 capitals, but seven bridges are written with 8."
                   :who "E53 par. 9" :mark #{:A :B :C :D}}]
                 {:vertex-name name :edge-label name :view "-2.2 -2.8 5.4 5.6"})))

(defn kempe-swap-steps
  "Kempe's chain swap stepped (alexandria.graph-theory.bridges/kempe-steps)."
  []
  (let [G b/kempe-map
        steps (b/kempe-steps)
        e-of (fn [chain] (set (keep (fn [{:keys [id] [u v] :ends}] (when (and (some #{u} chain) (some #{v} chain)) id)) (:edges G))))]
    (graph-steps "kempe-steps" G
                 (for [{:keys [colouring chain text]} steps]
                   {:text text :fill (into {} (map (fn [[v c]] [v (b/palette c)])) colouring)
                    :lit (e-of chain) :mark #{}})
                 {:vertex-name #(nth b/kempe-names %) :view "-2.6 -2.6 5.2 5.2"})))

(defn colour-map
  "Kempe's map: click a district to change its colour; clashes counted
   (structure.graph/clashes run in the page)."
  []
  (let [G b/kempe-map
        at (into {} (map (fn [[k p]] [k (flip p)])) (:at G))
        edges (mapv :ends (:edges G))]
    (ev/with-let [!c {:col (assoc b/kempe-start 0 1)}]
      (flow/template
       {'?c !c}
       (list 'let ['col '(get @?c :col)
                   'clash (list 'vec (list 'filter '(fn [[u v]] (= (get col u) (get col v))) edges))
                   'used '(count (distinct (vals col)))]
             [:div {:data-widget "colour-map" :data-clashes '(str (count clash))}
              (frame "colour-map"
                     (into [:svg {:viewBox "-2.6 -2.6 5.2 5.2" :style {:width "100%" :max-width "380px"}}]
                           (concat
                            (for [[u v] edges]
                              [:line {:x1 (first (at u)) :y1 (second (at u)) :x2 (first (at v)) :y2 (second (at v))
                                      :stroke (list 'if (list '= (list 'get 'col u) (list 'get 'col v)) "#C0392B" "#888")
                                      :stroke-width (list 'if (list '= (list 'get 'col u) (list 'get 'col v)) 0.12 0.05)}])
                            (for [v (:vertices G)]
                              [:g {:data-district v :style {:cursor "pointer"}
                                   :on-click (list 'fn [] (list 'swap! '?c 'update-in [:col v] '(fn [c] (mod (inc c) 4))))}
                               [:circle {:cx (first (at v)) :cy (second (at v)) :r 0.34 :fill (list 'nth b/palette (list 'get 'col v)) :stroke "#222" :stroke-width 0.04}]
                               [:text {:x (first (at v)) :y (+ 0.1 (second (at v))) :text-anchor "middle" :font-size 0.3 :fill "white"
                                       :style {:pointer-events "none"}} (nth b/kempe-names v)]])))
                     [:div
                      [:div {:style {:font-weight 600 :font-size "1.1em" :color '(if (empty? clash) "#2E8B57" "#C0392B")} :data-case-name true}
                       '(if (empty? clash) (str "properly coloured with " used " colours") (str (count clash) " borders between districts of one colour"))]
                      [:div "Each point is a district, each line a common border. Click a district to change its colour (red, yellow, green, blue)."]
                      (btn {} "Kempe's start, X clashing" (list 'fn [] (list 'reset! '?c {:col (assoc b/kempe-start 0 1)})))
                      (btn {} "After the swap" (list 'fn [] (list 'reset! '?c {:col (:colouring (peek (b/kempe-steps)))})))])])))))

;; ---------------------------------------------------------------------------
;; Derivation

(defn derivation
  "Kirchhoff's polynomial for four regions, the claims graded."
  []
  (clerk/col
   (clerk/tex (str "\\tau(c_{AB},\\dots,c_{CD}) = \\det L_{\\hat A} = "
                   (str/replace (str (emmy.env/->TeX (b/tree-polynomial))) "\\," " ")))
   (clerk/html
    (into [:ul {:style {:font-size "0.9em"}}]
          (for [{:keys [label grade]} (b/graded)]
            [:li {:data-grade (name grade)} [:code (name grade)] " " label])))))
