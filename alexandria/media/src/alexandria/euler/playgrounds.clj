(ns alexandria.euler.playgrounds
  "Free-form playgrounds for the Euler notebook (playground brief): after
   each player, the passage's whole setup open to the reader.

     basel-playground        E41: the terms 1/n^s and their partial sums
                             against zeta(s), and Euler's product over the
                             roots of sin x / x truncated at N factors; N, s
                             and the point x free, N and x draggable
     koenigsberg-playground  E53: four land masses (draggable) and the
                             bridges between each pair (add or remove);
                             degrees, Euler's table of par. 14, the rule of
                             par. 20 named live and the walk drawn when one
                             exists (Hierholzer's construction)
     vef-playground          E230 by Cauchy's map (1813): the Schlegel
                             diagram of the tetrahedron, cube or octahedron,
                             vertices draggable, edges rubbed out or
                             diagonals drawn, the outer face taken away;
                             V, E, F, crossings and pieces from a raster
                             kernel, a badge red when the map crosses itself
                             or falls apart

   Every number shown comes from a raster kernel compiled here. The faces
   of a drawn map are traced in the browser from the kernel's angles
   (half-edge rotation) and their number goes back through the kernel
   that forms F and V - E + F; likewise the pieces of the drawing."
  (:require [alexandria.euler.polyhedra :as polyhedra]
            [alexandria.euler.ties :as ties]
            [alexandria.raster :as raster]
            [alexandria.solids :as solids]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.mathbox.plot :as plot]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Shared browser helpers

(defn- kernel [f xs] (vc/compiled-fn f xs [0] {:simplify? false}))

(defn- fmt
  ([x] (fmt x 4))
  ([x n] (list 'let ['v x] (list 'if '(js/isFinite v) (list '.replace (list '.toFixed 'v n) "-" "−") "∞"))))

(defn- button [!s title values]
  [:button {:type "button" :data-preset title
            :style {:margin "0.25em" :padding "0.3em 0.6em" :cursor "pointer" :border-radius "0.3em"
                    :border "1px solid currentColor" :background "transparent" :color "inherit"}
            :on-click (list 'fn [] (list 'reset! !s values))} title])

(defn- frame
  "Figures left, controls right. The figure's text labels let the pointer
   through, so a letter set beside a movable point never hides it from a
   drag."
  [id figures controls]
  [:div {:data-playground id :style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
   [:style (str "[data-playground=\"" id "\"] .MafsView text { pointer-events: none; }")]
   (into [:div {:style {:flex "1 1 480px" :min-width "300px"}}] figures)
   [:div {:style {:flex "1 1 320px" :min-width "280px" :font-size "0.92em" :line-height 1.55}} controls]])

(defn- badge [ok? good bad attr]
  [:div (merge {:style {:margin "0.4em 0" :padding "0.3em 0.6em" :border-radius "0.3em" :color "white"
                        :background (list 'if ok? "#2E8B57" "#C0392B")}}
               attr)
   (list 'if ok? good bad)])

(defn- clamp01 [x] (ties/clamp x 0 1))

(defn- pow
  "a^b = exp(b log a) for a > 0 and a variable exponent b (the raster
   lowering takes number exponents only in expt)."
  [a b]
  (e/exp (e/* b (e/log a))))

;; ---------------------------------------------------------------------------
;; 1. Basel (E41): the terms, the partial sums, the product

(def basel-max "Terms and factors carried (N's slider goes this far)." 40)

(def ^:private zeta-head "Terms summed exactly before Euler-Maclaurin's tail." 200)

(def basel-samples
  "x positions where the kernel evaluates Euler's product for its curve
   (a 40-factor product handed to of-x overflows Emmy's simplifier)."
  (mapv #(- (* 0.3125 %) 12.5) (range 81)))

(defn basel-product
  "x |-> prod_{n <= N} (1 - x^2/(n^2 pi^2)), factor n switched on by
   clamp01(N - n + 1)."
  [N]
  (fn [x]
    (reduce e/* (for [n (range 1 (inc basel-max))]
                  (e/- 1 (e/divide (e/* (clamp01 (e/- N (dec n))) (e/square x)) (* n n Math/PI Math/PI)))))))

(defn basel-values
  "[S_N zeta(s) gap upper lower N^-s P_N(x) sinx/x P-sinc coef N s x,
    terms 1..40, partial sums 1..40, pi^2/6 pi^4/90 pi^6/945,
    P_N at each of basel-samples].
   Term m is switched on by clamp01(N - m + 1), so N moves continuously.
   zeta(s) = sum_{m<200} m^-s + 200^(1-s)/(s-1) + 200^-s/2 + s 200^(-s-1)/12
   (Euler-Maclaurin), s - 1 guarded at 1e-6. The tail after N terms lies
   between (N+1)^(1-s)/(s-1) and N^(1-s)/(s-1) (the integral test). The
   product is prod (1 - x^2/(n^2 pi^2)) over the factors switched on; its
   x^2 coefficient -(sum 1/n^2)/pi^2 is what Euler matches with -1/6.
   The curve of the product is sampled here, in the same kernel."
  [N s x]
  (fn [_]
    (let [term (fn [m] (pow m (e/- 0 s)))
          w (fn [m] (clamp01 (e/- N (dec m))))
          ws (mapv w (range 1 (inc basel-max)))
          SN (reduce e/+ (for [m (range 1 (inc basel-max))] (e/* (ws (dec m)) (term m))))
          M zeta-head
          s1 (ties/at-least (e/- s 1) 1e-6)
          zeta (e/+ (reduce e/+ (for [m (range 1 M)] (term m)))
                    (e/divide (pow M (e/- 1 s)) s1)
                    (e/* 0.5 (pow M (e/- 0 s)))
                    (e/divide (e/* s (pow M (e/- -1 s))) 12))
          pi2 (* Math/PI Math/PI)
          prod (fn [x2] (reduce e/* (for [n (range 1 (inc basel-max))]
                                      (e/- 1 (e/* (ws (dec n)) x2 (/ 1.0 (* n n pi2)))))))
          P (prod (e/square x))
          sinc (e/divide (e/sin x) (e/+ x 1e-12))
          coef (e/- 0 (e/divide (reduce e/+ (for [n (range 1 (inc basel-max))] (e/divide (ws (dec n)) (* n n)))) pi2))
          terms (for [m (range 1 (inc basel-max))] (term m))]
      (vec (concat [SN zeta (e/- zeta SN)
                    (e/divide (pow N (e/- 1 s)) s1) (e/divide (pow (e/+ N 1) (e/- 1 s)) s1)
                    (pow N (e/- 0 s)) P sinc (e/- P sinc) coef N s x]
                   terms (reductions e/+ terms)
                   [(e/divide pi2 6) (e/divide (* pi2 pi2) 90) (e/divide (* pi2 pi2 pi2) 945)]
                   (for [xs basel-samples] (prod (* xs xs))))))))

(def basel-init {:N 10 :s 2 :x 2})

(def basel-schema
  {:N {:min 1 :max basel-max :step 0.01 :label "terms / factors N"}
   :s {:min 0.3 :max 6 :step 0.01 :label "exponent s"}
   :x {:min -12.5 :max 12.5 :step 0.01 :label "x (product)"}})

(def ^:private T0 13) ; index of term 1 in the readout
(def ^:private S0 (+ T0 basel-max)) ; index of S_1
(def ^:private Z0 (+ S0 basel-max)) ; pi^2/6

(def ^:private P0 (+ Z0 3)) ; P_N at the first of basel-samples

(defn- basel-case []
  (list 'let ['s '(aget r 11)]
        (list 'cond
              '(< (js/Math.abs (- s 1)) 0.006) "s = 1, the harmonic series: diverges (Oresme, c. 1350)"
              '(< s 1) "s < 1: the terms shrink too slowly, the sums grow without bound: diverges"
              '(< (js/Math.abs (- s 2)) 0.006) (list 'str "converges: ζ(2) = π²/6 = " (fmt (list 'aget 'r Z0) 6) " (Euler, E41)")
              '(< (js/Math.abs (- s 4)) 0.006) (list 'str "converges: ζ(4) = π⁴/90 = " (fmt (list 'aget 'r (inc Z0)) 6) " (E41 §18)")
              '(< (js/Math.abs (- s 6)) 0.006) (list 'str "converges: ζ(6) = π⁶/945 = " (fmt (list 'aget 'r (+ 2 Z0)) 6) " (E41 §18)")
              '(< (js/Math.abs (- s 3)) 0.006) "converges: ζ(3), no closed form known to this day"
              :else "s > 1: converges to ζ(s)")))

(defn basel-figure
  "The terms 1/m^s as bars of width 1, the curve 1/x^s under them, the
   partial sums S_m and the limit zeta(s); the truncation handle N."
  [!s]
  (mafs/mafs {:height 330 :view-box {:x [-1 41] :y [0 2.6]} :preserve-aspect-ratio false}
             (mafs/cartesian {:x-axis {:lines 5} :y-axis {:lines 0.5}})
             (into [:<>]
                   (for [m (range 1 (inc basel-max)) :let [t (list 'aget 'r (+ T0 (dec m)))]]
                     (list 'when (list '<= m '(js/Math.ceil N))
                           (mafs/polygon {:points [[(dec m) 0] [m 0] [m t] [(dec m) t]]
                                          :color "#1F6FB4" :fill-opacity 0.35 :weight 1}))))
             (mafs/of-x {:y (ev/with-params {:atom !s :params [:s]}
                              (fn [s] (fn [x] (pow (ties/at-least x 0.35) (e/- 0 s)))))
                         :color "#E67E22" :domain [0.35 41]})
             (mafs/polyline {:points (vec (for [m (range 1 (inc basel-max))] [m (list 'aget 'r (+ S0 (dec m)))]))
                             :color "#2E8B57" :weight 2})
             (list 'when '(> (aget r 11) 1.006)
                   (mafs/segment {:point1 [0 '(aget r 1)] :point2 [41 '(aget r 1)] :color "#c0392b" :style "dashed"}))
             (mafs/segment {:point1 ['N '(aget r 0)] :point2 ['N '(aget r 1)] :color "#7B5EA7"})
             (mafs/point {:x 'N :y '(aget r 0) :color "#2E8B57"})
             (mafs/text "S_N" {:x 'N :y '(aget r 0) :attach "nw" :color "#2E8B57"})
             (mafs/movable-point {:point ['N 0] :color "#7B5EA7"
                                  :constrain '(fn [[x _]] [(max 1 (min 40 x)) 0])
                                  :on-move (list 'fn '[[x _]] (list 'swap! !s 'assoc :N 'x))})
             (mafs/text "N" {:x 'N :y 0 :attach "ne" :color "#7B5EA7"})))

(defn basel-roots-figure
  "Euler's picture: sin x / x and the product over its roots ±nπ (sampled
   by the kernel), the roots marked (red while their factor is on), the
   point x draggable along the curve, the residual between product and
   curve."
  [!s]
  (mafs/mafs {:height 280 :view-box {:x [-13 13] :y [-0.6 1.25]} :preserve-aspect-ratio false}
             (mafs/cartesian {:x-axis {:lines Math/PI :labels false} :y-axis {:lines 0.5}})
             (mafs/of-x {:y (fn [x] (e/divide (e/sin x) (e/+ x 1e-12))) :color "#1F6FB4" :weight 3})
             (mafs/polyline {:points (vec (for [[i xs] (map-indexed vector basel-samples)]
                                            [xs (list 'max -0.6 (list 'min 1.25 (list 'aget 'r (+ P0 i))))]))
                             :color "#E67E22" :weight 2})
             (into [:<>]
                   (for [n [1 2 3 4] sg [1 -1] :let [x0 (* sg n Math/PI)]]
                     [:<> (mafs/point {:x x0 :y 0 :color (list 'if (list '<= n '(js/Math.ceil N)) "#c0392b" "#8A9BB0")})
                      (mafs/text (str (if (neg? sg) "−" "") (if (= n 1) "" n) "π") {:x x0 :y 0 :attach "s" :size 13})]))
             (mafs/segment {:point1 ['(aget r 12) '(aget r 6)] :point2 ['(aget r 12) '(aget r 7)] :color "#7B5EA7" :weight 3})
             (mafs/movable-point {:point ['(aget r 12) '(aget r 7)] :color "#1F6FB4"
                                  :constrain '(fn [[x _]] (let [x (max -12.5 (min 12.5 x))]
                                                            [x (/ (js/Math.sin x) (+ x 1e-12))]))
                                  :on-move (list 'fn '[[x _]] (list 'swap! !s 'assoc :x 'x))})
             (mafs/text "x" {:x '(aget r 12) :y '(aget r 7) :attach "ne"})))

(defn basel-playground
  "E41 open: N, s and x free; N dragged on the terms, x along sin x / x."
  []
  (binding [vc/*backend* :raster]
    (ev/with-let [!s basel-init]
      (let [ksym (gensym "basel") k (kernel basel-values (mapv basel-init [:N :s :x]))]
        (list 'reagent.core/with-let [ksym k]
              (list 'let ['r (list ksym '(array 0) (list 'array (ev/get !s :N) (ev/get !s :s) (ev/get !s :x)))
                          'N '(aget r 10)
                          'conv '(> (aget r 11) 1.006)
                          'case-name (basel-case)]
                    [:div {:data-figure "euler-basel-playground" :data-case 'case-name
                           :data-sn '(str (aget r 0)) :data-resid '(str (aget r 8)) :data-n '(str N)}
                     (frame "euler-basel"
                            [(basel-figure !s)
                             [:div {:style {:font-size "0.85em" :opacity 0.75 :margin "0.2em 0 0.6em"}}
                              "bars: the terms 1/mˢ · orange: 1/xˢ · green: the partial sums S_m · red: ζ(s) · drag N"]
                             (basel-roots-figure !s)
                             [:div {:style {:font-size "0.85em" :opacity 0.75}}
                              "blue: sin x / x · orange: Euler's product over the first N pairs of roots ±nπ · drag x along the curve"]]
                            [:div
                             (leva/sub-panel {:fill true :flat true :title-bar false}
                                             (leva/controls {:atom !s :schema basel-schema}))
                             [:strong {:data-case true :style {:color '(if conv "#2E8B57" "#C0392B")}} 'case-name]
                             [:div "S_N = Σ_{m≤N} 1/mˢ = " [:span {:data-sn true} (fmt '(aget r 0) 6)]
                              ", N = " (fmt 'N 2) ", s = " (fmt '(aget r 11) 2)]
                             (list 'if 'conv
                                   [:div "ζ(s) = " (fmt '(aget r 1) 6) "; still missing ζ(s) − S_N = " [:span {:data-gap true} (fmt '(aget r 2) 6)]
                                    [:br] "integral test: " (fmt '(aget r 4) 5) " = (N+1)^{1−s}/(s−1) < gap < N^{1−s}/(s−1) = " (fmt '(aget r 3) 5)]
                                   [:div "no limit: the next term 1/Nˢ = " (fmt '(aget r 5) 4)
                                    " is small, but ∫ x^{−s} dx from N to ∞ is infinite"])
                             (badge 'conv "integral test: s > 1, so the sums have a limit"
                                    "hypothesis broken: s ≤ 1, ∫₁^∞ x^{−s} dx diverges and so do the sums"
                                    {:data-hypothesis "basel"})
                             [:div {:style {:margin-top "0.5em"}}
                              "Euler's product, " (fmt 'N 0) " factors: P_N(x) = Π(1 − x²/n²π²) = " (fmt '(aget r 6) 5)]
                             [:div "sin x / x = " (fmt '(aget r 7) 5) " at x = " (fmt '(aget r 12) 3)
                              "; residual P_N − sin x/x = " [:span {:data-resid true} (fmt '(aget r 8) 5)]]
                             [:div "x² coefficient: −(Σ_{n≤N} 1/n²)/π² = " (fmt '(aget r 9) 5)
                              " against −1/3! = " (fmt (list '- (list '/ (list 'aget 'r Z0) '(* js/Math.PI js/Math.PI))) 5)]
                             [:p {:style {:margin "0.6em 0 0.2em"}} [:strong "Try this:"]]
                             (button !s "Take all 40 factors: the product hugs sin x/x" {:N 40 :s 2 :x 2})
                             (button !s "Set s = 1: Oresme's harmonic series" {:N 40 :s 1 :x 2})
                             (button !s "Set s = 4: Euler's π⁴/90" {:N 40 :s 4 :x 2})
                             (button !s "Put x on 3π with only 2 factors" {:N 2 :s 2 :x (* 3 Math/PI)})
                             (button !s "Reset: Euler's 1 + 1/4 + 1/9 + …" basel-init)])])
              '(finally nil))))))

;; ---------------------------------------------------------------------------
;; 2. Koenigsberg (E53): four regions, the bridges between each pair

(def kb-regions "Euler's letters, in the kernel's order." ["A" "B" "C" "D"])

(def kb-pairs "The six pairs of regions, in the kernel's order."
  [[:AB 0 1] [:AC 0 2] [:AD 0 3] [:BC 1 2] [:BD 1 3] [:CD 2 3]])

(def kb-letter-order
  "Pairs in the order their bridges get letters: Euler's a b (A-B), c d
   (A-C), e (A-D), f (B-D), g (C-D); a bridge B-C, which Koenigsberg did not
   have, comes after."
  [0 1 2 4 5 3])

(def kb-max "Bridges a pair may carry." 4)
(def ^:private kb-samples "Points along each bridge's curve." 9)

(def kb-init
  "E53 par. 2: A-B two bridges, A-C two, A-D, B-D, C-D one each."
  {:AB 2 :AC 2 :AD 1 :BC 0 :BD 1 :CD 1})

(def kb-places
  "Where the regions sit on the figure: A the Kneiphof, B the north bank,
   C the south bank, D the land between the two branches, east."
  {:ax -1.0 :ay 0.0 :bx 0.0 :by 2.0 :cx 0.0 :cy -2.0 :dx 2.4 :dy 0.0})

(def ^:private KB-R 17) ; reach matrix (I + A)^3, row-major 4x4
(def ^:private KB-P 33) ; bridge curves

(defn- kb-idx
  "Index of coordinate c (0 x, 1 y) of sample k of bridge j of pair p."
  [p j k c]
  (+ KB-P (* 2 (+ k (* kb-samples (+ j (* kb-max p))))) c))

(defn kb-values
  "[d_A..d_D, parity_A..parity_D, odd regions, bridges E, letters needed,
    E + 1, excess, letters_A..letters_D, (I + A)^3 (16), bridge curves].
   Degrees: d = sum of the bridges at a region (par. 8). Parity
   (1 - cos(pi d))/2, letters (d + parity)/2 (par. 8, 12), their sum against
   E + 1 (par. 9, 14). A_ij = clamp(c_ij, 0, 1); (I + A)^3 > 0 where two
   regions are joined by some chain of bridges. Bridge j of a pair carrying
   c is a quadratic curve through the midpoint pushed (j - (c - 1)/2) 0.8
   along the normal, |v - u| guarded at 1e-6."
  [ax ay bx by cx cy dx dy cAB cAC cAD cBC cBD cCD]
  (fn [_]
    (let [P [[ax ay] [bx by] [cx cy] [dx dy]]
          C [cAB cAC cAD cBC cBD cCD]
          deg (vec (for [i (range 4)]
                     (reduce e/+ 0 (for [[p [_ u v]] (map-indexed vector kb-pairs) :when (or (= u i) (= v i))] (C p)))))
          par (mapv (fn [d] (e/* 0.5 (e/- 1 (e/cos (e/* Math/PI d))))) deg)
          E (reduce e/+ 0 C)
          lets (mapv (fn [d q] (e/* 0.5 (e/+ d q))) deg par)
          L (reduce e/+ 0 lets)
          adj (fn [i j] (if (= i j) 1
                            (let [p (first (keep-indexed (fn [p [_ u v]] (when (= #{u v} #{i j}) p)) kb-pairs))]
                              (ties/clamp (C p) 0 1))))
          A1 (vec (for [i (range 4)] (vec (for [j (range 4)] (adj i j)))))
          mm (fn [X Y] (vec (for [i (range 4)] (vec (for [j (range 4)] (reduce e/+ (for [k (range 4)] (e/* ((X i) k) ((Y k) j)))))))))
          R (mm (mm A1 A1) A1)
          curves (for [[p [_ u v]] (map-indexed vector kb-pairs) j (range kb-max) k (range kb-samples)]
                   (let [[ux uy] (P u) [vx vy] (P v)
                         ddx (e/- vx ux) ddy (e/- vy uy)
                         len (ties/at-least (e/sqrt (e/+ (e/square ddx) (e/square ddy))) 1e-6)
                         off (e/* 0.8 (e/- j (e/* 0.5 (e/- (C p) 1))))
                         mx (e/+ (e/* 0.5 (e/+ ux vx)) (e/* off (e/divide (e/- 0 ddy) len)))
                         my (e/+ (e/* 0.5 (e/+ uy vy)) (e/* off (e/divide ddx len)))
                         ;; control point so the curve passes through m at t = 1/2
                         qx (e/- (e/* 2 mx) (e/* 0.5 (e/+ ux vx)))
                         qy (e/- (e/* 2 my) (e/* 0.5 (e/+ uy vy)))
                         t (/ k (double (dec kb-samples)))
                         a (* (- 1 t) (- 1 t)) b (* 2 t (- 1 t)) c (* t t)]
                     [(e/+ (e/* a ux) (e/* b qx) (e/* c vx)) (e/+ (e/* a uy) (e/* b qy) (e/* c vy))]))]
      (vec (concat deg par [(reduce e/+ 0 par) E L (e/+ E 1) (e/- L (e/+ E 1))] lets
                   (apply concat R) (apply concat curves))))))

(def ^:private kb-walk
  "Browser form: Hierholzer's walk (1873) over `edges` [[u v letter] ...]
   from region `start`: [[region edge-index] ...], the first edge nil."
  '(fn [edges start]
     (let [used (atom #{}) stack (atom [[start nil]]) out (atom [])]
       (while (seq @stack)
         (let [[v _ :as top] (peek @stack)
               nxt (first (keep-indexed (fn [i [a b _]] (when (and (not (contains? @used i)) (or (= a v) (= b v))) [i (if (= a v) b a)]))
                                        edges))]
           (if nxt
             (do (swap! used conj (first nxt)) (swap! stack conj [(second nxt) (first nxt)]))
             (do (swap! out conj top) (swap! stack pop)))))
       (vec (reverse @out)))))

(defn kb-figure
  "The four regions (draggable, red when an odd number of bridges lead in),
   the bridges between them as curves (from the kernel), each lettered as
   Euler letters them, numbered in the order the walk crosses it."
  [!p]
  (mafs/mafs {:height 400 :view-box {:x [-3.6 4.4] :y [-3.2 3.2]}}
             (into [:<>]
                   (for [[p [_ u v]] (map-indexed vector kb-pairs) j (range kb-max)]
                     (list 'when (list '< j (list 'nth 'cs p))
                           (mafs/polyline {:points (vec (for [k (range kb-samples)]
                                                          [(list 'aget 'r (kb-idx p j k 0)) (list 'aget 'r (kb-idx p j k 1))]))
                                           :color (list 'if (list 'contains? 'walked [p j]) "#1F6FB4" "#5D6D7E") :weight 4})
                           (mafs/text (list 'str (list 'get 'labels [p j] "") (list 'if-let ['o (list 'get 'order [p j])] (list 'str " · " 'o) ""))
                                      {:x (list 'aget 'r (kb-idx p j 4 0)) :y (list 'aget 'r (kb-idx p j 4 1))
                                       :attach "ne" :size 15 :color "#1F6FB4"}))))
             (into [:<>]
                   (for [[i nm] (map-indexed vector kb-regions)
                         :let [kx (keyword (str (str/lower-case nm) "x")) ky (keyword (str (str/lower-case nm) "y"))]]
                     [:<>
                      (mafs/circle {:center [(ev/get !p kx) (ev/get !p ky)] :radius 0.42
                                    :color (list 'if (list '> (list 'aget 'r (+ 4 i)) 0.5) "#C0392B" "#2E8B57")
                                    :fill-opacity 0.25})
                      (mafs/movable-point {:point [(ev/get !p kx) (ev/get !p ky)] :color "#1d1f21"
                                           :constrain '(fn [[x y]] [(max -3.4 (min 4.2 x)) (max -3 (min 3 y))])
                                           :on-move (list 'fn '[[x y]] (list 'swap! !p 'assoc kx 'x ky 'y))})
                      (mafs/text (list 'str nm " (" (list 'js/Math.round (list 'aget 'r i)) ")")
                                 {:x (ev/get !p kx) :y (ev/get !p ky) :attach "sw" :size 16})]))))

(def kb-schema
  (into {} (for [[k u v] kb-pairs]
             [k {:min 0 :max kb-max :step 1 :label (str "bridges " (kb-regions u) "–" (kb-regions v))}])))

(defn- kb-case []
  '(let [n (js/Math.round (aget r 8)) E (js/Math.round (aget r 9))
         odds (apply str (interpose ", " (keep-indexed (fn [i nm] (when (> (aget r (+ 4 i)) 0.5) nm)) ["A" "B" "C" "D"])))]
     (cond (zero? E) "no bridges: nothing to walk"
           (not connected) "the bridges fall apart into separate pieces: Euler's count passes, but no single walk reaches them all"
           (zero? n) "every region even: a walk over each bridge once exists from any region, and ends where it began (§18)"
           (= n 2) (str "two odd regions, " odds ": the walk exists, starting in one and ending in the other (§19–20)")
           :else (str n " odd regions (" odds "): no walk; the letters exceed the walk by " (dec (/ n 2)) " (§19–20)"))))

(defn koenigsberg-playground
  "E53 open: the regions dragged, the bridges between each pair set freely."
  []
  (binding [vc/*backend* :raster]
    (ev/with-let [!s kb-init !p kb-places]
      (let [ksym (gensym "kb")
            pk [:ax :ay :bx :by :cx :cy :dx :dy]
            k (kernel kb-values (vec (concat (map kb-places pk) (map (comp double kb-init first) kb-pairs))))
            letters "abcdefghijklmnopqrstuvwx"
            reset (fn [title counts places]
                    [:button {:type "button" :data-preset title
                              :style {:margin "0.25em" :padding "0.3em 0.6em" :cursor "pointer" :border-radius "0.3em"
                                      :border "1px solid currentColor" :background "transparent" :color "inherit"}
                              :on-click (list 'fn [] (list 'reset! !s counts) (when places (list 'reset! !p places)))}
                     title])]
        (list 'reagent.core/with-let [ksym k]
              (list 'let ['cs (vec (for [[kk] kb-pairs] (list 'js/Math.round (ev/get !s kk))))
                          'r (list ksym '(array 0)
                                   (list* 'array (concat (for [kk pk] (ev/get !p kk)) (for [i (range 6)] (list 'nth 'cs i)))))
                          'deg (vec (for [i (range 4)] (list 'js/Math.round (list 'aget 'r i))))
                          'connected (list 'every? 'true?
                                           (list 'for '[i (range 4) j (range 4)]
                                                 (list 'or '(zero? (nth deg i)) '(zero? (nth deg j))
                                                       (list '> (list 'aget 'r (list '+ KB-R '(* 4 i) 'j)) 0.5))))
                          'edges (list 'vec (list 'for ['p kb-letter-order 'j (list 'range kb-max) ':when '(< j (nth cs p))]
                                                  (list 'let ['[_ u v] (list 'nth (mapv (fn [[_ u v]] [nil u v]) kb-pairs) 'p)]
                                                        '[u v [p j]])))
                          'labels (list 'into {} (list 'map-indexed (list 'fn '[i [_ _ pj]] ['pj (list 'subs letters 'i '(inc i))]) 'edges))
                          'nodd '(js/Math.round (aget r 8))
                          'possible '(and connected (seq edges) (<= nodd 2))
                          'start '(or (first (filter (fn [i] (odd? (nth deg i))) (range 4)))
                                      (first (filter (fn [i] (pos? (nth deg i))) (range 4))))
                          'walk (list 'if 'possible (list kb-walk 'edges 'start) [])
                          'order '(into {} (keep-indexed (fn [n [_ e]] (when e [(nth (nth edges e) 2) n])) walk))
                          'walked '(set (keys order))
                          'record '(apply str (map (fn [[v e]] (str (when e (get labels (nth (nth edges e) 2))) (nth ["A" "B" "C" "D"] v))) walk))
                          'case-name (kb-case)]
                    [:div {:data-figure "euler-koenigsberg-playground" :data-case 'case-name :data-nodd '(str nodd)
                           :data-letters '(str (aget r 10)) :data-bridges '(str (aget r 9)) :data-walk 'record}
                     (frame "euler-koenigsberg"
                            [(kb-figure !p)
                             [:div {:style {:font-size "0.85em" :opacity 0.75 :margin-top "0.2em"}}
                              "drag the regions A–D · a region is red when an odd number of bridges lead into it · blue: the walk, each bridge numbered in the order crossed"]]
                            [:div
                             (leva/sub-panel {:fill true :flat true :title-bar false}
                                             (leva/controls {:atom !s :schema kb-schema}))
                             [:strong {:data-case true :style {:color '(if possible "#2E8B57" "#C0392B")}} 'case-name]
                             [:table {:style {:margin "0.5em 0" :border-collapse "collapse"}}
                              [:thead [:tr [:th "region"] [:th "bridges"] [:th "letters"]]]
                              (into [:tbody]
                                    (for [[i nm] (map-indexed vector kb-regions)]
                                      [:tr [:td nm] [:td (fmt (list 'aget 'r i) 0) (list 'when (list '< (list 'aget 'r (+ 4 i)) 0.5) "*")]
                                       [:td (fmt (list 'aget 'r (+ 13 i)) 0)]]))]
                             [:div "letters needed: " (fmt '(aget r 13) 0) " + " (fmt '(aget r 14) 0) " + " (fmt '(aget r 15) 0) " + " (fmt '(aget r 16) 0)
                              " = " [:span {:data-letters true} (fmt '(aget r 10) 0)]
                              "; a walk over " (fmt '(aget r 9) 0) " bridges writes " (fmt '(aget r 11) 0) " letters (§9, §14)"]
                             [:div "odd regions: " (fmt '(aget r 8) 0) "; excess " (fmt '(aget r 12) 0)
                              (list 'if '(< (aget r 12) 0.5) " ≤ 0: the count allows a walk" " > 0: too many letters, no walk")]
                             (list 'when '(seq record) [:div "a walk, in Euler's letters: " [:code {:data-walk true} 'record]])
                             (badge 'connected "the bridges hang together: Euler's rule decides"
                                    "hypothesis broken: the bridges fall into separate pieces, so the rule of §20 does not apply"
                                    {:data-hypothesis "koenigsberg"})
                             [:p {:style {:margin "0.6em 0 0.2em"}} [:strong "Try this:"]]
                             (reset "Build an eighth bridge from B to C: only A and D stay odd" (assoc kb-init :BC 1) nil)
                             (reset "Cross each bridge twice (§18): every region even" (into {} (map (fn [[kk v]] [kk (* 2 v)]) kb-init)) nil)
                             (reset "Split the city: A–B and C–D only, two bridges each" {:AB 2 :AC 0 :AD 0 :BC 0 :BD 0 :CD 2} nil)
                             (reset "Reset: Euler's seven bridges" kb-init kb-places)])])
              '(finally nil))))))

;; ---------------------------------------------------------------------------
;; 3. V - E + F (E230, Cauchy 1813): the solid's map in the plane

(def vef-solids
  "Schlegel diagrams (the solid seen through one face, that face the outer
   region): vertices lettered A.., their places, the solid's edges, then the
   diagonals Cauchy draws in step 3 (one per bounded face that is not a
   triangle)."
  [{:id :tetrahedron :name "tetrahedron"
    :pts [[0 2.2] [-1.905 -1.1] [1.905 -1.1] [0 0]]
    :edges [[0 1] [1 2] [2 0] [0 3] [1 3] [2 3]] :diagonals []}
   {:id :cube :name "cube"
    :pts [[-2 -2] [2 -2] [2 2] [-2 2] [-0.8 -0.8] [0.8 -0.8] [0.8 0.8] [-0.8 0.8]]
    :edges [[0 1] [1 2] [2 3] [3 0] [4 5] [5 6] [6 7] [7 4] [0 4] [1 5] [2 6] [3 7]]
    :diagonals [[4 6] [0 5] [1 6] [2 7] [3 4]]}
   {:id :octahedron :name "octahedron"
    :pts [[0 2.4] [-2.078 -1.2] [2.078 -1.2] [0.779 0.45] [-0.779 0.45] [0 -0.9]]
    :edges [[0 1] [1 2] [2 0] [3 4] [4 5] [5 3] [3 2] [3 0] [4 0] [4 1] [5 1] [5 2]] :diagonals []}])

(defn- vef-all-edges [s] (vec (concat (:edges s) (:diagonals s))))

(defn vef-start
  "The state of solid i as Euler counts it: every edge on, no diagonal, the
   outer face kept."
  [i]
  (let [s (vef-solids i)]
    {:solid i :pts (vec (mapcat identity (:pts s)))
     :on (vec (concat (repeat (count (:edges s)) true) (repeat (count (:diagonals s)) false)))
     :outer false}))

(defn- step01 [x] (ties/clamp (e/* 1e4 x) 0 1))

(defn pseudo-angle
  "A key for the direction (dx, dy) that rises with its angle, as atan2
   does, but cut at straight down: s = dy / (|dx| + |dy|) on the right
   half, 2 - s on the left, over [-1, 3). The face tracing needs only the
   cyclic order of the half-edges round a vertex, which this key keeps.
   Branch-free and made of + - * / abs only, so the wasm kernel and the JS
   fallback that answers until the wasm has loaded agree. Emmy's JS
   fallback emits a two-argument atan as Math.atan(y, x), which ignores x
   and loses the quadrant."
  [dx dy]
  (let [s (e/divide dy (ties/at-least (e/+ (e/abs dx) (e/abs dy)) 1e-300))
        left (ties/clamp (e/* -1e12 dx) 0 1)]
    (e/+ s (e/* left (e/- 2 (e/* 2 s))))))

(defn vef-values
  "Solid s's kernel: args are the vertex coordinates, one flag per edge
   (the solid's, then the diagonals), Ft (faces traced in the browser from
   this kernel's angles), c (pieces of the drawing with an edge, found by
   a search in the browser) and the outer-face flag. Returns
   [V E X c iso F chi F_net chi_net, deg_0.., angle u->v and v->u of every
   edge]. X counts proper crossings of drawn edges (orientation tests,
   branch-free), iso the vertices with no edge; F = Ft - c + 1 counts the
   regions of the plane, the outer one included (each further piece's
   outer cycle is the same region); F_net drops the outer face (Cauchy's
   step 2). Pieces = c + iso."
  [s]
  (let [nv (count (:pts s)) es (vef-all-edges s) ne (count es)]
    (fn [& args]
      (fn [_]
        (let [args (vec args)
              P (vec (for [i (range nv)] [(args (* 2 i)) (args (inc (* 2 i)))]))
              on (subvec args (* 2 nv) (+ (* 2 nv) ne))
              Ft (args (+ (* 2 nv) ne)) c (args (+ (* 2 nv) ne 1)) outer (args (+ (* 2 nv) ne 2))
              E (reduce e/+ 0 on)
              deg (vec (for [i (range nv)]
                         (reduce e/+ 0 (for [[k [u v]] (map-indexed vector es) :when (or (= u i) (= v i))] (on k)))))
              orient (fn [[px py] [qx qy] [rx ry]] (e/- (e/* (e/- qx px) (e/- ry py)) (e/* (e/- qy py) (e/- rx px))))
              X (reduce e/+ 0 (for [i (range ne) j (range (inc i) ne)
                                    :let [[a b] (es i) [cc d] (es j)]
                                    :when (empty? (filter #{a b} [cc d]))]
                                (e/* (on i) (on j)
                                     (step01 (e/- 0 (e/* (orient (P a) (P b) (P cc)) (orient (P a) (P b) (P d)))))
                                     (step01 (e/- 0 (e/* (orient (P cc) (P d) (P a)) (orient (P cc) (P d) (P b))))))))
              iso (reduce e/+ 0 (for [i (range nv)] (e/- 1 (ties/clamp (deg i) 0 1))))
              pieces (e/+ c iso)
              F (e/+ (e/- Ft c) 1)
              chi (e/+ (e/- nv E) F)
              Fn (e/- F outer)
              angs (for [[u v] es :let [[ux uy] (P u) [vx vy] (P v)]]
                     [(pseudo-angle (e/- vx ux) (e/- vy uy)) (pseudo-angle (e/- ux vx) (e/- uy vy))])]
          (vec (concat [nv E X pieces iso F chi Fn (e/+ (e/- nv E) Fn)] deg (apply concat angs))))))))

(def ^:private vef-trace
  "Browser form: the faces of the drawing, traced from the kernel's angles
   (each half-edge u->v is followed by the edge just clockwise of v->u
   round v): a vector of vertex cycles, the outer region's included."
  '(fn [edges on ang]
     (let [hs (vec (for [e (range (count edges)) :when (nth on e) s [0 1]] [e s]))
           from (fn [[e s]] (nth (nth edges e) s))
           sorted (into {} (map (fn [[v xs]] [v (vec (sort-by (fn [[e s]] (ang e s)) xs))]) (group-by from hs)))
           nxt (fn [[e s]] (let [xs (get sorted (nth (nth edges e) (- 1 s)))
                                 i (first (keep-indexed (fn [i h] (when (= h [e (- 1 s)]) i)) xs))]
                             (nth xs (mod (dec i) (count xs)))))]
       (loop [todo hs seen #{} faces []]
         (if-let [h (first todo)]
           (if (contains? seen h)
             (recur (rest todo) seen faces)
             (let [cyc (loop [g h acc []] (if (and (seq acc) (= g h)) acc (recur (nxt g) (conj acc g))))]
               (recur (rest todo) (into seen cyc) (conj faces (mapv from cyc)))))
           faces)))))

(def ^:private vef-pieces
  "Browser form: how many pieces of the drawing carry an edge (a search
   from each unvisited end of a drawn edge)."
  '(fn [edges on]
     (let [es (keep-indexed (fn [k uv] (when (nth on k) uv)) edges)
           nb (reduce (fn [m [u v]] (-> m (update u conj v) (update v conj u))) {} es)]
       (loop [todo (keys nb) seen #{} n 0]
         (if-let [v (first todo)]
           (if (contains? seen v)
             (recur (rest todo) seen n)
             (recur (rest todo)
                    (loop [st [v] sn (conj seen v)]
                      (if-let [w (peek st)]
                        (let [fresh (remove sn (get nb w))]
                          (recur (into (pop st) fresh) (into sn fresh)))
                        sn))
                    (inc n)))
           n)))))

(def ^:private vef-colours ["#5AA0D0" "#E67E22" "#2f8a3e" "#7B5EA7" "#C8913A" "#1ABC9C" "#D35400" "#8E44AD"])

(defn- vef-case []
  '(cond (> X 0) (str "edges cross (" X "): this drawing is no map of a solid, and V − E + F = " chi " counts nothing")
         (> c 1) (str "the map falls into " c " pieces: V − E + F = 1 + " c " = " chi ", not 2")
         outer (str "Cauchy's net: the outer face taken away, V − E + F = " chi " (step 2)")
         (pos? ndiag) (str "the " nm " with " ndiag " diagonal" (when (> ndiag 1) "s") ": each added one edge and one face, V − E + F = " chi)
         (pos? ncut) (str "the " nm " with " ncut " edge" (when (> ncut 1) "s") " rubbed out: two faces merged each time, V − E + F = " chi)
         :else (str "the map of the " nm ": V − E + F = " chi " (E230)")))

(defn vef-figure
  "Solid i's map: the bounded faces filled (traced), the edges (diagonals
   dashed), the vertices draggable and lettered."
  [!s i]
  (let [{:keys [pts]} (vef-solids i) es (vef-all-edges (vef-solids i)) ne (count (:edges (vef-solids i)))
        P (fn [v] [(list 'nth 'pts (* 2 v)) (list 'nth 'pts (inc (* 2 v)))])]
    (list 'when (list '= 'sel i)
          [:<>
           (list 'for '[[k f] (map-indexed vector inner)]
                 (list 'with-meta
                       (mafs/polygon {:points '(mapv (fn [v] [(nth pts (* 2 v)) (nth pts (inc (* 2 v)))]) f)
                                      :color (list 'nth vef-colours '(mod k 8)) :fill-opacity 0.3 :weight 0})
                       '{:key k}))
           (into [:<>]
                 (for [[k [u v]] (map-indexed vector es)]
                   (list 'when (list 'nth 'on k)
                         (mafs/segment {:point1 (P u) :point2 (P v) :color "#1d1f21" :weight 3
                                        :style (if (>= k ne) "dashed" "solid")}))))
           (into [:<>]
                 (for [v (range (count pts))]
                   [:<>
                    (mafs/movable-point {:point (P v) :color "#C0392B"
                                         :constrain '(fn [[x y]] [(max -3.4 (min 3.4 x)) (max -3 (min 3 y))])
                                         :on-move (list 'fn '[[x y]] (list 'swap! !s 'update :pts 'assoc (* 2 v) 'x (inc (* 2 v)) 'y))})
                    (mafs/text (str (char (+ 65 v))) {:x (first (P v)) :y (second (P v)) :attach "nw" :size 15})]))])))

(defn vef-playground
  "E230 open on Cauchy's map: pick the solid, drag its vertices, rub out
   edges or draw diagonals, take the outer face away."
  []
  (binding [vc/*backend* :raster]
    (ev/with-let [!s (vef-start 1)]
      (let [ks (vec (for [s vef-solids]
                      (let [ne (count (vef-all-edges s))]
                        [(gensym "vef") (kernel (vef-values s) (vec (concat (map double (mapcat identity (:pts s))) (repeat ne 1.0) [0.0 1.0 0.0])))])))
            edges (mapv vef-all-edges vef-solids)
            nedges (mapv (comp count :edges) vef-solids)
            names (mapv :name vef-solids)
            call (fn [ft] (list* 'case 'sel
                                 (mapcat (fn [i [ksym _]]
                                           [i (list ksym '(array 0) (list 'to-array (list 'concat 'pts '(map (fn [b] (if b 1 0)) on) [ft 'npieces '(if outer 1 0)])))])
                                         (range) ks)))
            preset (fn [title state]
                     [:button {:type "button" :data-preset title
                               :style {:margin "0.25em" :padding "0.3em 0.6em" :cursor "pointer" :border-radius "0.3em"
                                       :border "1px solid currentColor" :background "transparent" :color "inherit"}
                               :on-click (list 'fn [] (list 'reset! !s state))}
                      title])
            cube (vef-start 1)
            Fi '(aget r (if outer 7 5))
            chii '(aget r (if outer 8 6))]
        (list 'reagent.core/with-let (vec (mapcat identity ks))
              (list 'let ['sel (ev/get !s :solid) 'pts (ev/get !s :pts) 'on (ev/get !s :on) 'outer (ev/get !s :outer)
                          'es (list 'nth edges 'sel) 'ne (list 'nth nedges 'sel) 'nm (list 'nth names 'sel)
                          'npieces (list vef-pieces 'es 'on)
                          'r0 (list '.slice (call 0))
                          'nv (list 'aget 'r0 0)
                          'ang '(fn [k s] (aget r0 (+ 9 nv (* 2 k) s)))
                          'faces (list vef-trace 'es 'on 'ang)
                          'r (call '(count faces))
                          'area '(fn [f] (* 0.5 (reduce + (map (fn [a b] (- (* (nth pts (* 2 a)) (nth pts (inc (* 2 b))))
                                                                             (* (nth pts (* 2 b)) (nth pts (inc (* 2 a))))))
                                                               f (concat (rest f) [(first f)])))))
                          'big '(when (seq faces) (apply max-key (fn [f] (js/Math.abs (area f))) faces))
                          'inner '(if big (filter (fn [f] (neg? (* (area f) (area big)))) faces) [])
                          'X '(js/Math.round (aget r 2)) 'c '(js/Math.round (aget r 3))
                          'chi (list 'js/Math.round chii)
                          'ndiag '(count (filter true? (subvec on ne)))
                          'ncut '(count (remove true? (subvec on 0 ne)))
                          'ok '(and (zero? X) (= c 1))
                          'case-name (vef-case)]
                    [:div {:data-figure "euler-vef-playground" :data-case 'case-name :data-chi '(str chi)
                           :data-vef (list 'str '(aget r 0) " " '(aget r 1) " " Fi) :data-crossings '(str X)}
                     (frame "euler-vef"
                            [(into (mafs/mafs {:height 420 :view-box {:x [-3.5 3.5] :y [-3.1 3.1]}})
                                   (for [i (range (count vef-solids))] (vef-figure !s i)))
                             [:div {:style {:font-size "0.85em" :opacity 0.75 :margin-top "0.2em"}}
                              "the solid seen through one face (Schlegel): that face is the outer region · drag the vertices · dashed: Cauchy's diagonals"]]
                            [:div
                             (into [:div {:style {:display "flex" :flex-wrap "wrap" :gap "0.3em" :margin-bottom "0.4em"}}]
                                   (for [i (range (count vef-solids))]
                                     [:button {:data-solid i :on-click (list 'fn [] (list 'reset! !s (vef-start i)))
                                               :style {:font-size "0.85em" :padding "0.1em 0.5em" :border-radius "0.3em" :cursor "pointer"
                                                       :border "1px solid currentColor" :color "inherit"
                                                       :background (list 'if (list '= 'sel i) "rgba(192,57,43,0.15)" "transparent")}}
                                      (names i)]))
                             [:div {:style {:font-size "0.85em"}} "edges (click to rub out or draw):"]
                             [:div {:style {:display "flex" :flex-wrap "wrap" :gap "0.2em" :margin "0.2em 0 0.4em"}}
                              (list 'for '[[k [u v]] (map-indexed vector es)]
                                    [:button {:key 'k :data-edge 'k :on-click (list 'fn [] (list 'swap! !s 'update :on 'update 'k 'not))
                                              :style {:font-size "0.8em" :padding "0 0.35em" :border-radius "0.25em" :cursor "pointer"
                                                      :border '(if (>= k ne) "1px dashed currentColor" "1px solid currentColor")
                                                      :color "inherit" :opacity '(if (nth on k) 1 0.35)
                                                      :background "transparent"}}
                                     '(str (char (+ 65 u)) (char (+ 65 v)))])]
                             [:label {:style {:display "block" :margin "0.3em 0"}}
                              [:input {:type "checkbox" :data-outer true :checked 'outer
                                       :on-change (list 'fn '[_] (list 'swap! !s 'update :outer 'not))}]
                              " take the outer face away (Cauchy's step 2)"]
                             [:strong {:data-case true :style {:color '(if ok "#2E8B57" "#C0392B")}} 'case-name]
                             [:div {:style {:margin-top "0.4em"}}
                              "V = " (fmt '(aget r 0) 0) ", E = " (fmt '(aget r 1) 0) ", F = " (fmt Fi 0)
                              (list 'if 'outer " (the outer face taken away)" " (the outer region counted)")]
                             [:div "V − E + F = " (fmt '(aget r 0) 0) " − " (fmt '(aget r 1) 0) " + " (fmt Fi 0)
                              " = " [:span {:data-chi true} (fmt chii 0)]]
                             [:div "in Euler's letters: S + H = " (fmt '(+ (aget r 0) (aget r 5)) 0) ", A + 2 = " (fmt '(+ (aget r 1) 2) 0)]
                             [:div {:style {:opacity 0.8}} "crossings " (fmt '(aget r 2) 0) " · pieces " (fmt '(aget r 3) 0)]
                             (badge 'ok "a connected map with no crossings: V − E + F = 2, or 1 without the outer face"
                                    "hypothesis broken: Cauchy's map must be connected and drawn without crossings"
                                    {:data-hypothesis "vef"})
                             [:p {:style {:margin "0.6em 0 0.2em"}} [:strong "Try this:"]]
                             (preset "Draw Cauchy's diagonals (step 3): each adds an edge and a face"
                                     (assoc cube :on (vec (repeat 17 true)) :outer true))
                             (preset "Rub out the four side edges: the map falls in two"
                                     (assoc cube :on (vec (concat (repeat 8 true) (repeat 9 false)))))
                             (preset "Pull corner E out across the square: edges cross"
                                     (assoc cube :pts (assoc (:pts cube) 8 2.8 9 0.4)))
                             (preset "Reset: Euler's cube" cube)])])
              '(finally nil))))))
