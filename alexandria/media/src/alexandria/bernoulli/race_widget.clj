(ns alexandria.bernoulli.race-widget
  "Bernoulli's problem as tied controls (widget brief, section 5; the bar
   is the Apollonius conics playground): the end point B = (X, Y), the
   radius r of the rolling circle, the end angle theta and the descent time
   T of the cycloid through A and B, every one a Leva control, tied by the
   equations of alexandria.bernoulli.brachistochrone:

     X = r (theta - sin theta),  Y = r (1 - cos theta),  T = theta sqrt(r / g)

     X moves       Y held: theta solved (Newton), then r and T
     Y moves       X held: the same
     r moves       theta held: B = r (theta - sin, 1 - cos), then T
     theta moves   r held: B follows, then T
     T moves       theta held: r = g T^2 / theta^2, then B

   Every source first gives a candidate B, clamped to the panel's box; the
   state is then solved from B, so no key leaves its range (a source is
   held at the edge). The end angle comes from p = theta / 2, the root of
   p - sin p cos p - (X / Y) sin^2 p, by Newton steps, each step one raster
   kernel the page calls ten times; the start is a closed form exact at
   both ends of X / Y. The race beside the panel: the cycloid, Galileo's
   circle arc and the straight line, each sampled with its time by one
   raster kernel (the arc's times by Gauss-Legendre in w = sqrt(angle)),
   the beads moved by lerping those samples, the winner named live."
  (:require [alexandria.bernoulli.brachistochrone :as b]
            [alexandria.medium.tie :as tie]
            [alexandria.raster :as raster]
            [emmy.clerk :as ec]
            [emmy.env :as e]
            [emmy.leva :as leva]
            [emmy.mafs :as mafs]
            [emmy.viewer :as ev]
            [emmy.viewer.compile :as vc]
            [emmy.viewer.raster]
            [nextjournal.clerk :as clerk]
            [clojure.walk]))

(def g b/g)

(def tied [:X :Y :r :theta :T])

(def box {:X [0.5 4] :Y [0.5 2]})

(def ranges
  "[min max step]. B's box bounds the others: X/Y in [1/4, 8] puts theta in
   about [0.72, 5.6] and r in about [0.25, 8.1]."
  {:X [0.5 4 0.01] :Y [0.5 2 0.01] :r [0.2 8.5 0.001] :theta [0.5 6.2 0.001] :T [0.1 4 0.001]})

(def labels {:X "X across (m)" :Y "Y down (m)" :r "r circle (m)" :theta "θ end angle" :T "T time (s)"})

(defn- lo [k] (first (ranges k)))
(defn- hi [k] (second (ranges k)))

;; ---------------------------------------------------------------------------
;; Kernels (Emmy; raster turns each into wasm in the page, ftm on the JVM)

(defn- xy-clamped [X Y]
  [(tie/clamp X (first (box :X)) (second (box :X)))
   (tie/clamp Y (first (box :Y)) (second (box :Y)))])

(defn candidates
  "The candidate B of each source, in the order of `tied`: 10 numbers."
  [X Y r th T]
  (fn [_]
    (let [fwd (fn [rr t] (b/cycloid-point rr t))
          th' (tie/clamp th (lo :theta) (hi :theta))
          r' (tie/clamp r (lo :r) (hi :r))
          rT (e// (e/* g (e/square (tie/clamp T (lo :T) (hi :T)))) (e/square th))]
      (vec (mapcat #(apply xy-clamped %)
                   [[X Y] [X Y] (fwd r' th) (fwd r th') (fwd rT th)])))))

(defn p-start
  "The first guess for p = theta/2 from k = X/Y: 1.5 pi k / (pi + 1.5 k),
   right as k -> 0 (p ~ 1.5 k) and as k -> infinity (p -> pi)."
  [X Y]
  (fn [_] (let [k (e// X Y)] [(e// (e/* 1.5 Math/PI k) (e/+ Math/PI (e/* 1.5 k)))])))

(defn p-step
  "One Newton step on p - sin p cos p - k sin^2 p, held in (0, pi)."
  [X Y p]
  (fn [_]
    (let [k (e// X Y) s (e/sin p) c (e/cos p)
          f (e/- p (e/* s c) (e/* k s s))
          df (e/* 2 s (e/- s (e/* k c)))]
      [(tie/clamp (e/- p (e// f df)) 1e-4 (- Math/PI 1e-4))])))

(defn from-xy
  "The state from B and its end angle: [X Y r theta T]. Declare the
   positive radicand to Emmy: without abs it chooses a complex branch."
  [X Y th]
  (fn [_] (let [r (b/radius-of Y th)]
            [X Y r th (e/* th (e/sqrt (e/abs (e// r g))))])))

(def newton-steps 10)

(def n-samples 24)

(def ^:private gl6
  "Gauss-Legendre, 6 nodes on [-1 1]."
  [[-0.9324695142031521 0.1713244923791704] [-0.6612093864662645 0.3607615730481386]
   [-0.2386191860831909 0.4679139345726910] [0.2386191860831909 0.4679139345726910]
   [0.6612093864662645 0.3607615730481386] [0.9324695142031521 0.1713244923791704]])

(defn- arc-time-6
  "The arc's time from A to the angle tu: sqrt(c/2g) times 6-point
   Gauss-Legendre of 2w/sqrt(sin w^2) on [0, sqrt tu]."
  [c tu]
  (let [bb (e/sqrt tu)]
    (e/* (e/sqrt (e// c (e/* 2 g)))
         (apply e/+ (for [[x wt] gl6]
                      (let [w (e/* 0.5 bb (e/+ x 1))]
                        (e/* wt bb (e// w (e/sqrt (e/sin (e/square w)))))))))))

(def readout-index
  "Where each number sits: the three times, the arc's centre, then for
   each of cycloid, arc, line n-samples + 1 triples [t x depth]."
  {:Tc 0 :Ta 1 :Tl 2 :c 3 :samples 4})

(defn readout
  "The race by raster, from the exact state: [Tc Ta Tl c, samples...]."
  [X Y r th _T]
  (fn [_]
    (let [n n-samples
          c (b/arc-centre X Y)
          tb (b/arc-angle X Y)
          Tc (b/descent-time r th)
          Ta (b/arc-time X Y)
          Tl (b/line-time X Y)
          us (map #(/ (double %) n) (range (inc n)))
          cyc (for [u us] (let [t (e/* u th) [x y] (b/cycloid-point r t)] [(e/* u Tc) x y]))
          arc (for [u us] (let [t (e/* u tb) [x y] (b/arc-point c t)]
                            [(if (zero? u) 0 (arc-time-6 c t)) x y]))
          line (for [u us] [(e/* (e/sqrt u) Tl) (e/* u X) (e/* u Y)])]
      (vec (concat [Tc Ta Tl c] (apply concat cyc) (apply concat arc) (apply concat line))))))

;; ---------------------------------------------------------------------------
;; The tie on the JVM (raster), for grades and the initial state

(defn solve-theta
  "theta through B by the page's Newton steps (raster kernels)."
  [X Y]
  (let [p0 (first (raster/value (fn [x y] ((p-start x y) 0)) X Y))]
    (* 2 (reduce (fn [p _] (first (raster/value (fn [x y q] ((p-step x y q) 0)) X Y p))) p0 (range newton-steps)))))

(defn solve
  "{source [X Y r theta T]}: what the page's tie gives for each source."
  [[X Y r th T]]
  (let [cand (raster/value (fn [a b' c d f] ((candidates a b' c d f) 0)) X Y r th T)]
    (into {} (map-indexed
              (fn [i k] (let [x (nth cand (* 2 i)) y (nth cand (inc (* 2 i)))]
                          [k (raster/value (fn [a b' t] ((from-xy a b' t) 0)) x y (solve-theta x y))]))
              tied))))

(defn init
  "Start at B = (2, 1), by raster."
  []
  (let [[X Y] b/example]
    (zipmap tied (raster/value (fn [a b' t] ((from-xy a b' t) 0)) X Y (solve-theta X Y)))))

(defn grades
  "The tie's grades: Newton = Brent, and every source's solve is a fixed
   point (solving the solved state from any key changes nothing)."
  []
  (let [pts [[2.0 1.0] [0.5 2.0] [4.0 0.5] [1.0 1.0] [Math/PI 2.0]]
        dth (for [[X Y] pts] (- (solve-theta X Y) (:theta (b/through X Y))))
        s0 (mapv (init) tied)
        sol (solve s0)
        fixed (for [k tied [a b'] (map vector s0 (sol k))] (- a b'))
        moved (solve (assoc s0 2 1.0))
        again (solve (moved :r))
        idem (for [k tied [a b'] (map vector (moved :r) (again k))] (- a b'))]
    [{:label "θ by the page's Newton kernel = θ by raster's Brent, five end points"
      :grade (if (every? #(< (Math/abs %) 1e-9) dth) :grade/numeric :grade/fails)}
     {:label "forward then inverse: solving the state from any of X, Y, r, θ, T returns it"
      :grade (if (every? #(< (Math/abs %) 1e-9) (concat fixed idem)) :grade/numeric :grade/fails)}]))

;; ---------------------------------------------------------------------------
;; The page

(def colours {:cycloid "#C0392B" :arc "#1F6FB4" :line "#7f8c8d"})
(def track-names {:cycloid "the cycloid" :arc "Galileo's circle arc" :line "the straight line"})

(defn- tie-fn-form
  "Browser form: the tie the kit's watch calls as (tie (array 0) state):
   candidates, ten Newton kernel steps per source, then the state."
  [[cs _] [ps _] [ss _] [fs _]]
  (list 'fn ['_ 'st]
        (list 'let ['cand (list cs '(array 0) 'st)]
              (list 'let ['out '(array)]
                    (list 'dotimes ['i 5]
                          (list 'let ['x '(aget cand (* 2 i)) 'y '(aget cand (inc (* 2 i)))
                                      'p (list 'loop ['p (list 'aget (list ps '(array 0) '(array x y)) 0) 'j 0]
                                               (list 'if (list '< 'j newton-steps)
                                                     (list 'recur (list 'aget (list ss '(array 0) '(array x y p)) 0) '(inc j))
                                                     'p))
                                      's (list fs '(array 0) '(array x y (* 2 p)))]
                                '(dotimes [j 5] (.push out (aget s j)))))
                    'out))))

(defn- n [i d] (tie/fmt (list 'aget 'r i) d))

(defn- bead-form
  "Browser form: [x depth] of the bead on track `j` (0 cycloid, 1 arc, 2
   line) at time tau, by lerping the readout's samples."
  [j]
  (let [base (+ (readout-index :samples) (* j 3 (inc n-samples)))]
    (list 'let ['base base 'm n-samples
                'at (list 'fn ['i 'q] '(aget r (+ base (* 3 i) q)))
                'i (list 'loop ['i 0] '(if (and (< i m) (> tau (at (inc i) 0))) (recur (inc i)) i))
                't0 '(at i 0) 't1 '(at (min m (inc i)) 0)
                'f '(if (> t1 t0) (min 1 (max 0 (/ (- tau t0) (- t1 t0)))) 1)]
          '[(+ (at i 1) (* f (- (at (min m (inc i)) 1) (at i 1))))
            (+ (at i 2) (* f (- (at (min m (inc i)) 2) (at i 2))))])))

(defn- figure [!s]
  (let [pf (fn [params f] (ev/with-params {:atom !s :params params} f))]
    (mafs/mafs
     {:view-box {:x [-0.2 4.3] :y [-2.6 0.4]} :height 360 :preserve-aspect-ratio false}
     (mafs/cartesian {:subdivisions 2})
     (mafs/parametric {:xy (pf [:r :theta] (fn [r th] (fn [u] (let [[x y] (b/cycloid-point r (e/* u th))] [x (e/- y)]))))
                       :t [0 1] :color (colours :cycloid) :weight 3})
     (mafs/parametric {:xy (pf [:X :Y] (fn [X Y] (fn [u] (let [[x y] (b/arc-point (b/arc-centre X Y) (e/* u (b/arc-angle X Y)))] [x (e/- y)]))))
                       :t [0 1] :color (colours :arc) :weight 2.5})
     (mafs/parametric {:xy (pf [:X :Y] (fn [X Y] (fn [u] [(e/* u X) (e/- (e/* u Y))])))
                       :t [0 1] :color (colours :line) :weight 2.5})
     '[beads])))

(defn- case-form []
  (list 'let ['th (list 'get (list 'deref '?s) :theta)]
        '(cond (< (js/Math.abs (- th js/Math.PI)) 0.005)
               "θ = π: B is the lowest point of the cycloid (Bernoulli's B = (πa, 2a))"
               (< th js/Math.PI) "θ < π: the bead is still descending when it reaches B"
               :else "θ > π: the cycloid dips below B and climbs back up to it")))

(defn- template [{:keys [cand start step state ro !p !s !x]}]
  (let [[rsym rform] ro]
    (clojure.walk/postwalk-replace
     {'?s !s}
     (list 'reagent.core/with-let
           [(first cand) (second cand) (first start) (second start) (first step) (second step)
            (first state) (second state) rsym rform
            'tie (tie-fn-form cand start step state)
            'pushing '(atom false)
            'pushed '(atom {})
            '_ (tie/watch-form 'tie !p !s tied ranges "__brachSolves")
            'clock '(reagent.core/atom 0)
            'raf '(atom nil)
            't0 '(atom nil)
            '_ (list 'let ['step (list 'fn 'step ['now]
                                       (list 'if (ev/get !x :race)
                                             '(do (when-not @t0 (reset! t0 now))
                                                  (reset! clock (/ (- now @t0) 1000)))
                                             '(do (reset! t0 nil) (reset! clock 0)))
                                       '(reset! raf (js/requestAnimationFrame step)))]
                     '(reset! raf (js/requestAnimationFrame step)))]
           (list 'let ['r (list rsym '(array 0) (tie/state-array #(ev/get !s %) tied))
                       'times '[[:cycloid (aget r 0)] [:arc (aget r 1)] [:line (aget r 2)]]
                       'order '(mapv first (sort-by second times))
                       'slowest '(apply max (map second times))
                       'tau '(let [c @clock] (if (pos? c) (mod c (+ slowest 0.8)) 0))
                       'beads (list 'fn []
                                    (into [:<>]
                                          (for [[j k] [[0 :cycloid] [1 :arc] [2 :line]]]
                                            (list 'let ['[x y] (bead-form j)]
                                                  (mafs/point {:x 'x :y '(- y) :color (colours k)})))))
                       'X (ev/get !s :X) 'Y (ev/get !s :Y)]
                 [:div {:data-figure "brachistochrone-race"
                        :data-order '(clojure.string/join "," (map name order))
                        :data-times '(js/JSON.stringify (clj->js (map second times)))}
                  [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
                   [:div {:style {:flex "1 1 460px" :min-width "320px"}} (figure !s)
                    [:div {:style {:font-size "0.85em" :opacity 0.8}}
                     "A at the origin, B = (X, −Y). Red: the cycloid; blue: Galileo's circle arc, tangent to the vertical at A; grey: the chord. Press race: the three beads start together."]]
                   [:div {:style {:flex "0 1 330px" :min-width "270px"}}
                    (leva/sub-panel {:fill true :flat true :title-bar false}
                                    (leva/controls {:atom !p :schema (tie/schema tied ranges labels)})
                                    (leva/controls {:atom !x :schema {:race {:label "race"}}}))
                    [:div {:style {:font-size "1.15em" :font-weight 600 :margin "0.4em 0"}}
                     "winner: "
                     [:span {:data-winner '(name (first order))
                             :style {:color '(get {:cycloid "#C0392B" :arc "#1F6FB4" :line "#7f8c8d"} (first order))}}
                      '(get {:cycloid "the cycloid" :arc "Galileo's circle arc" :line "the straight line"} (first order))]]
                    [:div {:data-case "1" :style {:font-weight 600 :margin "0.2em 0"}} (case-form)]
                    (list 'let ['tex (list 'str "T = \\theta\\sqrt{r/g} = " (tie/fmt (ev/get !s :theta) 3)
                                           "\\sqrt{" (tie/fmt (ev/get !s :r) 3) "/9.81} = " (n 0 4) "\\,\\mathrm{s}")]
                          '[:div {:data-formula tex} [:f> nextjournal.clerk.render/render-katex tex {:inline? false}]])
                    (list 'let ['tex (list 'str "X = r(\\theta - \\sin\\theta) = " (tie/fmt 'X 3)
                                           ",\\; Y = r(1 - \\cos\\theta) = " (tie/fmt 'Y 3))]
                          '[:div [:f> nextjournal.clerk.render/render-katex tex {:inline? false}]])
                    [:div {:style {:font-family "ui-monospace, monospace" :font-size "0.9em" :line-height 1.6}}
                     '(for [[i k] (map-indexed vector order)]
                        ^{:key k}
                        [:div {:style {:color (get {:cycloid "#C0392B" :arc "#1F6FB4" :line "#7f8c8d"} k)}}
                         (str (inc i) ". " (get {:cycloid "cycloid" :arc "circle arc" :line "line"} k) "  "
                              (.toFixed (second (first (filter #(= k (first %)) times))) 4) " s")])
                     [:div {:style {:opacity 0.7}} "τ = " '(.toFixed tau 2) " s"]]]]])
           (list 'finally (tie/unwatch-form !p) '(some-> @raf js/cancelAnimationFrame))))))

(defn widget
  "The race widget, an emmy-viewers fragment for a Clerk notebook mounted
   after (history-of-math.widgets/install!), compiled on the raster
   backend whatever the caller's binding."
  ([] (widget (init)))
  ([start]
   (binding [vc/*backend* :raster]
     (let [s (mapv start tied)
           [X Y _ th] s]
       (ev/with-let [!p start !s start !x {:race false}]
         (template {:cand (tie/kernel-binding "cand" candidates s)
                    :start (tie/kernel-binding "pstart" p-start [X Y])
                    :step (tie/kernel-binding "pstep" p-step [X Y (/ th 2)])
                    :state (tie/kernel-binding "state" from-xy [X Y th])
                    :ro (tie/kernel-binding "readout" readout s)
                    :!p !p :!s !s :!x !x}))))))

(defn derivation
  "Emmy's derivation for the page, then the grades (the derivation's and
   the tie's)."
  []
  (apply clerk/col
         (concat
          (for [{:keys [label eqs]} (b/equations)]
            (apply clerk/col
                   (clerk/html [:div {:style {:font-size "0.85em" :opacity 0.7 :margin-top "0.5em"}} label])
                   (for [q eqs] (ec/->TeX q :simplify? true)))))))
