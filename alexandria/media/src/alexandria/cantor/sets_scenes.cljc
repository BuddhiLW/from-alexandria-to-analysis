(ns alexandria.cantor.sets-scenes
  "The drawings of Cantor's counting (alexandria.cantor.sets), one method
   per stage (alexandria.medium.scene/draw), in Manim's idiom: a pointer
   walks the table of fractions diagonal by diagonal; the numbers of each
   height drop onto the line; the diagonal of a table of m/w rows is read,
   flipped, and shown to differ from every row.

   Three scene ids share these helpers: :cantor/zigzag, :cantor/heights,
   :cantor/diagonal. ctx :data carries the walk (:walk), the heights
   (:heights) and the table (:rows); ctx :figures :walk places the k-th
   cell of the walk, moving along it at time t."
  (:require [alexandria.medium.anim :as a]
            [alexandria.medium.figure :as figure]
            [alexandria.medium.math :as m]
            [alexandria.medium.scene :as scene]
            [alexandria.medium.svg :as svg]
            #?(:cljs [alexandria.medium.player :as player])))

#?(:cljs (def render "Clerk's render-fn for these scenes." player/render))

(defn- write [palette at s & [opts]]
  (svg/text at s (merge {:colour (:ink palette) :size 0.16} opts)))

(defn- caption [palette f at lines]
  (svg/layer (a/fade f [0 -0.1])
             (into [:g] (map-indexed (fn [i s] (write palette [(first at) (- (second at) (* 0.24 i))] s
                                                     {:anchor "start" :size 0.13 :mono? true}))
                                     lines))))

;; ---------------------------------------------------------------------------
;; The zig-zag

(def ^:private cell 0.42)
(def ^:private grid-n 7)

(defn- cell-at [[p q]] [(* cell (- p 1)) (- (* cell (- q 1)))])

(defn- grid
  "The table of fractions p/q, p, q <= grid-n; cells not in lowest terms
   dimmed by dim (0..1)."
  [palette f dim]
  (into [:g]
        (for [q (range 1 (inc grid-n)) p (range 1 (inc grid-n))
              :let [g (if (= 1 (loop [a p b q] (if (zero? b) a (recur b (mod a b))))) 1 (- 1 (* 0.75 dim)))]]
          (svg/layer (a/fade (* f g))
                     (write palette (cell-at [p q]) (str p "/" q) {:size 0.12 :mono? true})))))

(defn- path-of [cells] (mapv (fn [c] (let [[x y] (cell-at c)] [x (+ y 0.04)])) cells))

(defmethod scene/draw [:cantor/zigzag :grid] [_ _ p {:keys [palette]}]
  [:g (grid palette (a/play p 0 0.6) 0)
   (caption palette (a/play p 0.5 0.8) [3.1 0.1] ["row q, column p:" "every p/q is here"])])

(defn- walker
  "The pointer at fraction u of the first k cells of the walk (from the
   :walk figure), and the path walked so far."
  [palette figures cells u]
  (let [n (count cells)
        t (* u (dec n))
        i (long (m/floor t))
        done (take (inc i) cells)
        [x y] (first (figure/points (:walk figures) [t] [[0]]))]
    [:g (svg/polyline (conj (path-of done) [x (+ y 0.04)]) {:stroke (:construction palette) :width 0.025})
     (svg/circle [x (+ y 0.04)] 0.07 {:fill (:found palette)})]))

(defmethod scene/draw [:cantor/zigzag :walk] [_ _ p {:keys [palette data figures]}]
  (let [cells (:walk data)]
    [:g (grid palette 1 0)
     (walker palette figures cells (a/play p 0.05 0.95 a/linear))
     (caption palette (a/play p 0 0.2) [3.1 0.1] ["diagonals p + q = 2, 3, 4, ..." "each finite: the walk" "reaches every cell"])]))

(defmethod scene/draw [:cantor/zigzag :skip] [_ _ p {:keys [palette data figures]}]
  (let [cells (:walk data)]
    [:g (grid palette 1 (a/play p 0.1 0.5))
     (walker palette figures cells 1)
     (caption palette (a/play p 0.4 0.7) [3.1 0.1] ["2/2, 2/4, 3/3, ... skipped:" "counted already as 1, 1/2, 1"])]))

(defmethod scene/draw [:cantor/zigzag :count] [_ _ p {:keys [palette data]}]
  (let [xs (:list data)
        fs (a/lagged p (count xs) 0.35 0 0.9)]
    (into [:g (grid palette 0.35 1)]
          (map-indexed (fn [i [[label] f]]
                         (svg/layer (a/fade f [0 0.2])
                                    (write palette [(+ 3.1 (* 0.62 (mod i 4))) (- 0.1 (* 0.3 (quot i 4)))]
                                           (str (inc i) ": " label) {:size 0.12 :anchor "start" :mono? true
                                                                     :colour (:found palette)})))
                       (map vector (map vector xs) fs)))))

;; ---------------------------------------------------------------------------
;; Heights

(def ^:private line-scale 0.9)

(defmethod scene/draw [:cantor/heights :equation] [_ _ p {:keys [palette]}]
  [:g (svg/layer (a/fade (a/play p 0 0.4) [0 -0.2])
                 (write palette [0 0.6] "a₀ωⁿ + a₁ωⁿ⁻¹ + … + aₙ = 0" {:size 0.24}))
   (caption palette (a/play p 0.4 0.8) [-2.6 0] ["n, a₀ > 0, no common divisor, irreducible:"
                                                 "one definite equation for every ω"])])

(defmethod scene/draw [:cantor/heights :height] [_ _ p {:keys [palette]}]
  [:g (write palette [0 0.6] "a₀ωⁿ + a₁ωⁿ⁻¹ + … + aₙ = 0" {:size 0.2 :colour (:muted palette)})
   (svg/layer (a/fade (a/play p 0.1 0.5) [0 -0.2])
              (write palette [0 0.05] "N = n − 1 + |a₀| + |a₁| + … + |aₙ|" {:size 0.24 :colour (:found palette)}))
   (caption palette (a/play p 0.5 0.8) [-2.6 -0.5] ["2ω − 1 = 0:  N = 0 + 2 + 1 = 3,  ω = 1/2"
                                                    "ω² − 2 = 0:  N = 1 + 1 + 2 = 4,  ω = ±√2"])])

(defn- height-row
  "The numbers of height h on the line at y, faded in by f."
  [palette nums y f colour]
  (into [:g (svg/layer (a/fade f) (write palette [-3.2 (- y 0.05)] (str "N = " (:N (first nums)))
                                         {:size 0.12 :anchor "start" :mono? true}))]
        (map (fn [{:keys [x label]}]
               (svg/layer (a/fade f [0 0.3])
                          (svg/circle [(* line-scale x) y] 0.05 {:fill colour})
                          (write palette [(* line-scale x) (+ y 0.12)] label {:size 0.1 :mono? true})))
             nums)))

(defn- axis [palette y]
  (svg/segment [-3.0 y] [3.0 y] {:stroke (:muted palette) :width 0.01}))

(defmethod scene/draw [:cantor/heights :finite] [_ _ p {:keys [palette data]}]
  (let [rows (:heights data)
        fs (a/lagged p (count rows) 0.7 0 0.9)]
    (into [:g]
          (map-indexed (fn [i [nums f]]
                         [:g (axis palette (- 0.8 (* 0.55 i)))
                          (height-row palette nums (- 0.8 (* 0.55 i)) f (:construction palette))])
                       (map vector rows fs)))))

(defmethod scene/draw [:cantor/heights :first-heights] [_ _ p {:keys [palette data]}]
  (let [rows (take 3 (:heights data))]
    (into [:g (caption palette (a/play p 0.3 0.7) [-3.2 -1.0] ["φ(1) = 1   φ(2) = 2   φ(3) = 4"])]
          (map-indexed (fn [i nums]
                         [:g (axis palette (- 0.8 (* 0.55 i)))
                          (height-row palette nums (- 0.8 (* 0.55 i)) 1 (:found palette))])
                       rows))))

(defmethod scene/draw [:cantor/heights :list] [_ _ p {:keys [palette data]}]
  (let [xs (mapcat identity (:heights data))
        fs (a/lagged p (count xs) 0.4 0 0.95)]
    (into [:g (caption palette 1 [-3.2 1.0] ["ω₁, ω₂, ω₃, …  height by height, by size:"])]
          (map-indexed (fn [i [{:keys [label]} f]]
                         (svg/layer (a/fade f [0.2 0])
                                    (write palette [(+ -3.0 (* 0.75 (mod i 8))) (- 0.55 (* 0.3 (quot i 8)))]
                                           (str "ω" (inc i) "=" label) {:size 0.1 :anchor "start" :mono? true})))
                       (map vector xs fs)))))

;; ---------------------------------------------------------------------------
;; The diagonal

(def ^:private dc 0.3)

(defn- table-cell [palette i j c highlight]
  (let [x (* dc j) y (- (* dc i))]
    [:g (when (pos? highlight)
          (svg/polygon [[(- x 0.13) (- y 0.12)] [(+ x 0.13) (- y 0.12)] [(+ x 0.13) (+ y 0.16)] [(- x 0.13) (+ y 0.16)]]
                       {:fill (:found palette) :opacity (* 0.35 highlight)}))
     (write palette [x y] (if (= c :m) "m" "w") {:size 0.15 :mono? true
                                                 :colour (if (= c :m) (:ink palette) (:construction palette))})]))

(defn- table [palette rows f diag-f]
  (let [n (count rows)]
    (into [:g]
          (concat
           (for [i (range n)]
             (svg/layer (a/fade (a/play f (/ i n 1.5) (+ 0.35 (/ i n 1.5))))
                        (write palette [-0.5 (- (* dc i))] (str "E" (inc i)) {:size 0.13 :mono? true :colour (:muted palette)})))
           (for [i (range n) j (range n)
                 :let [g (a/play f (/ i n 1.5) (+ 0.35 (/ i n 1.5)))]]
             (svg/layer (a/fade g) (table-cell palette i j (nth (nth rows i) j)
                                               (if (= i j) (nth diag-f i 0) 0))))))))

(defmethod scene/draw [:cantor/diagonal :elements] [_ _ p {:keys [palette data]}]
  (let [rows (take 3 (:rows data))]
    [:g (table palette rows (a/play p 0 0.6) [])
     (caption palette (a/play p 0.5 0.8) [2.4 0.1] ["each E: m or w," "place after place"])]))

(defmethod scene/draw [:cantor/diagonal :table] [_ _ p {:keys [palette data]}]
  [:g (table palette (:rows data) (a/play p 0 0.9 a/linear) [])
   (caption palette (a/play p 0.6 0.9) [2.4 0.1] ["suppose this list" "held all of M"])])

(defmethod scene/draw [:cantor/diagonal :diagonal] [_ _ p {:keys [palette data]}]
  (let [n (count (:rows data))]
    [:g (table palette (:rows data) 1 (a/lagged p n 0.5 0 0.9))
     (caption palette (a/play p 0.7 0.95) [2.4 0.1] ["a₁,₁, a₂,₂, a₃,₃, …"])]))

(defn- flipped-row [palette rows f y]
  (let [n (count rows)
        fs (a/lagged f n 0.5 0 1)]
    (into [:g (svg/layer (a/fade (first fs)) (write palette [-0.5 y] "E0" {:size 0.13 :mono? true :colour (:found palette)}))]
          (map (fn [i g]
                 (let [c (nth (nth rows i) i)
                       b (if (= c :m) :w :m)
                       from [(* dc i) (- (* dc i))]
                       to [(* dc i) y]
                       [x yy] [(first from) (+ (second from) (* g (- (second to) (second from))))]]
                   (when (pos? g)
                     (write palette [x yy] (if (= b :m) "m" "w") {:size 0.15 :mono? true :colour (:found palette)}))))
               (range n) fs))))

(defmethod scene/draw [:cantor/diagonal :flip] [_ _ p {:keys [palette data]}]
  (let [rows (:rows data) n (count rows)
        y (- (* dc (+ n 0.6)))]
    [:g (table palette rows 1 (repeat n 1))
     (flipped-row palette rows (a/play p 0.05 0.9) y)
     (caption palette (a/play p 0.6 0.9) [2.4 0.1] ["bν = w where aν,ν = m," "bν = m where aν,ν = w"])]))

(defmethod scene/draw [:cantor/diagonal :differs] [_ _ p {:keys [palette data]}]
  (let [rows (:rows data) n (count rows)
        y (- (* dc (+ n 0.6)))
        k (min (dec n) (long (m/floor (* n (a/play p 0 0.95 a/linear)))))]
    [:g (table palette rows 1 (map #(if (= % k) 1 0.3) (range n)))
     (flipped-row palette rows 1 y)
     (svg/segment [(* dc k) (- (* dc k))] [(* dc k) (+ y 0.1)] {:stroke (:found palette) :width 0.02 :dash "0.04 0.04"})
     (caption palette 1 [2.4 0.1] [(str "E0 ≠ E" (inc k) " at place " (inc k))
                                   "so E0 is in no row"])]))

(defmethod scene/draw [:cantor/diagonal :reals] [_ _ p {:keys [palette data]}]
  (let [rows (:rows data) n (count rows)
        xs (:xs data)
        fs (a/lagged p n 0.4 0 0.8)]
    (into [:g (caption palette (a/play p 0.7 0.95) [-0.6 (- (* dc (+ n 1.2)))]
                       ["m = 0, w = 1: each row is a binary expansion;" "the flipped diagonal is a number missing from the list"])]
          (map (fn [i f]
                 (svg/layer (a/fade f [0.2 0])
                            (write palette [-0.5 (- (* dc i))]
                                   (str "0." (apply str (map #(if (= % :m) "0" "1") (nth rows i))) "…  ≈ " (nth xs i))
                                   {:size 0.13 :anchor "start" :mono? true})))
               (range n) fs))))
