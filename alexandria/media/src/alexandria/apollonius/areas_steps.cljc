(ns alexandria.apollonius.areas-steps
  "The application of areas drawn step by step, from the Pythagorean
   problem in Euclid to the three names of Apollonius' sections: each
   sentence of resources/alexandria/apollonius/areas_steps.edn is one step,
   and the plane figure grows by exactly what the sentence introduces.

     A1  I.44: the line AB = a, the triangle C of area S, the rectangle of
         height S/a laid on AB
     A2  VI.28: a height y, the full strip a.y on AB
     A3  the leftover at the B end must be a square: XB = y
     A4  the applied rectangle on AX = a - y: (a - y) y = S
     A5  y (and S) live on a Leva panel; the graph of (a - y) y against
         the level S: two roots (3 and 7 at a = 10, S = 21), the top
         (a/2)^2 = 25 of VI.27, no root above it
     A6  VI.29: the rectangle runs past B, the overhang a square:
         (a + y) y = S; the graph turns into a rising one, one root always
     A7  the conic: the line becomes p, the height the abscissa x, the
         area the square y^2 on the ordinate (Heath 1896, p. lxxx)
     A8  applied exactly: y^2 = p x, the parabola
     A9  the leftover a rectangle shaped like d x p, x high, (p/d) x wide:
         y^2 = p x - (p/d) x^2, the ellipse
     A10 the same rectangle as an excess: y^2 = p x + (p/d) x^2, the
         hyperbola

   Plane: world (u, v), v up, u along the line AB; every kernel returns
   SVG coordinates (u, -v) and the SVG viewBox does the scaling.

   JVM side: the Emmy figures (`figures`, each a raster kernel through
   alexandria.medium.kernel), `passage` (the edn), `data` (the numbers of
   the default state, from alexandria.raster: Brent's roots, values) and
   `identities` (each algebraic step, proved by Emmy). Both sides: the
   step schedule, `scene-state` and the transport, pure. Browser side:
   `render`, the Clerk render-fn: an SVG redrawn from one batch per kernel
   per frame, the steps beside it, a Leva panel for y and S. No arithmetic
   on coordinates in the browser: every point and every number shown is a
   kernel output; the browser only compares and formats them."
  (:require [alexandria.medium.anim :as anim]
            [clojure.string :as str]
            #?(:clj [alexandria.grade :as grade])
            #?(:clj [alexandria.raster :as raster])
            #?(:clj [clojure.edn :as edn])
            #?(:clj [clojure.java.io :as io])
            #?(:clj [emmy.env :as e])
            #?(:clj [hive-dsl.result :as r])
            #?(:cljs [alexandria.medium.figure :as figure])
            #?(:cljs [leva.core :as leva])
            #?(:cljs [nextjournal.clerk.render :as render])
            #?(:cljs [reagent.core :as rg])))

;; ---------------------------------------------------------------------------
;; The steps: what appears, and what leaves, over which part of which step

(def schedule
  "Per step, {element [a b]}: the element's progress runs 0 -> 1 (Manim's
   smooth) over the part [a b] of the step. Elements are what the
   sentence introduces (the edn's :introduces); `motions` move what is
   drawn."
  [{:AB [0 0.25] :A [0.1 0.25] :B [0.15 0.3] :a [0.2 0.35] :tri [0.25 0.45] :C [0.35 0.5]
    :S [0.4 0.55] :rect44 [0.5 0.85] :h44 [0.75 0.95]}
   {:strip [0.25 0.75] :y-left [0.65 0.9]}
   {:square [0 0.5] :X [0.35 0.55] :y-top [0.45 0.7] :y-side [0.55 0.8]}
   {:applied [0 0.4] :ax [0.3 0.6] :eq28 [0.55 0.85]}
   {:graph [0 0.08] :curve28 [0.03 0.16] :S-line [0.12 0.18] :sweep [0.18 0.6] :top [0.26 0.34]
    :roots [0.4 0.48] :M [0.6 0.65] :half-sq [0.62 0.7] :moved [0.7 0.84] :small-sq [0.82 0.95]}
   {:rect29 [0.2 0.45] :over29 [0.35 0.6] :X' [0.5 0.6] :y29 [0.55 0.7] :curve29 [0.4 0.75]
    :root29 [0.75 0.85] :eq29 [0.8 0.95]}
   {:pline [0.2 0.45] :p [0.35 0.5] :xstrip [0.4 0.6] :x [0.5 0.65] :axis [0.45 0.6] :Q [0.55 0.7]
    :ordinate [0.55 0.75] :ysq [0.7 0.95]}
   {:par-curve [0 0.6] :eq-par [0.5 0.8]}
   {:defect [0 0.45] :defect-w [0.35 0.55] :ell-curve [0.45 0.85] :eq-ell [0.7 0.95]}
   {:excess [0 0.5] :hyp-curve [0.4 0.8] :eq-hyp [0.7 0.95]}])

(defn schedule-of
  "The part [a b] of step j over which `key` runs."
  [j key]
  (get-in schedule [j key]))

(def motions
  "Schedule keys that move what is already drawn instead of adding to it:
   A5's sweep of y over its roots and the top and II.5's move of the
   rectangle on AM into the square on the half, A6's turn of the graph."
  #{:sweep :moved :curve29})

(def leaves
  "{element [step a b]}: the element fades out over the part [a b] of
   step `step` (0-based): I.44's rectangle when VI.28 begins, VI.28's
   rectangles and II.5's square when VI.29 begins, the whole of Euclid's
   figure when the line becomes the parameter."
  (merge
   (zipmap [:rect44 :h44 :tri :C :S] (repeat [1 0 0.25]))
   (zipmap [:strip :y-left :square :X :y-top :y-side :applied :ax :eq28 :roots :top
            :M :half-sq :moved :small-sq]
           (repeat [5 0 0.2]))
   (zipmap [:A :B :a :rect29 :over29 :X' :y29 :graph :curve28 :S-line :root29 :eq29]
           (repeat [6 0 0.25]))))

(def durations
  "Milliseconds per step."
  [7000 5000 5500 6000 16000 7000 7500 4500 6500 6000])

(def step-count (count schedule))

(defn- at-step
  "The eased progress of the part [a b] of step j, at transport state st."
  [{:keys [step progress]} j a b]
  (anim/play (cond (< j step) 1 (= j step) progress :else 0) a b))

(defn progress-of
  "{key progress} at transport state st: steps before the current one
   complete, later ones not begun."
  [st]
  (into {}
        (for [[j sched] (map-indexed vector schedule)
              [key [a b]] sched]
          [key (at-step st j a b)])))

(defn leaving-of
  "{key fade-out progress} at transport state st."
  [st]
  (into {} (for [[key [j a b]] leaves] [key (at-step st j a b)])))

(defn alpha-of
  "{key opacity} at st: in by `schedule`, out by `leaves`."
  [st]
  (let [in (progress-of st) out (leaving-of st)]
    (into {} (for [[key v] in] [key (* v (- 1 (get out key 0)))]))))

(defn sweep-y
  "The height y of A5's sweep at sweep progress q: from the first root up
   to the top a/2, on to the second root, and back to the first, where
   II.5 is then drawn (data's raster numbers; the motion between them is
   the transport's easing, not a computation)."
  [q {:keys [r1 r2 half]}]
  (let [leg (fn [from to t] (+ from (* (- to from) (anim/smooth t))))]
    (cond (<= q (/ 1 3)) (leg r1 half (* 3 q))
          (<= q (/ 2 3)) (leg half r2 (- (* 3 q) 1))
          :else (leg r2 r1 (- (* 3 q) 2)))))

(defn scene-state
  "The figure's parameters at st, for the slider state {:y :S} and data:
   {:alpha :v :y :S :s :k}. y is the slider's except while A5 plays its
   sweep; s turns the graph from (a - y) y (s = -1) to (a + y) y (s = 1)
   over A6; k is the conic's c / (p/d): 0 (parabola) until A9 runs it to
   -1 (ellipse), A10 on to +1 (hyperbola)."
  [st {:keys [y S]} data]
  (let [v (progress-of st)
        sweeping? (and (= 4 (:step st)) (< 0 (:sweep v) 1))]
    {:v v
     :alpha (alpha-of st)
     :y (if sweeping? (sweep-y (:sweep v) data) y)
     :sweeping? sweeping?
     :S S
     :s (- (* 2 (:curve29 v)) 1)
     :k (- (* 2 (:excess v)) (:defect v))}))

;; ---------------------------------------------------------------------------
;; The transport: prev / play / next over the steps, pure (as in
;; alexandria.apollonius.notation-steps)

(defn start "Step 1, playing once." [] {:step 0 :progress 0 :playing? true :through? false})

(defn last-step? [{:keys [step]}] (= step (dec step-count)))

(defn advance
  "st after dt milliseconds. A step started by next/prev or a click plays
   once and stops; play (:through? true) runs on to the end."
  [{:keys [step progress playing? through?] :as st} dt]
  (if-not playing?
    st
    (let [p (+ progress (/ dt (nth durations step)))]
      (cond (< p 1) (assoc st :progress p)
            (and through? (not (last-step? st))) (assoc st :step (inc step) :progress 0)
            :else (assoc st :progress 1 :playing? false)))))

(defn goto
  "Step i (clamped) from its start, playing once."
  [_ i]
  {:step (max 0 (min (dec step-count) i)) :progress 0 :playing? true :through? false})

(defn toggle
  "Pause, or play on through the steps."
  [{:keys [step progress playing?] :as st}]
  (cond playing? (assoc st :playing? false)
        (and (>= progress 1) (last-step? st)) {:step 0 :progress 0 :playing? true :through? true}
        (>= progress 1) {:step (inc step) :progress 0 :playing? true :through? true}
        :else (assoc st :playing? true :through? true)))

(defn seek "Paused at progress p of the current step." [st p]
  (assoc st :progress (max 0 (min 1 p)) :playing? false))

;; ---------------------------------------------------------------------------
;; The sentence's Markdown, as segments

(defn segments
  "[[kind text] ...] of a sentence: :b **bold**, :i *italic*, :m $TeX$,
   :t plain."
  [text]
  (mapv (fn [s]
          (cond (str/starts-with? s "**") [:b (subs s 2 (- (count s) 2))]
                (str/starts-with? s "*") [:i (subs s 1 (dec (count s)))]
                (str/starts-with? s "$") [:m (subs s 1 (dec (count s)))]
                :else [:t s]))
        (re-seq #"\*\*[^*]+\*\*|\*[^*]+\*|\$[^$]+\$|[^*$]+" text)))

;; ---------------------------------------------------------------------------
;; The kernels' outputs, by name

(def py-points
  "Points of the :py kernel (Euclid's figure), in order, 2 numbers each:
   the line AB, X at AX = a - y, the strip's top corners, I.44's rectangle
   (H1 H2), the triangle C (T1 T2 T3, Cc its centroid), VI.29's X' and
   tops; the graph (G0 origin, Gx Gy axis ends, Gcur the current y, Gr1
   Gr2 the roots, Gtop the top, GS0 GS1 the level S, Gr29 VI.29's root);
   II.5's midpoint M; then the label anchors."
  [:A :B :X :Xt :Bt :At :H1 :H2 :T1 :T2 :T3 :Cc :X' :X't :A29t :B29t
   :G0 :Gx :Gy :Gcur :Gr1 :Gr2 :Gtop :GS0 :GS1 :Gr29 :M
   :l-A :l-B :l-X' :l-Sc :l-a :l-h44 :l-y-left :l-y-top :l-y-side :l-ax :l-X :l-y29 :l-over :l-S :l-gy :l-gx
   :l-M :l-half :l-small])

(def py-scalars
  "Numbers after the points: a y, (a - y) y, a - y, y^2, S/a, the roots
   r1 r2 of (a - y) y = S (NaN when S > (a/2)^2), the top (a/2)^2, VI.29's
   root y29 and its rectangle (a + y29) y29, a + y29; II.5's numbers: a/2,
   the shorter piece ym = min(y, a - y) = a/2 - |a/2 - y|, a - ym, and the
   small square (a/2 - y)^2."
  [:ay :amyy :amy :yy :h44 :r1 :r2 :top :y29 :apyy29 :apy29 :half :ym :amym :sq5])
(def cn-points
  "Points of the :cn kernel (the conic): the upright p at P (P0 Pe), the
   rectangle of width x on it (Pt0 Pte), the top E of the applied rectangle
   (E0 Et), the plot (O its vertex P, V, Q, Ax the axis end, Sq1 Sq2 the
   far corners of the square on QV), then the label anchors."
  [:P0 :Pe :Pt0 :Pte :E0 :Et :O :V :Q :Ax :Sq1 :Sq2
   :l-p :l-x :l-px :l-P :l-V :l-Q :l-y :l-ysq :l-w])

(def cn-scalars
  "p x, the leftover's height (p/d) x, its area (p/d) x^2, y^2, y, the
   applied rectangle's height p + k (p/d) x and area."
  [:px :w :wx :yy :y :E :Ex])


(defn named-map
  "{name value} of one output row of a kernel whose outputs are `points`
   (2 numbers each) then `scalars`."
  [points scalars row]
  (let [n (count points)]
    (merge (zipmap points (map vec (partition 2 (take (* 2 n) row))))
           (zipmap scalars (drop (* 2 n) row)))))

(def view
  "The layout: the right panel's origins (the triangle and the graph at
   gx0, the conic's vertex at cx0), the graph's scale (f per unit up), the
   conic plot's reach in x, the viewBox."
  {:gx0 12.5 :cx0 13.2 :gscale 4 :xreach 4 :box [-0.9 -8.9 21.6 10.4] :v-top 8.6})

;; ---------------------------------------------------------------------------
;; JVM: the figures (Emmy, compiled to raster kernels), the steps, the data

#?(:clj
   (do
     (defn- pt "World (u, v) as SVG [u -v]." [u v] [u (e/negate v)])

     (defn- graph-pt
       "The graph's point for abscissa t (0..a over 7 units) and value f."
       [a t f]
       (let [{:keys [gx0 gscale]} view]
         (pt (e/+ gx0 (e// (e/* 7 t) a)) (e// f gscale))))

     (defn py-figure
       "Euclid's figure for the line a, the area S, the height y: the points
        `py-points` then the numbers `py-scalars`. The roots of (a - y) y = S
        and of (a + y) y = S are Emmy's closed forms (proved in
        `identities`), evaluated by the kernel. They are also the tie's
        inverse: the browser sets y from S by them, S from y by (a - y) y."
       [a S y]
       (fn [[_]]
         (let [{:keys [gx0]} view
               amy (e/- a y)
               h44 (e// S a)
               half (e// a 2)
               top (e/square half)
               disc (e/sqrt (e/- top S))
               r1 (e/- half disc) r2 (e/+ half disc)
               y29 (e/- (e/sqrt (e/+ top S)) half)
               apy29 (e/+ a y29)
               ym (e/- half (e/sqrt (e/square (e/- half y))))
               amym (e/- a ym)
               th (e// (e/* 2 S) 6)
               g (fn [t f] (graph-pt a t f))
               pts {:A (pt 0 0) :B (pt a 0) :X (pt amy 0) :Xt (pt amy y) :Bt (pt a y) :At (pt 0 y)
                    :H1 (pt 0 h44) :H2 (pt a h44)
                    :T1 (pt gx0 0) :T2 (pt (e/+ gx0 6) 0) :T3 (pt (e/+ gx0 1.5) th)
                    :Cc (pt (e/+ gx0 2.5) (e// th 3))
                    :X' (pt apy29 0) :X't (pt apy29 y29) :A29t (pt 0 y29) :B29t (pt a y29)
                    :G0 (g 0 0) :Gx (g a 0) :Gy (g 0 32) :Gcur (g y (e/* amy y))
                    :Gr1 (g r1 S) :Gr2 (g r2 S) :Gtop (g half top) :GS0 (g 0 S) :GS1 (g a S)
                    :Gr29 (g y29 S) :M (pt half 0)
                    :l-A (pt 0 -0.75) :l-B (pt a -0.75) :l-X' (pt apy29 -0.75)
                    :l-Sc (pt (e/+ gx0 2.5) (e/- (e// th 3) 0.65))
                    :l-a (pt half -1.3) :l-h44 (pt (e/+ a 0.3) (e// h44 2))
                    :l-y-left (pt -0.55 (e// y 2)) :l-y-top (pt (e/- a (e// y 2)) (e/+ y 0.3))
                    :l-y-side (pt (e/+ a 0.3) (e// y 2)) :l-ax (pt (e// amy 2) (e// y 2))
                    :l-X (pt amy -0.75) :l-y29 (pt (e/+ a (e// y29 2)) (e/+ y29 0.3))
                    :l-over (pt (e/+ a (e// y29 2)) (e// y29 2))
                    :l-S (pt (e/+ gx0 7.1) (e// S 4))
                    :l-gy (pt (e/- gx0 0.2) 8.2) :l-gx (pt (e/+ gx0 7.1) -0.55)
                    :l-M (pt half -0.75) :l-half (pt (e/* 3 (e// a 4)) (e/+ half 0.35))
                    :l-small (pt (e// (e/+ half amym) 2) (e// (e/+ ym half) 2))}
               nums {:ay (e/* a y) :amyy (e/* amy y) :amy amy :yy (e/square y) :h44 h44
                     :r1 r1 :r2 r2 :top top :y29 y29 :apyy29 (e/* apy29 y29) :apy29 apy29
                     :half half :ym ym :amym amym :sq5 (e/square (e/- half y))}]
           (vec (concat (mapcat pts py-points) (map nums py-scalars))))))

     (defn cn-figure
       "The conic's application for the parameter p, the transverse d, the
        abscissa x and k = c / (p/d) in [-1 1]: the upright p at P (as
        Apollonius draws it, perpendicular to the diameter), the rectangle
        of width x on it, its applied height E = p + k (p/d) x, the
        leftover x wide and (p/d) x high; in the plot of the section the
        ordinate y = sqrt(E x) and the square on it. `cn-points` then
        `cn-scalars`."
       [p d x k]
       (fn [[_]]
         (let [{:keys [cx0 xreach]} view
               w (e// (e/* p x) d)
               E (e/+ p (e/* k w))
               Ex (e/* E x)
               y (e/sqrt Ex)
               vx (e/+ cx0 x)
               pts {:P0 (pt 0 0) :Pe (pt 0 p) :Pt0 (pt x 0) :Pte (pt x p) :E0 (pt 0 E) :Et (pt x E)
                    :O (pt cx0 0) :V (pt vx 0) :Q (pt vx y) :Ax (pt (e/+ cx0 xreach 0.3) 0)
                    :Sq1 (pt (e/+ vx y) 0) :Sq2 (pt (e/+ vx y) y)
                    :l-p (pt -0.55 (e// p 2)) :l-x (pt (e// x 2) -0.75) :l-px (pt (e// x 2) (e// p 2))
                    :l-P (pt (e/- cx0 0.35) -0.6) :l-V (pt vx -0.6) :l-Q (pt (e/- vx 0.35) (e/+ y 0.3))
                    :l-y (pt (e/- vx 0.45) (e// y 2)) :l-ysq (pt (e/+ vx (e// y 2)) (e// y 2))
                    :l-w (pt (e/+ x 0.3) (e// (e/+ p E) 2))}
               nums {:px (e/* p x) :w w :wx (e/* w x) :yy Ex :y y :E E :Ex Ex}]
           (vec (concat (mapcat pts cn-points) (map nums cn-scalars))))))

     (defn rect-figure
       "The corner (u, v) in {0 1}^2 of the rectangle from u = u0 to u1 on
        the line, height h, grown fw of its width and fh of its height."
       [u0 u1 h fw fh]
       (fn [[u v]]
         (pt (e/+ u0 (e/* fw (e/- u1 u0) u)) (e/* fh h v))))

     (defn box-figure
       "The corner (u, v) in {0 1}^2 of the box [u0 u1] x [v0 v1], grown f
        of its height from v0 (II.5's square on the half and the small
        square in it; the conic's rectangles on the upright p)."
       [u0 u1 v0 v1 f]
       (fn [[u v]]
         (pt (e/+ u0 (e/* (e/- u1 u0) u)) (e/+ v0 (e/* f (e/- v1 v0) v)))))

     (defn moved-figure
       "II.5's move, t in [0 1]: the part of the applied rectangle on AM
        (a/2 wide, ym high, ym = min(y, a - y)) carried onto the column over
        the last ym of the line (ym wide, a/2 high), of the same area; with
        the part on MX it fills the square on the half all but the small
        square on a/2 - ym. The corner (u, v) in {0 1}^2."
       [a ym t]
       (fn [[u v]]
         (let [half (e// a 2)
               lerp (fn [p q] (e/+ p (e/* t (e/- q p))))
               u0 (lerp 0 (e/- a ym)) u1 (lerp half a) h (lerp ym half)]
           (pt (e/+ u0 (e/* (e/- u1 u0) u)) (e/* h v)))))

     (defn graph-figure
       "The graph of (a + s t) t for t = a q, q in [0 1]: s = -1 is VI.28's
        (a - y) y, s = 1 VI.29's (a + y) y."
       [a s]
       (fn [[q]]
         (let [t (e/* a q)]
           (graph-pt a t (e/* (e/+ a (e/* s t)) t)))))

     (defn conic-figure
       "The section y^2 = p x + k (p/d) x^2 (upper half) in the plot, x =
        xreach q from the vertex."
       [p d k]
       (fn [[q]]
         (let [{:keys [cx0 xreach]} view
               x (e/* xreach q)]
           (pt (e/+ cx0 x) (e/sqrt (e/+ (e/* p x) (e/* k (e// p d) x x)))))))

     (def figures
       "Every figure of the scene, by name, for alexandria.medium.kernel."
       {:py {:f py-figure :params [10 21 3] :state [0] :opts {:simplify? false}}
        :cn {:f cn-figure :params [7 14 2 0] :state [0] :opts {:simplify? false}}
        :rect {:f rect-figure :params [0 10 3 1 1] :state [0.5 0.5]}
        :box {:f box-figure :params [5 10 0 5 1] :state [0.5 0.5]}
        :moved {:f moved-figure :params [10 3 0.5] :state [0.5 0.5]}
        :graph {:f graph-figure :params [10 -1] :state [0.5]}
        :conic {:f conic-figure :params [7 14 0] :state [0.5]}})

     (def resource "alexandria/apollonius/areas_steps.edn")

     (defn passage
       "The passage: {:title :source :steps [{:id :text :quote :cite :introduces}]}."
       []
       (edn/read-string (slurp (io/resource resource))))

     (def given
       "Pedro's numbers: the line a = 10, the area S = 21, the first height
        y = 3; the conic's p = 7, d = 14 (so p/d = 1/2), x = 2."
       {:a 10 :S 21 :y 3 :p 7 :d 14 :x 2})

     (defn data
       "The numbers of the given state, every one from raster: Brent's roots
        of (a - y) y = S on either side of a/2 and of (a + y) y = S in [0 a]
        (alexandria.raster/root on the Emmy function's kernel), the top and
        I.44's height (alexandria.raster/value), II.5's small square at the
        first root, y^2 of the three sections at x."
       []
       (let [{:keys [a S p d x]} given
             f28 (fn [y] (e/- (e/* (e/- a y) y) S))
             f29 (fn [y] (e/- (e/* (e/+ a y) y) S))
             half (raster/value (fn [a] (e// a 2)) a)
             r1 (raster/root f28 0 half)
             r2 (raster/root f28 half a)
             r29 (raster/root f29 0 a)
             [top h44] (raster/value (fn [a S] [(e/square (e// a 2)) (e// S a)]) a S)
             sq5 (raster/value (fn [a y] (e/square (e/- (e// a 2) y))) a (:value r1))
             conic-y2 (fn [k] (raster/value (fn [p d x] (e/+ (e/* p x) (e/* k (e// p d) x x))) p d x))]
         (merge given view
                {:half half :r1 (:value r1) :r2 (:value r2) :y29 (:value r29) :top top :h44 h44
                 :sq5 sq5
                 :residuals [(:residual r1) (:residual r2) (:residual r29)]
                 :y2 {:parabola (conic-y2 0) :ellipse (conic-y2 -1) :hyperbola (conic-y2 1)}
                 :source raster/source})))

     (defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

     (defn identities
       "Every algebraic step of the passage as {:id :label :lhs :rhs :grade}:
        lhs and rhs Emmy expressions, graded by simplifying lhs - rhs to 0
        (alexandria.grade, :grade/proved)."
       []
       (let [[a S y p d x] (map symbol ["a" "S" "y" "p" "d" "x"])
             half (e// a 2)
             disc (e/sqrt (e/- (e/square half) S))
             y29 (e/- (e/sqrt (e/+ (e/square half) S)) half)
             rows [{:id :I.44 :label "I.44: the height S/a makes the area S"
                    :lhs (e/* a (e// S a)) :rhs S}
                   {:id :VI.28 :label "VI.28: the applied rectangle and the square make up the strip"
                    :lhs (e/+ (e/* (e/- a y) y) (e/square y)) :rhs (e/* a y)}
                   {:id :VI.28-roots :label "VI.28: y = a/2 - sqrt((a/2)^2 - S) solves (a - y) y = S"
                    :lhs (let [y (e/- half disc)] (e/* (e/- a y) y)) :rhs S}
                   {:id :VI.28-roots' :label "VI.28: y = a/2 + sqrt((a/2)^2 - S) solves it too"
                    :lhs (let [y (e/+ half disc)] (e/* (e/- a y) y)) :rhs S}
                   {:id :tie :label "the tie: S from y, then y back from S (y <= a/2), is y"
                    :lhs (let [S (e/* (e/- a y) y)] (e/- half (e/sqrt (e/- (e/square half) S)))) :rhs y}
                   {:id :II.5 :label "II.5: the applied rectangle and the small square make the square on the half"
                    :lhs (e/+ (e/* (e/- a y) y) (e/square (e/- half y))) :rhs (e/square half)}
                   {:id :VI.29 :label "VI.29: the strip plus the square"
                    :lhs (e/* (e/+ a y) y) :rhs (e/+ (e/* a y) (e/square y))}
                   {:id :VI.29-root :label "VI.29: y = sqrt((a/2)^2 + S) - a/2 solves (a + y) y = S"
                    :lhs (e/* (e/+ a y29) y29) :rhs S}
                   {:id :ellipse :label "ellipse: the side p - (p/d) x times x"
                    :lhs (e/* (e/- p (e/* (e// p d) x)) x) :rhs (e/- (e/* p x) (e/* (e// p d) (e/square x)))}
                   {:id :hyperbola :label "hyperbola: the side p + (p/d) x times x"
                    :lhs (e/* (e/+ p (e/* (e// p d) x)) x) :rhs (e/+ (e/* p x) (e/* (e// p d) (e/square x)))}
                   {:id :d-by-p :label "the leftover, x wide and (p/d) x high, has the shape of d by p"
                    :lhs (e// x (e/* (e// p d) x)) :rhs (e// d p)}]]
         (mapv #(assoc % :grade (variant (grade/grade :symbolic [(e/- (:lhs %) (:rhs %))]))) rows)))))


;; ---------------------------------------------------------------------------
;; Browser: the scene

#?(:cljs
   (do
     (def ^:private n-graph 121)
     (def ^:private n-conic 121)
     (def ^:private graph-states (mapv (fn [i] [(/ i (dec n-graph))]) (range n-graph)))
     (def ^:private conic-states (mapv (fn [i] [(/ i (dec n-conic))]) (range n-conic)))
     (def ^:private corners [[0 0] [1 0] [1 1] [0 1]])

     (def ^:private colours
       {:line "#ECEEE4" :rect44 "#8FC7E8" :tri "#B9A3E3" :strip "#9aa7b4" :square "#E8735A"
        :applied "#F2D16B" :graph "#7FD18B" :S "#B9A3E3" :over "#7FD18B" :defect "#E8735A"
        :ordinate "#7FD18B" :par "#F2D16B" :ell "#E8735A" :hyp "#8FC7E8" :letter "#ECEEE4"
        :half "#8FC7E8" :small "#E8735A"})

     (defn- finite? [[x y]] (and (js/isFinite x) (js/isFinite y)))

     (defn- in-box?
       "Whether an SVG point lies within the drawn height."
       [{:keys [v-top]} [_ y :as q]]
       (and q (finite? q) (<= (- v-top) y 0.01)))

     (defn- py-at
       "The :py kernel's named outputs for a, S, y."
       [py a S y]
       (named-map py-points py-scalars (first (figure/points py [a S y] [[0]]))))

     (defn- frame!
       "Every point and number of one frame, one batch per kernel, at
        transport state st and slider state p."
       [{:keys [py cn rect box moved graph conic]} {:keys [a p d x] :as data} st prm]
       (let [{:keys [alpha v y S s k sweeping?]} (scene-state st prm data)
             P (py-at py a S y)
             C (named-map cn-points cn-scalars (first (figure/points cn [p d x k] [[0]])))
             rc (fn [u0 u1 h fw fh] (figure/points rect [u0 u1 h fw fh] corners))
             bx* (fn [u0 u1 v0 v1 f] (figure/points box [u0 u1 v0 v1 f] corners))
             [ax _] (:A P) [bx _] (:B P) [xx _] (:X P) [x29 _] (:X' P)
             {:keys [half ym amym]} P
             E (:E C)]
         {:alpha alpha :v v :out (leaving-of st) :P P :C C :y y :S S :sweeping? sweeping? :k k
          :rect44 (rc ax bx (:h44 P) 1 (:rect44 v))
          :strip (rc ax bx y 1 (:strip v))
          :square (rc xx bx y 1 (:square v))
          :applied (rc ax xx y 1 (:applied v))
          :half-sq (bx* half a 0 half (:half-sq v))
          :mx-part (bx* half amym 0 ym 1)
          :moved (figure/points moved [a ym (:moved v)] corners)
          :small-sq (bx* half amym ym half (:small-sq v))
          :rect29 (rc ax bx (:y29 P) 1 (:rect29 v))
          :over29 (rc bx x29 (:y29 P) (:over29 v) 1)
          :xstrip (bx* 0 x 0 p (:xstrip v))
          :applied-cn (bx* 0 x 0 E 1)
          :leftover (bx* 0 x (min p E) (max p E) 1)
          :ysq (rc (first (:V C)) (first (:Sq1 C)) (:y C) 1 (:ysq v))
          :graph (filterv #(in-box? data %) (figure/points graph [a s] graph-states))
          :par (filterv #(in-box? data %) (figure/points conic [p d 0] conic-states))
          :ell (filterv #(in-box? data %) (figure/points conic [p d -1] conic-states))
          :hyp (filterv #(in-box? data %) (figure/points conic [p d 1] conic-states))}))

     (defn- pts-attr [pts] (str/join " " (map (fn [[x y]] (str x "," y)) pts)))

     (defn- poly [pts colour opacity & [{:keys [dashed? fill] :or {fill 0.28}}]]
       (when (and (pos? opacity) (every? finite? pts))
         [:polygon {:points (pts-attr pts) :fill colour :fill-opacity fill :stroke colour
                    :stroke-width 0.06 :opacity opacity
                    :stroke-dasharray (when dashed? "0.25 0.15")}]))

     (defn- path
       "A polyline drawn f of the way (Manim's Create)."
       [pts colour f width & [opacity]]
       (when (and (pos? f) (seq pts))
         [:polyline (merge {:points (pts-attr pts) :fill "none" :stroke colour :stroke-width width
                            :stroke-linecap "round" :opacity (or opacity 1)}
                           (anim/create f))]))

     (defn- seg [a b colour f width & [opacity]] (path [a b] colour f width opacity))

     (defn- label
       "A letter as SVG text (DOM text, not canvas)."
       [[x y :as q] text opacity & [{:keys [colour italic? size anchor]
                                     :or {colour (colours :letter) size 0.5 anchor "middle"}}]]
       (when (and (pos? opacity) q (finite? q))
         [:text {:x x :y y :fill colour :opacity opacity :font-size size :text-anchor anchor
                 :dominant-baseline "middle"
                 :font-family "Georgia, 'Times New Roman', serif"
                 :font-style (if italic? "italic" "normal")}
          text]))

     (defn- dot [q colour opacity & [r]]
       (when (and (pos? opacity) q (finite? q))
         [:circle {:cx (first q) :cy (second q) :r (or r 0.13) :fill colour :opacity opacity}]))

     (defn- fixed [n digits] (if (js/isFinite n) (.toFixed (js/Number n) digits) "—"))

     (defn- svg-scene
       [{:keys [alpha P C] :as f} {:keys [box]}]
       (let [al (fn [key] (get alpha key 0))
             ;; a stroke is drawn (:v) and fades out (:out) independently
             pr (fn [key] (get-in f [:v key] 0))
             op (fn [key] (- 1 (get-in f [:out key] 0)))
             y-colour (colours :square)
             ;; II.5 carries the applied rectangle into the square on the half:
             ;; the rectangle and the square XB dim while it moves
             dim (- 1 (* 0.7 (pr :moved)))
             moving (if (pos? (pr :moved)) (op :moved) 0)]
         [:svg {:viewBox (str/join " " box) :data-figure "apollonius-areas"
                :style {:width "100%" :height "auto" :background "#1d1f21" :border-radius "6px"}}
          ;; Euclid: I.44
          (poly (:rect44 f) (colours :rect44) (al :rect44))
          (poly [(:T1 P) (:T2 P) (:T3 P)] (colours :tri) (al :tri))
          ;; VI.28
          (poly (:strip f) (colours :strip) (al :strip) {:fill 0.18})
          (poly (:applied f) (colours :applied) (* dim (al :applied)) {:fill 0.35})
          (poly (:square f) (colours :square) (* dim (al :square)) {:fill 0.45})
          ;; II.5: the square on the half MB, the applied rectangle moved into it,
          ;; the small square on a/2 - y left over
          (poly (:half-sq f) (colours :half) (al :half-sq) {:fill 0.08 :dashed? true})
          (poly (:mx-part f) (colours :applied) moving {:fill 0.5})
          (poly (:moved f) (colours :applied) moving {:fill 0.5})
          (poly (:small-sq f) (colours :small) (al :small-sq) {:fill 0.6})
          ;; VI.29
          (poly (:rect29 f) (colours :applied) (al :rect29) {:fill 0.3})
          (poly (:over29 f) (colours :over) (al :over29) {:fill 0.45})
          ;; the conic's rectangles on the upright p
          (poly (:xstrip f) (colours :strip) (* (al :xstrip) (if (pos? (al :defect)) 0.4 1)) {:fill 0.22})
          (poly (:applied-cn f) (colours :applied) (max (al :defect) (al :excess)) {:fill 0.35})
          (when (> (js/Math.abs (:k f)) 0.01)
            (poly (:leftover f) (if (neg? (:k f)) (colours :defect) (colours :over))
                  (max (al :defect) (al :excess)) {:fill 0.5 :dashed? (neg? (:k f))}))
          (poly (:ysq f) (colours :ordinate) (al :ysq) {:fill 0.3})
          ;; the line AB, then the upright p
          (seg (:A P) (:B P) (colours :line) (pr :AB) 0.09 (- 1 (pr :pline)))
          (seg (:P0 C) (:Pe C) (colours :par) (pr :pline) 0.11)
          (dot (:M P) (colours :half) (al :M))
          ;; the graph
          (seg (:G0 P) (:Gx P) (colours :line) (pr :graph) 0.04 (op :graph))
          (seg (:G0 P) (:Gy P) (colours :line) (pr :graph) 0.04 (op :graph))
          (path (:graph f) (colours :graph) (pr :curve28) 0.08 (op :curve28))
          (seg (:GS0 P) (:GS1 P) (colours :S) (pr :S-line) 0.05 (op :S-line))
          (label (:l-S P) "S" (al :S-line) {:colour (colours :S) :italic? true :anchor "start"})
          (label (:l-gy P) "area" (al :graph) {:size 0.38 :anchor "start"})
          (label (:l-gx P) "y" (al :graph) {:italic? true :size 0.42})
          (dot (:Gr1 P) (colours :S) (al :roots))
          (dot (:Gr2 P) (colours :S) (al :roots))
          (dot (:Gtop P) (colours :applied) (al :top))
          (dot (:Gr29 P) (colours :over) (al :root29))
          ;; the slider's y on VI.28's graph, until the graph turns
          (dot (:Gcur P) (colours :square) (* (al :curve28) (- 1 (pr :curve29))) 0.16)
          ;; the conic's plot
          (seg (:O C) (:Ax C) (colours :line) (pr :axis) 0.04)
          (path (:par f) (colours :par) (pr :par-curve) 0.07)
          (path (:ell f) (colours :ell) (pr :ell-curve) 0.07)
          (path (:hyp f) (colours :hyp) (pr :hyp-curve) 0.07)
          (seg (:V C) (:Q C) (colours :ordinate) (pr :ordinate) 0.09)
          (dot (:Q C) (colours :ordinate) (al :Q))
          ;; letters
          (label (:l-A P) "A" (al :A))
          (label (:l-B P) "B" (al :B))
          (label (:l-a P) "a" (al :a) {:italic? true})
          (label (:Cc P) "C" (al :C))
          (label (:l-Sc P) "S" (al :S) {:italic? true :colour (colours :tri)})
          (label (:l-h44 P) "S/a" (al :h44) {:italic? true :anchor "start" :colour (colours :rect44)})
          (label (:l-y-left P) "y" (al :y-left) {:italic? true})
          (label (:l-y-top P) "y" (* dim (al :y-top)) {:italic? true :colour y-colour})
          (label (:l-y-side P) "y" (* dim (al :y-side)) {:italic? true :colour y-colour :anchor "start"})
          (label (:l-X P) "X" (al :X))
          (label (:l-M P) "M" (al :M))
          (label (:l-ax P) "(a − y)·y" (* dim (al :ax)) {:italic? true :size 0.45})
          (label (:l-half P) "(a/2)²" (al :half-sq) {:italic? true :size 0.42 :colour (colours :half)})
          (label (:l-small P) "(a/2 − y)²" (al :small-sq) {:italic? true :size 0.3})
          (label (:l-X' P) "X′" (al :X'))
          (label (:l-y29 P) "y" (al :y29) {:italic? true :colour (colours :over)})
          (label (:l-p C) "p" (al :p) {:italic? true :colour (colours :par)})
          (label (:l-x C) "x" (al :x) {:italic? true})
          (label (:l-px C) "p·x" (al :xstrip) {:italic? true :size 0.42})
          (label (:l-w C) "(p/d)·x" (max (al :defect-w) (al :excess)) {:italic? true :size 0.38 :anchor "start"})
          (label (:l-P C) "P" (al :axis))
          (label (:l-V C) "V" (al :axis))
          (label (:l-Q C) "Q" (al :Q))
          (label (:l-y C) "y" (al :ordinate) {:italic? true :colour (colours :ordinate)})
          (label (:l-ysq C) "y²" (al :ysq) {:italic? true})]))

     (def ^:private button-style
       {:font-family "ui-monospace, monospace" :font-size "0.95em" :padding "0.15em 0.7em"
        :margin-right "0.35em" :border "1px solid currentColor" :border-radius "0.35em"
        :background "transparent" :color "inherit" :cursor "pointer" :opacity 0.8})

     (defn- transport [st act]
       [:div {:style {:display "flex" :align-items "center" :margin-top "0.5em"}}
        [:button {:style button-style :data-act "prev" :on-click #(act goto (dec (:step st)))} "◀"]
        [:button {:style button-style :data-act "play" :on-click #(act toggle)} (if (:playing? st) "❚❚" "▶")]
        [:button {:style button-style :data-act "next" :on-click #(act goto (inc (:step st)))} "▶▶"]
        [:input {:type "range" :min 0 :max 1000 :style {:flex "1"}
                 :value (js/Math.round (* 1000 (:progress st)))
                 :on-change #(act seek (/ (js/parseInt (.. % -target -value)) 1000))}]])

     (defn- tex [s] [:f> render/render-katex s {:inline? true}])

     (defn- sentence [text]
       (into [:span]
             (for [[kind s] (segments text)]
               (case kind :b [:strong s] :i [:em s] :m (tex s) s))))

     (defn- step-list [steps st act]
       [:ol {:style {:margin 0 :padding-left "2.2em" :font-size "0.92em" :line-height 1.45}}
        (for [[i {:keys [id text quote cite]}] (map-indexed vector steps)
              :let [current? (= i (:step st))]]
          ^{:key id}
          [:li {:data-step (inc i) :data-current (str current?) :on-click #(act goto i)
                :style {:cursor "pointer" :margin-bottom "0.4em" :padding "0.2em 0.5em"
                        :border-left (str "3px solid " (if current? "#E8735A" "transparent"))
                        :background (if current? "rgba(232,115,90,0.08)" "transparent")
                        :opacity (if current? 1 0.45) :transition "opacity 300ms"}}
           [sentence text]
           (when (and current? quote)
             [:div {:data-quote id :style {:font-size "0.88em" :margin-top "0.35em" :opacity 0.85}}
              [:em (str "“" quote "”")] " " [:span {:style {:opacity 0.7}} (str "(" cite ")")]])])])

     (defn- readout
       "The current step's numbers, every one a kernel output."
       [shown]
       (let [{:keys [st P C y S]} @shown
             i (:step st)
             {:keys [ay amyy amy yy r1 r2 top y29 apyy29 apy29 h44 sq5]} P
             fits? (< (js/Math.abs (- amyy S)) 0.02)
             line (case i
                    0 (str "a = 10,\\; S = " (fixed S 2) ",\\; S/a = " (fixed h44 3) ",\\; a \\cdot \\tfrac{S}{a} = S")
                    1 (str "a \\cdot y = 10 \\cdot " (fixed y 2) " = " (fixed ay 2))
                    2 (str "XB = y = " (fixed y 2) ",\\; AX = a - y = " (fixed amy 2))
                    3 (str "(a - y)\\,y = " (fixed amy 2) " \\cdot " (fixed y 2) " = " (fixed amyy 2)
                           (if fits? " = S" (str " \\neq S = " (fixed S 2)))
                           ",\\; S + y^2 = " (fixed amyy 2) " + " (fixed yy 2) " = " (fixed ay 2) " = a\\,y")
                    4 (str "(a - y)\\,y + (a/2 - y)^2 = " (fixed amyy 2) " + " (fixed sq5 2) " = "
                           (fixed (+ amyy sq5) 2) " = (a/2)^2")
                    5 (str "(a + y)\\,y = S:\\; y = " (fixed y29 3) ",\\; " (fixed apy29 3) " \\cdot "
                           (fixed y29 3) " = " (fixed apyy29 2))
                    (let [{:keys [px wx yy y]} C]
                      (case i
                        6 (str "p\\,x = " (fixed px 2) " = y^2,\\; y = " (fixed y 3))
                        7 (str "y^2 = p\\,x = " (fixed yy 2) ",\\; y = " (fixed y 3))
                        8 (str "y^2 = p\\,x - \\tfrac{p}{d}x^2 = " (fixed px 2) " - " (fixed wx 2) " = " (fixed yy 2))
                        (str "y^2 = p\\,x + \\tfrac{p}{d}x^2 = " (fixed px 2) " + " (fixed wx 2) " = " (fixed yy 2)))))]
         [:div {:style {:font-size "0.9em" :line-height 1.7 :margin-top "0.6em" :padding-left "2.2em"}}
          [:div {:data-readout (str i) :data-y (fixed y 3) :data-s (fixed S 3)} (tex line)]
          (when (= i 4)
            [:div {:data-roots (str (fixed r1 3) " " (fixed r2 3))}
             (tex (if (js/isFinite r1)
                    (str "S = " (fixed S 2) ":\\; y = " (fixed r1 3) " \\text{ or } " (fixed r2 3)
                         ";\\; \\text{most at } y = a/2:\\; (a/2)^2 = " (fixed top 2))
                    (str "S = " (fixed S 2) " > (a/2)^2 = " (fixed top 2) ":\\; \\text{no } y \\text{ (VI.27)}")))])]))

     (defn- tie!
       "The tie (widget brief, section 5): y and S are both live, held
        together by (a - y) y = S with a fixed. The touched key is the only
        source. y moved: S := (a - y) y. S moved: y := the root on y's side
        of a/2; above (a/2)^2 there is none and y stays (VI.27). Both maps
        are outputs of the :py kernel (Emmy proves they compose to the
        identity, `identities` :tie). A pushed value never re-fires the
        solve: origin guard `pushing`, epsilon guard `pushed`."
       [py {:keys [a]} st prm pushing pushed]
       (add-watch prm ::tie
                  (fn [_ _ old new]
                    (when-not @pushing
                      (let [src (cond (not= (:S old) (:S new)) :S
                                      (not= (:y old) (:y new)) :y)
                            echo? (and src (contains? @pushed src)
                                       (< (js/Math.abs (- (get new src) (get @pushed src))) 1e-6))]
                        (when (and src (not echo?))
                          ;; the reader's move pauses the transport
                          (when (:playing? @st) (swap! st assoc :playing? false))
                          (let [P (py-at py a (:S new) (:y new))
                                out (case src
                                      :y {:S (:amyy P)}
                                      :S (let [r (if (<= (:y new) (:half P)) (:r1 P) (:r2 P))]
                                           (when (js/isFinite r) {:y r})))]
                            (when out
                              (reset! pushed out)
                              (reset! pushing true)
                              (try (swap! prm merge out)
                                   (finally (reset! pushing false)))))))))))

     (defn render
       "Clerk render-fn. value: {:steps [{:id :text :quote :cite}] :kernels
        {name kernel} :data (alexandria.apollonius.areas-steps/data)}."
       [{:keys [steps kernels data]}]
       (rg/with-let [figures (into {} (map (fn [[k v]] [k (figure/kernel-figure v)])) kernels)
                     st (rg/atom (start))
                     prm (rg/atom {:y (:y data) :S (:S data)})
                     pushing (atom false)
                     pushed (atom {})
                     act (fn [f & args] (apply swap! st f args))
                     _ (tie! (:py figures) data st prm pushing pushed)
                     shown (rg/atom (assoc (frame! figures data @st @prm) :st @st))
                     alive (atom true)
                     drawn (atom nil)
                     last-ts (atom nil)
                     loop! (fn loop! [ts]
                             (when @alive
                               (let [dt (if @last-ts (- ts @last-ts) 0)]
                                 (reset! last-ts ts)
                                 (when (:playing? @st) (swap! st advance dt))
                                 (let [s @st p @prm]
                                   (when-not (and (identical? s (first @drawn)) (identical? p (second @drawn)))
                                     (reset! drawn [s p])
                                     (let [f (frame! figures data s p)]
                                       ;; the sweep's y is the transport's, not the reader's:
                                       ;; pushed under the origin guard, S stays
                                       (when (:sweeping? f)
                                         (reset! pushing true)
                                         (try (swap! prm assoc :y (:y f))
                                              (finally (reset! pushing false))))
                                       (reset! shown (assoc f :st s))))))
                               (js/requestAnimationFrame loop!)))
                     _ (js/requestAnimationFrame loop!)]
         (let [s @st]
           [:div {:data-areas-step (inc (:step s)) :data-progress (str (:progress s))
                  :style {:display "flex" :flex-wrap "wrap" :gap "1em" :align-items "flex-start"}}
            [:div {:style {:flex "1 1 560px" :min-width "300px"}}
             [svg-scene @shown data]
             [transport s act]
             [:div {:style {:max-width "340px" :margin-top "0.5em"}}
              [leva/SubPanel {:fill true :flat true :titleBar false}
               [leva/Controls {:atom prm
                               :schema {:y {:min 0 :max 10 :step 0.01 :label "y"}
                                        :S {:min 1 :max 30 :step 0.01 :label "S"}}}]]]]
            [:div {:style {:flex "1 1 320px"}}
             [step-list steps s act]
             [readout shown]]])
         (finally (reset! alive false)
                  (remove-watch prm ::tie))))))
