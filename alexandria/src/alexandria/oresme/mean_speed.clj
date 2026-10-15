(ns alexandria.oresme.mean-speed
  "The mean degree theorem (the Merton rule) and Oresme's geometric proof.

   A uniformly difform quality (a motion whose speed grows by equal
   excesses in equal times) is worth as much as the uniform quality at its
   middle degree: the triangle (or trapezium) of the motion has the area of
   the rectangle on the same extension at the mean speed,

     d = vm t,   vm = (v0 + v1) / 2.

   Oresme's proof (De latitudinibus formarum, Padua 1486, art. 3, first
   conclusion, from suppositions 2 and 3): the degree DE at the midpoint D
   of the extension AB is half of the last degree BC (Euclid VI.2), and the
   rectangle on AB of height DE equals the triangle ABC, because the small
   triangle it cuts off above equals the one it adds below.

   Here, for the page:
   - the TIE of v0, v1, t, vm, d (alexandria.medium.tie): v0, v1 or t move
     vm and d; vm moves v0 and v1 together (their difference held, both
     kept at 0 or above); d moves t (the speeds held), with an origin guard
     on vm;
   - the BEND b: the speed v(s) = v0 + (v1 - v0) s/t + 4b (s/t)(1 - s/t)
     makes the figure difformly difform (b = 0 is Oresme's case). The page
     sums the `strip` kernel over 64 strips (the distance by raster) and
     sets it beside the rectangle vm t; with b != 0 they part by 2bt/3, and
     the badge turns red;
   - the player figure of Oresme's proof (`proof-figure`)."
  (:require [alexandria.grade :as grade]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; Branch-free clamps (alexandria.medium.tie's, the core cannot require media)

(defn at-least [x lo] (e/* 0.5 (e/+ x lo (e/abs (e/- x lo)))))
(defn at-most [x hi] (e/* 0.5 (e/- (e/+ x hi) (e/abs (e/- x hi)))))
(defn clamp [x lo hi] (at-most (at-least x lo) hi))

;; ---------------------------------------------------------------------------
;; The relation

(defn mean [v0 v1] (e// (e/+ v0 v1) 2))

(defn speed
  "The speed at time s of the reader's motion: Oresme's straight top plus
   the bend b (a parabolic bulge, 0 at both ends)."
  [v0 v1 t b s]
  (let [u (e// s t)]
    (e/+ v0 (e/* (e/- v1 v0) u) (e/* 4 b u (e/- 1 u)))))

(def tied [:v0 :v1 :t :vm :d])

(def ranges
  "[min max step]."
  {:v0 [0 10 0.05] :v1 [0 10 0.05] :t [0.2 10 0.05] :vm [0 10 0.05] :d [0 100 0.1]})

(def labels {:v0 "v₀ first degree" :v1 "v₁ last degree" :t "t extension (time)" :vm "vₘ mean degree" :d "d distance"})

(defn- lo [k] (first (ranges k)))
(defn- hi [k] (second (ranges k)))

(def guard "vm below this: d cannot set t (the origin guard)." 0.02)

(defn- state [v0 v1 t]
  (let [vm (mean v0 v1)] [v0 v1 t vm (e/* vm t)]))

(defn- from-v0 [v0 v1 t _ _] (state (clamp v0 (lo :v0) (hi :v0)) v1 t))
(defn- from-v1 [v0 v1 t _ _] (state v0 (clamp v1 (lo :v1) (hi :v1)) t))
(defn- from-t [v0 v1 t _ _] (state v0 v1 (clamp t (lo :t) (hi :t))))

(defn- from-vm
  "vm moves both speeds by the same amount; the difference is held and the
   lower speed stays at 0 or above."
  [v0 v1 t vm _]
  (let [half (e// (e/- v1 v0) 2)
        vm' (clamp vm (e/abs half) (e/- (hi :vm) (e/abs half)))]
    (state (e/- vm' half) (e/+ vm' half) t)))

(defn- from-d
  "d moves the time, the speeds held: t = d / vm (vm kept off 0)."
  [v0 v1 _ _ d]
  (let [vm (at-least (mean v0 v1) guard)]
    (state v0 v1 (clamp (e// (clamp d (lo :d) (hi :d)) vm) (lo :t) (hi :t)))))

(defn tie
  "The tie kernel: the solved [v0 v1 t vm d] for each source, in the order
   of `tied`."
  [v0 v1 t vm d]
  (fn [_] (vec (mapcat #(% v0 v1 t vm d) [from-v0 from-v1 from-t from-vm from-d]))))

(def strips "The raster sum's strips." 64)

(defn readout
  "Kernel: params [v0 v1 t b]. Out [vm rect exact bend v(t/2)
   small-triangle]: the mean degree, the rectangle vm t, the exact area
   vm t + 2bt/3 (graded below), the bend's share 2bt/3, the speed at the
   middle instant and each of the two small triangles of the proof."
  [v0 v1 t b]
  (fn [_]
    (let [vm (mean v0 v1)
          rect (e/* vm t)
          bend (e// (e/* 2 b t) 3)]
      [vm rect (e/+ rect bend) bend (speed v0 v1 t b (e// t 2))
       (e// (e/* t (e/abs (e/- v1 v0))) 8)])))

(def readout-index {:vm 0 :rect 1 :exact 2 :bend 3 :mid 4 :small-triangle 5})

(defn strip
  "Kernel: params [v0 v1 t b], state [i]. Out [s v(s) v(s)dt]: the midpoint
   s of strip i of `strips`, the speed there and the strip's area. The page
   sums the areas: the distance by raster."
  [v0 v1 t b]
  (fn [[i]]
    (let [dt (e// t strips) s (e/* dt (e/+ i 0.5)) v (speed v0 v1 t b s)]
      [s v (e/* v dt)])))

(defn curve
  "Kernel figure: params [v0 v1 t b], state [u] in [0 1]. Out [s v(s)]."
  [v0 v1 t b]
  (fn [[u]]
    (let [s (e/* u t)] [s (speed v0 v1 t b s)])))

(defn case-of
  "The name of the reader's figure: :uniform (v0 = v1, no bend),
   :uniformly-difform (b = 0; :uniformly-difform-from-none when an end is
   at no degree), :difformly-difform (b != 0)."
  [v0 v1 b]
  (cond (> (Math/abs (double b)) 1e-3) :difformly-difform
        (< (Math/abs (- (double v1) v0)) 1e-3) :uniform
        (or (< (Math/abs (double v0)) 1e-3) (< (Math/abs (double v1)) 1e-3)) :uniformly-difform-from-none
        :else :uniformly-difform))

;; ---------------------------------------------------------------------------
;; Oresme's proof, as a figure kernel for the player

(defn proof-figure
  "Params [v0 v1 T]: the extension AB = [0, T], the first degree v0 at A,
   the last BC = v1 at B. State [u]: the point at fraction u of AB.
   Out [x v(x) vm T*vm area T/2 small]: the abscissa, the degree on the
   straight top, the mean degree DE, the rectangle, the figure's area
   (rectangle on v0 plus the triangle), the midpoint D and each small
   triangle."
  [v0 v1 T]
  (fn [[u]]
    (let [x (e/* u T) vm (mean v0 v1)]
      [x (e/+ v0 (e/* (e/- v1 v0) u)) vm (e/* T vm)
       (e/+ (e/* T v0) (e// (e/* T (e/- v1 v0)) 2)) (e// T 2)
       (e// (e/* T (e/- v1 v0)) 8)])))

(def figures
  "The player's moving figures."
  {:proof {:f proof-figure :params [0 4 4] :state [0.5] :opts {:simplify? false}}})

;; ---------------------------------------------------------------------------
;; Grades

(defn- integral-0-t
  "∫₀ᵗ f by an antiderivative F that Emmy checks: F(t) − F(0), with DF = f
   verified symbolically in `identities`."
  [F t]
  (e/- (F t) (F 0)))

(def ^:private F-straight
  (fn [s] (e/+ (e/* 'v0 s) (e// (e/* (e/- 'v1 'v0) (e/square s)) (e/* 2 't)))))

(def ^:private F-bend
  (fn [s] (e/* 4 'b (e/- (e// (e/square s) (e/* 2 't)) (e// (e/cube s) (e/* 3 (e/square 't)))))))

(def identities
  "[label difference] pairs reduced to 0 by Emmy."
  [["Oresme, supposition 2: the degree at the midpoint of a straight top is the mean, v(t/2) = (v₀ + v₁)/2"
    (e/- (speed 'v0 'v1 't 0 (e// 't 2)) (mean 'v0 'v1))]
   ["Emmy's derivative of F(s) = v₀s + (v₁ − v₀)s²/(2t) is the speed: DF = v"
    (e/- ((e/D F-straight) 's) (speed 'v0 'v1 't 0 's))]
   ["the mean degree theorem: ∫₀ᵗ v ds = F(t) − F(0) = vₘ t"
    (e/- (integral-0-t F-straight 't) (e/* (mean 'v0 'v1) 't))]
   ["the two small triangles of the proof are equal, each (v₁ − v₀)t/8"
    (let [vm (mean 'v0 'v1)]
      (e/- (e/* (e// 1 2) (e// 't 2) (e/- 'v1 vm)) (e/* (e// 1 2) (e// 't 2) (e/- vm 'v0))))]
   ["Emmy's derivative of the bend's antiderivative is the bulge 4b(s/t)(1 − s/t)"
    (e/- ((e/D F-bend) 's) (e/- (speed 'v0 'v1 't 'b 's) (speed 'v0 'v1 't 0 's)))]
   ["a bend b adds 2bt/3 to the distance: the rule fails off the straight top"
    (e/- (integral-0-t F-bend 't) (e// (e/* 2 'b 't) 3))]
   ["Galileo's odd numbers: from rest, d(n+1) − d(n) = (2n + 1) d(1)"
    (let [d (fn [n] (e/* (mean 0 (e/* 'a n)) n))]
      (e/- (e/- (d (e/+ 'n 1)) (d 'n)) (e/* (e/+ (e/* 2 'n) 1) (d 1))))]])

(defn tie-values [v0 v1 t vm d]
  (binding [raster/*simplify?* false]
    (raster/value (fn [a b c x y] ((tie a b c x y) 0)) v0 v1 t vm d)))

(defn readout-values [v0 v1 t b]
  (binding [raster/*simplify?* false]
    (raster/value (fn [a c x y] ((readout a c x y) 0)) v0 v1 t b)))

(defn raster-distance
  "The distance as the page computes it: the strip kernel at every strip,
   summed."
  [v0 v1 t b]
  (binding [raster/*simplify?* false]
    (reduce + (map #(nth % 2) (raster/sample (fn [i] ((strip v0 v1 t b) [i])) (mapv vector (range strips)))))))

(defn round-trip
  "Forward then inverse = identity at (v0 v1 t): solve vm and d forward,
   then from each source the tie gives back the same five numbers."
  [v0 v1 t]
  (let [fwd (vec (take 5 (apply tie-values (concat [v0 v1 t] [0 0]))))
        out (apply tie-values fwd)]
    (for [i (range 5) j (range 5)] (- (nth out (+ (* 5 i) j)) (fwd j)))))

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn graded
  "[{:label :grade}]."
  []
  (let [[_ rect] (readout-values 0 4 4 0)
        [_ _ exact-bent] (readout-values 0 4 4 1.5)]
    (-> (mapv (fn [[label diff]] {:label label :grade (variant (grade/grade :symbolic [diff]))}) identities)
        (conj {:label "the tie, forward then inverse from every source, returns the state it started from (raster)"
               :grade (variant (grade/grade :numeric (round-trip 1 5 3) 1e-9)) :engine :raster}
              {:label "the raster sum of 64 strips equals vₘ t on Oresme's triangle (v₀ = 0, v₁ = 4, t = 4)"
               :grade (variant (grade/grade :numeric [(- (raster-distance 0 4 4 0) rect)] 1e-9)) :engine :raster}
              {:label "with a bend b = 1.5 the raster sum leaves the rectangle by 2bt/3 = 4"
               :grade (variant (grade/grade :numeric [(- (raster-distance 0 4 4 1.5) exact-bent)] 1e-3)) :engine :raster}))))
