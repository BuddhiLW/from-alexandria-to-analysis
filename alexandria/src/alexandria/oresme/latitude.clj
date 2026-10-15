(ns alexandria.oresme.latitude
  "Oresme's latitude of forms (Tractatus de configurationibus qualitatum et
   motuum, c. 1350; the school text De latitudinibus formarum, Padua 1486).

   A quality (heat, whiteness, speed) has an EXTENSION, laid along a line
   (the longitude: the parts of the subject, or the time), and at each point
   an INTENSITY, its degree, raised as a perpendicular (the latitude). The
   tops of the perpendiculars draw the figure of the quality, and its
   quantity is the area of the figure.

   Oresme's kinds:
     uniform                   the same degree throughout (a rectangle)
     uniformly difform         equal excesses of degree over equal distances
                               (a straight top: a triangle when it starts
                               from no degree, else a trapezium)
     difformly difform         anything else (a curved or broken top)

   The reader's figure is the curve through five control points over the
   extension [0, 4] (nodes 0, 1, 2, 3, 4): the Lagrange polynomial of degree
   at most 4. Its case is read off the differences of the five heights: all
   first differences 0 = uniform; all second differences 0 = uniformly
   difform; else difformly difform. Its area, the quantity of the quality,
   is Boole's rule (exact for a quartic, graded below) and, on the page, a
   raster sum of the clipped figure (a degree is never negative).

   Every number the page shows comes out of the kernels `profile` and
   `summary`, compiled by raster."
  (:require [alexandria.grade :as grade]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; Branch-free helpers (raster compiles them; the core cannot require media)

(defn at-least [x lo] (e/* 0.5 (e/+ x lo (e/abs (e/- x lo)))))

;; ---------------------------------------------------------------------------
;; The figure

(def extension "The line of extension runs from 0 to this." 4)

(def nodes "The abscissae of the five control points." [0 1 2 3 4])

(def eps "Below this a difference counts as 0 (the case naming)." 1e-3)

(defn- basis
  "The i-th Lagrange basis polynomial on `nodes`, at t."
  [i t]
  (reduce e/* 1 (for [j (range 5) :when (not= i j)]
                  (e// (e/- t (nodes j)) (- (nodes i) (nodes j))))))

(defn height
  "The degree at t of the figure through (node_i, h_i)."
  [hs t]
  (reduce e/+ 0 (map-indexed (fn [i h] (e/* h (basis i t))) hs)))

(defn profile
  "Kernel: params the five heights, state [t]. Out [t degree raw]: the
   degree clipped at 0 (no negative intensity) and the raw polynomial."
  [h0 h1 h2 h3 h4]
  (fn [[t]]
    (let [v (height [h0 h1 h2 h3 h4] t)]
      [t (at-least v 0) v])))

(defn boole
  "Boole's rule on the nodes 0..4 (step 1): the exact area of a quartic."
  [h0 h1 h2 h3 h4]
  (e/* (/ 2 45) (e/+ (e/* 7 h0) (e/* 32 h1) (e/* 12 h2) (e/* 32 h3) (e/* 7 h4))))

(defn summary
  "Kernel: params the five heights. Out [area mean d1 d2 d3 d4 s1 s2 s3
   max|d| max|s| mid-rect]: Boole's area of the raw figure, the mean degree
   area/extension, the first and second differences of the heights, their
   largest sizes, and the rectangle on the extension at the middle degree
   h2 (the mean degree theorem's rectangle)."
  [h0 h1 h2 h3 h4]
  (fn [_]
    (let [hs [h0 h1 h2 h3 h4]
          ds (mapv (fn [i] (e/- (hs (inc i)) (hs i))) (range 4))
          ss (mapv (fn [i] (e/- (ds (inc i)) (ds i))) (range 3))
          mx (fn [xs] (reduce (fn [a b] (at-least a b)) (map e/abs xs)))
          area (boole h0 h1 h2 h3 h4)]
      (-> [area (e// area extension)]
          (into (concat ds ss [(mx ds) (mx ss)]))
          (conj (e/* h2 extension))))))

(def summary-index {:area 0 :mean 1 :d1 2 :d2 3 :d3 4 :d4 5 :s1 6 :s2 7 :s3 8 :max-d 9 :max-s 10 :mid-rect 11})

(defn summary-values
  "summary at the heights hs, by raster."
  [hs]
  (binding [raster/*simplify?* false]
    (apply raster/value (fn [& xs] ((apply summary xs) 0)) hs)))

(defn case-of
  "Oresme's name of the figure through heights hs (JVM side; the page
   carries the same rule as a browser form): :uniform, :uniformly-difform
   (:uniformly-difform-from-none when it starts or ends at no degree) or
   :difformly-difform."
  [hs]
  (let [out (summary-values hs)
        g (fn [k] (nth out (summary-index k)))]
    (cond (< (g :max-d) eps) :uniform
          (< (g :max-s) eps) (if (or (< (Math/abs (double (first hs))) eps)
                                     (< (Math/abs (double (last hs))) eps))
                               :uniformly-difform-from-none
                               :uniformly-difform)
          :else :difformly-difform)))

(defn raster-area
  "The area of the clipped figure through hs by a midpoint raster sum of
   n strips (each degree from the profile kernel)."
  [hs n]
  (let [dt (/ (double extension) n)
        ts (mapv (fn [i] [(* dt (+ i 0.5))]) (range n))
        vs (binding [raster/*simplify?* false]
             (raster/sample (fn [t] ((apply profile hs) [t])) ts))]
    (* dt (reduce + (map second vs)))))

(def kinds
  "Oresme's kinds, as five heights each (the player and the presets)."
  {:uniform [2.5 2.5 2.5 2.5 2.5]
   :uniformly-difform [0 1 2 3 4]
   :uniformly-difform-trapezium [1 1.75 2.5 3.25 4]
   :difformly-difform [0.5 2.6 3.4 2.6 0.5]})

;; ---------------------------------------------------------------------------
;; Grades

(def identities
  "[label difference] pairs reduced to 0 by Emmy."
  (let [hs '[h0 h1 h2 h3 h4]
        line (mapv (fn [i] (e/+ 'a (e/* 'b i))) (range 5))]
    (concat
     (for [k (range 5)]
       [(str "Boole's rule is exact on t^" k ": ∫₀⁴ t^" k " dt = 4^" (inc k) "/" (inc k))
        (e/- (apply boole (map #(e/expt % k) nodes)) (/ (long (Math/pow 4 (inc k))) (inc k)))])
     [["the figure passes through its five control points"
       (e/- (reduce e/+ (map (fn [i] (height hs (nodes i))) (range 5))) (reduce e/+ hs))]
      ["equal excesses (h_i = a + b i): every second difference is 0"
       (let [out ((apply summary line) 0)]
         (e/+ (nth out 6) (nth out 7) (nth out 8)))]
      ["equal excesses draw a straight top: the curve through h_i = a + b i is a + b t"
       (e/- (height line 't) (e/+ 'a (e/* 'b 't)))]
      ["a uniformly difform figure has the area of its mean degree times the extension: 4 (a + 2b)"
       (e/- (apply boole line) (e/* 4 (e/+ 'a (e/* 2 'b))))]])))

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn graded
  "[{:label :grade}]: the identities (proved) and the raster sum against
   Boole's rule on the arched preset (numeric)."
  []
  (let [hs (:difformly-difform kinds)
        exact (nth (summary-values hs) 0)]
    (conj (mapv (fn [[label diff]] {:label label :grade (variant (grade/grade :symbolic [diff]))}) identities)
          {:label "the raster sum of 400 strips agrees with Boole's rule on the arched figure"
           :grade (variant (grade/grade :numeric [(- (raster-area hs 400) exact)] 1e-4))
           :engine :raster})))
