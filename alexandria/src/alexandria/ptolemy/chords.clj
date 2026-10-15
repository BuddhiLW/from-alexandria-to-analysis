(ns alexandria.ptolemy.chords
  "Ptolemy, Almagest I.10-11 (Halma's chapter IX): the table of chords.

   Ptolemy measures a chord in parts of which the diameter has 120, and an
   arc in degrees of which the circle has 360. In those units

     crd(2a) = 120 sin a   (= 2R sin a, R = 60)

   He builds the table from four theorems, all proved here by Emmy:

     - the sides of the regular pentagon and decagon (crd 72, crd 36) and of
       the hexagon, square and triangle (crd 60, 90, 120);
     - the supplement: crd(180 - x)^2 = 120^2 - crd(x)^2 (Euclid III.31);
     - his lemma on the inscribed quadrilateral (Ptolemy's theorem),
       AC.BD = AB.CD + AD.BC, which gives the chord of a difference and of a
       sum of two arcs;
     - the half arc: GD^2 = AG.GZ with GZ = (AG - AB)/2.

   The proofs run in the rational parametrisation of the circle,
   P(t) = R ((1 - t^2)/(1 + t^2), 2t/(1 + t^2)), in which every chord is
   2R|s - t| / sqrt((1 + s^2)(1 + t^2)): the square roots of Ptolemy's
   identity cancel (each point appears once in each product) and what is
   left is a polynomial identity Emmy reduces to 0. Every number the page
   shows (the table, the error against Ptolemy's sexagesimal values, the
   construction figures) comes from raster kernels of these Emmy
   functions.

   Ptolemy's values are Halma's (1813), Almageste I, ch. IX, pp. 28-36,
   and the table of chords that follows (p. 38 ff.)."
  (:require [alexandria.grade :as grade]
            [alexandria.raster :as raster]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; Units

(def diameter "Ptolemy's diameter, in parts." 120)
(def radius (/ diameter 2))

(defn rad [deg] (e/* deg (/ Math/PI 180)))

(defn crd
  "The chord of an arc of `deg` degrees, in parts of a diameter of 120:
   crd(2a) = 2R sin a."
  ([deg] (crd radius deg))
  ([R deg] (e/* 2 R (e/sin (e// (rad deg) 2)))))

(defn sexagesimal
  "p;m,s (parts, minutes, seconds) as a number of parts."
  [[p m s]]
  (+ p (/ m 60.0) (/ (or s 0) 3600.0)))

(defn sexagesimal-str
  "x as p;m,s rounded to the second, Ptolemy's notation p\u1d56 m\u2032 s\u2033."
  [x]
  (let [total (Math/round (* 3600.0 (double x)))
        p (quot total 3600) m (quot (rem total 3600) 60) s (rem total 60)]
    (format "%dᵖ %d′ %d″" p m s)))

;; ---------------------------------------------------------------------------
;; Ptolemy's values, as he states them (Halma 1813)

(def ptolemy-values
  "[arc-degrees [p m s] page what]: the chords Ptolemy computes in the text of
   I.10 (Halma's ch. IX)."
  [[36 [37 4 55] 28 "side of the decagon"]
   [60 [60 0 0] 28 "side of the hexagon, the radius"]
   [72 [70 32 3] 28 "side of the pentagon"]
   [90 [84 51 10] 29 "side of the square"]
   [120 [103 55 23] 29 "side of the triangle"]
   [144 [114 7 37] 29 "supplement of 36°"]
   [3/2 [1 34 15] 32 "by halving 12° three times"]
   [3/4 [0 47 8] 32 "by halving once more"]
   [1 [1 2 50] 36 "between the bounds of the lemma"]
   [1/2 [0 31 25] 36 "half of 1°"]])

(defn table
  "Each of Ptolemy's values beside 120 sin(a/2) by raster:
   [{:arc :ptolemy :raster :error :error-seconds :page :what}]."
  []
  (let [arcs (mapv (comp double first) ptolemy-values)
        vals (raster/sample (fn [d] (crd d)) (mapv vector arcs))]
    (mapv (fn [[arc psm page what] v]
            (let [p (sexagesimal psm)]
              {:arc arc :ptolemy p :ptolemy-str (sexagesimal-str p) :raster v
               :raster-str (sexagesimal-str v) :error (- p v) :error-seconds (* 3600 (- p v))
               :page page :what what}))
          ptolemy-values vals)))

(defn half-degree-table
  "The table's own grid, every half degree from 1/2 to 180: [[arc crd] ...]
   by raster (one kernel, one call per arc)."
  []
  (let [arcs (mapv #(* 0.5 %) (range 1 361))]
    (mapv vector arcs (raster/sample (fn [d] (crd d)) (mapv vector arcs)))))

;; ---------------------------------------------------------------------------
;; The circle, rationally

(defn on-circle
  "P(t) on the circle of radius R about the origin: t = tan(phi/2)."
  [R t]
  (let [w (e/+ 1 (e/square t))]
    [(e/* R (e// (e/- 1 (e/square t)) w)) (e/* R (e// (e/* 2 t) w))]))

(defn dist2 [[x0 y0] [x1 y1]]
  (e/+ (e/square (e/- x1 x0)) (e/square (e/- y1 y0))))

(defn chord-t
  "The chord between P(s) and P(t), s > t: 2R (s - t)/sqrt((1+s^2)(1+t^2))."
  [R s t]
  (e// (e/* 2 R (e/- s t)) (e/sqrt (e/* (e/+ 1 (e/square s)) (e/+ 1 (e/square t))))))

(defn ptolemy-residual
  "AC.BD - (AB.CD + AD.BC) for four points P(a), P(b), P(c), P(d) in order
   on the circle (a < b < c < d), chords by `chord-t`, with the square roots
   factored out: the cofactor polynomial."
  [a b c d]
  (e/- (e/* (e/- c a) (e/- d b))
       (e/+ (e/* (e/- b a) (e/- d c)) (e/* (e/- d a) (e/- c b)))))

(def identities
  "[label difference] pairs, each reduced to 0 by Emmy."
  (let [R 'R s 's t 't]
    [["crd(2a) = 2R sin a: the chord from angle -a to angle a"
      (e/- (dist2 [(e/* R (e/cos 'a)) (e/* R (e/sin (e/- 'a)))] [(e/* R (e/cos 'a)) (e/* R (e/sin 'a))])
           (e/square (e/* 2 R (e/sin 'a))))]
     ["the chord between P(s) and P(t) is 2R|s - t| / sqrt((1+s²)(1+t²))"
      (e/- (dist2 (on-circle R s) (on-circle R t))
           (e// (e/* 4 (e/square R) (e/square (e/- s t))) (e/* (e/+ 1 (e/square s)) (e/+ 1 (e/square t)))))]
     ["supplement (III.31): crd(x)² + crd(180° - x)² = diameter²"
      (e/- (e/+ (dist2 [(e/- R) 0] (on-circle R t)) (dist2 [R 0] (on-circle R t)))
           (e/square (e/* 2 R)))]
     ["Ptolemy's lemma: AC·BD = AB·CD + AD·BC on the circle (the square roots cancel)"
      (ptolemy-residual 'a 'b 'c 'd)]
     ["half arc: GD² = AG·GZ (D the midpoint of arc GB, Z its foot on AG)"
      (let [[dx dy] (on-circle R s)]
        (e/- (dist2 [R 0] [dx dy]) (e/* (e/* 2 R) (e/- R dx))))]
     ["half arc: GZ = (AG - AB)/2, squared (B = P(2s/(1-s²)), the double of D)"
      (let [[dx _] (on-circle R s)
            [bx by] (on-circle R (e// (e/* 2 s) (e/- 1 (e/square s))))]
        (e/- (e/square (e/- (e/* 2 R) (e/* 2 (e/- R dx))))
             (dist2 [(e/- R) 0] [bx by])))]]))

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn graded
  "[{:label :grade}]: the identities (proved), Ptolemy's ten values against
   raster within 3 seconds (numeric), the half-degree table monotone and
   ending in the diameter (numeric)."
  []
  (let [rows (table)
        grid (half-degree-table)]
    (-> (mapv (fn [[label diff]] {:label label :grade (variant (grade/grade :symbolic [diff]))}) identities)
        (conj {:label "Ptolemy's sexagesimal chords agree with 120 sin(a/2) by raster within 3″"
               :grade (variant (grade/grade :numeric (map #(/ (:error %) 3) rows) (/ 1.0 3600)))
               :engine :raster}
              {:label "the half-degree table ends at crd 180° = 120ᵖ, the diameter"
               :grade (variant (grade/grade :numeric [(- (second (peek grid)) 120)] 1e-9))
               :engine :raster}))))

;; ---------------------------------------------------------------------------
;; The two constructions, as figure kernels (alexandria.medium.figure)

(defn half-figure
  "The half-arc construction (Halma p. 31-32) at arc GB = beta degrees:
   semicircle ABG on the diameter AG (A = (-R, 0), G = (R, 0)), B on it at
   angle beta from G, D the midpoint of arc GB, AE = AB on AG, DZ
   perpendicular to AG. Out: [Bx By Dx Dy Ex Zx AB GB GD GZ (AG-AB)/2
   AG.GZ GD^2 crd(beta/2)]."
  [R]
  (fn [[beta]]
    (let [b (rad beta) h (e// b 2)
          bx (e/* R (e/cos b)) by (e/* R (e/sin b))
          dx (e/* R (e/cos h)) dy (e/* R (e/sin h))
          AB (e/sqrt (dist2 [(e/- R) 0] [bx by]))
          GB (e/sqrt (dist2 [R 0] [bx by]))
          GD (e/sqrt (dist2 [R 0] [dx dy]))
          GZ (e/- R dx)]
      [bx by dx dy (e/+ (e/- R) AB) dx AB GB GD GZ (e// (e/- (e/* 2 R) AB) 2)
       (e/* 2 R GZ) (e/square GD) (crd R (e// beta 2))])))

(defn sum-figure
  "The sum construction (Halma p. 32-33) for arcs AB = alpha and BG = beta
   from A on the circle about Z of diameter AD (A = (-R, 0), D = (R, 0)),
   BE a diameter. Out: [Bx By Gx Gy Ex Ey AB BG BD GE DE BE GD-by-Ptolemy
   AG-from-GD crd(alpha+beta)]."
  [R]
  (fn [[alpha beta]]
    (let [a (rad alpha) ab (rad (e/+ alpha beta))
          bx (e/- (e/* R (e/cos a))) by (e/* R (e/sin a))
          gx (e/- (e/* R (e/cos ab))) gy (e/* R (e/sin ab))
          ex (e/- bx) ey (e/- by)
          AB (e/sqrt (dist2 [(e/- R) 0] [bx by]))
          BG (e/sqrt (dist2 [bx by] [gx gy]))
          BD (e/sqrt (dist2 [bx by] [R 0]))
          GE (e/sqrt (dist2 [gx gy] [ex ey]))
          DE AB
          BE (e/* 2 R)
          GD (e// (e/- (e/* BD GE) (e/* BG DE)) BE)
          AG (e/sqrt (e/- (e/square (e/* 2 R)) (e/square GD)))]
      [bx by gx gy ex ey AB BG BD GE DE BE GD AG (crd R (e/+ alpha beta))])))

(def figures
  "Every moving figure of the player, by name: {:f :params :state :opts}."
  {:half {:f half-figure :params [radius] :state [72] :opts {:simplify? false}}
   :sum {:f sum-figure :params [radius] :state [72 60] :opts {:simplify? false}}})
