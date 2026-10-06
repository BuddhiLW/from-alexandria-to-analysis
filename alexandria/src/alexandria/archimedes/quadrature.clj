(ns alexandria.archimedes.quadrature
  "Archimedes, Quadrature of the Parabola, Propositions 21-24: the segment
   exhausted by triangles (Heath 1897, pp. 246-252).

   The segment of y = 1 - x^2 cut by the chord from (-1, 0) to (1, 0). Its
   first triangle has area 1 and the segment 4/3, so every ratio Archimedes
   uses is an exact rational. Ported from set-theory.archimedes.

     stage-triangles   the new triangles of stage n, exact
     area              a triangle's area, exact
     partial-area      the inscribed polygon through stage n
     geometric-partial 1 + 1/4 + ... + (1/4)^n
     prop-23-holds?    Proposition 23 as the finite identity it is
     identities        Props 22-23 symbolic in n and in the chord, graded by Emmy
     figures           the stage figure, an Emmy function a medium compiles
                       to a raster kernel"
  (:require [alexandria.grade :as grade]
            [alexandria.proofs :as proofs]
            [emmy.env :as e]
            [hive-dsl.result :as r]
            [alexandria.raster :as raster]))

(def segment-area
  "The parabolic segment in these coordinates (Prop. 24: 4/3 of the first
   triangle, whose area is 1)."
  4/3)

(defn point "The point of y = 1 - x^2 above x." [x] [x (- 1 (* x x))])

(defn triangle
  "The triangle Archimedes inscribes on chord [a b]: its vertex is where the
   tangent is parallel to the chord, above the middle of the chord."
  [[a b]]
  {:a a :b b :v (point (/ (+ (first a) (first b)) 2))})

(defn area
  "Exact area of triangle {:a :b :v}."
  [{[x1 y1] :a [x2 y2] :b [x3 y3] :v}]
  (abs (/ (+ (* x1 (- y2 y3)) (* x2 (- y3 y1)) (* x3 (- y1 y2))) 2)))

(defn children
  "The two triangles inscribed in the segments left on either side."
  [{:keys [a b v]}]
  [(triangle [a v]) (triangle [v b])])

(def first-triangle (triangle [(point -1) (point 1)]))

(defn stage-triangles
  "The new triangles of stage n; stage 0 is the first triangle."
  [n]
  (nth (iterate #(vec (mapcat children %)) [first-triangle]) n))

(defn stage-area [n] (reduce + (map area (stage-triangles n))))

(defn partial-area
  "The inscribed polygon through stage n."
  [n]
  (reduce + (map stage-area (range (inc n)))))

(defn geometric-partial
  "1 + 1/4 + ... + (1/4)^n, exact."
  [n]
  (reduce + (map #(/ 1 (long (Math/pow 4 %))) (range (inc n)))))

(defn prop-23-holds?
  "Proposition 23: the areas through stage n with one third of the last
   make 4/3 of the first, exactly."
  [n]
  (= 4/3 (+ (geometric-partial n) (* 1/3 (/ 1 (long (Math/pow 4 n)))))))

(defn uncovered
  "The segment minus the polygon through stage n, exact."
  [n]
  (- segment-area (partial-area n)))

(defn- signed-area [[x1 y1] [x2 y2] [x3 y3]]
  (e// (e/+ (e/* x1 (e/- y2 y3)) (e/* x2 (e/- y3 y1)) (e/* x3 (e/- y1 y2))) 2))

(defn- sym-point [x] [x (e/- 1 (e/square x))])

(def identities
  "[label difference] pairs, each zero, symbolic: a and b the ends of any
   chord of y = 1 - x^2, n the last stage."
  (let [a 'a b 'b m (e// (e/+ 'a 'b) 2)
        A (sym-point a) B (sym-point b) V (sym-point m)
        L (sym-point (e// (e/+ a m) 2)) R (sym-point (e// (e/+ m b) 2))
        big (signed-area A B V)
        ;; the segment over chord AB: the curve minus the chord, integrated
        ;; by its antiderivative F (Emmy's derivative checks F' below)
        chord (fn [x] (e/+ (e/- 1 (e/square a)) (e/* (e/- (e/+ a b)) (e/- x a))))
        F (fn [x] (e/- (e/- x (e// (e/cube x) 3))
                       (e/+ (e/* (e/- 1 (e/square a)) x) (e/* (e/- (e/+ a b)) (e/- (e// (e/square x) 2) (e/* a x))))))]
    [["Prop. 22: the two triangles on AV and VB are together one fourth of ABV, for every chord AB"
      (e/- (e/+ (signed-area A V L) (signed-area V B R)) (e/* 1/4 big))]
     ["Prop. 23 for every n: (1 + 1/4 + ... + (1/4)^n) + (1/3)(1/4)^n = 4/3"
      (let [q (e/expt 1/4 'n)]
        (e/- (e/+ (e// (e/- 1 (e/* 1/4 q)) 3/4) (e/* 1/3 q)) 4/3))]
     ["F' is the curve minus the chord, so F(b) - F(a) is the segment"
      (e/- ((e/D F) 'x) (e/- (e/- 1 (e/square 'x)) (chord 'x)))]
     ["Prop. 24: the segment on every chord AB is 4/3 of the triangle ABV"
      (e/- (e/- (F b) (F a)) (e/* 4/3 big))]]))

(defn graded
  "[{:label :grade}] of identities, graded by alexandria.grade, and the
   exact rational checks of Props 21-24 through stage 10."
  []
  (let [g (fn [diff] (let [res (grade/grade :symbolic [diff])]
                       (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails)))
        exact (fn [label ok?] {:label label :grade (if ok? :grade/proved :grade/fails)})]
    (conj (mapv (fn [[label diff]] {:label label :grade (g diff)}) identities)
          (exact "stages 1-10: each stage's triangles are one fourth of the stage before, in exact rationals"
                 (every? #(= (stage-area %) (* 1/4 (stage-area (dec %)))) (range 1 11)))
          (exact "stages 0-10: polygon = (1 + 1/4 + ... + 1/4^n) of the first triangle, below 4/3, short by 1/(3 4^n)"
                 (every? #(and (= (partial-area %) (geometric-partial %))
                               (= (uncovered %) (/ 1 (* 3 (long (Math/pow 4 %))))))
                         (range 11)))
          (exact "Prop. 23 holds exactly for n = 0..10" (every? prop-23-holds? (range 11))))))

(defn stage-figure
  "Vertex j (0, 1, 2: a, v, b) of triangle k of stage s, among the 2^s new
   triangles of that stage, grown in by t (0: flat on its chord, 1: in
   place). An Emmy function, for media to compile to a raster kernel."
  [s t]
  (fn [[k j]]
    (let [w (e// 2 (e/expt 2 s))
          xa (e/+ -1 (e/* k w))
          xb (e/+ xa w)
          xm (e/+ xa (e// w 2))
          x (e/+ (e/* (e/- 1 j) (e/- 2 j) (e// xa 2))
                 (e/* j (e/- 2 j) xm)
                 (e/* j (e/- j 1) (e// xb 2)))
          y-chord (e/+ (e/- 1 (e/* xa xa)) (e/* (e/- (e/+ xa xb)) (e/- x xa)))
          y-curve (e/- 1 (e/* x x))]
      [x (e/+ y-chord (e/* t (e/- y-curve y-chord)))])))

(def figures
  "{name {:f :params :state}} for alexandria.medium.kernel."
  {:stage {:f stage-figure :params [0 1] :state [0 0]}})

(defn arc
  "The parabola y = 1 - x^2 over the chord, n + 1 points, sampled by
   alexandria.raster."
  [n]
  (raster/sample (fn [x] [x (e/- 1 (e/square x))])
                 (mapv (fn [i] [(- (* 2 (/ i n)) 1)]) (range (inc n)))))

(def proofs-resource "alexandria/archimedes/quadrature_of_the_parabola.proofs.edn")

(defn proof [] (proofs/read-proofs proofs-resource))
