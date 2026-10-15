(ns alexandria.hilbert.moulton
  "Moulton's bent-line incidence plane. A negative-slope line has half its
   left-hand slope to the right of x=0 when bend=0.5. Positive-slope lines
   and vertical lines stay ordinary. Intersections and Desargues' residual
   are calculated from line equations, not stored diagram coordinates."
  (:require [alexandria.raster :as raster]
            [emmy.env :as e]))

(def origin [1 1])
(def triangle-a
  "No side is horizontal or vertical, so every slope sign is defined."
  [[-4 -5] [-3 -2] [3 -4]])
(def ray-x [3 -2 2])

(defn positive [x] (e// (e/+ x (e/abs x)) 2))

(defn negative-slope?
  "1 when the line through [x y] and [u v] falls to the right, else 0,
   branch-free (sign by t/|t|) so a raster kernel can carry it."
  [[x y] [u v]]
  (let [t (e/* (e/- u x) (e/- v y))]
    (e// (e/- 1 (e// t (e/abs t))) 2)))

(defn abscissa
  "The model abscissa: x itself, except that a falling line (negative? = 1)
   has its right half x > 0 compressed by the factor bend."
  [bend negative? x]
  (e/+ x (e/* negative? (e/- bend 1) (positive x))))

(defn ordinate
  "Y on the model line through p and q at x."
  [bend p q x]
  (let [neg? (negative-slope? p q)
        u (partial abscissa bend neg?)
        m (e// (e/- (second q) (second p)) (e/- (u (first q)) (u (first p))))]
    (e/+ (second p) (e/* m (e/- (u x) (u (first p)))))))

(defn line-parts [bend p q]
  [(ordinate bend p q 0)
   (e/- (ordinate bend p q 0) (ordinate bend p q -1))
   (e/- (ordinate bend p q 1) (ordinate bend p q 0))])

(defn intersection
  "Intersect two bent lines. Select the left or right linear branch from
   the sign of the left candidate, using |x| rather than a picture lookup."
  [bend p q r s]
  (let [[a ml mr] (line-parts bend p q)
        [b nl nr] (line-parts bend r s)
        xl (e// (e/- b a) (e/- ml nl))
        xr (e// (e/- b a) (e/- mr nr))
        right (e// (e/+ 1 (e// xl (e/abs xl))) 2)
        x (e/+ (e/* (e/- 1 right) xl) (e/* right xr))]
    [x (ordinate bend p q x)]))

(defn configuration [bend]
  (let [[a b c] triangle-a
        [ap bp cp] (mapv (fn [p x] [x (ordinate bend origin p x)]) triangle-a ray-x)
        x (intersection bend a b ap bp)
        y (intersection bend b c bp cp)
        z (intersection bend c a cp ap)]
    {:first [a b c] :second [ap bp cp] :center origin
     :crossings [x y z]}))

(defn residual [bend]
  (let [[x y z] (:crossings (configuration bend))]
    (e/- (e/* (e/- (first y) (first x)) (e/- (second z) (second x)))
         (e/* (e/- (second y) (second x)) (e/- (first z) (first x))))))

(defn coordinates
  "Flat [x y ...] of O, A, B, C, A', B', C', X, Y, Z, the kinks of BC and
   B'C' on x = 0, then the residual (index 24)."
  [bend]
  (let [{:keys [first second center crossings]} (configuration bend)
        [_ b c] first
        [_ bp cp] second
        kinks [[0 (ordinate bend b c 0)] [0 (ordinate bend bp cp 0)]]]
    (vec (concat (mapcat identity (concat [center] first second crossings kinks))
                 [(residual bend)]))))

(defn numeric [bend]
  (let [f (raster/kernels (fn [b] (coordinates b)) 1)]
    (mapv #(% (double bend)) f)))