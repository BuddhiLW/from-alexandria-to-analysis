(ns alexandria.structure.arith
  "Language stratum of alexandria.structure: Emmy-generic, branch-free
   arithmetic that every backend compiles alike (symbols for TeX and exact
   grades, doubles on the JVM, alexandria.raster and the :raster backend of
   emmy-viewers for the page).

   Clamps: at-least, at-most, clamp, step-up.
   Inverse functions with one-argument atan only: acos*, acosh*, asinh*,
   angle2.
   Complex numbers as pairs [x y]: c+ c- c* cconj cscale norm2 c-div one cis.

   Contract: every function takes and returns Emmy values (numbers, symbols,
   literal expressions); none branches on its arguments."
  (:require [emmy.env :as e]))

;; ---------------------------------------------------------------------------
;; Clamps

(defn at-least "max(x, lo)." [x lo] (e/* 0.5 (e/+ x lo (e/abs (e/- x lo)))))
(defn at-most "min(x, hi)." [x hi] (e/* 0.5 (e/- (e/+ x hi) (e/abs (e/- x hi)))))
(defn clamp "x held in [lo hi]." [x lo hi] (at-most (at-least x lo) hi))
(defn step-up "1 where x > 0, 0 where x < 0 (undefined at 0)." [x] (e/* 0.5 (e/+ 1 (e/divide x (e/abs x)))))

(defn rad "Degrees to radians." [d] (e/* d (/ Math/PI 180)))
(defn deg "Radians to degrees." [r] (e/* r (/ 180 Math/PI)))

(defn wrap
  "x reduced modulo m into [0, m), branch-free: the angle of x on a clock
   of m hours, atan2(-sin, -cos) + pi of 2 pi (x + 1/2)/m, read back in
   hours (the + 1/2 keeps an exact multiple of m off the cut). Exact for
   numbers within the double's precision; Emmy-generic, so raster
   compiles it."
  [x m]
  (let [t (e/divide (e/* 2 Math/PI (e/+ x 0.5)) m)]
    (e/- (e/* (e/divide m (* 2 Math/PI)) (e/+ Math/PI (e/atan (e/- 0 (e/sin t)) (e/- 0 (e/cos t))))) 0.5)))

;; ---------------------------------------------------------------------------
;; Inverse functions

(defn acosh* "acosh x = log(x + sqrt(x^2 - 1)), x clamped to >= 1." [x]
  (let [x (at-least x 1)] (e/log (e/+ x (e/sqrt (e/- (e/square x) 1))))))

(defn asinh* "asinh x = log(x + sqrt(x^2 + 1))." [x]
  (e/log (e/+ x (e/sqrt (e/+ (e/square x) 1)))))

(defn acos*
  "acos x = 2 atan sqrt((1 - x)/(1 + x)), x clamped to [-1 1]."
  [x]
  (let [x (clamp x -1 1)] (e/* 2 (e/atan (e/sqrt (e/divide (e/- 1 x) (e/+ 1 x)))))))

(defn angle2
  "The angle in [0 pi] with sine proportional to s >= 0 and cosine to c:
   2 atan(s / (sqrt(s^2 + c^2) + c))."
  [s c]
  (e/* 2 (e/atan (e/divide s (e/+ (e/sqrt (e/+ (e/square s) (e/square c))) c 1e-300)))))

;; ---------------------------------------------------------------------------
;; Complex numbers as pairs

(def one "The complex unit [1 0]." [1 0])

(defn c+ [[a b] [c d]] [(e/+ a c) (e/+ b d)])
(defn c- [[a b] [c d]] [(e/- a c) (e/- b d)])
(defn c* [[a b] [c d]] [(e/- (e/* a c) (e/* b d)) (e/+ (e/* a d) (e/* b c))])
(defn cconj [[a b]] [a (e/- 0 b)])
(defn cscale [s [a b]] [(e/* s a) (e/* s b)])
(defn norm2 "|z|^2." [[a b]] (e/+ (e/square a) (e/square b)))
(defn c-div "z / w." [z w]
  (let [n (norm2 w)] (cscale (e/divide 1 n) (c* z (cconj w)))))
(defn cis "e^{i t} = [cos t, sin t]." [t] [(e/cos t) (e/sin t)])

(defn dot "The dot product of plane vectors." [[a b] [c d]] (e/+ (e/* a c) (e/* b d)))
(defn cross "The cross product (a scalar) of plane vectors." [[a b] [c d]] (e/- (e/* a d) (e/* b c)))
(defn angle-between "The angle in [0 pi] between plane vectors u and v." [u v]
  (angle2 (e/abs (cross u v)) (dot u v)))
