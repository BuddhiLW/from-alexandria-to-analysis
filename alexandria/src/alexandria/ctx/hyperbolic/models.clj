(ns alexandria.ctx.hyperbolic.models
  "Exact coordinate maps for the standard models of H².

   Searched before writing: desargues.geometry.projective supplies the
   homogeneous/projective operations used by alexandria.ctx.hyperbolic for the
   Cayley-Klein layer; Emmy supplies generic arithmetic and elementary
   functions. These model-change formulas are the standard closed-form maps
   between the Klein disk, Poincare disk, upper half-plane and the hyperboloid."
  (:require [emmy.env :as e]))

(defn sq [x] (e/* x x))

(defn norm2
  "Euclidean squared norm of a two-coordinate point."
  [[x y]]
  (e/+ (sq x) (sq y)))

(defn klein->hyperboloid
  "Klein disk [x y] -> upper sheet hyperboloid [X Y T], T²-X²-Y²=1."
  [[x y]]
  (let [s (e/sqrt (e/- 1 (norm2 [x y])))]
    [(e// x s) (e// y s) (e// 1 s)]))

(defn hyperboloid->klein
  "Upper sheet hyperboloid [X Y T] -> Klein disk [X/T Y/T]."
  [[X Y T]]
  [(e// X T) (e// Y T)])

(defn klein->poincare
  "Klein disk -> Poincare disk."
  [[x y]]
  (let [d (e/+ 1 (e/sqrt (e/- 1 (norm2 [x y]))))]
    [(e// x d) (e// y d)]))

(defn poincare->klein
  "Poincare disk -> Klein disk."
  [[x y]]
  (let [d (e/+ 1 (norm2 [x y]))]
    [(e// (e/* 2 x) d) (e// (e/* 2 y) d)]))

(defn poincare->half-plane
  "Poincare disk -> upper half-plane, by z |-> i(1+z)/(1-z).
   Points are represented as [real imaginary]."
  [[x y]]
  (let [d (e/+ (sq (e/- 1 x)) (sq y))]
    [(e// (e/* -2 y) d)
     (e// (e/- 1 (norm2 [x y])) d)]))

(defn half-plane->poincare
  "Upper half-plane -> Poincare disk, inverse to poincare->half-plane."
  [[u v]]
  (let [d (e/+ (sq u) (sq (e/+ v 1)))]
    [(e// (e/+ (sq u) (sq v) -1) d)
     (e// (e/* -2 u) d)]))

(defn klein->half-plane [p]
  (poincare->half-plane (klein->poincare p)))

(defn half-plane->klein [p]
  (poincare->klein (half-plane->poincare p)))

(defn hyperboloid->poincare [p]
  (klein->poincare (hyperboloid->klein p)))

(defn poincare->hyperboloid [p]
  (klein->hyperboloid (poincare->klein p)))

(defn hyperboloid->half-plane [p]
  (klein->half-plane (hyperboloid->klein p)))

(defn half-plane->hyperboloid [p]
  (klein->hyperboloid (half-plane->klein p)))

(defn ->klein
  "Convert point p from model to Klein coordinates."
  [model p]
  (case model
    :klein p
    :poincare (poincare->klein p)
    :half-plane (half-plane->klein p)
    :hyperboloid (hyperboloid->klein p)))

(defn from-klein
  "Convert Klein point p to model coordinates."
  [model p]
  (case model
    :klein p
    :poincare (klein->poincare p)
    :half-plane (klein->half-plane p)
    :hyperboloid (klein->hyperboloid p)))
