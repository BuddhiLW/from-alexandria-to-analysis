(ns alexandria.calculus.playground
  "Numeric kernels for the six calculus playgrounds."
  (:require [alexandria.raster :as raster]
            [emmy.env :as e]))

(defn height [curve x]
  (let [selector (fn [k] (reduce e/* 1 (for [j (range 4) :when (not= j k)]
                                         (e// (e/- curve j) (- k j)))))]
    (e/+ (e/* (selector 0) (e/* x x 0.5))
         (e/* (selector 1) x)
         (e/* (selector 2) (e/sqrt (e/abs (e/- (e/* 2 x) (e/* x x)))))
         (e/* (selector 3) (e/* x x x 0.25)))))

(defn slope [curve x]
  (let [selector (fn [k] (reduce e/* 1 (for [j (range 4) :when (not= j k)]
                                         (e// (e/- curve j) (- k j)))))]
    (e/+ (e/* (selector 0) x)
         (selector 1)
         (e/* (selector 2) (e// (e/- 1 x)
                                   (e/sqrt (e/+ 1e-8 (e/abs (e/- (e/* 2 x) (e/* x x)))))))
         (e/* (selector 3) (e/* 0.75 x x)))))

(defn triangle [curve x dx]
  (fn [_]
    (let [y (height curve x) q (height curve (e/+ x dx))
          m (slope curve x) z (e/- y (e/* x m))
          sector (e/* 0.5 (e/- (e/* x q) (e/* (e/+ x dx) y)))]
      [y q (e/- q y) m z sector (e/* 0.5 z dx) (e/+ x dx)])))

(defn value
  "The numbers of f at args, computed by raster. f is either a playground
   kernel factory (args -> (fn [_] number-or-vector), the shape the widget
   hands tie/kernel-binding), whose body raster compiles and evaluates at
   args; or a JVM driver (series, rectangles) that already computes through
   raster and returns its numbers directly. raster/value itself only takes
   an Emmy function of the numbers, so the factory is unwrapped here."
  [f & args]
  (let [r (apply f args)]
    (if (fn? r)
      (apply raster/value (fn [& xs] ((apply f xs) nil)) args)
      r)))

(defn series [n]
  (let [n (long n)
        terms (raster/sample (fn [k] (e// (e/cos (e/* Math/PI k)) (e/+ 1 (e/* 2 k))))
                             (mapv vector (range n)))
        s (reduce + 0.0 terms)
        target (raster/value (fn [x] (e/* 0.25 Math/PI)) 0)]
    [s target (- s target) (raster/value (fn [k] (e// 1 (e/+ 1 (e/* 2 k)))) n)]))

(defn series-kernel [n]
  (fn [_]
    (let [s (reduce e/+ 0 (for [k (range 200)]
                            (e/* (e// (e/- 1 (e// (e/- k n)
                                                     (e/sqrt (e/+ 1e-10 (e/square (e/- k n)))))) 2)
                                 (e// (if (even? k) 1 -1) (+ 1 (* 2 k))))))
          target (e/* Math/PI 0.25)]
      [s target (e/- s target) (e// 1 (e/+ 1 (e/* 2 n)))])))

(defn product [u v du dv]
  (fn [_] [(e/* u v) (e/* u dv) (e/* v du) (e/* du dv)
           (e/- (e/* (e/+ u du) (e/+ v dv)) (e/* u v))]))

(defn rectangles [curve n]
  (let [n (long n)
        ys (mapv #(raster/value (fn [x] (height curve x)) (/ % n)) (range (inc n)))
        a (/ (reduce + (butlast ys)) n)
        b (/ (reduce + (rest ys)) n)]
    [(min a b) (max a b) (Math/abs (- a b)) (/ 1.0 n)]))

(defn lemma-kernel [curve n]
  (fn [_]
    (let [w (e// 1 n)
          gate (fn [i] (e// (e/- 1 (e// (e/- i n) (e/sqrt (e/+ 1e-10 (e/square (e/- i n)))))) 2))
          low (reduce e/+ 0 (for [i (range 100)] (e/* w (gate i) (height curve (e/* i w)))))
          high (reduce e/+ 0 (for [i (range 100)] (e/* w (gate i) (height curve (e/* (inc i) w)))))]
      [low high (e/abs (e/- high low)) w])))

(defn- cross [[ax ay] [bx by]] (- (* ax by) (* ay bx)))
(defn impulse-step [ax ay bx by sx sy offaxis]
  (fn [_]
    (let [fx (e/+ sx offaxis)
          r (e/sqrt (e/+ 0.0225 (e/square (e/- bx fx)) (e/square (e/- by sy))))
          kick (e// 0.04 (e/* r r r))]
      [(e/+ (e/- (e/* 2 bx) ax) (e/* kick (e/- fx bx)))
       (e/+ (e/- (e/* 2 by) ay) (e/* kick (e/- sy by)))])))

(defn polygon [n dt sx sy & [offaxis]]
  ;; the impulse kernel's 1/r^3 kick, simplified by Emmy, expands past the
  ;; JVM's method size: lower it unsimplified (raster/*simplify?*)
  (binding [raster/*simplify?* false]
    (loop [ps [[1.0 0.0] [1.0 (* 1.2 dt)]]]
      (if (>= (count ps) (inc n)) ps
          (let [[[ax ay] [bx by]] (take-last 2 ps)]
            (recur (conj ps (value impulse-step ax ay bx by sx sy (or offaxis 0.0)))))))))

(defn areas
  ([n dt sx sy] (areas n dt sx sy 0.0))
  ([n dt sx sy offaxis]
   (let [ps (polygon n dt sx sy offaxis)
         aa (mapv (fn [[a b]] (* 0.5 (Math/abs (cross (mapv - a [sx sy]) (mapv - b [sx sy]))))) (partition 2 1 ps))]
     [(first aa) (apply max aa) (apply min aa) (- (apply max aa) (apply min aa))])))

(defn ellipse [ecc p theta cx cy]
  (fn [_]
    (let [r (e// p (e/+ 1 (e/* ecc (e/cos theta))))
          x (e/* r (e/cos theta)) y (e/* r (e/sin theta))
          d (e/sqrt (e/+ (e/square (e/- x cx)) (e/square (e/- y cy))))
          f (e// 1 (e/* p (e/square r)))
          torque (e/* (e// f r) (e/- (e/* x cy) (e/* y cx)))]
      [x y r d f torque])))

(defn impulse-readout [n dt sx sy offaxis]
  (fn [_]
    (let [a (e/* 0.6 dt (e/- 1 sx))]
      [a a a (e/abs offaxis)])))

(defn orbit-derivative [centre eccentricity p]
  (fn [_ x y vx vy]
    (let [offset (e/* centre (e/* -1 eccentricity p))
          rx (e/- x offset)
          rr (e/sqrt (e/+ 1e-6 (e/square rx) (e/square y)))
          ;; focus: inverse square; ellipse centre: linear oscillator
          strength (e/+ (e/* (e/- 1 centre) (e// 1 (e/* rr rr rr))) centre)]
      [vx vy (e/* -1 strength rx) (e/* -1 strength y)])))

(defn orbit-state [ecc p theta centre]
  (let [r (raster/value (fn [ecc p theta] (e// p (e/+ 1 (e/* ecc (e/cos theta))))) ecc p theta)
        x (raster/value (fn [r theta] (e/* r (e/cos theta))) r theta)
        y (raster/value (fn [r theta] (e/* r (e/sin theta))) r theta)
        h (Math/sqrt p)
        vr (/ (* ecc (Math/sin theta)) h)
        vt (/ (+ 1 (* ecc (Math/cos theta))) h)
        vx (- (* vr (Math/cos theta)) (* vt (Math/sin theta)))
        vy (+ (* vr (Math/sin theta)) (* vt (Math/cos theta)))
        scale (if (zero? centre) 1.0 (* r (Math/sqrt (/ p (* (- 1 (* ecc ecc)) r)))))]
    [x y (* scale vx) (* scale vy)]))

(defn orbit [ecc p theta centre]
  (let [initial (orbit-state ecc p theta centre)]
    (:us (raster/ode (orbit-derivative centre ecc p) initial 5.0 0.05))))

(defn orbit-readout [ecc p theta centre]
  (fn [_]
    (let [r (e// p (e/+ 1 (e/* ecc (e/cos theta))))
          x (e/* r (e/cos theta)) y (e/* r (e/sin theta))
          shifted (e/+ x (e/* centre ecc p))
          d (e/sqrt (e/+ (e/square shifted) (e/square y)))]
      [x y d (e/- (e/* r (e/+ 1 (e/* ecc (e/cos theta)))) p)
       (e/+ (e/* (e/- 1 centre) (e// 1 (e/square d))) (e/* centre d))])))

(defn orbit-rk4-step [ecc p centre x y vx vy dt]
  (let [f (orbit-derivative centre ecc p)
        a (f 0 x y vx vy)
        at (fn [state k h] (mapv e/+ state (mapv #(e/* h %) k)))
        state [x y vx vy]
        b (apply f 0 (at state a (e/* 0.5 dt)))
        c (apply f 0 (at state b (e/* 0.5 dt)))
        d (apply f 0 (at state c dt))]
    (at state (mapv (fn [i] (e/* (e// dt 6) (e/+ (nth a i) (e/* 2 (nth b i)) (e/* 2 (nth c i)) (nth d i)))) (range 4)) 1)))
