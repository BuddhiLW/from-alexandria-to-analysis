(ns alexandria.medium.anim
  "Manim's animation vocabulary as pure functions of a step's progress p in
   [0 1], for the browser and the JVM alike.

     rate functions  linear, smooth (Manim's sigmoid), smoothstep, rush-into,
                     rush-from, there-and-back, there-and-back-with-pause,
                     double-smooth, wiggle; the clj originals are
                     desargues.videos.timeline, smooth is Manim's own
     play            the eased progress of an animation that runs over the
                     part [a b] of the step
     lagged          Manim's LaggedStart: the eased progress of n animations
                     whose starts are spread by lag-ratio
     create, fade    what an element looks like at a progress: Create's
                     drawn fraction of a stroke, FadeIn's opacity and shift"
  (:require [alexandria.medium.math :as m]))

(defn clamp01 [t] (max 0 (min 1 t)))

(defn linear [t] (clamp01 t))

(defn- sigmoid [x] (/ 1 (+ 1 (m/exp (- x)))))

(defn smooth
  "Manim's smooth: a sigmoid of inflection 10 rescaled to [0 1], flatter at
   both ends than smoothstep."
  [t]
  (let [error (sigmoid -5)]
    (clamp01 (/ (- (sigmoid (* 10 (- (clamp01 t) 0.5))) error) (- 1 (* 2 error))))))

(defn smoothstep [t] (let [t (clamp01 t)] (- (* 3 t t) (* 2 t t t))))

(defn rush-into "Fast start, slow end." [t] (let [t (clamp01 t)] (* 2 t (- 1 (/ t 2)))))
(defn rush-from "Slow start, fast end." [t] (- 1 (rush-into (- 1 t))))

(defn there-and-back "0 to 1 and back to 0." [t]
  (let [t (clamp01 t)] (smooth (if (< t 0.5) (* 2 t) (* 2 (- 1 t))))))

(defn there-and-back-with-pause [t]
  (let [t (clamp01 t)]
    ;; (/ 1 3), not 1/3: a ratio literal does not compile as ClojureScript
    (cond (< t (/ 1 3)) (smooth (* 3 t)) (< t (/ 2 3)) 1 :else (smooth (* 3 (- 1 t))))))

(defn double-smooth [t] (smooth (smooth t)))

(defn wiggle "A shake that starts and ends at rest." [t]
  (let [t (clamp01 t)] (* (m/sin (* m/pi t)) (m/sin (* m/pi 2 t)))))

(defn play
  "The progress, eased by rate (default smooth), of an animation that runs
   over the part [a b] of a step at progress p: 0 before a, 1 after b."
  ([p a b] (play p a b smooth))
  ([p a b rate] (rate (clamp01 (/ (- p a) (- b a))))))

(defn lagged
  "Manim's LaggedStart over the part [a b] of a step: the eased progress of
   each of n animations, each starting lag-ratio of an animation's length
   after the one before."
  ([p n lag-ratio a b] (lagged p n lag-ratio a b smooth))
  ([p n lag-ratio a b rate]
   (let [span (- b a)
         each (/ span (+ 1 (* lag-ratio (max 0 (dec n)))))]
     (mapv (fn [i] (let [start (+ a (* i lag-ratio each))]
                     (play p start (+ start each) rate)))
           (range n)))))

(defn create
  "Create: the stroke attributes that show a fraction f of an element's
   outline drawn (the element is given pathLength 1)."
  [f]
  {:path-length 1 :stroke-dasharray "1 1" :stroke-dashoffset (- 1 (clamp01 f))})

(defn fade
  "FadeIn (f from 0 to 1) or FadeOut (from 1 to 0) with Manim's shift: the
   group attributes of an element faded to f and moved by (1 - f) shift."
  ([f] (fade f [0 0]))
  ([f [dx dy]]
   (let [f (clamp01 f) k (- 1 f)]
     {:opacity f :transform (str "translate(" (* k dx) "," (- (* k dy)) ")")})))
