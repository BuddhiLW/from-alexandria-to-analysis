(ns alexandria.medium.timeline
  "A proof's playback as a value. Pure: every function takes a Timeline and
   returns one.

     Timeline  {:step i :progress p :playing? bool :durations [ms ...]}
               step indexes the proof's steps, progress p in [0 1] runs
               through the current step in (durations step) milliseconds."
  (:require [alexandria.medium.math :as m]
            [alexandria.medium.anim :as anim]))

(def default-duration 4000)

(defn timeline
  "The timeline of `steps` ([{:stage ...}]), paused at the start. durations:
   {stage ms}, default-duration for the stages it leaves out."
  [steps durations]
  {:step 0 :progress 0 :playing? false
   :durations (mapv #(get durations (:stage %) default-duration) steps)})

(defn step-count [tl] (count (:durations tl)))
(defn last-step? [tl] (= (:step tl) (dec (step-count tl))))
(defn ended? [tl] (and (last-step? tl) (>= (:progress tl) 1)))

(defn advance
  "tl after dt milliseconds: progress moves while playing, rolls into the next
   step at 1, and stops at the end of the last step."
  [tl dt]
  (if-not (:playing? tl)
    tl
    (let [p (+ (:progress tl) (/ dt (nth (:durations tl) (:step tl))))]
      (cond
        (< p 1) (assoc tl :progress p)
        (last-step? tl) (assoc tl :progress 1 :playing? false)
        :else (assoc tl :step (inc (:step tl)) :progress 0)))))

(defn goto
  "tl at the start of step i (clamped), playing."
  [tl i]
  (assoc tl :step (max 0 (min (dec (step-count tl)) i)) :progress 0 :playing? true))

(defn seek
  "tl paused at progress p of the current step."
  [tl p]
  (assoc tl :progress (max 0 (min 1 p)) :playing? false))

(defn toggle
  "tl playing if paused, paused if playing; from the end, plays the last step again."
  [tl]
  (if (ended? tl)
    (assoc tl :progress 0 :playing? true)
    (update tl :playing? not)))

;; ---------------------------------------------------------------------------
;; Shaping progress inside a step

(defn ease
  "Manim's smooth of p clamped to [0 1] (alexandria.medium.anim/smooth)."
  [p]
  (anim/smooth p))

(defn window
  "Progress through the part [a b] of a step at progress p, eased: 0 before
   a, 1 after b."
  [p a b]
  (ease (/ (- p a) (- b a))))

(defn stages
  "Progress p split over k equal parts: [i t], the part index and the eased
   progress inside it; p = 1 is [(dec k) 1]."
  [p k]
  (let [q (* (max 0 (min 1 p)) k)
        i (min (dec k) (long (m/floor q)))]
    [i (ease (- q i))]))
