(ns alexandria.galileo.motion
  "Galileo, Two New Sciences (1638): naturally accelerated motion (Third
   Day) and the motion of projectiles (Fourth Day), as Emmy values.

   Galileo's quantities are magnitudes in ratio; here they are Emmy
   symbols: a the acceleration (equal increments of speed in equal times),
   t and T times, u a uniform horizontal speed, k the fall in the first
   unit of time.

     speed, distance   v = a t and s = a t^2 / 2, with s' = v by Emmy's D
     identities        the claims of Third Day Props I, II and Cor. I,
                       Fourth Day Props I and VII, as differences that
                       Emmy simplifies to 0 (graded :grade/proved)
     marks             the inclined plane: exact rational distances and
                       the water clock's times
     figures           Emmy functions of one point, for media to compile

   Reuse searched: emmy.env (D, simplify, square), alexandria.grade (the
   Grade ADT), alexandria.proofs (Collect for step lists). Neither Emmy nor
   desargues names Galileo's kinematics, so the functions below are new;
   each is a line or two of Emmy arithmetic."
  (:require [alexandria.grade :as grade]
            [alexandria.proofs :as proofs]
            [emmy.env :as e]
            [hive-dsl.result :as r]
            [alexandria.raster :as raster]))

;; ---------------------------------------------------------------------------
;; Naturally accelerated motion

(defn speed
  "Speed at time t of a body starting from rest with acceleration a."
  [a]
  (fn [t] (e/* a t)))

(defn distance
  "Distance from rest at time t: the area under the speed line, a t^2 / 2."
  [a]
  (fn [t] (e/* 1/2 a (e/square t))))

(defn gnomon
  "The n-th odd number, the L-shaped band that turns the square of side n
   into the square of side n + 1."
  [n]
  (e/+ (e/* 2 n) 1))

(defn odd-steps
  "Distances in the first n equal times, in units of the first: 1 3 5 7 ..."
  [n]
  (let [s (distance 2)]
    (mapv (fn [i] (- (s (inc i)) (s i))) (range n))))

(defn mean-speed-rectangle
  "Distance covered in time T at the uniform mean speed (0 + aT)/2:
   the rectangle AGFB of Galileo's Fig. 47."
  [a T]
  (e/* T (e// (e/+ 0 ((speed a) T)) 2)))

;; ---------------------------------------------------------------------------
;; The inclined plane (Third Day, pp. 178-179)

(def plane
  "Galileo's groove: about 12 cubits long, one end lifted one or two
   cubits. Lengths in cubits; the ball is timed by weighing water."
  {:length 12 :lift 1})

(defn marks
  "Where the ball is at the ends of the first n equal times, as fractions
   of the first mark: 1 4 9 16 ..."
  [n]
  (mapv (fn [i] (* (inc i) (inc i))) (range n)))

(defn time-for
  "The time to roll a fraction f of the groove, the whole taking time 1:
   the square root of f. A quarter of the length takes half the time."
  [f]
  (e/sqrt f))

;; ---------------------------------------------------------------------------
;; Projectiles (Fourth Day)

(defn projectile
  "Where the projectile is at time t: carried uniformly sideways by u and
   fallen k t^2. [x drop], drop measured down from the edge b of the plane."
  [u k]
  (fn [t] [(e/* u t) (e/* k (e/square t))]))

(defn latus-rectum
  "Apollonius' parameter p of the semi-parabola traced by (projectile u k):
   ordinate^2 = p x abscissa, ordinates sideways, abscissas the fall."
  [u k]
  (e// (e/square u) k))

(defn range-of
  "The range on the level of a shot whose speed has horizontal part c and
   vertical part w, under acceleration g: 2 c w / g."
  [c w g]
  (e// (e/* 2 c w) g))

;; ---------------------------------------------------------------------------
;; The claims, graded

(def identities
  "{proposition-id [[label difference] ...]}, each difference zero."
  (let [a 'a T 'T t 't t1 't_1 t2 't_2 n 'n u 'u k 'k c 'c w 'w g 'g
        s (distance a)
        v-max ((speed a) T)
        [x1 y1] ((projectile u k) t1)
        [x2 y2] ((projectile u k) t2)
        [x y] ((projectile u k) t)
        ;; the components of a shot at 45 + delta and at 45 - delta, over v/sqrt 2,
        ;; with C = cos delta and S = sin delta
        C 'C S 'S]
    {:galileo/third-day-1
     [["the speed grows as the time: s' = v" (e/- ((e/D s) t) ((speed a) t))]
      ["triangle AEB = rectangle AGFB: s(T) = T (aT)/2" (e/- (s T) (mean-speed-rectangle a T))]
      ["what AGI lacks, IEF supplies: triangle AGI = triangle IEF"
       (e/- (e/* 1/2 (e// T 2) (e// v-max 2))
            (e/* 1/2 (e/- T (e// T 2)) (e/- v-max (e// v-max 2))))]]
     :galileo/third-day-2
     [["distances as the squares of the times" (e/- (e/* (s t1) (e/square t2)) (e/* (s t2) (e/square t1)))]
      ["the ratio of the speeds is the ratio of the times" (e/- (e/* ((speed a) t1) t2) (e/* ((speed a) t2) t1))]]
     :galileo/corollary-1
     [["(n+1)^2 - n^2 = 2n + 1" (e/- (e/- (e/square (e/+ n 1)) (e/square n)) (gnomon n))]
      ["the distance in the (n+1)-th time is 2n+1 times the first" (e/- (e/- (s (e/+ n 1)) (s n)) (e/* (gnomon n) (s 1)))]
      ["a gnomon turns the square n^2 into the square (n+1)^2"
       (e/- (e/+ (e/square n) (gnomon n)) (e/square (e/+ n 1)))]]
     :galileo/fourth-day-1
     [["the symptom of Conics I.11: ordinate^2 = p x abscissa" (e/- (e/square x) (e/* (latus-rectum u k) y))]
      ["Galileo's form: hl^2 : fg^2 = lb : bg" (e/- (e/* (e/square x1) y2) (e/* (e/square x2) y1))]]
     :galileo/fourth-day-7
     [["Euclid II.5: c^2 + w^2 - 2cw = (c - w)^2"
       (e/- (e/- (e/+ (e/square c) (e/square w)) (e/* 2 c w)) (e/square (e/- c w)))]
      ["so range = (v^2 - (c - w)^2)/g, v^2 = c^2 + w^2: greatest when c = w, at 45 degrees"
       (e/- (range-of c w g) (e// (e/- (e/+ (e/square c) (e/square w)) (e/square (e/- c w))) g))]
      ["at 45 degrees the amplitude is double the altitude"
       (e/- (e// (range-of w w g) 2) (e/* 2 (e// (e/square w) (e/* 2 g))))]
      ["shots at 45 + delta and 45 - delta reach the same range (Prop. VIII)"
       (e/- (range-of (e/- C S) (e/+ C S) g) (range-of (e/+ C S) (e/- C S) g))]
      ["the slope of sin cos in the angle is cos^2 - sin^2, zero where sin = cos"
       (e/- ((e/D (fn [th] (e/* (e/sin th) (e/cos th)))) 'theta)
            (e/- (e/square (e/cos 'theta)) (e/square (e/sin 'theta))))]]}))

(defn- grade-of [kind diffs]
  (let [res (grade/grade kind diffs)]
    (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails)))

(defn graded
  "[{:label :grade}] of proposition id's identities."
  [id]
  (mapv (fn [[label diff]] {:label label :grade (grade-of :symbolic [diff])})
        (identities id)))

(defn plane-graded
  "The inclined plane's measured ratios, exact: the marks fall at 1:4:9:16
   and a quarter of the groove takes half the time."
  []
  (let [s (distance 2)]
    [{:label "marks at the ends of equal times: 1 : 4 : 9 : 16"
      :grade (grade-of :symbolic [(e/- (e/* (s 4) 1) (e/* (s 1) 16)) (e/- (s 3) (e/* 9 (s 1)))])}
     {:label "a quarter of the length in half the time"
      :grade (grade-of :symbolic [(e/- (time-for 1/4) 1/2)])}]))

(defn max-range-numeric
  "The elevation (degrees, 1..89) of the longest shot, found by trying
   every whole degree: a numeric check of Prop. VII's corollary. Emmy
   writes the range factor sin th cos th of the elevation in degrees; a
   raster kernel evaluates it at each degree (alexandria.raster/sample)."
  []
  (let [degs (range 1 90)
        rng (raster/sample (fn [deg] (let [th (e/* deg (e// Math/PI 180))] (e/* (e/sin th) (e/cos th))))
                           (mapv vector degs))]
    (first (apply max-key second (map vector degs rng)))))

;; ---------------------------------------------------------------------------
;; Figures, for media

(defn shot-figure
  "Figure: the arc of a shot at elevation deg (degrees) with v^2/g = v2g,
   (fn [deg v2g] (fn [[u]] [x y])), u in [0 1] running from the gun to the
   landing point at range 2 v2g sin cos."
  [deg v2g]
  (fn [[u]]
    (let [th (e/* deg (e// Math/PI 180)) co (e/cos th) si (e/sin th)
          x (e/* u 2 v2g si co)]
      [x (e/- (e/* x (e// si co)) (e// (e/square x) (e/* 2 v2g (e/square co))))])))

(def figures
  "Every moving figure: {:f figure :params initial-params :state initial-state}.
   :throw   the projectile at time t (state [t]), params [u k]
   :roll    the ball on the groove at time t, params [a cos sin] of the
            groove's slope, drawn from the raised end (0, 1.6)
   :shot    the arc of a shot at elevation deg, params [deg v2g], state [u]
   :speed   the speed s at elevation deg split into its horizontal and
            vertical components [c w] = s [cos sin], params [deg], state [s]"
  {:throw {:f (fn [u k] (fn [[t]] [(e/* u t) (e/- 0 (e/* k (e/square t)))]))
           :params [1 0.25] :state [0]}
   :roll {:f (fn [a co si] (fn [[t]] (let [d (e/* 1/2 a (e/square t))]
                                       [(e/* d co) (e/- 1.6 (e/* d si))])))
          :params [0.75 0.995 0.1] :state [0]}
   :shot {:f shot-figure :params [45 4] :state [0]}
   :speed {:f (fn [deg] (fn [[s]] (let [th (e/* deg (e// Math/PI 180))]
                                    [(e/* s (e/cos th)) (e/* s (e/sin th))])))
           :params [45] :state [1]}})

;; ---------------------------------------------------------------------------
;; The propositions as data

(def proofs-resource "alexandria/galileo/two_new_sciences.proofs.edn")

(defn proof [] (proofs/read-proofs proofs-resource))
