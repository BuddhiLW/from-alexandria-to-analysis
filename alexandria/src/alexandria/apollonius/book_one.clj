(ns alexandria.apollonius.book-one
  "Conics I.11-14 read verbatim, in Heath's letters (Heath 1896, pp. 8-14,
   his Propositions 1-4): the parabola (I.11), the hyperbola (I.12), the
   ellipse (I.13) and the opposite branches (I.14).

   The figure is Heath's, built once in space from the cone and the plane
   of alexandria.apollonius.conics (slope k, tilt theta, P at height h on a
   generator, the base at height zb, the second nappe's base at -zb2):

     A          the apex
     B C        the base diameter in the axial triangle, B on the generator
                through P (Heath: PM is parallel to AC in I.11, meets CA
                produced in I.12, meets AC in I.13)
     P M        the diameter of the section, M on the base line (BC produced
                when the plane leaves the base past C)
     D E        where the plane cuts the base circle (when it does)
     V Q Q'     the foot of the ordinate at abscissa PV = x, the ordinate QV
     H K        the diameter through V parallel to BC, of the circle HQK
     L          PL at right angles to PM in the plane, PL = p
     P' R       P' the other end of the transverse PP'; R on P'L with VR
                parallel to PL, VR = p + c x
     F          AF parallel to PM, meeting BC (produced) in F
     B' C' F' M' D' E' L' V2 Q2 R2   the same on the opposite nappe (I.14)

   `figure` is one Emmy function, compiled to one raster kernel: every
   letter's place on the page (projected from (yaw, pitch)) and every
   length Heath compares. `identities` are Heath's steps, each graded by
   Emmy from the construction with letters (k, theta, h, zb, x), never with
   numbers. The x <-> y tie of each proposition's widget is the symptoma's
   own kernel (alexandria.apollonius.symptom/symptom-figure) and tie
   (alexandria.apollonius.symptom-scene/tie), the ones the symptoma widget
   already runs."
  (:require [alexandria.apollonius.conics :as conics]
            [alexandria.raster :as raster]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [emmy.env :as e]))

;; ---------------------------------------------------------------------------
;; The construction, for symbols and numbers alike

(defn geometry
  "Heath's lengths for the cone of slope k cut by the plane whose diameter
   runs from P = (k h, 0, h) along (-co, 0, si), base at zb, at abscissa
   x. Signed along BC (from C to B) and along PM (from P), so one formula
   serves I.12 and I.13."
  [k co si h zb x]
  (let [n (e/sqrt (e/+ 1 (e/square k)))
        vx (e/- (e/* k h) (e/* x co))
        zv (e/+ h (e/* x si))
        p (e/* 2 k h (e/+ (e/* k si) co))
        c (e/- (e/* k k si si) (e/* co co))]
    {:n n :vx vx :zv zv :p p :c c
     :hv (e/- (e/* k zv) vx)
     :vk (e/+ vx (e/* k zv))
     :qv2 (e/- (e/square (e/* k zv)) (e/square vx))
     :pa (e/* h n) :ba (e/* zb n) :ac (e/* zb n) :bc (e/* 2 k zb)
     :bf (e/* zb (e/+ k (e// co si)))
     :fc (e/* zb (e/- k (e// co si)))
     :af (e// zb si)
     :vr (e/+ p (e/* c x))}))

(defn transverse
  "d = PP', signed along PM (negative when P' is beyond the apex: I.12).
   At the parabola's tilt exactly P' is at infinity: ##Inf, not a boxed
   divide by zero."
  [k co si h]
  (let [den (e/- co (e/* k si))]
    (if (and (number? den) (zero? den))
      ##Inf
      (e// (e/* 2 k h) den))))

;; ---------------------------------------------------------------------------
;; Heath's steps, graded by Emmy

(defn identities
  "{prop [[label diff]]}: each of Heath's steps as a difference that Emmy
   simplifies to 0. I.11 at the tilt where PM is parallel to AC (k sin =
   cos, so sin = 1/n, cos = k/n); I.12-13 at any tilt; I.14 on both
   nappes."
  []
  (let [k 'k theta 'theta h 'h zb 'zb zb2 'zb2 x 'x x2 'x2
        n (e/sqrt (e/+ 1 (e/square k)))
        par (geometry k (e// k n) (e// 1 n) h zb x)
        co (e/cos theta) si (e/sin theta)
        g (geometry k co si h zb x)
        d (transverse k co si h)
        g2 (geometry k co si h zb2 x)
        ;; the opposite nappe: base at -zb2, F' on it, B' C' on CA, BA produced
        bf' (e/* zb2 (e/+ (e// co si) k))
        fc' (e/* zb2 (e/- k (e// co si)))
        af' (e// zb2 si)
        ;; the second branch at abscissa x2 from P', away from P (s = d - x2)
        vx2 (e/- (e/* k h) (e/* (e/- d x2) co))
        zv2 (e/+ h (e/* (e/- d x2) si))]
    {:I.11
     [["PM parallel to AC: c = 0" (:c par)]
      ["the circle HQK: QV² = HV·VK" (e/- (:qv2 par) (e/* (:hv par) (:vk par)))]
      ["HV : PV = BC : AC" (e/- (e/* (:hv par) (:ac par)) (e/* x (:bc par)))]
      ["VK : PA = BC : BA" (e/- (e/* (:vk par) (:ba par)) (e/* (:pa par) (:bc par)))]
      ["HV·VK : PV·PA = BC² : BA·AC"
       (e/- (e/* (:hv par) (:vk par) (:ba par) (:ac par)) (e/* x (:pa par) (e/square (:bc par))))]
      ["PL : PA = BC² : BA·AC gives PL = p" (e/- (e/* (:p par) (:ba par) (:ac par)) (e/* (:pa par) (e/square (:bc par))))]
      ["QV² = PL·PV" (e/- (:qv2 par) (e/* (:p par) x))]]
     :I.12-13
     [["the circle HQK: QV² = HV·VK" (e/- (:qv2 g) (e/* (:hv g) (:vk g)))]
      ["HV : PV = BF : AF" (e/- (e/* (:hv g) (:af g)) (e/* x (:bf g)))]
      ["VK : P′V = FC : AF" (e/- (e/* (:vk g) (:af g)) (e/* (e/- x d) (:fc g)))]
      ["HV·VK : PV·P′V = BF·FC : AF²"
       (e/- (e/* (:hv g) (:vk g) (e/square (:af g))) (e/* x (e/- x d) (:bf g) (:fc g)))]
      ["PL : PP′ = BF·FC : AF² gives PL = p" (e/+ (e/* (:p g) (e/square (:af g))) (e/* d (:bf g) (:fc g)))]
      ["VR : P′V = PL : PP′ (P′, V, R and P′, P, L)" (e/- (e/* (:vr g) d) (e/* (:p g) (e/- d x)))]
      ["QV² = PV·VR" (e/- (:qv2 g) (e/* x (:vr g)))]
      ["VR − PL = c·PV, c = −p/d: overlaps (I.12) or falls short (I.13)"
       (e/- (e/- (:vr g) (:p g)) (e/* (e/- 0 (e// (:p g) d)) x))]]
     :I.14
     [["BF·FC : AF² = B′F′·F′C′ : AF′²"
       (e/- (e/* (:bf g) (:fc g) (e/square af')) (e/* bf' fc' (e/square (:af g))))]
      ["P′L′ : P′P = B′F′·F′C′ : AF′² gives P′L′ = PL" (e/+ (e/* (:p g2) (e/square af')) (e/* d bf' fc'))]
      ["the far branch: Q₂V₂² = P′V₂·V₂R₂, the same p and d"
       (e/- (e/- (e/square (e/* k zv2)) (e/square vx2)) (e/* x2 (e/+ (:p g) (e/* (:c g) x2))))]]}))

(defn graded
  "{prop [{:label :grade}]}."
  []
  (update-vals (identities)
               (fn [rows] (mapv (fn [[label diff]] {:label label :grade (conics/grade-diff diff)}) rows))))

;; ---------------------------------------------------------------------------
;; The kernel: every letter on the page and every length compared

(def points
  "The letters the kernel places, in order (2 numbers each, the screen of
   alexandria.apollonius.conics/project, z up: the scene flips it so the
   apex is at the top and the base below, as Heath draws it)."
  [:A :B :C :P :M :D :E :V :Q :Q' :H :K :L :P' :R :F :B' :C' :F' :M' :D' :E' :L' :V2 :Q2 :R2])

(def scalars
  "The numbers after the points: theta (radians), p c d, PV VR QV QV²,
   HV VK HV.VK, PV.VR, PL.PV, PA BA AC BC, BF FC AF, P'V, the base
   chord's half-width squared w (D, E exist when w > 0) and its twin w2 on
   the opposite base, the second branch's x2, Q2V2² and P'V2.V2R2, VR -
   PL, the height of V, and the ratios Heath compares: BC^2 : BA.AC and
   PL : PA (I.11), BF.FC : AF^2 and PL : PP' (I.12-14)."
  [:theta :p :c :d :x :vr :y :qv2 :hv :vk :hvvk :rect :plpv :pa :ba :ac :bc :bf :fc :af :p'v
   :w :w2 :x2 :qv2' :rect2 :overlap :zv :rat-bc :rat-pa :rat-bf :rat-pp])

(defn figure
  "Heath's figure for slope k, tilt deg (degrees), P at height h, the bases
   at zb and -zb2, abscissa x on the near branch and x2 on the far one,
   seen from (yaw, pitch): `points` then `scalars`."
  [k deg h zb zb2 x x2 yaw pitch]
  (fn [[_]]
    (let [theta (e/* deg (/ Math/PI 180))
          co (e/cos theta) si (e/sin theta)
          {:keys [vx zv p c hv vk qv2 pa ba ac bc bf fc af vr]} (geometry k co si h zb x)
          d (transverse k co si h)
          sp (fn [s] [(e/- (e/* k h) (e/* s co)) (e/+ h (e/* s si))])
          pr (fn [X Y Z] (conics/project yaw pitch [X Y Z]))
          y (e/sqrt (e/abs qv2))
          sm (e// (e/- zb h) si)
          [mx _] (sp sm)
          w (e/- (e/square (e/* k zb)) (e/square mx))
          hw (e/sqrt (e/abs w))
          sm' (e// (e/- (e/- 0 zb2) h) si)
          [mx' _] (sp sm')
          w2 (e/- (e/square (e/* k zb2)) (e/square mx'))
          hw2 (e/sqrt (e/abs w2))
          [px' pz'] (sp d)
          [vx2 vz2] (sp (e/- d x2))
          qv2' (e/- (e/square (e/* k vz2)) (e/square vx2))
          y2 (e/sqrt (e/abs qv2'))
          vr2 (e/+ p (e/* c x2))
          fx (e/- 0 (e/* zb (e// co si)))
          fx' (e/* zb2 (e// co si))
          pts {:A (pr 0 0 0)
               :B (pr (e/* k zb) 0 zb) :C (pr (e/- 0 (e/* k zb)) 0 zb)
               :P (pr (e/* k h) 0 h)
               :M (pr mx 0 zb) :D (pr mx hw zb) :E (pr mx (e/- 0 hw) zb)
               :V (pr vx 0 zv) :Q (pr vx y zv) :Q' (pr vx (e/- 0 y) zv)
               :H (pr (e/* k zv) 0 zv) :K (pr (e/- 0 (e/* k zv)) 0 zv)
               :L (pr (e/* k h) p h)
               :P' (pr px' 0 pz')
               :R (pr vx vr zv)
               :F (pr fx 0 zb)
               :B' (pr (e/- 0 (e/* k zb2)) 0 (e/- 0 zb2)) :C' (pr (e/* k zb2) 0 (e/- 0 zb2))
               :F' (pr fx' 0 (e/- 0 zb2))
               :M' (pr mx' 0 (e/- 0 zb2)) :D' (pr mx' hw2 (e/- 0 zb2)) :E' (pr mx' (e/- 0 hw2) (e/- 0 zb2))
               :L' (pr px' p pz')
               :V2 (pr vx2 0 vz2) :Q2 (pr vx2 y2 vz2) :R2 (pr vx2 vr2 vz2)}
          nums {:theta theta :p p :c c :d d :x x :vr vr :y y :qv2 qv2 :hv hv :vk vk
                :hvvk (e/* hv vk) :rect (e/* x vr) :plpv (e/* p x)
                :pa pa :ba ba :ac ac :bc bc :bf bf :fc fc :af af :p'v (e/- x d)
                :w w :w2 w2 :x2 x2 :qv2' qv2' :rect2 (e/* x2 vr2) :overlap (e/- vr p) :zv zv
                :rat-bc (e// (e/square bc) (e/* ba ac)) :rat-pa (e// p pa)
                :rat-bf (e/abs (e// (e/* bf fc) (e/square af))) :rat-pp (e/abs (e// p d))}]
      (vec (concat (mapcat pts points) (map nums scalars))))))

(defn section-figure
  "The section seen from (yaw, pitch), at angle phi round the axis (the
   generator through (k cos phi, k sin phi, 1)): [u v Z], Z for the page
   to keep only what lies between the bases."
  [k deg h yaw pitch]
  (fn [[phi]]
    (let [theta (e/* deg (/ Math/PI 180))
          [X Y Z] ((conics/section-at k theta h) phi)
          [u v] (conics/project yaw pitch [X Y Z])]
      [u v Z])))

(defn circle-figure
  "The circle of the cone at height z (a base, or HQK through V), at
   angle phi, seen from (yaw, pitch)."
  [k z yaw pitch]
  (fn [[phi]]
    (conics/project yaw pitch [(e/* k z (e/cos phi)) (e/* k z (e/sin phi)) z])))

(def figures
  "The kernels of the walk, for alexandria.medium.kernel."
  {:heath {:f figure :params [0.6 59.04 0.7 2.2 1.6 0.9 0.5 0.4 0.25] :state [0] :opts {:simplify? false}}
   :section {:f section-figure :params [0.6 59.04 0.7 0.4 0.25] :state [0.5] :opts {:simplify? false}}
   :circle {:f circle-figure :params [0.6 2.2 0.4 0.25] :state [0.5]}})

(defn named
  "{name value} of one output row of `figure`."
  [row]
  (let [np (count points)]
    (merge (zipmap points (map vec (partition 2 (take (* 2 np) row))))
           (zipmap scalars (drop (* 2 np) row)))))

(defn numbers
  "{name value} of `figure` at the given parameters, from one raster
   kernel (alexandria.raster/value)."
  [k deg h zb zb2 x x2 yaw pitch]
  (named (raster/value (fn [k deg h zb zb2 x x2 yaw pitch] ((figure k deg h zb zb2 x x2 yaw pitch) [0]))
                       k deg h zb zb2 x x2 yaw pitch)))

;; ---------------------------------------------------------------------------
;; The walk: Heath's sentences, the cases, the tilt of each

(def resource "alexandria/apollonius/book_one.edn")

(defn walk
  "{:source :props [{:id :case :tilt :x :title :greek :heiberg :steps [{:id
   :text :quote :cite :introduces}]}]} from the edn."
  []
  (edn/read-string (slurp (io/resource resource))))

(def k0 "The cone's slope (the symptoma widget's)." 0.6)

(defn parabola-tilt
  "The tilt (degrees) at which PM runs parallel to AC, by raster."
  []
  (raster/value (fn [k] (e/* (conics/parabola-tilt k) (/ 180 Math/PI))) k0))
