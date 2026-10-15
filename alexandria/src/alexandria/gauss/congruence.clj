(ns alexandria.gauss.congruence
  "Gauss, Disquisitiones arithmeticae (1801), sections I-IV, as special
   cases of alexandria.structure: nothing general is defined here.

     clock         the tie b = a + k n (art. 2: every residue of a is
                   a + k m) over the clock of n hours; its readout puts a
                   and b on the n-gon and names a = b (mod n) live. The
                   clock's hand is the action of structure.groups/cyclic n
                   on the vertices, compiled for raster through
                   structure.arith/wrap.
     powers        the walk x -> a x (mod m): the orbit of 1 under the
                   powers of a in structure.groups/units-mod m; its length
                   is structure.group/order, which divides phi(m) (Fermat,
                   Euler, art. 49-50); a primitive root when it fills the
                   units (art. 57).
     table         the multiplication table of structure.ring/zmod n: row a
                   a permutation exactly when a is a unit; the unit group
                   named by structure.ring/structure (cyclic or not).
     reciprocity   the Legendre character structure.ring/legendre on a
                   grid of odd primes p, q; Gauss' fundamental theorem
                   (art. 131): (p/q)(q/p) = -1 exactly when p = q = 3 (mod 4).
     primes        pi(x) against Li(x) = integral 2..x of dt/log t (raster's
                   quadrature), the bridge to Dirichlet and Riemann.

   Ties, readouts and named cases are structure.tie data; every number the
   page shows comes from a raster kernel or a structure instance; the
   grades compare the two."
  (:require [alexandria.grade :as grade]
            [alexandria.raster :as raster]
            [alexandria.structure.arith :as a]
            [alexandria.structure.group :as g]
            [alexandria.structure.groups :as gs]
            [alexandria.structure.ring :as ring]
            [alexandria.structure.tie :as st]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

;; ---------------------------------------------------------------------------
;; 1. The clock: b = a + k n

(def clock
  "The tie of art. 2 on the clock of n hours: moving n, a or b re-solves k;
   moving k re-solves b = a + k n."
  (let [k-of (fn [n a b] (e/divide (e/- b a) n))]
    (st/tie {:tie/kind :sources :keys [:n :a :b :k]
             :ranges {:n [2 24 1] :a [-30 30 1] :b [-30 30 1] :k [-15 15 0.01]}
             :sources {:n (fn [n a b _] [n a b (k-of n a b)])
                       :a (fn [n a b _] [n a b (k-of n a b)])
                       :b (fn [n a b _] [n a b (k-of n a b)])
                       :k (fn [n a _ k] [n a (e/+ a (e/* k n)) k])}})))

(def clock-labels {:n "modulus n (hours)" :a "a" :b "b" :k "k = (b − a)/n"})

(defn hand
  "The vertex k of the n-gon after the rotation by a: (a + k) mod n, the
   action of structure.groups/cyclic, branch-free for raster."
  [a k n]
  (a/wrap (e/+ a k) n))

(defn vertex "Vertex k of the regular n-gon, 0 at the top, clockwise." [k n]
  (let [t (e/divide (e/* 2 Math/PI k) n)] [(e/sin t) (e/cos t)]))

(def clock-readout
  {:outputs [:ra :rb :gap :ax :ay :bx :by]
   :f (fn [n a b _]
        (let [ra (a/wrap a n) rb (a/wrap b n) w (a/wrap (e/- b a) n)
              [ax ay] (vertex ra n) [bx by] (vertex rb n)]
          ;; gap: distance of b - a from the nearest multiple of n
          {:ra ra :rb rb :gap (e/- (e/divide n 2) (e/abs (e/- w (e/divide n 2))))
           :ax ax :ay ay :bx bx :by by}))})

(def clock-cases
  "a = b (mod n) when b - a sits on a multiple of n (the band absorbs the
   single precision of the page's WebAssembly kernels)."
  {:cases [[:congruent [:abs< :gap 1e-3]] [:incongruent :else]]})

;; ---------------------------------------------------------------------------
;; 2. Powers: the orbit of 1 under a in (Z/m)^*

(defn step
  "One step of the walk: x -> a x (mod m), the action of a in
   structure.groups/units-mod m, for raster."
  [a x m]
  (a/wrap (e/* a x) m))

(def moduli "The moduli the page offers." (vec (range 3 41)))

(defn walk
  "The powers a^0 = 1, a, a^2 ... (mod m) by structure.group/act, until
   they repeat."
  [m a]
  (let [U (gs/units-mod m)]
    (loop [x 1 seen []]
      (if (some #{x} seen) seen (recur (g/act U a x) (conj seen x))))))

(defn modulus-facts
  "For the modulus m, from structure.ring and structure.group: phi(m), the
   units, each unit's order, the primitive roots and the unit group's name."
  [m]
  (let [{G :group els :elements} (ring/unit-group (ring/zmod m))
        s (ring/structure G els)]
    {:m m :phi (count els) :units els :orders (ring/element-orders G els)
     :roots (ring/generators G els) :name (:name s) :cyclic? (:cyclic? s)}))

(def power-cases
  "Named from the walk's readout: :order (0 when the walk never returns
   to 1), :phi."
  {:cases [[:not-a-unit [:< :order 0.5]]
           [:primitive-root [:abs< :short 0.5]]
           [:divides :else]]})

;; ---------------------------------------------------------------------------
;; 3. The multiplication table

(def table-moduli (vec (range 2 31)))

(defn table-facts
  "Row by row: a permutation? (structure.ring/permutation-row?) and a
   unit? (gcd 1), and the unit group's structure."
  [n]
  (let [R (ring/zmod n)
        {G :group els :elements} (ring/unit-group R)]
    {:n n :perm (mapv #(ring/permutation-row? R %) (ring/elements R))
     :units els :structure (ring/structure G els)}))

;; ---------------------------------------------------------------------------
;; 4. Reciprocity

(def odd-primes (vec (filter #(and (odd? %) (every? (fn [d] (pos? (mod % d))) (range 2 %))) (range 3 60))))

(defn reciprocity-grid
  "[[{:p :q :pq (p/q) :qp (q/p) :both3 bool :law? bool}]] over odd-primes,
   the symbols from structure.ring/legendre."
  []
  (vec (for [p odd-primes]
         (vec (for [q odd-primes]
                (let [pq (ring/legendre p q) qp (ring/legendre q p)
                      both3 (and (= 3 (mod p 4)) (= 3 (mod q 4)))]
                  {:p p :q q :pq pq :qp qp :both3 both3
                   :law? (or (= p q) (= (* pq qp) (if both3 -1 1)))}))))))

;; ---------------------------------------------------------------------------
;; 5. pi(x) and Li(x)

(def xs "The sample points of the bridge, log-spaced 10 .. 10^6."
  (mapv #(Math/round (Math/pow 10 (+ 1 (* 5 (/ % 120.0))))) (range 121)))

(defn prime-count
  "pi(x) at each of xs, by one sieve."
  [xs]
  (let [n (long (apply max xs))
        sieve (boolean-array (inc n) true)]
    (aset sieve 0 false) (aset sieve 1 false)
    (doseq [i (range 2 (inc (long (Math/sqrt n)))) :when (aget sieve i)
            j (range (* i i) (inc n) i)]
      (aset sieve j false))
    (let [cum (long-array (inc n))]
      (loop [i 1] (when (<= i n) (aset cum i (+ (aget cum (dec i)) (if (aget sieve i) 1 0))) (recur (inc i))))
      (mapv #(aget cum (long %)) xs))))

(defn Li "Li(x) = integral from 2 to x of dt / log t, by raster's quadgk." [x]
  (:value (raster/integral (fn [t] (e/divide 1 (e/log t))) 2 x)))

(defn primes-table
  "At each of xs: pi(x) (sieve), Li(x) (raster quadgk), x/log x (raster),
   and :better = |Li - pi| - |x/log x - pi| (negative where Li is closer)."
  []
  (let [pis (prime-count xs)]
    (mapv (fn [x p]
            (let [li (Li x) xl (raster/value (fn [x] (e/divide x (e/log x))) x)]
              {:x x :pi p :li li :xlog xl :better (- (Math/abs (- li p)) (Math/abs (- xl p)))}))
          xs pis)))

(def prime-cases
  "Which approximation of pi(x) is closer: Gauss' Li or Legendre's x/log x."
  {:cases [[:li-closer [:< :better 0]] [:xlog-closer :else]]})

(def table-cases
  "Row a of the multiplication table: :repeats counts values that occur twice."
  {:cases [[:permutation [:abs< :repeats 0.5]] [:not-a-permutation :else]]})

(def group-cases
  "The unit group: :factors counts its cyclic factors (structure.ring/structure)."
  {:cases [[:cyclic [:< :factors 1.5]] [:not-cyclic :else]]})

(def reciprocity-cases
  "Art. 131 for the odd primes p, q, read from p mod 4 and q mod 4."
  {:cases [[:same-prime [:abs< :pq-diff 0.5]]
           [:exceptional [:and [:> :p4 2] [:> :q4 2]]]
           [:ordinary :else]]})

;; ---------------------------------------------------------------------------
;; Derivations and grades

(defn- variant [res] (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails))

(defn- numeric [diffs tol] (variant (grade/grade :numeric (map double diffs) tol)))

(def tex
  {:congruence "b \\equiv a \\pmod n \\iff n \\mid b - a \\iff b = a + k\\,n,\\ k \\in \\mathbb Z"
   :fermat "a^{p} - a = \\sum_{j=1}^{p-1}\\binom{p}{j}a^{j} + \\big((a-1)^p - (a-1)\\big) - \\ldots,\\qquad p \\mid \\binom{p}{j}\\ (0<j<p)"
   :order "a^{t} \\equiv 1 \\pmod p,\\ t \\text{ least} \\implies t \\mid p - 1,\\qquad a^{\\varphi(m)} \\equiv 1 \\pmod m"
   :law "\\left(\\frac{p}{q}\\right)\\left(\\frac{q}{p}\\right) = (-1)^{\\frac{p-1}{2}\\cdot\\frac{q-1}{2}}"
   :euler "\\left(\\frac{a}{p}\\right) \\equiv a^{\\frac{p-1}{2}} \\pmod p"
   :li "\\pi(x) \\sim \\operatorname{Li}(x) = \\int_2^x \\frac{dt}{\\log t}"})

(defn derived-tex
  "Emmy's own forms: the tie solved back, and the binomial of Euler's proof."
  []
  {:k-from-b (e/->TeX (e/simplify (e/divide (e/- (e/+ 'a (e/* 'k 'n)) 'a) 'n)))
   :binomial-5 (e/->TeX (e/simplify (e/- (e/expt (e/+ 'a 1) 5) (e/expt 'a 5) 1)))})

(defn binomial-diff
  "(a+1)^p - a^p - 1 - p * (the integer polynomial Euler divides out): 0
   when every middle binomial coefficient is p times an integer."
  [p]
  (let [middle (reduce e/+ (for [j (range 1 p)]
                             (e/* (/ (reduce * (range (inc (- p j)) (inc p))) (reduce * (range 1 (inc j))) p)
                                  (e/expt 'a j))))]
    (e/- (e/expt (e/+ 'a 1) p) (e/expt 'a p) 1 (e/* p middle))))

(defn clock-gaps
  "The raster clock kernel against structure.groups/cyclic's action, every
   n <= 24, a, k in -30..30 sampled."
  []
  (let [pts (for [n (range 2 25) a [-30 -17 -1 0 5 23] k (range n)] [a k n])
        vs (raster/sample hand (mapv #(mapv double %) pts))]
    (map (fn [[a k n] v] (- v (g/act (gs/cyclic n) (mod a n) k))) pts vs)))

(defn raster-walk
  "x_0 = 1, x_{j+1} = step(a, x_j, m) by the raster kernel, each x held to
   the residue it stands for (Math/round: the state is an integer, the
   kernel's trigonometry leaves 1e-15 on it), n steps: [x_0 .. x_n]."
  [a m n]
  (let [kern (raster/kernel step 3)]
    (vec (take (inc n) (iterate #(double (Math/round (double (kern (double a) % (double m))))) 1.0)))))

(defn walk-gaps
  "The raster walk against structure.group/order: for every modulus and
   unit, the raster steps from 1 return to 1 after exactly the order, and
   not before."
  []
  (for [m moduli a (:units (modulus-facts m))
        :let [t (:ok (g/order (gs/units-mod m) a))
              xs (raster-walk a m t)]]
    (+ (Math/abs (- (peek xs) 1.0))
       (if (some #(== % 1.0) (rest (pop xs))) 1.0 0.0))))

(defn graded []
  (let [grid (reciprocity-grid)
        pt (primes-table)]
    [{:label "the tie solved back: from b = a + k n, (b − a)/n = k (symbolic)"
      :grade (variant (grade/grade :symbolic [(e/- (e/divide (e/- (e/+ 'a (e/* 'k 'n)) 'a) 'n) 'k)]))}
     {:label "Euler's proof (art. 50): (a+1)^p − a^p − 1 is p times an integer polynomial, p = 3, 5, 7, 11, 13 (symbolic)"
      :grade (variant (grade/grade :symbolic (map binomial-diff [3 5 7 11 13])))}
     {:label "the clock's raster kernel is the action of Z/n (structure.groups/cyclic) on the n-gon, n ≤ 24 (raster)"
      :grade (numeric (clock-gaps) 1e-9)}
     {:label "the raster walk x → a x (mod m) returns to 1 after exactly the order of a in (Z/m)^* (structure.group/order), every unit of every m ≤ 40"
      :grade (numeric (walk-gaps) 1e-9)}
     {:label "the order of every unit divides φ(m), m ≤ 40 (Fermat, Euler; art. 49)"
      :grade (numeric (for [m moduli :let [{:keys [phi orders]} (modulus-facts m)] [_ t] orders] (mod phi t)) 0.5)}
     {:label "row a of the table mod n is a permutation exactly when gcd(a, n) = 1, n ≤ 30"
      :grade (numeric (for [n table-moduli :let [{:keys [perm]} (table-facts n)] a (range n)]
                        (if (= (perm a) (= 1 (ring/gcd a n))) 0 1)) 0.5)}
     {:label "the units mod n are cyclic exactly for n = 2, 4, p^k, 2p^k (art. 92), n ≤ 30"
      :grade (numeric (for [n table-moduli
                            :let [odd (loop [m n] (if (even? m) (recur (quot m 2)) m))
                                  pk? (and (> odd 1) (= 1 (count (filter #(and (zero? (mod odd %)) (every? (fn [d] (pos? (mod % d))) (range 2 %))) (range 2 (inc odd))))))
                                  expect (or (#{2 4} n) (and pk? (or (odd? n) (= n (* 2 odd)))))]]
                        (if (= (boolean expect) (:cyclic? (:structure (table-facts n)))) 0 1)) 0.5)}
     {:label "the fundamental theorem (art. 131) holds in every cell of the grid of odd primes below 60"
      :grade (numeric (for [row grid c row] (if (:law? c) 0 1)) 0.5)}
     {:label "Euler's criterion on raster: q^((p−1)/2) mod p by the walk kernel = the Legendre symbol (structure.ring/legendre), every cell"
      :grade (numeric (for [row grid {:keys [p q qp]} row :when (not= p q)]
                        (- (peek (raster-walk q p (quot (dec p) 2))) (if (= qp 1) 1 (dec p)))) 1e-9)}
     {:label "Li(x) exceeds π(x) at every sample from 100 to 10^6 (Gauss and Goldschmidt's count, Riemann 1859)"
      :grade (numeric (for [{:keys [x pi li]} pt :when (>= x 100)] (if (> li pi) 0 1)) 0.5)}
     {:label "Li(10^6) = 78626.5, π(10^6) = 78498 (raster quadgk and the sieve)"
      :grade (let [{:keys [pi li]} (peek pt)] (numeric [(- li 78626.504) (- pi 78498)] 1e-2))}]))
