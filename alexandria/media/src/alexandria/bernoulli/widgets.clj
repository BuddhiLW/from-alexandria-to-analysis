(ns alexandria.bernoulli.widgets
  "The widget of the Bernoulli notebook (Ars conjectandi IV, 1713).

     law-widget   Choose the ratio r : s of fertile to sterile cases, a scale
                  k (r, s, t = r + s all multiplied by k, so the tolerance is
                  1/t as in Bernoulli) and the odds c : 1; then slide n. It
                  shows the exact P(|S_n/n - p| <= 1/t) against n, Chebyshev's
                  bound and the n that Bernoulli's lemmas give for odds c; the
                  histogram of S_n/n with the band shaded; and the tie among
                  n, tolerance and guaranteed odds.

   Every probability is a sum of exact integer binomial weights
   (alexandria.structure.probability), converted to a double only to be drawn;
   the miss 1 - P is computed exactly before conversion, so odds of 1000 : 1
   are not lost to rounding. The one-draw sample per n is a seeded
   pseudo-random draw, drawn here and marked as a sample in the widget; the
   structure itself draws nothing."
  (:require [alexandria.structure.probability :as pr]
            [nextjournal.clerk :as clerk])
  (:import (java.math BigInteger)))

(def trials "Trial counts of the curve and of the histogram." [1 2 3 5 8 12 20 30 50 80 120 200 300 500 800 1200 2000 3000])
(def ratios "The ratios r0 : s0 the widget offers." [[3 2] [1 1] [4 1]])
(def scales "Multipliers k: t = k (r0 + s0), tolerance 1/t." [2 5 10])
(def odds "Moral certainty c : 1." [10 100 1000])
(def bins "Bars of the histogram." 41)
(def seed "The seed of the sample draws." 1713)

(defn- ->double [num den] (double (/ (bigint num) (bigint den))))

(defn- band-sum
  "The BigInteger sum of the weights of S_n with |k/n - p| <= eps (exact)."
  [ws n p eps]
  (reduce (fn [^BigInteger acc k] (if (<= (abs (- (/ k n) p)) eps) (.add acc ^BigInteger (nth ws k)) acc))
          BigInteger/ZERO (range (inc n))))

(defn at-n
  "{:n :p-in :miss} for the case (a/d, eps) at n, exact."
  [a d eps n]
  (let [ws (pr/weights n a d)
        tot (.pow (BigInteger/valueOf (long d)) (int n))
        in (band-sum ws n (/ a d) eps)]
    {:n n :in (->double in tot) :miss (->double (.subtract tot in) tot)}))

(defn- hist
  "Heights of `bins` equal bars of S_n/n over [0,1], exact masses as doubles."
  [a d n]
  (let [ws (pr/weights n a d)
        tot (.pow (BigInteger/valueOf (long d)) (int n))
        sums (reduce (fn [v k] (update v (min (dec bins) (long (Math/floor (* bins (/ k (double n)))))) #(.add ^BigInteger % ^BigInteger (nth ws k))))
                     (vec (repeat bins BigInteger/ZERO)) (range (inc n)))]
    (mapv #(->double % tot) sums)))

(defn- sample-freq
  "S_n/n for one seeded draw of n trials with success probability a/d."
  [rng a d n]
  (/ (count (filter (fn [_] (< (.nextInt rng (int d)) a)) (range n))) (double n)))

(defn case-data
  "Everything the widget needs for the ratio r0 : s0 scaled by k."
  [rng [r0 s0] k]
  (let [r (* r0 k) s (* s0 k) t (+ r s) p (/ r t) a (numerator p) d (denominator p) eps (/ 1 t)]
    {:r r :s s :t t :p (double p) :eps (double eps)
     :curve (mapv (fn [n] (merge (at-n a d eps n) {:cheb (double (pr/chebyshev-bound n p eps))
                                                   :hist (hist a d n) :sample (sample-freq rng a d n)}))
                  trials)
     :bernoulli (into {} (for [c odds]
                           (let [b (pr/bernoulli-n r s c)] [c (assoc b :exact (at-n a d eps (:n b)))])))}))

(defn law-data
  "{:ratios :scales :odds :ns :bins :cases {[ratio k] case}}."
  []
  (let [rng (java.util.Random. seed)]
    {:ratios ratios :scales scales :odds odds :ns trials :bins bins :seed seed
     :cases (into {} (for [ra ratios k scales] [[ra k] (case-data rng ra k)]))}))

(defn finite-law-data?
  "Every case has every n, finite probabilities in [0,1], miss in [0,1], and
   a histogram that sums to 1."
  [{:keys [cases] ns-list :ns}]
  (boolean
   (and (seq cases)
        (every? (fn [{:keys [curve bernoulli]}]
                  (and (= (count curve) (count ns-list))
                       (every? (fn [{:keys [in miss hist]}]
                                 (and (<= 0 in 1) (<= 0 miss 1) (< (abs (- 1 (reduce + hist))) 1e-9)))
                               curve)
                       (every? (fn [[_ {:keys [exact n]}]] (and (pos? n) (<= 0 (:in exact) 1))) bernoulli)))
                (vals cases)))))

(def ^:private render
  '(fn [{:keys [ratios scales odds ns bins cases seed]}]
     (reagent.core/with-let [ri (reagent.core/atom 0) ki (reagent.core/atom 2) ci (reagent.core/atom 2) ni (reagent.core/atom 12)]
       (let [cs (get cases [(nth ratios @ri) (nth scales @ki)])
             c (nth odds @ci)
             pt (nth (:curve cs) @ni) n (nth ns @ni)
             bern (get (:bernoulli cs) c)
             lx (fn [m] (+ 30 (* 330 (/ (js/Math.log m) (js/Math.log 3000)))))
             ly (fn [v] (- 130 (* 110 v)))
             path (fn [key] (str "M" (apply str (interpose " L" (map (fn [q] (str (.toFixed (lx (:n q)) 1) " " (.toFixed (ly (key q)) 1))) (:curve cs))))))
             maxh (apply max (:hist pt))
             bw (/ 330 bins)
             lo (- (:p cs) (:eps cs)) hi (+ (:p cs) (:eps cs))
             hx (fn [x] (+ 30 (* 330 x)))
             btn (fn [label act on? f] [:button {:type "button" :data-act act :on-click f
                                                 :style {:margin "0.15em" :padding "0.2em 0.5em" :border-radius "0.3em" :cursor "pointer"
                                                         :border "1px solid currentColor" :color "inherit"
                                                         :background (if on? "#1F6FB433" "transparent")}} label])
             target (/ c (+ c 1.0))
             ok? (>= (:in pt) target)]
         [:div {:data-figure "bernoulli-law" :data-r (str (:r cs)) :data-s (str (:s cs)) :data-t (str (:t cs)) :data-c (str c) :data-n (str n)
                :data-in (str (:in pt)) :data-miss (str (:miss pt)) :data-cheb (str (:cheb pt))
                :data-bern-n (str (:n bern)) :data-bern-in (str (:in (:exact bern))) :data-bern-miss (str (:miss (:exact bern)))
                :data-certain (str ok?) :style {:margin "1em 0"}}
          [:div "ratio r : s " (for [i (range (count ratios))] ^{:key i} (btn (str (first (nth ratios i)) " : " (second (nth ratios i))) (str "ratio-" i) (= i @ri) #(reset! ri i)))
           " scale k " (for [i (range (count scales))] ^{:key i} (btn (str (nth scales i)) (str "scale-" i) (= i @ki) #(reset! ki i)))
           " odds " (for [i (range (count odds))] ^{:key i} (btn (str (nth odds i) " : 1") (str "odds-" i) (= i @ci) #(reset! ci i)))]
          [:div {:style {:margin "0.3em 0"}} "r = " (:r cs) ", s = " (:s cs) ", t = " (:t cs) ", p = r/t = " (.toFixed (:p cs) 3)
           ", tolerance 1/t = " (.toFixed (:eps cs) 3)]
          [:div {:style {:display "flex" :flex-wrap "wrap" :gap "1em"}}
           [:svg {:width 390 :height 160 :viewBox "0 0 390 160" :style {:max-width "100%"}}
            [:rect {:x 30 :y (ly 1) :width 330 :height 110 :fill "none" :stroke "#8A9BB0"}]
            [:line {:x1 30 :x2 360 :y1 (ly target) :y2 (ly target) :stroke "#2E8B57" :stroke-dasharray "4 3"}]
            [:path {:d (path :cheb) :fill "none" :stroke "#E39B2D" :stroke-width 1.8 :data-curve "chebyshev"}]
            [:path {:d (path :in) :fill "none" :stroke "#1F6FB4" :stroke-width 2.4 :data-curve "exact"}]
            [:line {:x1 (lx n) :x2 (lx n) :y1 (ly 1) :y2 (ly 0) :stroke "#C0392B"}]
            [:text {:x 32 :y 150 :font-size 10} "n = 1 ... 3000 (log), blue exact, orange Chebyshev, green: odds c : 1"]]
           [:svg {:width 390 :height 160 :viewBox "0 0 390 160" :style {:max-width "100%"}}
            [:rect {:x (hx lo) :y 10 :width (* 330 (- hi lo)) :height 110 :fill "#2E8B5733" :data-band true}]
            (for [i (range bins)]
              ^{:key i} [:rect {:x (+ 30 (* i bw)) :y (- 120 (* 105 (/ (nth (:hist pt) i) maxh))) :width (- bw 0.5)
                                :height (* 105 (/ (nth (:hist pt) i) maxh)) :fill "#1F6FB4"}])
            [:line {:x1 (hx (:p cs)) :x2 (hx (:p cs)) :y1 10 :y2 120 :stroke "#C0392B" :stroke-dasharray "3 2"}]
            [:circle {:cx (hx (:sample pt)) :cy 126 :r 4 :fill "none" :stroke "#E39B2D" :stroke-width 2 :data-sample true}]
            [:text {:x 32 :y 150 :font-size 10} "S_n/n, band p ± 1/t shaded; circle: one seeded draw (sample, not proof)"]]]
          [:div {:style {:margin-top "0.4em"}}
           [:label "n = "] [:strong {:data-n-text true} n] " "
           [:input {:type "range" :min 0 :max (dec (count ns)) :value @ni :data-act "n-slider"
                    :on-change #(reset! ni (js/parseInt (.. % -target -value)))}]]
          [:div {:style {:line-height 1.6}}
           "P(|S_n/n − p| ≤ 1/t) = " [:strong {:data-prob true} (.toFixed (:in pt) 10)]
           ", miss " [:strong {:data-miss-text true} (.toExponential (:miss pt) 3)]
           ", Chebyshev bound " [:strong (.toFixed (:cheb pt) 6)]]
          [:div {:data-verdict true :style {:font-weight 600 :color (if ok? "#2E8B57" "#C0392B")}}
           (if ok? (str "at n = " n " the band holds with odds above " c " : 1")
               (str "at n = " n " the odds are not yet " c " : 1 (" (.toFixed (/ (:in pt) (max 1e-300 (:miss pt))) 1) " : 1)"))]
          [:div {:data-bernoulli true :style {:margin-top "0.4em"}}
           "Bernoulli's lemmas give n ≥ " [:strong (:n bern)] " trials for odds " c " : 1 (m = " (:m1 bern) " and " (:m2 bern)
           " on the two sides, n = " (:n1 bern) " and " (:n2 bern) "). The exact probability at that n is "
           [:strong (.toFixed (:in (:exact bern)) 10)] ", miss " [:strong (.toExponential (:miss (:exact bern)) 3)] ", odds "
           [:strong (.toExponential (/ (:in (:exact bern)) (max 1e-300 (:miss (:exact bern)))) 2)] " : 1."]]))))

(defn law-widget
  "The law of large numbers, exact, with Bernoulli's n and Chebyshev's bound."
  []
  (let [d (law-data)]
    (assert (finite-law-data? d) "the law data are incomplete")
    (clerk/with-viewer {:render-fn render :transform-fn clerk/mark-presented} d)))
