(ns alexandria.structure.tie
  "Widget-shape stratum (below the media): the three shapes every era
   widget repeated, as data plus one function or multimethod each. The
   kernels they produce are Emmy functions in the calling convention of
   alexandria.medium.tie / medium.tied / medium.flow ((fn [& args] (fn [_]
   [numbers]))), so the same value gives raster numbers on the JVM, a wasm
   kernel on the page and TeX on symbols.

   TIE  quantities linked by an equation; moving one re-solves the others.
        {:tie/kind kind :keys [k ...] :ranges {k [lo hi step]} ...}
        `solve` (multimethod, open on :tie/kind) gives the solved state
        from source k. Kinds here:
          :pivot    every key is an invertible function of one pivot
                    parameter: {:to-pivot {k (fn [v] pivot)}
                                :state (fn [pivot] [v ...])}
                    (the source value is clamped to its range first)
          :sources  {:sources {k (fn [& state] state')}}, any policy.
        `kernel` is the tie kernel (every source's block side by side),
        `blocks` splits its output, `round-trip-gaps` the numbers a grade
        checks (solving from any key of a solved state returns it).
   READOUT  {:outputs [k ...] :f (fn [& args] {k value})}: `readout-kernel`
        the vector in the order of :outputs, `index` the map k -> position.
   NAMED CASE  {:cases [[name test] ...]} with test :else or a clause
        [op k x] (op :< :<= :> :>= :abs<, k an output key, x a number) or
        [:and clause ...]; the first case whose test holds names the
        object. `case-of` decides it on a value map, `case-form` emits the
        browser form deciding it from a readout array."
  (:require [alexandria.structure.arith :as a]
            [malli.core :as m]))

;; ---------------------------------------------------------------------------
;; Tie

(def Tie
  [:map
   [:tie/kind keyword?]
   [:keys [:vector keyword?]]
   [:ranges [:map-of keyword? [:tuple number? number? number?]]]])

(defn tie
  "A tie value (validated with malli)."
  [m]
  (m/assert Tie m)
  m)

(defmulti solve
  "The state (a vector in the order of :keys) solved from source key k,
   given the current state (a vector). Open on :tie/kind."
  (fn [t _k _state] (:tie/kind t)))

(defmethod solve :pivot [{:keys [ranges to-pivot state] ks :keys} k st]
  (let [[lo hi] (ranges k)
        v (nth st (.indexOf ^java.util.List ks k))]
    (vec (state ((to-pivot k) (a/clamp v lo hi))))))

(defmethod solve :sources [{:keys [sources]} k st]
  (vec (apply (sources k) st)))

(defn kernel
  "The tie kernel of t: (fn [& state] (fn [_] [block ...])), one block per
   source key in the order of :keys."
  [t]
  (fn [& st]
    (fn [_] (vec (mapcat #(solve t % (vec st)) (:keys t))))))

(defn blocks
  "{source state-vector} from a tie kernel's output."
  [t out]
  (let [n (count (:keys t)) v (vec out)]
    (into {} (map-indexed (fn [i k] [k (subvec v (* i n) (* (inc i) n))]) (:keys t)))))

(defn- position [t k] (.indexOf ^java.util.List (:keys t) k))

(defn start
  "The state vector solved from source k at value v (the other keys at
   their range minima)."
  [t k v]
  (mapv double (solve t k (assoc (mapv #(first ((:ranges t) %)) (:keys t)) (position t k) v))))

(defn round-trip-gaps
  "For each value v of source k: solve from k, then from every key of the
   solved state; the differences from the first solution."
  [t k vs]
  (for [v vs
        :let [s0 (start t k v)]
        j (:keys t)
        [x y] (map vector s0 (solve t j s0))]
    (- (double y) (double x))))

;; ---------------------------------------------------------------------------
;; Readout

(defn index "{k position} of the outputs." [{:keys [outputs]}] (zipmap outputs (range)))

(defn readout-kernel
  "The readout kernel: (fn [& args] (fn [_] [value ...])) in the order of
   :outputs."
  [{:keys [outputs f]}]
  (fn [& args]
    (fn [_] (let [m (apply f args)] (mapv m outputs)))))

;; ---------------------------------------------------------------------------
;; Named case

(defn- clause-holds? [values [op k x :as clause]]
  (case op
    :and (every? #(clause-holds? values %) (rest clause))
    (let [v (double (values k))]
      (case op :< (< v x) :<= (<= v x) :> (> v x) :>= (>= v x) :abs< (< (Math/abs v) x)))))

(defn case-of
  "The name of the first case of named whose test holds on values (a map
   output -> number)."
  [{:keys [cases]} values]
  (some (fn [[nm test]] (when (or (= :else test) (clause-holds? values test)) nm)) cases))

(defn- clause-form [get-form [op k x :as clause]]
  (if (= :and op)
    (list* 'and (map #(clause-form get-form %) (rest clause)))
    (let [v (get-form k)]
      (case op
        :< (list '< v x) :<= (list '<= v x) :> (list '> v x) :>= (list '>= v x)
        :abs< (list '< (list 'js/Math.abs v) x)))))

(defn case-form
  "Browser form naming the case: (cond test name ...), the names as
   strings; get-form maps an output key to the browser form reading it."
  [{:keys [cases]} get-form]
  (list* 'cond (mapcat (fn [[nm test]] [(if (= :else test) :else (clause-form get-form test)) (name nm)]) cases)))
