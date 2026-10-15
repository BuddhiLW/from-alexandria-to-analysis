(ns alexandria.medium.tie
  "Tied controls for emmy-viewers widgets (widget brief, section 5), the
   pattern of the Apollonius playground (alexandria.apollonius.playground-
   widget) made reusable.

   A widget has a vector of tied keys, each a Leva control with a range
   [min max step]. One raster kernel, the tie, takes the state (one number
   per key, in order) and returns the solved state for each possible
   source, one block of (count keys) numbers per key, in the same order.
   Only the touched key is a source. The page keeps two atoms: !p is
   Leva's (Leva rounds what it shows and echoes it back), !s the exact
   solved state that every kernel and plot reads.

   `watch-form` is the browser code that re-solves on a reader's edit:
   origin guard (nothing while the page pushes) and epsilon guard (a change
   within half a slider step of the value just pushed, or of the exact
   state, is Leva's rounding, not the reader). The template must bind
   `pushing` (atom false) and `pushed` (atom {}) in its reagent with-let.

   Also the branch-free Emmy clamps the ties use (raster compiles them),
   the kernel binding for a template, and a number formatter."
  (:require [emmy.env :as e]
            [emmy.viewer.compile :as vc]))

;; ---------------------------------------------------------------------------
;; Emmy helpers, numbers and symbols alike, branch-free for raster

(defn at-least [x lo] (e/* 0.5 (e/+ x lo (e/abs (e/- x lo)))))
(defn at-most [x hi] (e/* 0.5 (e/- (e/+ x hi) (e/abs (e/- x hi)))))
(defn clamp [x lo hi] (at-most (at-least x lo) hi))

(defn step-up
  "1 where x > 0, 0 where x < 0 (x never 0 at the points it is used)."
  [x]
  (e/* 0.5 (e/+ 1 (e// x (e/abs x)))))

;; ---------------------------------------------------------------------------
;; Template pieces

(defn kernel-binding
  "[sym form]: f compiled by the current backend, called in the browser as
   (sym (array 0) (array ...params))."
  [nm f init-params]
  [(gensym nm) (vc/compiled-fn f init-params [0] {:simplify? false})])

(defn fmt
  "Browser form: x to n places with a true minus sign; non-finite as ∞."
  ([x] (fmt x 2))
  ([x n] (list 'let ['v x] (list 'if '(js/isFinite v) (list '.replace (list '.toFixed 'v n) "-" "−") "∞"))))

(defn state-array
  "Browser form: the exact state as a JS array, in the order of `tied`."
  [get-key tied]
  (list* 'array (map get-key tied)))

(defn schema
  "The Leva schema of the tied keys."
  [tied ranges labels]
  (into {} (map (fn [k] (let [[a b s] (ranges k)] [k {:min a :max b :step s :label (labels k)}])) tied)))

(defn- solve-form
  "Browser form: solve from the touched key `src` with the tie kernel; the
   kernel's block for `src` becomes !s, the keys of !p that differ are
   pushed into the panel after the current notification (setTimeout 0).
   Counts solves in globalThis[counter]."
  [tie-sym !p !s tied counter]
  (let [n (count tied)
        offsets (into {} (map-indexed (fn [i k] [k (* i n)]) tied))]
    (list 'js/setTimeout
          (list 'fn []
                (list 'let ['cur (list 'assoc (list 'deref !s) 'src (list 'get (list 'deref !p) 'src))
                            'out (list tie-sym '(array 0) (list* 'array (map (fn [k] (list 'get 'cur k)) tied)))
                            'off (list 'get offsets 'src)
                            'solved (list 'zipmap tied (list 'map '(fn [j] (aget out (+ off j))) (list 'range n)))
                            'shown (list 'deref !p)
                            'push '(into {} (filter (fn [[k v]] (> (js/Math.abs (- v (get shown k))) 1e-12)) solved))]
                      (list 'aset 'js/globalThis counter (list 'inc (list 'or (list 'aget 'js/globalThis counter) 0)))
                      (list 'reset! !s 'solved)
                      (list 'when '(seq push)
                            '(reset! pushing true)
                            (list 'swap! !p 'merge 'push)
                            '(reset! pushing false)
                            '(swap! pushed merge push)
                            '(js/setTimeout (fn [] (swap! pushed (fn [m] (apply dissoc m (keys push))))) 600))))
          0)))

(defn watch-form
  "Browser form: the watch on the panel atom !p that re-solves the exact
   state !s from the touched key (origin and epsilon guards, see the ns
   doc). `counter` names the globalThis property counting solves (the e2e
   checks one solve per edit)."
  [tie-sym !p !s tied ranges counter]
  (let [half (into {} (map (fn [k] [k (/ (nth (ranges k) 2) 2)]) tied))]
    (list 'add-watch !p ::tie
          (list 'fn ['_ '_ 'old 'new]
                (list 'when-not '@pushing
                      (list 'let ['src (list 'first (list 'filter '(fn [k] (not= (get old k) (get new k))) tied))
                                  'echo? (list 'and 'src
                                               (list 'or
                                                     (list 'and '(contains? @pushed src)
                                                           (list '<= '(js/Math.abs (- (get new src) (get @pushed src)))
                                                                 (list '+ 1e-9 (list 'get half 'src))))
                                                     (list '<= (list 'js/Math.abs (list '- '(get new src) (list 'get (list 'deref !s) 'src)))
                                                           (list '+ 1e-9 (list 'get half 'src)))))]
                            (list 'when (list 'and 'src '(not echo?)) (solve-form tie-sym !p !s tied counter))))))))

(defn unwatch-form
  "Browser form for the template's finally: remove the tie watch."
  [!p]
  (list 'remove-watch !p ::tie))

(defn blocks
  "JVM side: split the tie kernel's output into {source state-vector}."
  [tied out]
  (let [n (count tied) v (vec out)]
    (into {} (map-indexed (fn [i k] [k (subvec v (* i n) (* (inc i) n))]) tied))))

;; ---------------------------------------------------------------------------
;; Additions from the Kepler and Galileo widgets: source offsets, degrees,
;; the with-let locals for the watch, and the conic named live in
;; Apollonius' Greek (the playground, Kepler's orbit, Galileo's shot).

(defn source-block
  "{key offset}: where each source's solved state starts in the tie's output."
  [tied]
  (zipmap tied (map #(* % (count tied)) (range))))

(defn rad [d] (e/* d (/ Math/PI 180)))
(defn deg [r] (e/* r (/ 180 Math/PI)))

(defn locals
  "The with-let bindings the watch needs, the watch installed last:
   [pushing pushed _watch]. opts: {:tie-sym :p :s :keys :ranges :counter}.
   Pair with (unwatch-form p) in the template's finally."
  [{:keys [tie-sym p s keys ranges counter]}]
  ['pushing '(atom false) 'pushed '(atom {}) '_ (watch-form tie-sym p s keys ranges counter)])

(def parabola-band "|e - 1| below this names the conic a parabola." 0.005)
(def circle-band "e below this names the conic a circle." 0.005)

(defn conic-name
  "Browser form (hiccup): the conic of eccentricity `ecc-form` named live,
   in English and in Apollonius' Greek, as the conics playground names
   it. `far-note` is appended to the hyperbola's name; `circle?` names
   e = 0 a circle (κύκλος)."
  ([ecc-form] (conic-name ecc-form {}))
  ([ecc-form {:keys [far-note circle?] :or {far-note ""}}]
   (list 'let ['ecc ecc-form
               'kind (list 'cond
                           (list 'and circle? (list '< 'ecc circle-band)) "circle"
                           (list '< '(js/Math.abs (- ecc 1)) parabola-band) "parabola"
                           '(< ecc 1) "ellipse" :else "hyperbola")]
         [:div {:style {:font-size "1.25em" :font-weight 600 :margin "0.3em 0"}}
          [:span {:data-conic-name 'kind} 'kind]
          (list 'case 'kind
                "circle" " (e = 0): κύκλος"
                "ellipse" " (e < 1): ἔλλειψις"
                "parabola" " (e = 1): παραβολή"
                (str " (e > 1): ὑπερβολή" far-note))])))
