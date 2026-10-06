(ns alexandria.notation
  "Historical notations for Alexandria vocabulary.

   TeX rendering plugs into the extensible Emmy renderer with
   `emmy.expression.render/TeX-renderer`, the same extension point used by
   `desargues.logic.tex`: operator metadata becomes `:special-handlers`,
   `:infix?`, and `:precedence-map` rather than reimplementing TeX printing.

   The `:al-khwarizmi` notation is a rhetorical algebra style after al-Khwarizmi's
   ninth-century *al-jabr*, where relations are phrased as magnitudes and things
   rather than symbolic formulae."
  (:refer-clojure :exclude [render])
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [emmy.env :as e]
            [emmy.expression.render :as er]))

(declare render-expr)

(defmulti render
  "Render one vocabulary operation in a notation."
  (fn [notation op _args] [notation op]))

(defmethod render :default [notation op args]
  (case notation
    :tex (er/->TeX (apply list op args))
    (str "(" (str/join " " (cons op args)) ")")))

(def ^:private tex-handlers
  {'area (fn [[a b c]] (str "[" a b c "]"))
   'length (fn [[a b]] (str "\\overline{" a b "}"))
   'angle (fn [[a o b]] (str "\\angle " a o b))
   'ratio (fn [[a b]] (str "\\frac{" a "}{" b "}"))
   'segment-area (fn [[p a b]] (str "\\operatorname{seg}(" p ";" a b ")"))
   'hyperbola (fn [[a b]] (str "\\mathcal{H}_{" a "," b "}"))
   'parabola (fn [[a b]] (str "\\mathcal{P}_{" a "," b "}"))
   'ellipse (fn [[a b]] (str "\\mathcal{E}_{" a "," b "}"))
   'circle (fn [[o r]] (str "\\odot(" o "," r ")"))
   'series-sum (fn [[r n]] (str "\\sum_{k=0}^{" n "}" r "^k"))
   'differential (fn [[q]] (str "\\mathrm{d}\\left(" q "\\right)"))})

(defn- tex-renderer
  ([] (tex-renderer {}))
  ([extra-handlers]
   (apply er/TeX-renderer
          (mapcat identity
                  {:precedence-map '{:apply 7, * 5, + 4, - 4, = 3, / 0}
                   :infix? '#{= + - * /}
                   :special-handlers (merge tex-handlers extra-handlers)}))))

(defmethod render [:tex :expr] [_ _ [expr]]
  ((tex-renderer) expr))

;; ---------------------------------------------------------------------------
;; TeX dialects: one frozen expression typeset in a historical notation.
;; A dialect is {:handlers {op (fn [rendered-args] tex)} :rewrite (fn [form] form)}:
;; the rewrite runs on the frozen form first (e.g. turning the difference dx
;; into Newton's fluxion), then Emmy's TeX renderer with the dialect's
;; handlers over the shared ones.

(defmulti tex-dialect
  "The TeX dialect of a notation keyword, or nil when it is not a TeX one."
  identity)

(defmethod tex-dialect :default [_] nil)

(defmethod tex-dialect :tex [_] {:handlers {} :rewrite identity})

(defn difference-of
  "The quantity whose Leibnizian difference symbol is s (dx -> x, dy1 -> y1),
   or nil. A difference is d before one letter, as in Leibniz's text."
  [s]
  (when (symbol? s)
    (when-let [[_ q] (re-matches #"d([a-zA-Z][0-9]*)" (name s))] (symbol q))))

(defmethod tex-dialect :leibniz [_]
  ;; Nova Methodus (1684): dx is the difference of x; the difference of a
  ;; compound quantity is d with a vinculum over it, as in d\overline{xv}.
  {:handlers {'differential (fn [[q]] (str "d\\overline{" q "}"))}
   :rewrite identity})

(defmethod tex-dialect :newton [_]
  ;; Newton's fluxions (pricked letters, printed in the Quadrature of Curves,
  ;; 1704): the fluxion of x is x with a dot over it; of a compound quantity,
  ;; the quantity under a vinculum with the dot over it.
  {:handlers {'fluxion (fn [[q]] (str "\\dot{" q "}"))
              'differential (fn [[q]] (str "\\dot{\\overline{" q "}}"))}
   :rewrite (fn [form]
              (walk/postwalk #(if-let [q (difference-of %)] (list 'fluxion q) %) form))})

(defn- letters [args]
  (apply str (map name args)))

(defmethod render [:euclid 'area] [_ _ args]
  (str "the triangle " (letters args)))

(defmethod render [:euclid 'length] [_ _ [a b]]
  (str "the straight line " (name a) (name b)))

(defmethod render [:euclid 'angle] [_ _ [a o b]]
  (str "the angle " (name a) (name o) (name b)))

(defmethod render [:euclid '=] [_ _ [a b]]
  (str (render-expr :euclid a) " is equal to " (render-expr :euclid b)))

(defmethod render [:euclid '+] [_ _ args]
  (str/join " together with " (map #(render-expr :euclid %) args)))

(defmethod render [:euclid '*] [_ _ args]
  (str/join " times " (map #(render-expr :euclid %) args)))

(defmethod render [:euclid '/] [_ _ [a b]]
  (str (render-expr :euclid a) "/" (render-expr :euclid b)))

(defmethod render [:euclid 'ratio] [_ _ [a b]]
  (str (render-expr :euclid a) " has to " (render-expr :euclid b) " a ratio"))

(defmethod render [:al-khwarizmi 'hyperbola] [_ _ [a b]]
  (str "a hyperbola whose first magnitude is " a " and whose second magnitude is " b))

(defmethod render [:al-khwarizmi '=] [_ _ [a b]]
  (str (render-expr :al-khwarizmi a) " equals " (render-expr :al-khwarizmi b)))

(defn render-expr [notation expr]
  (let [expr (e/freeze expr)]
    (if-let [{:keys [handlers rewrite]} (tex-dialect notation)]
      ((tex-renderer handlers) (rewrite expr))
      (if (seq? expr)
        (render notation (first expr) (vec (rest expr)))
        (str expr)))))

(defmethod render [:euclid 'square] [_ _ [x]]
  (str "the square on " (str/replace (render-expr :euclid x) #"^the straight line " "")))
