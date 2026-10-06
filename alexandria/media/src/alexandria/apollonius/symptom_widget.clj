(ns alexandria.apollonius.symptom-widget
  "The symptoma of Apollonius (Conics I.11-13) live, for a Clerk notebook,
   to sit after the notation figure (alexandria.apollonius.notation-steps-
   view/figure) and look like it: the same MathBox cone, palette, HTML
   letters and kernels, with one state {:tilt :x} the reader moves.

     (widget)    the live figure: the cone and the section with Q sliding
                 on it; beside it y^2 = p x + c x^2 with the current
                 numbers (both sides kernel outputs), the section's name
                 in Greek, and the application of areas to scale
     (equation)  the three forms and the one form, TeX from Emmy
     (proofs)    the symptoma's identities with Emmy's grades

   The scene is alexandria.apollonius.symptom-scene/render; its kernels are
   the notation figure's plus alexandria.apollonius.symptom's, compiled
   here on the raster backend (alexandria.medium.kernel). Needs a page that
   loads the series' bundle (history-of-math.build)."
  (:require [alexandria.apollonius.notation-steps :as notation]
            [alexandria.apollonius.symptom :as symptom]
            [alexandria.medium.kernel :as kernel]
            [clojure.string :as str]
            [emmy.env :as e]
            [nextjournal.clerk :as clerk]))

(def bundled-cljs
  "The namespaces the render-fn requires that the bundle already compiles.
   Clerk must not ship their sources: they require npm modules SCI cannot
   load."
  '#{mathbox.core mathbox.primitives})

(defn- register-bundle! []
  (swap! @(requiring-resolve 'nextjournal.clerk.cljs-libs/already-loaded-sci-namespaces)
         into bundled-cljs))

(def figures
  "Every kernel of the scene: the notation figure's, and the symptoma's."
  (merge (select-keys notation/figures [:cone :section :plane :segment :named])
         symptom/figures))

(defn template
  "y^2 = p x + c x^2 as TeX by emmy.env/->TeX, the letters standing for
   the numbers the browser substitutes (alexandria.apollonius.symptom-
   scene/fill)."
  []
  (let [[y p x c] (map symbol ["YYY" "PPP" "XXX" "CCC"])]
    (str (e/->TeX (e/square y)) " = " (e/->TeX (e/+ (e/* p x) (e/* c (e/square x)))))))

(def start
  "The opening state: the notation figure's ellipse tilt and abscissa."
  {:tilt0 35 :x0 0.85})

(defn value
  "The plain value the render-fn reads."
  []
  {:kernels (kernel/kernels figures)
   :data (merge notation/view start
                {:outputs symptom/outputs
                 :template (template)
                 :source :raster})})

(def viewer
  {:name `widget
   :require-cljs true
   :transform-fn clerk/mark-presented
   :render-fn 'alexandria.apollonius.symptom-scene/render})

(defn widget
  "The live symptoma, as a Clerk value."
  []
  (register-bundle!)
  (clerk/with-viewer viewer (value)))

(defn equation
  "The symptoma in the notation's letters: the three forms of I.11-13 and
   the one form, TeX from Emmy expressions."
  []
  (let [{:keys [parabola hyperbola ellipse general c]} (symptom/tex)]
    (clerk/tex
     (str "\\begin{aligned}"
          parabola "&\\quad\\text{parabola (I.11)}\\\\ "
          hyperbola "&\\quad\\text{hyperbola (I.12)}\\\\ "
          ellipse "&\\quad\\text{ellipse (I.13)}\\\\[0.4em] "
          general "&\\quad " c
          "\\end{aligned}"))))

(defn proofs
  "The symptoma's identities, each with Emmy's grade, as badges."
  []
  (clerk/html
   (into [:div {:style {:display "flex" :flex-wrap "wrap" :gap "0.4em" :margin "0.6em 0"}}]
         (for [{:keys [label grade]} (symptom/graded)
               :let [ok? (= :grade/proved grade)
                     tone (if ok? "#2f8a3e" "#c0392b")]]
           [:div {:data-grade (name grade)
                  :style {:padding "0.15em 0.8em" :border-radius "1em" :font-size "0.82em"
                          :font-family "ui-monospace, monospace"
                          :border (str "1px solid " tone) :color tone}}
            (str (if ok? "proved by Emmy: " "FAILS: ") label)]))))

(defn heath
  "Heath's conclusion of one proposition (:parabola :hyperbola :ellipse) as
   a Markdown quote."
  [k]
  (clerk/md (str "> " (symptom/heath k) "\n\n*Heath 1896, "
                 ({:parabola "I.11, p. 9" :hyperbola "I.12, p. 11" :ellipse "I.13, p. 13"} k) "*")))

(comment
  (str/includes? (template) "YYY"))
