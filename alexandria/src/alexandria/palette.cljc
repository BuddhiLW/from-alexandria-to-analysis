(ns alexandria.palette
  "Colour palettes, exchangeable like terminal themes. A palette maps each
   drawing Role (alexandria.value/Role) and the surface colours to a CSS
   colour:

     {:background :ink :given :construction :found :muted}

   Open by registration: a new palette is one defmethod. Media take the
   palette from the scene (:palette, a keyword or a map), so one figure can
   be drawn in the deck's colours, in Monokai or in print black on white.")

(defmulti palette
  "The palette named k."
  identity)

(defmethod palette :default [k]
  (throw (ex-info (str "No palette " k) {:palette k :known (keys (methods palette))})))

(defmethod palette :deck [_]
  ;; the set-theory decks' own: chalk on a dark green board
  {:background "#0f1a16" :ink "#ECEEE4" :given "#ECEEE4" :construction "#8FC7E8"
   :found "#F2D16B" :muted "#6f8479"})

(defmethod palette :monokai [_]
  {:background "#272822" :ink "#F8F8F2" :given "#F8F8F2" :construction "#66D9EF"
   :found "#E6DB74" :muted "#75715E"})

(defmethod palette :solarized-light [_]
  {:background "#FDF6E3" :ink "#073642" :given "#073642" :construction "#268BD2"
   :found "#B58900" :muted "#93A1A1"})

(defmethod palette :print [_]
  {:background "#FFFFFF" :ink "#000000" :given "#000000" :construction "#555555"
   :found "#000000" :muted "#999999"})

(defmethod palette :series [_]
  ;; the history-of-math series: the deck's chalk board, plus one colour per
  ;; era of its timeline (history-of-math/resources/history_of_math/timeline.edn)
  ;; and one per strand, a field of mathematics with its own timeline
  ;; (history-of-math/resources/history_of_math/strands.edn)
  (merge (palette :deck)
         {:era/greek "#F2D16B" :era/medieval "#D98E73" :era/new-sciences "#9FD38A"
          :era/calculus "#8FC7E8" :era/rigour "#C7A8E8" :era/structures "#E8A8C7"
          ;; a strand split off another keeps its parent's hue, darker or
          ;; deeper: differential from plain geometry, measure from analysis,
          ;; abstract algebra from algebra
          :strand/geometry "#F2D16B" :strand/differential-geometry "#B8860B"
          :strand/analysis "#8FC7E8" :strand/measure-theory "#3F7FD0"
          :strand/trigonometry "#F0A35E" :strand/algebra "#C7A8E8" :strand/abstract-algebra "#8A5CD6"
          :strand/number-theory "#E8A8C7" :strand/probability "#9FD38A"
          :strand/topology "#7FD6C2" :strand/graph-theory "#E8615A" :strand/set-theory "#D4EC5C"
          :strand/logic-computing "#B9C4D0" :strand/mechanics-physics "#D98E73"}))

(defn resolve-palette
  "p as a palette map: a map is itself, a keyword names a registered palette,
   nil is :deck."
  [p]
  (cond (map? p) p
        (nil? p) (palette :deck)
        :else (palette p)))

(defn names
  "The registered palette names."
  []
  (sort (remove #{:default} (keys (methods palette)))))
