(ns alexandria.euclid.elements
  "Euclid, Elements, Book I as it sits on the shelf
   (resources/alexandria/euclid/elements_1.edn): each proposition's
   construction run point by point, its checks graded, and the steps of its
   proof (elements_1.proofs.edn).

   Reuse searched before writing (carto over desargues, Emmy, alexandria):
   desargues.board.construction/point and check carry every construction
   step and measure (circles meeting, lines produced and cut, squares and
   translations are registered by alexandria.ops.euclid); desargues.board
   compiles a whole board to a raster kernel. What was missing is running a
   construction outside a kernel: with numbers, for a figure a medium draws,
   and with symbols, so a claim is proved for every figure. That is `run`.

     run        the points of a construction, numbers or Emmy symbols
     graded     every check of a proposition, :grade/proved when Emmy
                simplifies it to 0 with the given points left symbolic,
                :grade/numeric when only the figure's numbers decide it
     figure     plain numbers a scene draws, by point id"
  (:require [alexandria.grade :as grade]
            [alexandria.library :as library]
            [alexandria.notation :as notation]
            [alexandria.ops.euclid]
            [alexandria.proofs :as proofs]
            [alexandria.vocab]
            [desargues.board.construction :as c]
            [emmy.env :as e]
            [hive-dsl.result :as r]
            [clojure.walk]
            [desargues.board :as board]))

(def proofs-resource "alexandria/euclid/elements_1.proofs.edn")

(defn proof
  "Result of {proposition-id {:steps [{:claim :why :stage}]}}."
  []
  (proofs/read-proofs proofs-resource))

(defn construction
  "The construction board of proposition id, or nil."
  [id]
  (let [res (library/proposition id)]
    (when (r/ok? res) (get-in res [:ok :construction]))))

(defn- given-point
  "A given point's coordinates: its numbers, or symbols Ax Ay."
  [{:keys [id at]} mode]
  (if (= :symbolic mode)
    [(symbol (str (name id) "x")) (symbol (str (name id) "y"))]
    at))

(defn- with-params
  "The board's points with its params at their initial values (numeric), or
   left as symbols (symbolic)."
  [{:keys [params points]} mode]
  (if (= :symbolic mode)
    points
    (clojure.walk/postwalk-replace (into {} (map (juxt :id :init)) params) points)))

(defn run
  "The env of a construction board run point by point: {:points {id {:x :y}}
   :param-syms}, every given point and param numeric (mode :numeric) or an
   Emmy symbol (mode :symbolic). A point that cannot be made throws, as
   desargues does."
  [board mode]
  (reduce (fn [env {:keys [op] :as p}]
            (let [[x y] (if (or (nil? op) (= :free op))
                          (given-point p mode)
                          (:xy (c/point p env)))]
              (assoc-in env [:points (:id p)] {:x x :y y})))
          {:points {} :param-syms (if (= :symbolic mode) (mapv :id (:params board)) [])}
          (with-params board mode)))

(defn check-diff
  "The difference a check states is 0: check minus its :against, or the
   check itself."
  [check env]
  (e/- (c/check check env) (if-let [against (:against check)] (c/check against env) 0)))

(defn- symbolic-zero?
  "True when Emmy simplifies the check to exact 0 in sym-env. Tried for
   vocabulary claims only: they are written as polynomials in lengths
   squared and areas, which Emmy decides in seconds. A named check (an
   angle, an arccosine; collinearity of a point cut by a circle, nested
   square roots) can keep Emmy's simplifier busy without end, so named
   checks are decided on the figure's raster numbers. A claim Emmy's
   polynomial division refuses (inexact coefficients from trigonometry) is
   not proved here either."
  [check sym-env]
  (when (contains? check :claim)
    (try
      (let [d (e/simplify (check-diff check sym-env))]
        (and (e/exact? d) (e/zero? d)))
      (catch IllegalStateException _ false))))

(defn- label
  "A check in Euclid's words, when it is a vocabulary claim."
  [check]
  (cond
    (:claim check) (notation/render-expr :euclid (:claim check))
    (:equal-lengths check) (str "equal: " (clojure.string/join ", " (map #(apply str (map name %)) (:equal-lengths check))))
    (:angle check) (str "the angle " (apply str (map name (:angle check)))
                        " is equal to the angle " (apply str (map name (get-in check [:against :angle]))))
    (:collinear check) (str (apply str (map name (:collinear check))) " is one straight line")
    (:perpendicular check) (let [[[a b] [_ d]] (:perpendicular check)]
                             (str "the angle " (name b) (name a) (name d) " is right"))
    :else (pr-str check)))

(def frame
  "The frame the board's raster kernel computes (desargues.board/compile-board!):
   one configuration at the given points and initial params, by output key.
   Memoized on the board."
  (memoize
   (fn [board]
     (let [out-dir (str (System/getProperty "java.io.tmpdir") "/alexandria-elements")]
       (update-vals (:board/frame (board/compile-board! board {:out-dir out-dir})) first)))))

(defn raster-checks
  "The value of every check of construction board, check minus :against,
   from its raster frame."
  [board]
  (let [fr (frame board)]
    (mapv (fn [i]
            (let [k (keyword (str "o-ck" i))
                  rk (keyword (str "o-ck" i "ref"))]
              (if (contains? fr rk) (- (fr k) (fr rk)) (fr k))))
          (range (count (:checks board))))))

(defn graded
  "[{:label :grade :engine}] for every check of proposition id:
   :grade/proved (engine :emmy) when Emmy simplifies it to 0 for every
   position of the given points, otherwise alexandria.grade on the value
   the board's raster kernel computes (engine :raster), :grade/numeric or
   :grade/fails."
  [id]
  (let [board (construction id)
        sym-env (run board :symbolic)
        values (delay (raster-checks board))]
    (vec (map-indexed
          (fn [i check]
            (if (symbolic-zero? check sym-env)
              {:label (label check) :grade :grade/proved :engine :emmy}
              (let [res (grade/grade :numeric [(nth @values i)])]
                {:label (label check) :engine :raster
                 :grade (if (r/ok? res) (get-in res [:ok :adt/variant]) :grade/fails)})))
          (:checks board)))))

(defn figure
  "The [x y] of every point of proposition id, by id, as its raster kernel
   computes them."
  [id]
  (let [board (construction id)
        fr (frame board)]
    (into {} (map-indexed (fn [i {pid :id}]
                            [pid [(fr (keyword (str "o-pt" i "x"))) (fr (keyword (str "o-pt" i "y")))]])
                          (:points board)))))

(defn motion
  "The one moving figure of Book I, an Emmy function that media compiles
   to a raster kernel (the browser does the arithmetic in WebAssembly).

   A state [x y a1x a1y a2x a2y b1x b1y b2x b2y s] is a point P with two
   slides, a = a2 - a1 and b = b2 - b1, named by points of the
   construction. Its image at params [cx cy theta wa wb]: P slid by wa a,
   turned about the centre (cx, cy) by s theta, then slid by wb b.

     I.1   P = B, theta = 2 pi, s over [0 1]: the circle of Post. 3 traced
     I.47  the windmill: a square sheared along a parallel (wa, I.41),
           turned a right angle about a corner (s, I.4: FBC onto ABD),
           sheared again into its rectangle (wb)."
  [cx cy theta wa wb]
  (fn [[x y a1x a1y a2x a2y b1x b1y b2x b2y s]]
    (let [ang (e/* theta s)
          u (e/- (e/+ x (e/* wa (e/- a2x a1x))) cx)
          v (e/- (e/+ y (e/* wa (e/- a2y a1y))) cy)]
      [(e/+ cx (e/- (e/* u (e/cos ang)) (e/* v (e/sin ang))) (e/* wb (e/- b2x b1x)))
       (e/+ cy (e/+ (e/* u (e/sin ang)) (e/* v (e/cos ang))) (e/* wb (e/- b2y b1y)))])))

(def figures
  "{name {:f :params :state}} for alexandria.medium.kernel."
  {:motion {:f motion :params [0 0 0 0 0] :state [1 0 0 0 0 0 0 0 0 0 0]}})

(defn motion-state
  "The state of point id of a figure under `motion`: its [x y], slide a
   from a1 to a2, slide b from b1 to b2 (point ids, or nil for no slide),
   and turned fraction s."
  ([fig id s] (motion-state fig id nil nil s))
  ([fig id [a1 a2] [b1 b2] s]
   (let [at (fn [k] (fig (or k id)))]
     (vec (concat (fig id) (at a1) (at a2) (at b1) (at b2) [s])))))

(defn windmill
  "I.47's windmill as data for a scene: for each half, the turning centre,
   the angle, and the four corners of its square as `motion` states. The
   left square GB (A B F G) is sheared along AC, turned a right angle
   about B (FBC onto ABD), and sheared along AL into the rectangle BL; the
   right square HC (A C K H) likewise about C into CL. Corners are the
   points the board's raster kernel computes."
  []
  (let [fig (figure :euclid/I.47)
        half (fn [centre theta corners]
               {:centre (fig centre) :theta theta
                :states (mapv (fn [[id a b]] (motion-state fig id a b 1)) corners)})]
    {:left (half :B (- (/ Math/PI 2))
                 [[:A [:A :C] nil] [:B nil nil] [:F nil [:A :M]] [:G [:A :C] [:A :M]]])
     :right (half :C (/ Math/PI 2)
                  [[:A [:A :B] nil] [:C nil nil] [:K nil [:A :M]] [:H [:A :B] [:A :M]]])
     :points fig}))
