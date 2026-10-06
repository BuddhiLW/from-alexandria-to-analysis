(ns alexandria.medium.figure
  "The port a scene draws through: a figure gives the plane points of many
   states at once. Two adapters:

     FnFigure      an Emmy function (fn [& params] (fn [state] [x y])),
                   evaluated point by point (JVM tests, any host)
     KernelFigure  (cljs) a kernel compiled by alexandria.medium.kernel:
                   one WebAssembly batch call per frame, Emmy's :js function
                   while the module is not ready")

(defprotocol IFigure
  (points [figure params states]
    "The [x y] of each state (a vector of numbers) at params (a vector)."))

(defrecord FnFigure [f]
  IFigure
  (points [_ params states]
    (let [g (apply f params)]
      (mapv (fn [s] (mapv double (g s))) states))))

#?(:cljs
   (do
     (defn- revive
       "The JS function of kernel data {:glue :fallback}."
       [{:keys [glue fallback]}]
       (let [fb (.apply js/Function nil (clj->js fallback))]
         ((js/Function "fb" glue) fb)))

     (defn- flat-states
       "The states as one Float64Array, d numbers each."
       [states]
       (let [flat (into [] cat states)
             xs (js/Float64Array. (count flat))]
         (dotimes [i (count flat)] (aset xs i (nth flat i)))
         xs))

     (defn- pairs
       "[[x y ...] ...] of a Float64Array of n points, d numbers each: d = 2
        for a plane figure, more for a kernel that returns several numbers
        (the sides of an application of areas, a trace with its test)."
       [out n d]
       (mapv (fn [i] (mapv #(aget out (+ (* d i) %)) (range d))) (range n)))

     (defn- outputs
       "The number of outputs per point of kernel f (its dims), 2 when the
        kernel carries no dims (Emmy's :js fallback)."
       [^js f]
       (or (some-> (.-dims f) (.-outputs)) 2))

     (defrecord KernelFigure [f memo]
       IFigure
       (points [_ params states]
         (let [n (count states)
               xs (let [[s xs] @memo]
                    (if (identical? s states)
                      xs
                      (let [xs (flat-states states)] (reset! memo [states xs]) xs)))
               ps (clj->js params)]
           (if-let [out (.batch f xs n ps)]
             (pairs out n (outputs f))
             (mapv (fn [s] (vec (f (clj->js s) ps))) states)))))

     (defn kernel-figure
       "The figure of kernel data {:glue :fallback}."
       [kernel]
       (->KernelFigure (revive kernel) (atom nil)))))
