(ns history-of-math.goedel-turing-widgets
  (:require [nextjournal.clerk :as clerk]))

(defn- widget [name]
  (clerk/with-viewer
    {:name name :require-cljs true :transform-fn clerk/mark-presented
     :render-fn name}
    {}))

(defn numbering [] (widget 'history-of-math.goedel-turing-view/numbering))
(defn diagonal [] (widget 'history-of-math.goedel-turing-view/diagonal))

(defn turing-machine [] (widget 'history-of-math.goedel-turing-view/turing-machine))
(defn halting [] (widget 'history-of-math.goedel-turing-view/halting))
