(ns alexandria.notebooks-build
  "Static Clerk build for Alexandria notebooks.

   Mirrors set-theory's :notebooks alias shape: clojure -X:notebooks
   :out-path '\"target/notebooks\"' writes each notebook into its own stable
   directory under out-path. These notebooks use Clerk's built-in viewers, so
   the default Clerk static bundle is enough."
  (:require [nextjournal.clerk :as clerk]))

(def notebooks
  [{:id "euclid-i1-surfaces"
    :path "notebooks/alexandria/notebooks/euclid_i1_surfaces.clj"}])

(defn build!
  "Build each Alexandria notebook into out-path/<id>/index.html."
  [{:keys [out-path] :or {out-path "public/notebooks"}}]
  (doseq [{:keys [id path]} notebooks]
    (println "notebook" id "<-" path)
    (clerk/build! {:paths [path]
                   :out-path (str out-path "/" id)})))
