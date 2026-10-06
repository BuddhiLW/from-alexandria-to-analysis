(ns build-euler
  "Scratch build of the Euler notebook alone, for the badge check:
   clojure -J-Xmx2g -M:notebooks dev/build_euler.clj"
  (:require [nextjournal.clerk :as clerk]))

(clerk/build! {:paths ["notebooks/euler.clj"] :out-path "target/hom-euler"})
(shutdown-agents)
(System/exit 0)
