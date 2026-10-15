(ns history-of-math.build-load-order-e2e
  "One JVM, one history-of-math.build/build!: the graph notebook, then a
   notebook that loads alexandria.structure.groups (as number-theory-gauss
   does through alexandria.gauss.congruence). Clerk's analysis of the graph
   notebook macroexpands the defprotocol of alexandria.structure.group; the
   records of structure.groups loaded after that lose the protocol
   (\"No implementation of :unit of protocol Group for UnitsMod\").

   Needs Clerk, so the default :test runner skips it (*-e2e). By hand:

     clojure -J-Xmx2g -A:notebooks:test -M -e \\
       \"(require 'history-of-math.build-load-order-e2e) (clojure.test/run-tests 'history-of-math.build-load-order-e2e)\""
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [history-of-math.build :as build]))

(def ^:private groups-notebook
  "(ns groups-probe
  {:history/year 1801
   :history/title \"Probe: a notebook that loads structure.groups\"
   :history/era \"Analysis made rigorous\"
   :history/fields #{:graph-theory}}
  (:require [alexandria.structure.group :as g]
            [alexandria.structure.groups :as gs]))

(g/unit (gs/units-mod 7))
")

(defn- temp-notebooks []
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory "hom-load-order" (make-array java.nio.file.attribute.FileAttribute 0)))
        graph (str build/notebooks-dir "/graph_theory_bridges_to_colours.clj")]
    (spit (io/file dir "graph_theory_bridges_to_colours.clj") (slurp graph))
    (spit (io/file dir "groups_probe.clj") groups-notebook)
    dir))

(deftest graph-notebook-then-structure-groups-in-one-build
  (let [dir (temp-notebooks)
        out (str dir "/out")]
    (build/build! {:dir (str dir) :out-path out :cljs-namespaces []})
    (is (.exists (io/file out "groups-probe" "index.html")) "the second notebook was built")
    (let [units-mod (requiring-resolve 'alexandria.structure.groups/units-mod)
          unit (requiring-resolve 'alexandria.structure.group/unit)]
      (is (= 1 (unit (units-mod 7))) "UnitsMod still implements Group after the graph notebook's analysis"))))
