(ns alexandria.ctx.plane-test
  (:require [alexandria.ctx :as ctx]
            [alexandria.ctx.plane]
            [alexandria.ctx-contract :refer [defcontract]]))

(defcontract plane (ctx/make :plane {}) {:num-tests 50 :tolerance 1.0e-4})
