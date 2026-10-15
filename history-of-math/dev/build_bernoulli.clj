;; Build only the Bernoulli notebook (and the apex and series pages) into
;; target/bernoulli-site:
;;   clojure -J-Xmx2g -M:notebooks -i dev/build_bernoulli.clj
(require 'history-of-math.build)
(let [r ((resolve 'history-of-math.build/build!)
         {:out-path "target/bernoulli-site" :only ["probability-bernoulli-1713"]})]
  (println "BUILD-RESULT" (pr-str r))
  (shutdown-agents))
