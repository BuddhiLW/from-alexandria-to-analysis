(ns alexandria.proofs
  "Collect stage for proofs told step by step: an EDN resource of
   {proposition-id {:steps [{:claim :why :stage}] ...}}, where :stage names
   what a medium shows while the step is read."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [hive-dsl.result :as r]))

(defn read-proofs
  "Result of the proofs in classpath resource `resource`."
  [resource]
  (if-let [url (io/resource resource)]
    (r/try-effect* :proofs/read-failed (edn/read-string (slurp url)))
    (r/err {:alexandria/error :resource/not-found :resource resource})))

(defn steps
  "The steps of proposition id in resource, or one step explaining why they
   are missing."
  [resource id]
  (let [res (read-proofs resource)]
    (or (when (r/ok? res) (get-in res [:ok id :steps]))
        [{:stage :error :why "unavailable"
          :claim (pr-str (or (:error res) {:alexandria/error :proposition/not-found :id id}))}])))
