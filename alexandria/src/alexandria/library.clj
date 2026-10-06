(ns alexandria.library
  "Collect stage for Alexandria shelves.

   Reuse searched: desargues.specs.board/board-spec for the board envelope;
   desargues.board.construction owns construction ops/checks/draw lowering. This
   namespace adds only shelf/proposition metadata validation and Result rails."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [desargues.specs.board :as board]
            [hive-dsl.result :as r]
            [malli.core :as m]
            [malli.error :as me]))

(def catalogue-resource "alexandria/catalogue.edn")

(def Metadata
  [:map
   [:id qualified-keyword?]
   [:title string?]
   [:statement string?]
   [:source string?]
   [:author string?]
   [:work string?]])

(def Tradition
  [:map
   [:id qualified-keyword?]
   [:resource string?]])

(def Catalogue
  [:map [:traditions [:vector Tradition]]])

(defn- read-resource [resource]
  (if-let [url (io/resource resource)]
    (try
      (r/ok (edn/read-string (slurp url)))
      (catch Exception e
        (r/err {:alexandria/error :resource/read-failed
                :resource resource
                :cause (ex-message e)})))
    (r/err {:alexandria/error :resource/not-found
            :resource resource})))

(defn- validate-malli [schema value where]
  (if (m/validate schema value)
    (r/ok value)
    (r/err {:alexandria/error :validation/malli
            :where where
            :explain (me/humanize (m/explain schema value))})))

(defn- construction-board [p]
  (merge {:kind :construction :params []}
         (select-keys p [:id :kind :params :f :var :window :label])))

(defn- validate-board [p where]
  (let [b (construction-board p)]
    (if (and (= :construction (:kind b)) (s/valid? ::board/board-spec b))
      (r/ok p)
      (r/err {:alexandria/error :validation/board
              :where where
              :board b
              :explain (s/explain-data ::board/board-spec b)}))))

(defn- tradition-defaults [{:keys [tradition]}]
  (select-keys tradition [:author :work :source]))

(defn- validate-proposition [defaults p]
  (let [p (merge defaults p)
        where (:id p)]
    (r/let-ok [_ (validate-malli Metadata (select-keys p [:id :title :statement :source :author :work]) where)
               _ (validate-board p where)]
      (r/ok p))))

(defn- collect-tradition [{:keys [id resource]}]
  (r/let-ok [shelf (read-resource resource)]
    (let [defaults (tradition-defaults shelf)]
      (reduce
       (fn [acc p]
         (r/let-ok [m acc
                    vp (validate-proposition defaults p)]
           (if (contains? m (:id vp))
             (r/err {:alexandria/error :proposition/duplicate
                     :id (:id vp)
                     :tradition id})
             (r/ok (assoc m (:id vp) vp)))))
       (r/ok {})
       (:propositions shelf)))))

(defn catalogue
  ([] (catalogue catalogue-resource))
  ([resource]
   (r/let-ok [c (read-resource resource)
              _ (validate-malli Catalogue c :catalogue)]
     (r/ok c))))

(defn shelves
  ([] (shelves catalogue-resource))
  ([catalogue-resource]
   (r/let-ok [c (catalogue catalogue-resource)]
     (reduce
      (fn [acc t]
        (r/let-ok [m acc
                   shelf (collect-tradition t)]
          (r/ok (merge m shelf))))
      (r/ok {})
      (:traditions c)))))

(defn proposition [id]
  (r/let-ok [m (shelves)]
    (if-let [p (get m id)]
      (r/ok p)
      (r/err {:alexandria/error :proposition/not-found
              :id id}))))
