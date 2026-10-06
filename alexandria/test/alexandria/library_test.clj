(ns alexandria.library-test
  (:require [alexandria.library :as library]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]))

(deftest shelf-validation-returns-propositions
  (let [result (library/shelves)]
    (is (r/ok? result))
    (let [shelves (:ok result)]
      (is (contains? shelves :euclid/I.1))
      (is (= "Euclid" (get-in shelves [:euclid/I.1 :author])))))
  (testing "small fixture tradition validates without depending on elements_1.edn"
    (let [result (library/shelves "alexandria/fixture_catalogue.edn")]
      (is (r/ok? result))
      (is (= "Fixture Author" (get-in (:ok result) [:fixture/one :author]))))))

(deftest proposition-finds-and-misses-on-rails
  (testing "known proposition"
    (let [result (library/proposition :euclid/I.1)]
      (is (r/ok? result))
      (is (= :euclid/I.1 (:id (:ok result))))))
  (testing "unknown proposition"
    (let [result (library/proposition :missing/none)]
      (is (r/err? result))
      (is (= :proposition/not-found (get-in result [:error :alexandria/error]))))))
