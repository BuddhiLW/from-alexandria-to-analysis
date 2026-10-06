(ns alexandria.galileo.motion-test
  (:require [alexandria.galileo.motion :as motion]
            [alexandria.grade :as grade]
            [alexandria.library :as library]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [desargues.board.construction :as c]
            [emmy.env :as e]
            [hive-dsl.result :as r]))

(def propositions
  [:galileo/third-day-1 :galileo/third-day-2 :galileo/corollary-1
   :galileo/fourth-day-1 :galileo/fourth-day-7])

(deftest every-identity-is-proved
  (doseq [id propositions
          {:keys [label grade]} (motion/graded id)]
    (testing (str id " " label)
      (is (= :grade/proved grade)))))

(deftest the-inclined-plane-is-exact
  (is (= [1 4 9 16] (motion/marks 4)))
  (is (= [1 3 5 7] (motion/odd-steps 4)))
  (is (= 1/2 (motion/time-for 1/4)))
  (is (every? #{:grade/proved} (map :grade (motion/plane-graded)))))

(deftest a-broken-claim-is-not-proved
  (testing "distances as the cubes of the times fail"
    (let [s (motion/distance 'a)
          diff (e/- (e/* (s 't_1) (e/cube 't_2)) (e/* (s 't_2) (e/cube 't_1)))]
      (is (not= :grade/proved (get-in (grade/grade :symbolic [diff]) [:ok :adt/variant]))))))

(deftest forty-five-degrees-numerically
  (is (= 45 (motion/max-range-numeric))))

(def gnomon-property
  (prop/for-all [n gen/nat]
    (= (motion/gnomon n) (- (* (inc n) (inc n)) (* n n)))))

(def parabola-property
  (prop/for-all [u (gen/choose 1 20) k (gen/choose 1 20) t (gen/choose -50 50)]
    (let [[x y] ((motion/projectile u k) t)]
      (= (* x x) (* (motion/latus-rectum u k) y)))))

(deftest properties
  (is (:pass? (tc/quick-check 100 gnomon-property)))
  (is (:pass? (tc/quick-check 100 parabola-property))))

(deftest the-shelf-boards-check-at-symbols
  (let [shelves (:ok (library/shelves))
        mean-env {:points {:A {:x 0 :y 0} :B {:x 'T :y 0} :E {:x 'T :y 'v}
                           :F {:x 'T :y (e// 'v 2)} :G {:x 0 :y (e// 'v 2)} :I {:x (e// 'T 2) :y (e// 'v 2)}}}
        sq-env {:points {:A {:x 0 :y 0} :D {:x 1 :y 0} :E {:x 's :y 0} :O {:x 1 :y 1} :P {:x 's :y 's}}}]
    (is (r/ok? (library/shelves)))
    (doseq [[id env] [[:galileo/third-day-1 mean-env] [:galileo/third-day-2 sq-env]]
            check (get-in shelves [id :construction :checks])]
      (testing (str id " " (:label check))
        (is (e/zero? (e/simplify (c/check check env))))))))

(deftest the-proofs-are-shelf-data
  (let [res (motion/proof)]
    (is (r/ok? res))
    (doseq [id (conj propositions :galileo/inclined-plane)]
      (let [p (get-in res [:ok id])]
        (is (seq (:steps p)) (str id))
        (is (every? (every-pred :claim :why :stage) (:steps p)) (str id))
        (is (string? (:quote p)) (str id))))))
