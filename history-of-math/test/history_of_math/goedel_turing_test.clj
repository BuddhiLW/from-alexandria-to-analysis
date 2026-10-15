(ns history-of-math.goedel-turing-test
  (:require [clojure.test :refer [deftest is testing]]
            [history-of-math.goedel-turing :as gt]))

(def alphabet (keys gt/codes))

(defn- formulas-up-to [n]
  (mapcat (fn [k] (reduce (fn [acc _] (for [s acc c alphabet] (str s c)))
                          [""] (range k)))
          (range 1 (inc n))))

(deftest encode-decode-round-trip
  (testing "every formula of one to four signs survives encode then decode"
    (doseq [s (formulas-up-to 4)]
      (is (= s (gt/decode (gt/encode s))) s)))
  (testing "distinct formulas get distinct numbers"
    (let [all (formulas-up-to 4)]
      (is (= (count all) (count (set (map gt/encode all)))))))
  (testing "a worked example: x+1 is 2^3 * 3^4 * 5^2"
    (is (= (* 8 81 25) (gt/encode "x+1")))))

(deftest invalid-input
  (is (nil? (gt/encode "")))
  (is (nil? (gt/encode "x+1=0")))
  (is (nil? (gt/encode "y")))
  (is (nil? (gt/decode 0)))
  (is (nil? (gt/decode 7)) "an exponent on 7 with none on 5 is not a formula")
  (is (nil? (gt/decode (* 2 2 2 2 2 2))) "code 6 names no sign"))

(deftest diagonal-differs-from-every-row
  (let [anti (gt/anti-diagonal)]
    (doseq [{:keys [index row]} (gt/diagonal-rows)]
      (is (not= (nth anti (dec index)) (nth row (dec index))))
      (is (not= anti row)))))

(deftest turing-increment
  (doseq [n (range 0 300)]
    (let [bits (Integer/toBinaryString n)]
      (is (= (Integer/toBinaryString (inc n)) (gt/increment bits)) bits)))
  (testing "step is the identity once halted"
    (let [done (gt/run (gt/machine "11"))]
      (is (gt/halted? done))
      (is (= done (gt/step done)))
      (is (= 3 (:steps done))))))

(deftest halting-diagonal
  (doseq [{:keys [h-on-self d-on-self]} (gt/halting-rows)]
    (is (not= h-on-self d-on-self))))
