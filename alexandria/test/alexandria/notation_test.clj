(ns alexandria.notation-test
  (:require [alexandria.formula :as formula]
            [alexandria.notation :as notation]
            [clojure.test :refer [deftest is]]))

(deftest renders-claim-in-tex-and-euclid
  (let [claim '(= (+ (area A L V) (area V R B)) (* 1/4 (area A V B)))]
    (is (= "[ALV] + [VRB] = \\frac{1}{4}\\,[AVB]"
           (notation/render-expr :tex claim)))
    (is (= "the triangle ALV together with the triangle VRB is equal to 1/4 times the triangle AVB"
           (notation/render-expr :euclid claim)))))

(deftest frozen-hyperbola-thaws-and-renders
  (let [h (formula/freeze {:name 'H1
                           :expr '(hyperbola a b)
                           :bindings {'a 1 'b 0}
                           :passage {:author :apollonius
                                     :work :conics
                                     :locus "I.11"
                                     :quote "A section of an obtuse-angled cone."}})]
    (is (= '(hyperbola 1 0) (formula/thaw h)))
    (is (= "\\mathcal{H}_{1,0}" (formula/render h :tex)))
    (is (= "a hyperbola whose first magnitude is 1 and whose second magnitude is 0"
           (formula/render h :al-khwarizmi)))))

(deftest one-claim-in-leibniz-and-newton
  (let [claim '(= (differential (* x v)) (+ (* x dv) (* v dx)))]
    (is (= "d\\overline{x\\,v} = x\\,dv + v\\,dx" (notation/render-expr :leibniz claim)))
    (is (= "\\dot{\\overline{x\\,v}} = x\\,\\dot{v} + v\\,\\dot{x}" (notation/render-expr :newton claim)))
    (is (= "\\dot{x}" (notation/render-expr :newton 'dx)))
    (is (= "dx" (notation/render-expr :leibniz 'dx)))
    (is (= 'x (notation/difference-of 'dx)))
    (is (nil? (notation/difference-of 'differential)))))
