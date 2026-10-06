(ns alexandria.grade
  "Pipeline stage: grade proposition checks.

   Reuse searched: desargues.geometry.projective/proofs proves by Emmy
   simplification to exact zero; desargues.board.construction/check is the open
   multimethod for board checks; Emmy's emmy.env/simplify and zero? decide the
   symbolic case."
  (:require [desargues.board.construction :as construction]
            [emmy.env :as e]
            [hive-dsl.adt :as adt :refer [defadt]]
            [hive-dsl.result :as r]))

(defadt Grade
  [:grade/proved]
  [:grade/closed-form]
  [:grade/numeric]
  [:grade/fails])

(def default-tolerance 1e-9)

(defn ->grade [variant]
  (adt/make-variant :Grade variant nil))

(defn- zero-exact? [x]
  (let [v (e/simplify x)]
    (and (e/exact? v)
         (e/zero? v))))

(defn- within? [x tol]
  (when (number? x)
    (<= (Math/abs (double x)) tol)))

(defn grade
  "Grade ctx-fidelity and symbolic/numeric differences on Result rails.

   ctx-fidelity is :symbolic, :closed-form, or :numeric. diffs is a seq of
   expressions already written as lhs-rhs; numeric closed-form values pass when
   each value is within tolerance."
  ([ctx-fidelity diffs] (grade ctx-fidelity diffs default-tolerance))
  ([ctx-fidelity diffs tolerance]
   (let [simplified (map e/simplify diffs)
         any-diffs? (seq simplified)
         all-exact-zero? (and any-diffs? (every? zero-exact? simplified))
         within-tolerance? (and any-diffs? (every? #(within? % tolerance) simplified))]
     (r/ok
      (cond
        all-exact-zero? (->grade :grade/proved)
        (not within-tolerance?) (->grade :grade/fails)
        (= :numeric ctx-fidelity) (->grade :grade/numeric)
        :else (->grade :grade/closed-form))))))

(defn board-check-diffs
  "Return a Result of the board's check expressions that are differences.
   Reuses desargues.board.construction/check; checks with :against are graded
   as check - against, checks without :against as check - 0."
  [env checks]
  (r/try-effect* :grade/check-failed
    (mapv (fn [c]
            (let [lhs (construction/check c env)
                  rhs (if-let [against (:against c)]
                        (construction/check against env)
                        0)]
              (e/- lhs rhs)))
          checks)))

(defn grade-board-checks
  ([ctx-fidelity env board] (grade-board-checks ctx-fidelity env board default-tolerance))
  ([ctx-fidelity env board tolerance]
   (r/let-ok [diffs (board-check-diffs env (:checks board))]
     (grade ctx-fidelity diffs tolerance))))
