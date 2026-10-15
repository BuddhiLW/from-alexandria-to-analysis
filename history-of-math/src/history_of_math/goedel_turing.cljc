(ns history-of-math.goedel-turing
  "Small, exact finite models for the two notebook experiments. These symbol
   codes are pedagogical and are not Gödel's alphabet. Four signs at most
   keep products below 2^53, so browser integer arithmetic is exact."
  (:require [clojure.string :as str]))

(def codes {\0 1 \1 2 \x 3 \+ 4 \= 5})
(def signs (into {} (map (fn [[k v]] [v k]) codes)))
(def primes [2 3 5 7])

(defn valid-formula? [s]
  (and (string? s) (<= 1 (count s) 4) (every? codes s)))

(defn encode [s]
  (when (valid-formula? s)
    (reduce * 1 (map (fn [p ch] (reduce * 1 (repeat (codes ch) p)))
                     primes s))))

(defn decode [number]
  (when (and (integer? number) (pos? number))
    (let [[remainder exponents]
          (reduce (fn [[n es] p]
                    (loop [v n e 0]
                      (if (zero? (mod v p))
                        (recur (quot v p) (inc e))
                        [v (conj es e)])))
                  [number []] primes)
          used (take-while pos? exponents)]
      (when (and (= remainder 1)
                 (seq used)
                 (every? zero? (drop (count used) exponents))
                 (every? signs used))
        (apply str (map signs used))))))

(defn numbering [s]
  (when-let [n (encode s)]
    {:formula s :number n :decoded (decode n)
     :factors (mapv (fn [p ch] [p (codes ch)]) primes s)}))

(defn diagonal-rows []
  (mapv (fn [i bits]
          {:index i :row bits :diagonal (nth bits (dec i))
           :opposite (if (= \0 (nth bits (dec i))) \1 \0)})
        (range 1 5) ["0011" "1010" "1100" "0101"]))

(defn anti-diagonal [] (apply str (map :opposite (diagonal-rows))))

;; Binary increment: the head starts on the last digit in state :carry.
;; A 1 becomes 0 and the carry moves left; a 0 or a blank becomes 1 and
;; the machine halts. Rules map [state symbol] to [write move next].
(def increment-rules
  {[:carry \1] [\0 -1 :carry]
   [:carry \0] [\1 0 :halt]
   [:carry \_] [\1 0 :halt]})

(defn machine [bits]
  {:tape (zipmap (range) bits) :head (dec (count bits)) :state :carry :steps 0})

(defn halted? [m] (= :halt (:state m)))

(defn step [{:keys [tape head state] :as m}]
  (if (halted? m)
    m
    (let [sym (get tape head \_)
          [write move next] (increment-rules [state sym])]
      (-> m (assoc-in [:tape head] write) (update :head + move)
          (assoc :state next) (update :steps inc)))))

(defn run [m] (first (drop-while (complement halted?) (iterate step m))))

(defn tape-string [{:keys [tape]}]
  (let [ks (keys tape)]
    (apply str (map #(get tape % \_) (range (apply min ks) (inc (apply max ks)))))))

(defn increment [bits] (tape-string (run (machine bits))))

;; A claimed halting decider H, tabulated on programs P1..P4 run on their
;; own descriptions. D is built to do the opposite of H's verdict on the
;; diagonal, so D cannot be any listed program: on input Pi it behaves
;; unlike Pi is predicted to behave.
(def claimed-h
  [[:halts :loops :halts :halts]
   [:loops :loops :halts :loops]
   [:halts :halts :halts :loops]
   [:loops :halts :loops :loops]])

(defn halting-rows []
  (mapv (fn [i row]
          (let [verdict (nth row i)]
            {:program (str "P" (inc i)) :row row :h-on-self verdict
             :d-on-self (if (= verdict :halts) :loops :halts)}))
        (range) claimed-h))
