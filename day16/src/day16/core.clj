(ns day16.core
  (:gen-class)
  (:use clojure.set))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn str->int
  "Convert a string to integer."
  [^String s]
  (Integer. s))

(defn parse
  "Splits the input string by \n\n."
  [s]
  (clojure.string/split s #"\n\n"))

(def input (parse (slurp input-file)))
(def input-ticket-rules (first input))
(def input-your-ticket (second input))
(def input-nearby-tickets (last input))

; a list of rules, each rule has the form ((min, max) (min, max))
(def ticket-rules
  (let [rules (clojure.string/split input-ticket-rules #"\n")
        split-rules (map #(clojure.string/split % #"(: )|( or )") rules)]
    (reduce (fn [result split-rule]
              (let [first-range (last (butlast split-rule))
                    second-range (last split-rule)
                    rule (map #(clojure.string/split % #"-") [first-range second-range])]
                (conj result (map (partial map str->int) rule))))
            []
            split-rules)))

; how many rules we have
(def rules-count (count ticket-rules))

; a list of tickets, each ticket is a list of values
(def nearby-tickets
  (let [ticket-values (rest (clojure.string/split input-nearby-tickets #"\n")) ]
    (map #(map str->int %) (map #(clojure.string/split % #",") ticket-values))))

;a list of values for our ticket
(def our-ticket
  (let [ticket-values (second (clojure.string/split input-your-ticket #"\n"))]
    (map str->int (clojure.string/split ticket-values #","))))

(defn contained?
  "Returns true if value is between (first range) and (second range) inclusive."
  [value range]
  (and (<= value (second range)) (>= value (first range))))

; ---------------------------------------
; problem 1

(defn value-valid?
  "Returns true if a ticket value is valid."
  [value rules]
  (if (empty? rules)
    false
    (let [rule (first rules)
          range1 (first rule)
          range2 (second rule)]
      (if (or (contained? value range1) (contained? value range2))
        true
        (recur value (rest rules))))))

(defn ticket-error-rate
  "Returns the scanning error rate for a specific ticket."
  [ticket]
  (reduce (fn [result value]
            (if (value-valid? value ticket-rules)
              result
              (+ result value)))
          0
          ticket))

; ---------------------------------------
; problem 2

(defn ticket-valid?
  "Returns true if a ticket is valid, false otherwise."
  [ticket]
  (if (empty? ticket)
    true
    (if (value-valid? (first ticket) ticket-rules)
      (recur (rest ticket))
      false)))

(defn valid-nearby-tickets
  "Collects all valid nearby tickets."
  []
  (filter ticket-valid? nearby-tickets))

(defn satisfied-rules
  "Returns the indexes of the satisfied rules for the specified value."
  ([rules value] (satisfied-rules rules value 0 []))
  ([rules value index result]
   (if (empty? rules)
     result
     (let [[range1 range2] (first rules)]
       (if (or (contained? value range1) (contained? value range2))
         (recur (rest rules) value (inc index) (conj result index))
         (recur (rest rules) value (inc index) result))))))

(defn ticket-satisfied-rules
  "Returns a vector, each element is a vector of all the satisfied rules for
   the ticket value at the corresponding index."
  [ticket]
  (reduce (fn [result value]
            (conj result (satisfied-rules ticket-rules value)))
          []
          ticket))

(defn all-ticket-satisfied-rules
  "Returns a vector of vectors. Each vector corresponds to the result of
  ticket-satisfied-rules() for the nearby-ticket at the corresponding index."
  []
  (reduce (fn [result ticket]
            (conj result (ticket-satisfied-rules ticket)))
          []
          (valid-nearby-tickets)))

(defn nth-value-satisfied-rules
  "Returns a list. Each element is a vector of the satisfied rules
  for the nth value of each ticket."
  [n]
  (map #(get % n) (all-ticket-satisfied-rules)))

(defn all-values-satisfied-rules
  "Returns a vector that is the result of applying nth-value-satisfied-rules()
  to all-ticket-satisfied-rules."
  ([] (all-values-satisfied-rules 0 []))
  ([index result]
   (if (= index rules-count)
     result
     (let [field-valid-tickets (nth-value-satisfied-rules index)]
       (all-values-satisfied-rules (inc index) (conj result field-valid-tickets))))))

(def memoized-values-satisfied-rules (memoize all-values-satisfied-rules))

(def values-all-satisfied-rules
  "Returns a list of sets. Each set corresponds to the nth values of each ticket
  and has the rules that are satisfied by at least one of those values."
  (reduce (fn [result rules]
            (let [value-satisfied-rules (set (apply concat (map set rules)))]
              (conj result value-satisfied-rules)))
          []
          (memoized-values-satisfied-rules)))

(defn process-all-value-satisfied-rules
  "Removes from all-rules every rule that does not appear in every element of rules."
  [rules all-rules]
  (let [non-satisfied (reduce (fn [result satisfied-rules]
                                (into result (difference all-rules (set satisfied-rules))))
                              #{}
                              rules)]
    (difference all-rules non-satisfied)))

(defn init-all-values-satisfied-rules
  "Removes from the i-th element of values-all-satisfied-rules the rules that
  are not satisfied at the same time by every i-th value of the tickets."
  []
  (map #(process-all-value-satisfied-rules %1 %2) (memoized-values-satisfied-rules) values-all-satisfied-rules))

(defn compute-rules-index
  "Returns a vector of the permutation of the rules from the input file based
  on our ticket. For example if the first element is 17 then the first element in our
  ticket corresponds to 17th line of the input file."
  ([rules] (compute-rules-index rules (apply vector (take rules-count (repeat -1)))))
  ([rules result]
   (let [one-element (some #(and (= 1 (count %)) %) rules)]
     (if one-element
       (let [index (count (take-while #(not= 1 (count %)) rules))
             new-rules (map #(difference % one-element) rules)
             new-result (assoc result index (first one-element))]
         (recur new-rules new-result))
       result))))

(def memoized-rules-indexes (memoize (comp compute-rules-index init-all-values-satisfied-rules)))

(defn compute-solution2
  "Computes the final solution to problem 2."
  ([] (compute-solution2 0 1))
  ([index result]
   (if (= rules-count index)
     result
     (let [index-value (get (memoized-rules-indexes) index)]
       (if (and (<= index-value 5) (>= index-value 0))
         (recur (inc index) (* result (nth our-ticket index)))
         (recur (inc index) result))))))

; ---------------------------------------
; results

(defn day16-1
  []
  (apply + (map ticket-error-rate nearby-tickets)))

(defn day16-2
  []
  (compute-solution2))

(defn -main
  []
  (println (day16-1))                                         ; 20231
  (println (day16-2)))                                        ; 1940065747861
