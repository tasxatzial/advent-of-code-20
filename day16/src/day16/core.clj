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

; a list of ticket, each ticket is a list of values
(def nearby-tickets
  (let [ticket-values (rest (clojure.string/split input-nearby-tickets #"\n")) ]
    (map #(map str->int %) (map #(clojure.string/split % #",") ticket-values))))

;a list of values for your ticket
(def your-ticket
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

(def valid-nearby-tickets
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

; a vector of vectors. Each vector corresponds to the result of
; ticket-satisfied-rules() for the nearby-ticket at the corresponding index.
(def all-ticket-satisfied-rules
  (reduce (fn [result ticket]
            (conj result (ticket-satisfied-rules ticket)))
          []
          valid-nearby-tickets))

; how many rules we have
(def rules-count (count ticket-rules))

(defn nth-value-satisfied-rules
  "Returns a list. Each element is a vector of the satisfied rules
  for the nth value of each ticket."
  [n]
  (map #(get % n) all-ticket-satisfied-rules))

(defn all-values-satisfied-rules
  "Returns a vector that is the result of applying nth-value-satisfied-rules()
  to all-ticket-satisfied-rules."
  ([] (all-values-satisfied-rules 0 []))
  ([index result]
   (if (= index rules-count)
     result
     (let [field-valid-tickets (nth-value-satisfied-rules index)]
       (all-values-satisfied-rules (inc index) (conj result field-valid-tickets))))))

(def values-satisfied-rules (all-values-satisfied-rules))

; ---------------------------------------
; results

(def day16-1 (apply + (map ticket-error-rate nearby-tickets)))

(defn -main
  []
  (println day16-1))
