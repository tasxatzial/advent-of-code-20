(ns day16.core
  (:gen-class))

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

; ---------------------------------------
; problem 1

(defn value-valid?
  "Returns true if a ticket value is valid."
  [value rules]
  (if (empty? rules)
    false
    (let [rule (first rules)
          range1 (first rule)
          range2 (second rule)
          contained? #(and (<= value (second %)) (>= value (first %)))]
      (if (or (contained? range1) (contained? range2))
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

(def valid-nearby-tickets
  (filter #(= 0 (ticket-error-rate %)) nearby-tickets))

; ---------------------------------------
; results

(def day16-1 (apply + (map ticket-error-rate nearby-tickets)))

(defn -main
  []
  (println day16-1))
