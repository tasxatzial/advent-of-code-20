(ns day19.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n."
  [s]
  (clojure.string/split s #"\n"))

(defn parse-rule-value
  "Parses a string of the form \"12 13 a\" into the appropriate structure.
  For this example it returns [12 13 \"a\"]"
  [value]
  (let [split-rule (clojure.string/split value #" ")
        rule-values (filter #(boolean (seq %)) split-rule)]
    (mapv #(try
            (Integer. ^String %)
            (catch NumberFormatException e
              (second (clojure.string/split % #"\""))))
         rule-values)))

(defn parse-rules
  "Parses the rules from the input file into a map that has the form:
  rule_index : ((rule_index1 rule_index2 ...) (rule_index3 rule_index4 ...) ...)"
  [input]
  (let [init-rules (take-while #(not= "" %) input)]
    (reduce (fn [rules rules-string]
              (let [split-rule (clojure.string/split rules-string #":")
                    rule-key (Integer. ^String (first split-rule))
                    rule-values (clojure.string/split (second split-rule) #"\|")
                    parsed-rule-values (map parse-rule-value rule-values)]
                (assoc rules rule-key parsed-rule-values)))
            {}
            init-rules)))

(defn parse-messages
  "Parses the messages from the input file into a list of strings."
  [input]
  (rest (drop-while #(not= "" %) input)))

(def input (parse (slurp input-file)))
(def rules (parse-rules input))
(def messages (set (parse-messages input)))

; ---------------------------------------
; problem 1

(defn expand-pattern
  "Expands a pattern using the rules (single pass). For example
  pattern [A B] using rules A -> B | C, B -> D is expanded to
  [B D] [C D] "
  [[pattern index]]
  (let [pattern-item (get pattern index)]
    (if (not pattern-item)
      (list [pattern index])
      (if (or (= pattern-item "a") (= pattern-item "b"))
        (list [pattern (inc index)])
        (let [item-rules (get rules pattern-item)
              update-pattern #(apply vector (flatten (assoc pattern index (% item-rules))))]
          (if (= (count item-rules) 1)
            (list [(update-pattern first) index])
            (list [(update-pattern first) index]
                  [(update-pattern second) index])))))))

(defn gen-zero-patterns
  "Generates all patterns that contain only \"a\" or \"b\" using the pattern
  for rule 0 is used as the initial pattern."
  ([] (gen-zero-patterns [] (list [(first (get rules 0)) 0])))
  ([patterns new-patterns]
   (if (= patterns new-patterns)
     new-patterns
     (let [updated-patterns (apply concat (map expand-pattern new-patterns))]
       (recur new-patterns updated-patterns)))))

;collects all messages that match rule 0
(defn zero-matches
  []
  (let [zero-msgs (gen-zero-patterns)]
    (reduce (fn [result pattern]
              (let [msg (apply str (first pattern))]
                (if (contains? messages msg)
                  (conj result msg)
                  result)))
            #{}
            zero-msgs)))

(defn -main
  []
  (println (count (zero-matches))))
