(ns day19.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n."
  [s]
  (clojure.string/split s #"\n"))

(def input (parse (slurp input-file)))

(defn parse-rule-value
  "Parses a string of the form \"12 13 a\" into the appropriate structure.
  For this example it returns (12 13 \"a\")"
  [value]
  (let [split-rule (clojure.string/split value #" ")
        rule-values (filter #(boolean (seq %)) split-rule)]
    (map #(try
            (Integer. ^String %)
            (catch NumberFormatException e %))
         rule-values)))

(defn parse-rules
  "Parses the rules from the input file into a map that has the form:
  rule_index : ((rule_index1 rule_index2 ...) (rule_index3 rule_index4 ...) ...)"
  [input]
  (let [init-rules (take-while #(not= "" %) input)]
    (reduce (fn [rules rules-string]
              (let [split-rule (clojure.string/split rules-string #":")
                    rule-key (first split-rule)
                    rule-values (clojure.string/split (second split-rule) #"\|")
                    parsed-rule-values (map parse-rule-value rule-values)]
                (assoc rules rule-key parsed-rule-values)))
            {}
            init-rules)))

(defn parse-messages
  "Parses the messages from the input file into a list of strings."
  [input]
  (rest (drop-while #(not= "" %) input)))

(defn -main
  []
  (println (parse-messages input)))
