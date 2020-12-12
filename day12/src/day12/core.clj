(ns day12.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn str->int
  [^String s]
  (Integer. s))

(defn parse
  "Splits the input string by \n."
  [s]
  (clojure.string/split s #"\n"))

(def parsed-input (parse (slurp input-file)))

; create the appropriate structure for the instructions
(def instructions
  (reduce (fn [result instruction]
            (conj result [(keyword (str (first instruction))) (str->int (apply str (drop 1 instruction)))]))
          []
          parsed-input))

(defn -main
  []
  (println instructions))
