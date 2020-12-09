(ns day09.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn str->BigInt
  "Converts a string to BigInteger."
  [^String s]
  (BigInteger. s))

(defn parse
  "Splits the input string by \n."
  [s]
  (clojure.string/split s #"\n"))

; convert the input file to a list of BigInteger
(def parsed-input (map str->BigInt (parse (slurp input-file))))

(defn -main
  []
  (println parsed-input))
