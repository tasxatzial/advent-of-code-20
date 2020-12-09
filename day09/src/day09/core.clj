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
(def parsed-input (apply vector (map str->BigInt (parse (slurp input-file)))))

(def preamble-size 25)

; ---------------------------------------
; problem 1

(defn sorted-insert
  "Adds num to a sorted col so that they new col is also sorted."
  ([sorted-col num]
   (sorted-insert sorted-col num '()))
  ([sorted-col num result]
   (if (empty? sorted-col)
     (concat result [num])
     (if (> (first sorted-col) num)
       (concat result [num] sorted-col)
       (recur (rest sorted-col) num (concat result [(first sorted-col)]))))))

(defn -main
  []
  (println parsed-input))
