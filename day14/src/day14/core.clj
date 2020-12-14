(ns day14.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n."
  [s]
  (clojure.string/split s #"\n"))

(def parsed-input (parse (slurp input-file)))

(defn parse-mask
  "Parses a mask line from the input file and returns a mask string."
  [mask-line]
  (second (clojure.string/split mask-line #" = ")))

(defn parse-mem
  "Parses a mem line from the input file and returns a two element
  vector, first one is the mem address (int), second one is the value in binary
  format (string)"
  [mem-line]
  (let [split-line (filter #(boolean (seq %)) (clojure.string/split mem-line #"[^\d]+"))
        integer-line (map #(Integer. ^String %) split-line)
        binary-value (Integer/toBinaryString (second integer-line))
        padding (apply str (take (- 36 (count binary-value)) (repeat "0")))]
    [(first integer-line) (str padding binary-value)]))

(defn parse-line
  "Parses a line from the input file and returns either a mask string
  or a two element vector that corresponds to a mem instruction."
  [line]
  (if (= '\a (second line))
    (parse-mask line)
    (parse-mem line)))

; ---------------------------------------
; results

(defn -main
  []
  (println parsed-input))
