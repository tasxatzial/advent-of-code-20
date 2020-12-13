(ns day13.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn str->int
  "Converts a string to integer."
  [^String s]
  (Integer. s))

(defn parse
  "Splits the input string by \n."
  [s]
  (clojure.string/split s #"\n"))

(def parsed-input (parse (slurp input-file)))
(def timestamp (str->int (first parsed-input)))
(def bus-ids (map str->int (filter #(not= "x" %) (clojure.string/split (second parsed-input) #","))))

(defn -main
  []
  (println bus-ids))
