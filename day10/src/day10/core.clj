(ns day10.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn str->Int
  "Converts a string to int."
  [^String s]
  (Integer. s))

(defn parse
  "Splits the input string by \n."
  [s]
  (map #(str->Int %) (clojure.string/split s #"\n")))

(def parsed-input (parse (slurp input-file)))

(defn -main
  []
  (println parsed-input))
