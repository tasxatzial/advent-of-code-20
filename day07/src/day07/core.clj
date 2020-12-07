(ns day07.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n."
  [input]
  (clojure.string/split input #"\n"))

(def parsed-input (parse (slurp input-file)))

(defn -main
  []
  (println parsed-input))
