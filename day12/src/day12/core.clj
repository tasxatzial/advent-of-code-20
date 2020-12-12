(ns day12.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n."
  [s]
  (clojure.string/split s #"\n"))

(def parsed-input (parse (slurp input-file)))

(defn -main
  []
  (println parsed-input))
