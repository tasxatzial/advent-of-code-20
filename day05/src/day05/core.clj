(ns day05.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  [s]
  (clojure.string/split s #"\n"))

(defn -main
  []
  (println (parse (slurp input-file))))
