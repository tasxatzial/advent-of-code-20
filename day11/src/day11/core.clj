(ns day11.core
  (:gen-class))

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n."
  [s]
  (clojure.string/split s #"\n"))

(defn -main
  []
  (println (parse (slurp input-file))))
