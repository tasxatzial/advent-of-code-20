(ns day06.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n\n and then splits again by \n.
  Returns a list of vectors, each element of the vector represents
  the answers of one user of the group."
  [input]
  (map #(clojure.string/split % #"\n") (clojure.string/split input #"\n\n")))

(defn -main
  []
  (println (parse (slurp input-file))))
