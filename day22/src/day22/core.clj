(ns day22.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n\n"
  [s]
  (clojure.string/split s #"\n\n"))

(def input (parse (slurp input-file)))

(defn gen-cards
  "Accepts a string describing either player1 data or player2 data. This string is one of
  the two items from the list returned by parse(). Returns a vector of numbers that describes
  a player's cards."
  [s]
  (let [data (rest (clojure.string/split s #"\n"))]
    (mapv #(Integer. ^String %) data)))

(def player1-cards (gen-cards (first input)))
(def player2-cards (gen-cards (second input)))

(defn -main
  []
  (println player2-cards))
