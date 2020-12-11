(ns day11.core
  (:gen-class))

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n."
  [s]
  (clojure.string/split s #"\n"))

(def parsed-input (parse (slurp input-file)))
(def max-row (dec (count parsed-input)))
(def max-col (dec (count (first parsed-input))))

(defn find-adjacents-pos
  "Returns a map of the adjacent coordinates of [row-index col-index]."
  [row-index col-index]
  {:left [row-index (dec col-index)]
   :right [row-index (inc col-index)]
   :top [(dec row-index) col-index]
   :bottom [(inc row-index) col-index]
   :top-left [(dec row-index) (dec col-index)]
   :top-right [(dec row-index) (inc col-index)]
   :bottom-left [(inc row-index) (dec col-index)]
   :bottom-right [(inc row-index) (inc col-index)]})

(defn -main
  []
  (println (process-input)))
