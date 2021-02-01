(ns day20.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n."
  [s]
  (clojure.string/split s #"\n\n"))

(defn parse-tile
  "Parses a string tile (and its legend) to the appropriate map structure."
  [input-image]
  (let [tile-lines (clojure.string/split input-image #"\n")
        tile-text (first tile-lines)
        parsed-tile-text (clojure.string/split tile-text #"[ |:]")]
    {(Integer. ^String (second parsed-tile-text)) (rest tile-lines)}))

(def tiles
  (into {} (map parse-tile (parse (slurp input-file)))))

(defn -main
  []
  (println tiles))
