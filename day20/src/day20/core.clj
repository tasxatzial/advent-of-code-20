(ns day20.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n."
  [s]
  (clojure.string/split s #"\n\n"))

(defn get-top-side
  "Returns the top side of a tile."
  [tile]
  (map identity (first tile)))

(defn get-bottom-side
  "Returns the bottom side of a tile."
  [tile]
  (map identity (last tile)))

(defn get-left-side
  "Returns the left side of a tile."
  [tile]
  (map first tile))

(defn get-right-side
  "Returns the right side of a tile."
  [tile]
  (map last tile))

(defn get-tile-sides
  "Returns a map that contains all sides of a tile."
  [tile]
  (let [top-side (get-top-side tile)
        bottom-side (get-bottom-side tile)
        left-side (get-left-side tile)
        right-side (get-right-side tile)]
    (assoc {} :top top-side :bottom bottom-side :left left-side :right right-side)))

(defn parse-tile
  "Parses a string tile (and its legend) to the appropriate map structure.
  Key is the tile id, value is a list of rows"
  [input-image]
  (let [tile-lines (clojure.string/split input-image #"\n")
        tile-text (first tile-lines)
        tile (rest tile-lines)
        parsed-tile-text (clojure.string/split tile-text #"[ |:]")]
    {(Integer. ^String (second parsed-tile-text)) (get-tile-sides tile)}))

(def tiles-sides
  (into {} (map parse-tile (parse (slurp input-file)))))

(defn -main
  []
  (println tiles-sides))
