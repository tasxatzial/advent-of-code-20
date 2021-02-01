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
        left-side (get-bottom-side tile)
        right-side (get-right-side tile)]
    (assoc {} :top top-side :bottom bottom-side :left left-side :right right-side)))

(defn -main
  []
  (println tiles))
