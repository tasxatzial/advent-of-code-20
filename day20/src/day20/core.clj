(ns day20.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n\n"
  [s]
  (clojure.string/split s #"\n\n"))

(defn create-tile
  "Returns a [tile-number tile-image] that represents a tile.
  tile-image is a list of strings (one string per line)."
  [input]
  (let [tile-lines (clojure.string/split input #"\n")
        tile-text (first tile-lines)
        tile (rest tile-lines)
        parsed-tile-text (clojure.string/split tile-text #"[ |:]")]
    [(Integer. ^String (second parsed-tile-text)) tile]))

(defn get-top-side
  "Returns the top side of a tile."
  [tile-image]
  (map identity (first tile-image)))

(defn get-bottom-side
  "Returns the bottom side of a tile."
  [tile-image]
  (map identity (last tile-image)))

(defn get-left-side
  "Returns the left side of a tile."
  [tile-image]
  (map first tile-image))

(defn get-right-side
  "Returns the right side of a tile."
  [tile-image]
  (map last tile-image))

(defn get-tile-sides
  "Returns a map that contains all sides of a tile."
  [time-image]
  (let [top-side (get-top-side time-image)
        bottom-side (get-bottom-side time-image)
        left-side (get-left-side time-image)
        right-side (get-right-side time-image)]
    (assoc {} :top top-side :bottom bottom-side :left left-side :right right-side)))

(defn -main
  []
  (println (create-tile (first (parse (slurp input-file))))))
