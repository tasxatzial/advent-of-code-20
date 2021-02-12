(ns day20.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n\n"
  [s]
  (clojure.string/split s #"\n\n"))

(def parsed-input (parse (slurp input-file)))

(defn create-tile
  "Returns a [tile-number tile-image] that represents a tile.
  tile-image is a vector of strings (one string per input line)."
  [input]
  (let [tile-lines (clojure.string/split input #"\n")
        tile-text (first tile-lines)
        tile (vec (rest tile-lines))
        parsed-tile-text (clojure.string/split tile-text #"[ |:]")]
    [(Integer. ^String (second parsed-tile-text)) tile]))

(def first-tile (create-tile (first parsed-input)))
(def tile-dim (count (second first-tile)))
(def tile-const (dec tile-dim))

(defn get-top-side
  "Returns the top side of a tile-image."
  [tile-image]
  (map identity (first tile-image)))

(defn get-bottom-side
  "Returns the bottom side of a tile-image."
  [tile-image]
  (map identity (last tile-image)))

(defn get-left-side
  "Returns the left side of a tile-image."
  [tile-image]
  (map first tile-image))

(defn get-right-side
  "Returns the right side of a tile-image."
  [tile-image]
  (map last tile-image))

(defn get-tile-sides
  "Returns a map that contains all sides of a tile-image."
  [time-image]
  (let [top-side (get-top-side time-image)
        bottom-side (get-bottom-side time-image)
        left-side (get-left-side time-image)
        right-side (get-right-side time-image)]
    (assoc {} :top top-side :bottom bottom-side :left left-side :right right-side)))

(defn transform
  [tile-image transform-func]
  "Rotates a tile-image 90 degrees clockwise."
  (loop [result []
         result-row []
         x 0
         y 0]
    (if (= y tile-dim)
      result
      (if (= x tile-dim)
        (recur (conj result (apply str result-row)) [] 0 (inc y))
        (let [new-xy (transform-func tile-image x y)]
          (recur result (conj result-row new-xy) (inc x) y))))))

(defn transform-4123
  "Rotates a tile-image 90 degrees clockwise."
  [tile-image x y]
  (get (get tile-image x) (- tile-const y)))

(defn transform-3412
  "Rotates a tile-image 180 degrees."
  [tile-image x y]
  (get (get tile-image (- tile-const y)) (- tile-const x)))

(defn -main
  []
  (println first-tile)
  (println (transform (second first-tile) transform-3412)))
