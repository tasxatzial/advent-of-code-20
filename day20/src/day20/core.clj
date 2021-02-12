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
    {(keyword (str (Integer. ^String (second parsed-tile-text)))) tile}))

(def tiles (into {} (map create-tile parsed-input)))
(def tile-dim (count (second (first tiles))))
(def tile-const (dec tile-dim))

(defn get-top-side
  "Returns the top side of a tile-image."
  [tile-image]
  (first tile-image))

(defn get-bottom-side
  "Returns the bottom side of a tile-image."
  [tile-image]
  (last tile-image))

(defn get-left-side
  "Returns the left side of a tile-image."
  [tile-image]
  (apply str (map first tile-image)))

(defn get-right-side
  "Returns the right side of a tile-image."
  [tile-image]
  (apply str (map last tile-image)))

(defn get-tile-sides
  "Returns a map that contains all sides of a tile-image."
  [time-image]
  (let [top-side (get-top-side time-image)
        bottom-side (get-bottom-side time-image)
        left-side (get-left-side time-image)
        right-side (get-right-side time-image)]
    (assoc {} :top top-side :bottom bottom-side :left left-side :right right-side)))

(defn transform
  "Applies transform-func to tile-image."
  [tile-image transform-func]
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
  "Returns the new value of [x, y] when transform is: rotate 90 degrees clockwise."
  [tile-image x y]
  (get-in tile-image [x (- tile-const y)]))

(defn transform-3412
  "Returns the new value of [x, y] when transform is: rotate 180 degrees."
  [tile-image x y]
  (get-in tile-image [(- tile-const y) (- tile-const x)]))

(defn transform-2341
  "Returns the new value of [x, y] when transform is: rotate 270 degrees clockwise."
  [tile-image x y]
  (get-in tile-image [(- tile-const x) y]))

(defn transform-2143
  "Returns the new value of [x, y] when transform is: flip along the vertical axis."
  [tile-image x y]
  (get-in tile-image [y (- tile-const x)]))

(defn transform-4321
  "Returns the new value of [x, y] when transform is: flip along the horizontal axis."
  [tile-image x y]
  (get-in tile-image [(- tile-const y) x]))

(defn transform-1432
  "Returns the new value of [x, y] when transform is: flip along the top-left diagonal."
  [tile-image x y]
  (get-in tile-image [x y]))

(defn transform-3214
  "Returns the new value of [x, y] when transform is: flip along the top-right diagonal."
  [tile-image x y]
  (get-in tile-image [(- tile-const x) (- tile-const y)]))

(def func-to-key
  {transform-4123 :4123
   transform-3412 :3412
   transform-2341 :2341
   transform-2143 :2143
   transform-4321 :4321
   transform-1432 :1432
   transform-3214 :3214})

(def key-to-func
  {:4123 transform-4123
   :3412 transform-3412
   :2341 transform-2341
   :2143 transform-2143
   :4321 transform-4321
   :1432 transform-1432
   :3214 transform-3214})

(defn create-tile-sides
  "Returns a list of two items. First item is the tile-num keyword.
  Second item is a list of (transform-keyword {:top top-side :bottom bottom-side
  :left left-side :right right-side}) for each of the transforms."
  [[tile-num tile-image]]
  (let [tile-sides (reduce (fn [result [func keyword]]
                             (let [transformed-image (transform tile-image func)
                                   tile-sides (get-tile-sides transformed-image)]
                               (conj result (list keyword tile-sides))))
                           '()
                           func-to-key)]
    (list tile-num tile-sides)))

(defn create-all-tiles-sides
  "Maps create-tile-sides() to every item in the initial tiles map."
  []
  (map create-tile-sides tiles))

(defn -main
  []
  (println (create-all-tiles-sides)))
