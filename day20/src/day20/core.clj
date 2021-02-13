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
  tile-image is a vector of strings (one string per input line).
  tile-number is the number of the tile (keyword)."
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
  "If called with one argument, it returns a struct that represents the sides of the
  tile-image for each one of the 8 similarity transforms. First item of the struct is the
  tile number keyword. Second item is a list of
  (transform-keyword {:top top-side :bottom bottom-side :left left-side :right right-side}).
  If called with zero arguments, it maps itself to the tiles list."
  ([[tile-num tile-image]]
   (let [tile-sides (reduce (fn [result [func keyword]]
                              (let [transformed-image (transform tile-image func)
                                    tile-sides (get-tile-sides transformed-image)]
                                (conj result (list keyword tile-sides))))
                            '()
                            func-to-key)]
     (list tile-num tile-sides)))
  ([]
   (map create-tile-sides tiles)))

(def memoized-create-all-tile-sides (memoize create-tile-sides))

(defn combine-matches
  "Combines a struct returned by gen-matches() with all-matches. For example if
  matches = {:2687 {:right {:3813 :3214}, :bottom {}, :left {}, :top {}}}
  all-matches = {:2687 {:right {:3413 :3214}, :bottom {:3413 :3214}, :left {:3413 :3214}, :top {:3413 :3214}}}
  then the result would be
  {:2687 {:top {:3413 :3214}, :left {:3413 :3214}, :right {:3813 :3214, :3413 :3214}, :bottom {:3413 :3214}}}"
  [matches all-matches]
  (reduce (fn [result [tile-num tile-matches]]
            (if-let [present-matches (tile-num all-matches)]
              (let [side-keys [:top :left :right :bottom]
                    new-matches (into {} (map #(hash-map % (into (% tile-matches) (% present-matches))) side-keys))]
                (assoc result tile-num new-matches))
              (assoc result tile-num matches)))
          all-matches
          matches))

(defn gen-matches
  "Returns a struct that represents which tile sides match. For example
  {:2687 {:left {:2987 :3214}, :top {}}, :2987 {:right {:2687 :3214}, :bottom {}}}
  means tile 2687 left side matches with tile 2987 right side."
  ([tile1-num [tile1-transform tile1-sides] tile2-num [tile2-transform tile2-sides]]
   (let [r1 (when (= (:top tile1-sides) (:bottom tile2-sides))
              [:top :bottom])
         r2 (when (= (:left tile1-sides) (:right tile2-sides))
              [:left :right])
         r3 (when (= (:bottom tile1-sides) (:top tile2-sides))
              [:bottom :top])
         r4 (when (= (:right tile1-sides) (:left tile2-sides))
              [:right :left])
         r0 (filter seq (conj '() r1 r2 r3 r4))
         r5 (into {} (map #(hash-map (first %) {tile2-num tile2-transform}) r0))
         r6 (into {} (map #(hash-map (second %) {tile1-num tile1-transform}) r0))]
     (hash-map tile1-num r5 tile2-num r6))))

(def first-tile-sides (first (memoized-create-all-tile-sides)))
(def second-tiles-sides (second (memoized-create-all-tile-sides)))

(defn -main
  []
  (println first-tile-sides))
