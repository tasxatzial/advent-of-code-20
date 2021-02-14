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
(def side-keys [:top :left :right :bottom])
(def default-matches {:left {} :bottom {} :right {} :top {}})

(defn combine-matches
  "Combines tile-matches (returned by gen-matches()) with all-matches. For example if
  tile-matches = {:2687 {:3514 {:left {:2987 :1243}, :top {:2987 :1243} :bottom {} :right {}}}},
  all-matches = {:2687 {:3514 {:top {:3578 :1243}, :bottom {}, :left {}, :right {}}}}
  then the result would be
  {:2687 {:3514 {:top {:3578 :1243, :2987 :1243}, :left {:2987 :1243}, :right {}, :bottom {}}}}"
  [tile-matches all-matches]
  (reduce (fn [result [tile-num transform-matches]]
            (let [transform-key (first (first transform-matches))
                  matches (second (first transform-matches))]
              (if-let [present-num (tile-num all-matches)]
                (if-let [present-transform (transform-key present-num)]
                  (let [new-matches (into {} (map #(hash-map % (into (% present-transform) (% matches))) side-keys))]
                    (assoc result tile-num {transform-key new-matches}))
                  (assoc result tile-num (into present-num {transform-key matches})))
                (assoc result tile-num {transform-key matches}))))
          all-matches
          tile-matches))

(defn gen-matches
  "Returns a struct that represents which tile sides match. For example if
  tile1-num = :2698
  [tile1-transform tile1-sides] = (:3214 {:top #####.#..., :bottom ##.###.###, :left ###.#..###, :right ....#.#..#})
  tile2-num = :2987
  tile2-transform tile2-sides] = (:3214 {:top ##.##....., :bottom ##.##....., :left ###.#..###, :right ###.#..###})
  then the result would be
  {:2687 {:3214 {:left {:2987 :3214}, :bottom {}, :right {}, :top {}}},
  :2987 {:3214 {:left {}, :bottom {}, :right {:2687 :3214}, :top {}}}}
  meaning that the left side of tile 2687 (after transform-3214) matches with the right side
  of tile 2987 (after transform-1243)."
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
         r5 (into default-matches (map #(hash-map (first %) {tile2-num tile2-transform}) r0))
         r6 (into default-matches (map #(hash-map (second %) {tile1-num tile1-transform}) r0))]
     {tile1-num {tile1-transform r5} tile2-num {tile2-transform r6}})))


(def first-tile-sides (first (memoized-create-all-tile-sides)))
(def second-tiles-sides (second (memoized-create-all-tile-sides)))

(defn -main
  []
  (println first-tile-sides))
