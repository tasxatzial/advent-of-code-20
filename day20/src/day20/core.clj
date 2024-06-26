(ns day20.core
  (:require clojure.set)
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
(def tile-xy-size (count (second (first tiles))))
(def tile-count (count tiles))                              ;;we know it is a perfect square
(def tiles-per-row (Math/round (Math/sqrt tile-count)))

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

(defn get-sides
  "Returns a map that contains all sides of a tile-image."
  [time-image]
  (let [top-side (get-top-side time-image)
        bottom-side (get-bottom-side time-image)
        left-side (get-left-side time-image)
        right-side (get-right-side time-image)]
    (assoc {} :top top-side :bottom bottom-side :left left-side :right right-side)))

(defn transform-image
  "Applies transform-func to a two dimensional square image."
  [image transform-func]
  (let [image-xy-size (count image)
        image-const (dec image-xy-size)]
    (loop [result []
           result-row []
           x 0
           y 0]
      (if (= y image-xy-size)
        result
        (if (= x image-xy-size)
          (recur (conj result (apply str result-row)) [] 0 (inc y))
          (let [new-xy (transform-func image image-const x y)]
            (recur result (conj result-row new-xy) (inc x) y)))))))

(defn transform-1234
  "Returns the new value of [x, y] when transform is: identity."
  [image _ x y]
  (get-in image [y x]))

(defn transform-4123
  "Returns the new value of [x, y] when transform is: rotate 90 degrees clockwise.
  image-const should be (image-row-count - 1)"
  [image image-const x y]
  (get-in image [x (- image-const y)]))

(defn transform-3412
  "Returns the new value of [x, y] when transform is: rotate 180 degrees.
  image-const should be (image-row-count - 1)"
  [image image-const x y]
  (get-in image [(- image-const y) (- image-const x)]))

(defn transform-2341
  "Returns the new value of [x, y] when transform is: rotate 270 degrees clockwise.
  image-const should be (image-row-count - 1)"
  [image image-const x y]
  (get-in image [(- image-const x) y]))

(defn transform-2143
  "Returns the new value of [x, y] when transform is: flip along the vertical axis.
  image-const should be (image-row-count - 1)"
  [image image-const x y]
  (get-in image [y (- image-const x)]))

(defn transform-4321
  "Returns the new value of [x, y] when transform is: flip along the horizontal axis.
  image-const should be (image-row-count - 1)"
  [image image-const x y]
  (get-in image [(- image-const y) x]))

(defn transform-1432
  "Returns the new value of [x, y] when transform is: flip along the top-left diagonal.
  image-const should be (image-row-count - 1)"
  [image _ x y]
  (get-in image [x y]))

(defn transform-3214
  "Returns the new value of [x, y] when transform is: flip along the top-right diagonal.
  image-const should be (image-row-count - 1)"
  [image image-const x y]
  (get-in image [(- image-const x) (- image-const y)]))

(def func-to-key
  {transform-1234 :1234
   transform-4123 :4123
   transform-3412 :3412
   transform-2341 :2341
   transform-2143 :2143
   transform-4321 :4321
   transform-1432 :1432
   transform-3214 :3214})

(def key-to-func
  {:1234 transform-1234
   :4123 transform-4123
   :3412 transform-3412
   :2341 transform-2341
   :2143 transform-2143
   :4321 transform-4321
   :1432 transform-1432
   :3214 transform-3214})

(defn create-transformed-tile-sides
  "If called with one argument, it returns a struct that represents the sides of the
  tile-image for each one of the 8 similarity transforms. First item of the struct is the
  tile number keyword. Second item is a list of
  (transform-keyword {:top top-side :bottom bottom-side :left left-side :right right-side}).
  If called with zero arguments, it maps itself to the tiles list."
  ([[tile-key tile-image]]
   (let [tile-sides (reduce (fn [result [func transform-key]]
                              (let [transformed-image (transform-image tile-image func)
                                    tile-sides (get-sides transformed-image)]
                                (conj result (list transform-key tile-sides))))
                            '()
                            func-to-key)]
     (list tile-key tile-sides)))
  ([]
   (map create-transformed-tile-sides tiles)))

(def default-matches {:left '() :bottom '() :right '() :top '()})

(defn update-tile-matches
  "Updates the map of matches for a tile. For example if
  old-matches = {:left ([:3278 :2314]) :top () :bottom () :right ()}
  new-matches = {:left ([:2987 :4123]) :top () :bottom () :right ()}
  then the result would be
  {:left ([:3278 :2314] [:2987 :4123]) :top () :bottom () :right ()}}}"
  [old-matches new-matches]
  (into {} (map #(hash-map % (into (% old-matches) (% new-matches))) [:left :right :top :bottom])))

(defn combine-matches
  "Combines tile-matches (returned by gen-transform-matches()) with all-matches. For example if
  tile-matches = {:2687 {:3214 {:left (), :bottom ([:1013 :3214]), :right (), :top ([:1013 :3214])}},
                  :1013 {:3214 {:left (), :bottom ([:2687 :3214]), :right (), :top ([:2687 :3214])}}},
  all-matches = {:2687 {:3514 {:top ([:1054 :4123]), :bottom (), :left (), :right ()}}}
  then the result would be
  {:2687 {:3214 {:left (), :bottom ([:1013 :3214]), :right (), :top ([:1013 :3214] [:1054 :4123])}},
   :1013 {:3214 {:left (), :bottom ([:2687 :3214]), :right (), :top ([:2687 :3214])}}}"
  [tile-matches all-matches]
  (reduce (fn [result [tile-key transform-matches]]
            (let [transform-key (first (first transform-matches))
                  matches (second (first transform-matches))]
              (if-let [old-transform-matches (tile-key all-matches)]
                (if-let [old-matches (transform-key old-transform-matches)]
                  (let [new-matches (update-tile-matches old-matches matches)]
                    (assoc result tile-key (into old-transform-matches {transform-key new-matches})))
                  (assoc result tile-key (into old-transform-matches {transform-key matches})))
                (assoc result tile-key {transform-key matches}))))
          all-matches
          tile-matches))

(defn gen-transform-matches
  "Returns a struct that represents which tile sides match. For example if
  tile1-num = :2698
  [tile1-transform tile1-sides] = (:3214 {:top #####.#..., :bottom ##.###.###, :left ###.#..###, :right ....#.#..#})
  tile2-num = :2987
  tile2-transform tile2-sides] = (:2134 {:top ##.##....., :bottom ##.##....., :left ###.#..###, :right ###.#..###})
  then the result would be
  {:2687 {:3214 {:left ([:2987 :2134]), :bottom (), :right ()), :top ()}},
  :2987 {:2134 {:left {}, :bottom {}, :right ([:2687 :3214]), :top ()}}}
  meaning that the left side of tile 2687 (after transform-3214) matches with the right side of tile 2987
  (after transform-2134)"
  [tile1-key [tile1-transform-key tile1-sides] tile2-key [tile2-transform-key tile2-sides]]
  (let [r1 (when (= (:top tile1-sides) (:bottom tile2-sides))
             [:top :bottom])
        r2 (when (= (:left tile1-sides) (:right tile2-sides))
             [:left :right])
        r3 (when (= (:bottom tile1-sides) (:top tile2-sides))
             [:bottom :top])
        r4 (when (= (:right tile1-sides) (:left tile2-sides))
             [:right :left])
        r0 (filter seq (conj '() r1 r2 r3 r4))
        r5 (into default-matches (map #(hash-map (first %) (list [tile2-key tile2-transform-key])) r0))
        r6 (into default-matches (map #(hash-map (second %) (list [tile1-key tile1-transform-key])) r0))]
    {tile1-key {tile1-transform-key r5} tile2-key {tile2-transform-key r6}}))

(defn gen-matches
  "Generates a struct that represents which tiles match. Each tile (all its transforms) is matched
  with the rest of the tiles (all their transforms). For example the item representing
  the matches for tile 2687 is
  :2687 {:3214 {:left ([:2657 :4321]), :right ([:2221 :2143]), :top ([:3169 :2143]), :bottom ()},
         :1432 {:left ([:2221 :4321]), :right ([:2657 :2143]), :top (), :bottom ([:3169 :4321])},
         :4321 {:left (), :right ([:3169 :3214]), :top ([:2657 :1432]), :bottom ([:2221 :3214])},
         :2143 {:left ([:3169 :1432]), :right (), :top ([:2221 :1432]), :bottom ([:2657 :3214])},
         :2341 {:left ([:2657 :1234]), :right ([:2221 :3412]), :top (), :bottom ([:3169 :3412])},
         :3412 {:left ([:3169 :4123]), :right (), :top ([:2657 :2341]), :bottom ([:2221 :4123])},
         :4123 {:left ([:2221 :1234]), :right ([:2657 :3412]), :top ([:3169 :1234]), :bottom ()},
         :1234 {:left (), :right ([:3169 :2341]), :top ([:2221 :2341]), :bottom ([:2657 :4123])}}"
  ([tiles] (gen-matches tiles {}))
  ([tiles result]
   (if (empty? tiles)
     result
     (let [[num1 transforms1] (first tiles)
           result1 (reduce (fn [result2 [num2 transforms2]]
                            (reduce (fn [result3 transform1]
                                      (reduce (fn [result4 transform2]
                                                (let [matches (gen-transform-matches num1 transform1 num2 transform2)]
                                                  (combine-matches matches result4)))
                                              result3
                                              transforms2))
                                    result2
                                    transforms1))
                          result
                          (rest tiles))]
       (recur (rest tiles) result1)))))

(def memoized-matches (memoize (comp gen-matches create-transformed-tile-sides)))

(defn unique-matches-in-transforms?
  "Accepts as input a map of transforms - as generated by gen-matches() -
  Returns true iff each side of a tile has only one match."
  [transforms]
  (loop [tile-transforms transforms
         result true]
    (if (empty? tile-transforms)
      result
      (let [[_ side-matches] (first tile-transforms)]
        (if (and (> 2 (count (:left side-matches)))
                 (> 2 (count (:right side-matches)))
                 (> 2 (count (:top side-matches)))
                 (> 2 (count (:bottom side-matches))))
          (recur (rest tile-transforms) result)
          false)))))

;;will return true for the input tiles
(defn unique-matches?
  "Accepts as input the struct generated by gen-matches().
  Returns true iff each side of a tile has only one match."
  []
  (let [matches (memoized-matches)]
    (loop [side-matches matches
           result true]
      (if (empty? side-matches)
        result
        (let [[_ tile-transforms] (first side-matches)
              res (unique-matches-in-transforms? tile-transforms)]
          (or res (recur (rest side-matches) res)))))))

(defn find-top-left-corner-in-transforms
  "Accepts as input a map of transforms - as generated by gen-matches() -
  Returns the transform key of the tile that is the top-left
  corner of the assembled image."
  [transforms]
  (loop [tile-transforms transforms
         result nil]
    (if (empty? tile-transforms)
      result
      (let [[transform-key side-matches] (first tile-transforms)]
        (if (and (empty? (:top side-matches))
                 (empty? (:left side-matches))
                 (= 1 (count (:right side-matches)))
                 (= 1 (count (:bottom side-matches))))
          transform-key
          (recur (rest tile-transforms) result))))))

(defn find-top-left-corner
  "Accepts as input the struct generated by gen-matches().
  Returns the [tile-key transform-key] of the top-left corner of the image."
  []
  (let [side-matches (memoized-matches)]
    (loop [matches side-matches
           result nil]
      (if (empty? matches)
        result
        (let [[tile-key tile-transforms] (first matches)
              transform-key (find-top-left-corner-in-transforms tile-transforms)]
          (if transform-key
            [tile-key transform-key]
            (recur (rest matches) result)))))))

;;converts a two dimensional vector of [x y] vector at position [x y]
;;to a one dimensional vector by concatenating its rows.
;;the size of each dimension is tiles-per-row
;;the one dimensional vector has tile-count items
(def tile-xy
  (let [to-xy #(vector (mod % tiles-per-row) (quot % tiles-per-row))]
    (mapv to-xy (range tile-count))))

(defn get-tile-xy
  "Returns the corresponding two dimensional coordinates of a one dimensional
  vector. Size of each dimension is tiles-per-row.
  Accepts an index from 0 to (tile-count - 1)"
  [index]
  (get tile-xy index))

(defn get-next-tile
  "Returns the [tile-key transform-key] that corresponds to a tile that is
  to the right of the tile at position index (of the final image).
  Matches is the struct (memoized-matches).
  Image is the assembled image so far (vector of length tile-count).
  Each item in the image is a vector of [tile-key transform-key]"
  [index matches image]
  (let [[x _] (get-tile-xy index)]
    (cond
      (= (dec tiles-per-row) x) (let [[tile-key tile-transform] (get image (- index (dec tiles-per-row)))
                                  bottom-matches (:bottom (tile-transform (tile-key matches)))]
                              (first bottom-matches))
      :else (let [[tile-key tile-transform] (get image index)
                  right-matches (:right (tile-transform (tile-key matches)))]
              (first right-matches)))))

(defn assemble-tile-keys
  "Return the [tile-key transform-key] of the tiles in the final assembled image."
  ([]
   (let [side-matches (memoized-matches)
         initial-image (vec (take tile-count (repeat 0)))]
     (assemble-tile-keys side-matches initial-image)))
  ([side-matches initial-image]
   (loop [image (assoc initial-image 0 (find-top-left-corner))
          index 0]
     (if (= index tile-count)
       (vec (map vec (partition tiles-per-row image)))
       (let [next-tile (get-next-tile index side-matches image)
             new-image (assoc image (inc index) next-tile)]
         (recur new-image (inc index)))))))

(def memoized-assembled-image-numbers (memoize assemble-tile-keys))

(defn key->int
  "Convert a key like :123 to int 123"
  [key]
  (Integer. ^String (name key)))

; ---------------------------------------
; problem 2

(defn remove-image-sides
  "Removes the top/bottom/left/right side of the image and returns the new image."
  [image]
  (let [no-top (rest image)
        no-top-bottom (butlast no-top)
        no-left-top-bottom (map rest no-top-bottom)
        no-right-left-top-bottom (map butlast no-left-top-bottom)]
    (mapv clojure.string/join no-right-left-top-bottom)))

(defn create-transformed-tile-image
  "Reads the tile-image that corresponds to tile-key from the tiles struct,
  applies to it the transform that corresponds to transform-key and returns the new image."
  [[tile-key transform-key]]
  (let [transform-func (transform-key key-to-func)
        tile-image (tile-key tiles)]
    (transform-image tile-image transform-func)))

(defn join-tile-images
  "Joins the right side of tile1-image with the left side of tile2-image and returns the new image."
  [tile1-image tile2-image]
  (mapv #(str (get tile1-image %) (get tile2-image %)) (range tile-xy-size)))

(defn assemble-image
  "Returns the final assembled image as a vector of strings, one per line."
  []
  (let [assembled-image-keys (memoized-assembled-image-numbers)]
    (reduce (fn [result line-tile-keys]
              (let [line-images (mapv (comp remove-image-sides create-transformed-tile-image) line-tile-keys)
                    line-image (reduce join-tile-images line-images)]
                (into result line-image)))
            []
            assembled-image-keys)))

(def image-dim (* tiles-per-row (- tile-xy-size 2)))
(def memoized-assembled-image (memoize assemble-image))
(def monster-x-size 20)
(def monster-y-size 3)
(def monster-positions
  [[18 0] [0 1] [5 1] [6 1] [11 1] [12 1] [17 1] [18 1] [19 1] [1 2] [4 2] [7 2] [10 2] [13 2] [16 2]])

(defn get-monster
  "Returns a list of all [y_pos x_pos] coordinates of the monster hashtags when image contains
  a monster image with top-left position [x y]. Otherwise it returns an empty list."
  [image x y]
  (let [xy-positions (map (fn [[x1 y1]]
                            [(+ y y1) (+ x x1)]) monster-positions)
        ch (map #(get-in image %) xy-positions)]
    (if (every? #(= \# %) ch)
      xy-positions
      '())))

(defn get-monster-hashtag-positions
  "Scans the image and returns a set of all [y_pos x_pos] coordinates of the hashtags that
  are part of a monster."
  [image]
  (loop [x 0
         y 0
         result #{}]
    (if (= y (- image-dim (dec monster-y-size)))
      result
      (if (= x (- image-dim (dec monster-x-size)))
        (recur 0 (inc y) result)
        (let [monster (get-monster image x y)]
          (recur (inc x) y (into result monster)))))))

(defn count-monster-hashtags
  "Every image transform is applied to the assembled image until we find an image that has
  monsters. For that image it returns the total number of hashtags that are part of a monster."
  []
  (let [image (memoized-assembled-image)]
    (loop [transform-funcs func-to-key]
      (if (empty? transform-funcs)
        0
        (let [transform-func (first (first transform-funcs))
              monsters (get-monster-hashtag-positions (transform-image image transform-func))
              count-hashtags (count monsters)]
          (if (> count-hashtags 0)
            count-hashtags
            (recur (rest transform-funcs))))))))

(defn count-image-hashtags
  "Counts the total number of hashtags in the assembled image."
  []
  (let [image (memoized-assembled-image)]
    (reduce (fn [result image-row]
              (+ result (count (filter #(= \# %) image-row))))
            0
            image)))

; ---------------------------------------
; results

(defn day20-1
  []
  (let [assembled-image-keys (memoized-assembled-image-numbers)
        get-tile-num #(key->int (first (get-in assembled-image-keys %)))
        top-left-num (get-tile-num [0 0])
        bottom-left-num (get-tile-num [(dec tiles-per-row) 0])
        top-right-num (get-tile-num [0 (dec tiles-per-row)])
        bottom-right-num (get-tile-num [(dec tiles-per-row) (dec tiles-per-row)])]
    (* top-left-num bottom-left-num top-right-num bottom-right-num)))

(defn day20-2
  []
  (- (count-image-hashtags) (count-monster-hashtags)))

(defn -main
  []
  (println (day20-1))
  (println (day20-2)))
