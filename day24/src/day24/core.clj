(ns day24.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n"
  [s]
  (clojure.string/split s #"\n"))

(def input (parse (slurp input-file)))

(def two-letter-directions (hash-set "se" "sw" "nw" "ne"))

(def move-instructions
  {"w" [-2 0]
   "e" [2 0]
   "se" [1 -1]
   "sw" [-1 -1]
   "nw" [-1 1]
   "ne" [1 1]})

(defn parse-directions
  "Parses a string that contains the directions to a tile into a vector."
  ([directions]
   (parse-directions directions []))
  ([tile result]
   (if (empty? tile)
     result
     (let [first-letter (str (first tile))
           two-letters (str first-letter (second tile))]
       (if (contains? two-letter-directions two-letters)
         (recur (drop 2 tile) (conj result two-letters))
         (recur (rest tile) (conj result first-letter)))))))

(defn get-tile
  "Returns the [x y] of the tile that is identified by
  the specified list of directions."
  ([directions]
   (get-tile directions [0 0]))
  ([directions position]
   (if (empty? directions)
     position
     (reduce (fn [[x y] direction]
               (let [instructions (get move-instructions direction)]
                 [(+ (first instructions) x) (+ (second instructions) y)]))
             position
             directions))))

; ---------------------------------------
; problem 1

(defn get-flipped-tiles
  "Returns a set of the tiles that have been flipped to black."
  ([]
   (let [all-directions (map parse-directions input)]
     (get-flipped-tiles all-directions #{})))
  ([all-directions flipped-tiles]
   (reduce (fn [result directions]
             (let [tile (get-tile directions)]
               (if (contains? result tile)
                 (disj result tile)
                 (conj result tile))))
           flipped-tiles
           all-directions)))

(def memoized-get-flipped-tiles (memoize get-flipped-tiles))

; ---------------------------------------
; problem 2

(defn get-adjacent-tiles
  "Returns the set of the adjacent tiles of tile [x y]."
  [[x y]]
  #{[(+ x 2) y]
    [(- x 2) y]
    [(inc x) (inc y)]
    [(inc x) (dec y)]
    [(dec x) (inc y)]
    [(dec x) (dec y)]})

(defn change-adjacent-to-black
  "Processes a set of adjacent tiles and returns a set of only those that will be
  changed to black. Tiles that are already black are skipped."
  [tiles current-blacks]
  (reduce (fn [result tile]
            (if (contains? current-blacks tile)
              result
              (let [adjacent-tiles (get-adjacent-tiles tile)
                    adjacent-blacks (filter #(contains? current-blacks %) adjacent-tiles)]
                (if (= 2 (count adjacent-blacks))
                  (conj result tile)
                  result))))
          #{}
          tiles))

(defn change-to-black
  "Processes a tile & its adjacent tiles and returns a set of only those
  that will be changed to black."
  [tile current-blacks]
  (let [adjacent-tiles (get-adjacent-tiles tile)
        adjacent-blacks (filter #(contains? current-blacks %) adjacent-tiles)
        new-adjacent-blacks (change-adjacent-to-black adjacent-tiles current-blacks)]
    (if (or (= 0 (count adjacent-blacks)) (> (count adjacent-blacks) 2))
      new-adjacent-blacks
      (conj new-adjacent-blacks tile))))

(defn flip-tiles
  "Flips the tiles max-iter times according to problem 2 rules. The set of
  black tiles from problem 1 is used as starting set."
  [tiles max-iter]
  (if (= 0 max-iter)
    tiles
    (let [new-tiles (reduce (fn [result tile]
                              (let [new-tiles (change-to-black tile tiles)]
                                (into result new-tiles)))
                            #{}
                            tiles)]
      (recur new-tiles (dec max-iter)))))

; ---------------------------------------
; results

(defn day24-1
  []
  (count (memoized-get-flipped-tiles)))

(defn day24-2
  []
  (count (flip-tiles (memoized-get-flipped-tiles) 100)))

(defn -main
  []
  (println (day24-1))
  (println (day24-2)))
