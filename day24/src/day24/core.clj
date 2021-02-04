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
  {"w" [-1 0] "e" [1 0] "se" [1 -1] "sw" [-1 -1] "nw" [-1 1] "ne" [1 1]})

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
  "Returns a set of tile coordinates for the black flipped tiles."
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

(defn -main
  []
  (println (get-flipped-tiles)))
