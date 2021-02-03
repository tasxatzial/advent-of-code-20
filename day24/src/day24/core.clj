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

(defn -main
  []
  (println input))
