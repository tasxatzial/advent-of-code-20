(ns day23.core
  (:gen-class))

; ---------------------------------------
; common

(def input 418976235)

(defn num-to-seq
  "Returns a list of the digits of the integer num."
  [num]
  (->> num
       str
       (map (comp read-string str))))

(def initial-cups (num-to-seq input))

; ---------------------------------------
; problem 1

(def max-cup_1 9)

(defn find-dest-cup_1
  "Finds the final destination cup using dest as the initial destination."
  [dest picked-cups max-cup]
  (if (= dest 0)
    (recur max-cup picked-cups max-cup)
    (if (or (= dest (first picked-cups))
            (= dest (second picked-cups))
            (= dest (last picked-cups)))
      (recur (dec dest) picked-cups max-cup)
      dest)))

(defn get-index_1
  "Returns the index of num in coll or -1 if num is not found."
  ([num coll]
   (get-index_1 num coll 0))
  ([num col index]
   (if (empty? col)
     -1
     (if (= (first col) num)
       index
       (recur num (rest col) (inc index))))))

(defn play-game_1
  "Makes total-moves in the crab game."
  [cups total-moves max-cup]
  (if (= 0 total-moves)
    cups
    (let [current-cup (first cups)
          picked-cups (take 3 (rest cups))
          rest-cups (drop 4 cups)
          dest-cup (find-dest-cup_1 (dec current-cup) picked-cups max-cup)
          dest-index (inc (get-index_1 dest-cup rest-cups))
          new-seq (concat (take dest-index rest-cups) picked-cups (drop dest-index rest-cups) (list current-cup))]
      (recur new-seq (dec total-moves) max-cup))))

(defn collect-cups_1
  "Collects in clockwise fashion all cup labels starting after the cup labeled 1.
  Returns the labels concatenated in a string."
  [cups]
  (let [index-one (get-index_1 1 cups)
        new-cups (concat (drop index-one cups) (take index-one cups))]
    (apply str (rest new-cups))))

; ---------------------------------------
; problem 2

; ---------------------------------------
; results

(defn day23-1
  []
  (collect-cups_1 (play-game_1 initial-cups 100 max-cup_1)))

(defn -main
  []
  (println (day23-1)))
