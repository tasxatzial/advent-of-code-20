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

(defn find-dest-cup
  "Finds the final destination cup using dest as the initial destination."
  [dest picked-cups max-cup]
  (if (= dest 0)
    (recur max-cup picked-cups max-cup)
    (if (or (= dest (first picked-cups))
            (= dest (second picked-cups))
            (= dest (last picked-cups)))
      (recur (dec dest) picked-cups max-cup)
      dest)))

(defn get-index
  "Returns the index of num in col. Returns -1 if num does not appear
  anywhere in col."
  ([num col]
   (get-index num col 0))
  ([num col index]
   (if (empty? col)
     -1
     (if (= (first col) num)
       index
       (recur num (rest col) (inc index))))))

(defn play-game
  "Plays total max-moves of the crab game."
  [cups max-moves max-cup]
  (if (= 0 max-moves)
    cups
    (let [current-cup (first cups)
          picked-cups (take 3 (rest cups))
          rest-cups (drop 4 cups)
          dest-cup (find-dest-cup (dec current-cup) picked-cups max-cup)
          dest-index (inc (get-index dest-cup rest-cups))
          new-seq (concat (take dest-index rest-cups) picked-cups (drop dest-index rest-cups) (list current-cup))]
      (recur new-seq (dec max-moves) max-cup))))

; ---------------------------------------
; problem 1

(def cups1 (num-to-seq input))
(def max-cup1 9)

(defn collect-cups1
  "Collects in clockwise fashion all cup labels starting after the cup labeled 1.
  Returns the labels concatenated in a string."
  [cups]
  (let [index-one (get-index 1 cups)
        new-cups (concat (drop index-one cups) (take index-one cups))]
    (apply str (rest new-cups))))

; ---------------------------------------
; results

(defn day23-1
  []
  (collect-cups1 (play-game cups1 100 max-cup1)))

(defn -main
  []
  (println (day23-1)))
