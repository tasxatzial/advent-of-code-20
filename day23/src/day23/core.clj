(ns day23.core
  (:gen-class))

(def input 418976235)

(defn num-to-seq
  "Returns a list of the digits of the integer num."
  [num]
  (->> num
       str
       (map (comp read-string str))))

(defn find-dest-cup
  "Finds the final destination cup using dest as the initial destination."
  [dest picked-cups]
  (if (= dest 0)
    (recur 9 picked-cups)
    (if (or (= dest (first picked-cups))
            (= dest (second picked-cups))
            (= dest (last picked-cups)))
      (recur (dec dest) picked-cups)
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
  ([max-moves]
   (let [cups (num-to-seq input)]
     (play-game cups max-moves)))
  ([int-seq max-iter]
   (if (= 0 max-iter)
     int-seq
     (let [current-cup (first int-seq)
           picked-cups (take 3 (rest int-seq))
           rest-cups (drop 4 int-seq)
           dest-cup (find-dest-cup (dec current-cup) picked-cups)
           dest-index (inc (get-index dest-cup rest-cups))
           new-seq (concat (take dest-index rest-cups) picked-cups (drop dest-index rest-cups) (list current-cup))]
       (recur new-seq (dec max-iter))))))

(defn collect-cups
  "Collects in clockwise fashion all cup labels starting after the cup labeled 1.
  Returns the labels concatenated in a string."
  [cups]
  (let [index-one (get-index 1 cups)
        new-cups (concat (drop index-one cups) (take index-one cups))]
    (apply str (rest new-cups))))

(defn -main
  []
  (println (collect-cups (play-game 100))))
