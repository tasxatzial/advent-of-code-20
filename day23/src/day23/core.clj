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

(defn -main
  []
  (println (num-to-seq input)))
