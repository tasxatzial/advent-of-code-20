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

(defn -main
  []
  (println (num-to-seq input)))
