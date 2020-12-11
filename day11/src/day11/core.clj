(ns day11.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n."
  [s]
  (clojure.string/split s #"\n"))

(def parsed-input (parse (slurp input-file)))
(def max-row (dec (count parsed-input)))
(def max-col (dec (count (first parsed-input))))

(defn find-adjacents-pos
  "Returns a map of the adjacent coordinates of [row-index col-index]."
  [row-index col-index]
  {:left [row-index (dec col-index)]
   :right [row-index (inc col-index)]
   :top [(dec row-index) col-index]
   :bottom [(inc row-index) col-index]
   :top-left [(dec row-index) (dec col-index)]
   :top-right [(dec row-index) (inc col-index)]
   :bottom-left [(inc row-index) (dec col-index)]
   :bottom-right [(inc row-index) (inc col-index)]})

(defn compute-seat-index
  "Returns the index of a seat at coordinates [row-index, col-index]."
  [row-index col-index]
  (+ col-index (* row-index (inc max-col))))

(defn process-adjacents-pos
  "Accepts a map of adjacents as returned by find-adjacents-pos() and creates
  a list of seat keys that correspond to these adjacents. Invalid positions from
  the map are not taken into account."
  [adjacents]
  (reduce (fn [result [pos-name adjacent-pos]]
            (let [row-index (first adjacent-pos)
                  col-index (second adjacent-pos)]
              (if (and (>= row-index 0)
                       (<= row-index max-row)
                       (>= col-index 0)
                       (<= col-index max-col))
                (conj result (compute-seat-index row-index col-index))
                result)))
          '()
          adjacents))

(defn process-input
  "Processes the input returned by parse() and creates the appropriate structure
  that represents the seats and their adjacent seats."
  ([] (process-input [] 0 0))
  ([result row-index col-index]
   (if (> row-index max-row)
     result
     (if (> col-index max-col)
       (recur result (inc row-index) 0)
       (let [adjacents (process-adjacents-pos (find-adjacents-pos row-index col-index))
             seat-state (get (get parsed-input row-index) col-index)
             result (conj result [seat-state adjacents])]
         (recur result row-index (inc col-index)))))))

(def seat-map (process-input))

; ---------------------------------------
; problem 1

(defn -main
  []
  (println seat-map))
