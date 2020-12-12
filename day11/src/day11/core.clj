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

(defn compute-seat-index
  "Returns the index of a seat at coordinates [row-index, col-index]."
  [row-index col-index]
  (+ col-index (* row-index (inc max-col))))

(defn seat-state-changed?
  "Returns true if at least one seat has changed state, nil otherwise."
  [old-seats new-seats]
  (let [old-states (map #(first %) old-seats)
        new-states (map #(first %) new-seats)]
    (some false? (map #(= %1 %2) old-states new-states))))

(defn count-occupied
  "Counts the number of occupied seats."
  [seats]
  (count (filter #(= '\# (first %)) seats)))

(defn left-pos
  "Returns the position to the left of [row-index, col-index]."
  [row-index col-index]
  [row-index (dec col-index)])

(defn right-pos
  "Returns the position to the right of [row-index, col-index]."
  [row-index col-index]
  [row-index (inc col-index)])

(defn bottom-pos
  "Returns the position to the bottom of [row-index, col-index]."
  [row-index col-index]
  [(inc row-index) col-index])

(defn top-pos
  "Returns the position to the top of [row-index, col-index]."
  [row-index col-index]
  [(dec row-index) col-index])

(defn top-left-pos
  "Returns the position to the top-left of [row-index, col-index]."
  [row-index col-index]
  [(dec row-index) (dec col-index)])

(defn top-right-pos
  "Returns the position to the top-left of [row-index, col-index]."
  [row-index col-index]
  [(dec row-index) (inc col-index)])

(defn bottom-left-pos
  "Returns the position to the bottom-left of [row-index, col-index]."
  [row-index col-index]
  [(inc row-index) (dec col-index)])

(defn bottom-right-pos
  "Returns the position to the bottom-right of [row-index, col-index]."
  [row-index col-index]
  [(inc row-index) (inc col-index)])

; ---------------------------------------
; problem 1

(defn find-adjacents-pos
  "Returns a map of the adjacent coordinates of [row-index col-index]."
  [[row-index col-index]]
  {:left (left-pos row-index col-index)
   :right (right-pos row-index col-index)
   :top (top-pos row-index col-index)
   :bottom (bottom-pos row-index col-index)
   :top-left (top-left-pos row-index col-index)
   :top-right (top-right-pos row-index col-index)
   :bottom-left (bottom-left-pos row-index col-index)
   :bottom-right (bottom-right-pos row-index col-index)})

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

(defn process-input1
  "Processes the input returned by parse() and creates the appropriate structure
  that represents the seats and their adjacent seats (problem 1)"
  ([] (process-input1 [] 0 0))
  ([result row-index col-index]
   (if (> row-index max-row)
     result
     (if (> col-index max-col)
       (recur result (inc row-index) 0)
       (let [adjacents (process-adjacents-pos (find-adjacents-pos [row-index col-index]))
             seat-state (get (get parsed-input row-index) col-index)
             result (conj result [seat-state adjacents])]
         (recur result row-index (inc col-index)))))))

(def seat-vector1 (process-input1))

(defn advance-seat-rules1
  "Returns a seat with the new state."
  [current-seats seat]
  (let [current-state (first seat)
        adjacents (second seat)
        adjacent-seats (map #(get current-seats %) adjacents)
        occupied-seats (filter #(= '\# (first %)) adjacent-seats)]
    (cond
      (and (= current-state '\L) (= (count occupied-seats) 0)) ['\# adjacents]
      (and (= current-state '\#) (>= (count occupied-seats) 4)) ['\L adjacents]
      :else seat)))

(defn run-simulation1
  "Runs a simulation and returns the seats when seat state stabilizes."
  ([] (run-simulation1 0 seat-vector1))
  ([iteration old-seats]
   (let [new-seats (reduce (fn [result seat]
                              (conj result (advance-seat-rules1 old-seats seat)))
                            []
                            old-seats)]
     (if (not (seat-state-changed? old-seats new-seats))
       new-seats
       (recur (inc iteration) new-seats)))))

; ---------------------------------------
; problem 2

(defn all-left-pos
  "Returns all positions to the left of [row-index, col-index]."
  [row-index col-index]
  (iterate left-pos [row-index col-index]))

(defn all-right-pos
  "Returns all positions to the right of [row-index, col-index]."
  [row-index col-index]
  (iterate right-pos [row-index col-index]))

(defn all-bottom-pos
  "Returns all positions to the bottom of [row-index, col-index]."
  [row-index col-index]
  (iterate bottom-pos [row-index col-index]))

(defn all-top-pos
  "Returns all positions to the top of [row-index, col-index]."
  [row-index col-index]
  (iterate right-pos [row-index col-index]))

(defn all-top-left-pos
  "Returns all positions to the top-left of [row-index, col-index]."
  [row-index col-index]
  (iterate top-left-pos [row-index col-index]))

(defn all-top-right-pos
  "Returns all positions to the top-left of [row-index, col-index]."
  [row-index col-index]
  (iterate top-right-pos [row-index col-index]))

(defn all-bottom-left-pos
  "Returns all positions to the bottom-left of [row-index, col-index]."
  [row-index col-index]
  (iterate bottom-left-pos [row-index col-index]))

(defn all-bottom-right-pos
  "Returns all positions to the bottom-right of [row-index, col-index]."
  [row-index col-index]
  (iterate bottom-right-pos [row-index col-index]))

(defn process-input2
  "Processes the input returned by parse() and creates the appropriate structure
  that represents the seats and their adjacent seats (problem 2)"
  ([] (process-input2 [] 0 0))
  ([result row-index col-index]
   (if (> row-index max-row)
     result
     (if (> col-index max-col)
       (recur result (inc row-index) 0)
       (let [seat-state (get (get parsed-input row-index) col-index)
             result (conj result [seat-state row-index col-index])]
         (recur result row-index (inc col-index)))))))

; ---------------------------------------
; results

(def day11-1 (count-occupied (run-simulation1)))

(defn -main
  []
  (println day11-1))
