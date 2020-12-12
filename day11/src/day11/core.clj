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
  [[row-index col-index]]
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
  [[row-index col-index]]
  [row-index (dec col-index)])

(defn right-pos
  "Returns the position to the right of [row-index, col-index]."
  [[row-index col-index]]
  [row-index (inc col-index)])

(defn bottom-pos
  "Returns the position to the bottom of [row-index, col-index]."
  [[row-index col-index]]
  [(inc row-index) col-index])

(defn top-pos
  "Returns the position to the top of [row-index, col-index]."
  [[row-index col-index]]
  [(dec row-index) col-index])

(defn top-left-pos
  "Returns the position to the top-left of [row-index, col-index]."
  [[row-index col-index]]
  [(dec row-index) (dec col-index)])

(defn top-right-pos
  "Returns the position to the top-left of [row-index, col-index]."
  [[row-index col-index]]
  [(dec row-index) (inc col-index)])

(defn bottom-left-pos
  "Returns the position to the bottom-left of [row-index, col-index]."
  [[row-index col-index]]
  [(inc row-index) (dec col-index)])

(defn bottom-right-pos
  "Returns the position to the bottom-right of [row-index, col-index]."
  [[row-index col-index]]
  [(inc row-index) (inc col-index)])

(defn run-simulation
  "Runs a simulation and returns the seats when seat state stabilizes."
  ([seat-vector advance-seat-rules] (run-simulation 0 seat-vector advance-seat-rules))
  ([iteration old-seats advance-seat-rules]
   (let [new-seats (reduce (fn [result seat]
                             (conj result (advance-seat-rules old-seats seat)))
                           []
                           old-seats)]
     (if (not (seat-state-changed? old-seats new-seats))
       new-seats
       (recur (inc iteration) new-seats advance-seat-rules)))))

; ---------------------------------------
; problem 1

(defn find-adjacents-pos
  "Returns a map of the adjacent coordinates of [row-index col-index]."
  [pos]
  {:left (left-pos pos)
   :right (right-pos pos)
   :top (top-pos pos)
   :bottom (bottom-pos pos)
   :top-left (top-left-pos pos)
   :top-right (top-right-pos pos)
   :bottom-left (bottom-left-pos pos)
   :bottom-right (bottom-right-pos pos)})

(defn process-adjacents-pos
  "Accepts a map of adjacents as returned by find-adjacents-pos() and creates
  a list of seat keys that correspond to these adjacents. Invalid positions from
  the map are not taken into account."
  [adjacents]
  (reduce (fn [result [pos-key [row-index col-index]]]
            (if (and (>= row-index 0) (<= row-index max-row) (>= col-index 0) (<= col-index max-col))
              (conj result (compute-seat-index [row-index col-index]))
              result))
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
  "Returns the seat in its new state (problem 1)"
  [current-seats seat]
  (let [current-state (first seat)
        adjacents (second seat)
        adjacent-seats (map #(get current-seats %) adjacents)
        occupied-seats (filter #(= '\# (first %)) adjacent-seats)]
    (cond
      (and (= current-state '\L) (= (count occupied-seats) 0)) ['\# adjacents]
      (and (= current-state '\#) (>= (count occupied-seats) 4)) ['\L adjacents]
      :else seat)))

; ---------------------------------------
; problem 2

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

(def seat-vector2 (process-input2))

; restrictions for the row-index, col-index for each direction
(def directional-restrictions
  {:left #(>= (second %) 0)
   :right #(<= (second %) max-col)
   :bottom #(<= (first %) max-row)
   :top #(>= (first %) 0)
   :top-left #(and (>= (first %) 0) (>= (second %) 0))
   :top-right #(and (>= (first %) 0) (<= (second %) max-col))
   :bottom-right #(and (<= (first %) max-row) (<= (second %) max-col))
   :bottom-left #(and (<= (first %) max-row) (>= (second %) 0))})

; mapping of directions to the function that returns the next [row-index, col-index]
; at this direction
(def directional-funcs
  {:left left-pos
   :right right-pos
   :bottom bottom-pos
   :top top-pos
   :top-left top-left-pos
   :top-right top-right-pos
   :bottom-left bottom-left-pos
   :bottom-right bottom-right-pos})

(defn get-state
  "Returns the state of seat pos. Seat is a 2 element vector [row-index, col-index]."
  [current-seats pos]
  (first (get current-seats (compute-seat-index pos))))

(defn first-visible-pos
  "Returns the first visible seat (problem 2)"
  [key current-seats pos]
  (if (= '\. (get-state current-seats pos))
    nil
    (let [restrictions (key directional-restrictions)
          directional-func (key directional-funcs)
          directional-seats (iterate directional-func (directional-func pos))
          until-visible (take-while #(and (restrictions %) (= (get-state current-seats %) '\.)) directional-seats)
          found-pos (if (empty? until-visible)
                      (directional-func pos)
                      (directional-func (last until-visible)))]
      (if (not (restrictions found-pos))
        nil
        found-pos))))

(def directional-partial-funcs
  [(partial first-visible-pos :left)
   (partial first-visible-pos :right)
   (partial first-visible-pos :bottom)
   (partial first-visible-pos :top)
   (partial first-visible-pos :top-left)
   (partial first-visible-pos :top-right)
   (partial first-visible-pos :bottom-left)
   (partial first-visible-pos :bottom-right)])

(defn advance-seat-rules2
  "Returns the seat in its next state (problem 2)."
  [current-seats [seat-state row-index col-index :as seat]]
  (let [visible-positions (filter identity (map #(% current-seats [row-index col-index]) directional-partial-funcs))
        seat-states (map #(get-state current-seats %) visible-positions)
        occupied-seats (filter #(= '\# %) seat-states)]
    (cond
      (and (= seat-state '\L) (= (count occupied-seats) 0)) ['\# row-index col-index]
      (and (= seat-state '\#) (>= (count occupied-seats) 5)) ['\L row-index col-index]
      :else seat)))

; ---------------------------------------
; results

(def day11-1 (count-occupied (run-simulation seat-vector1 advance-seat-rules1)))

(def day11-2 (count-occupied (run-simulation seat-vector2 advance-seat-rules2)))

(defn -main
  []
  (println day11-1)                                         ;2299
  (println day11-2))                                        ;2047
