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

(defn count-occupied-seats
  "Counts the number of occupied seats."
  [seats]
  (count (filter #(= '\# (first %)) seats)))

; mapping of directions to the function that returns the next [row-index, col-index]
; at this direction
(def directional-funcs
  {:left (fn [[row-index col-index]]
           [row-index (dec col-index)])
   :right (fn [[row-index col-index]]
            [row-index (inc col-index)])
   :bottom (fn [[row-index col-index]]
             [(inc row-index) col-index])
   :top (fn [[row-index col-index]]
          [(dec row-index) col-index])
   :top-left (fn [[row-index col-index]]
               [(dec row-index) (dec col-index)])
   :top-right (fn [[row-index col-index]]
                [(dec row-index) (inc col-index)])
   :bottom-left (fn [[row-index col-index]]
                  [(inc row-index) (dec col-index)])
   :bottom-right (fn [[row-index col-index]]
                   [(inc row-index) (inc col-index)])})

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

(defn keys-from-adjacents-pos
  "Accepts a list of adjacents positions [i, j] and creates the corresponding list of seat keys.
  Invalid positions are not taken into account."
  [adjacents]
  (reduce (fn [result seat]
            (if (and ((:left directional-restrictions) seat)
                     ((:right directional-restrictions) seat)
                     ((:top directional-restrictions) seat)
                     ((:bottom directional-restrictions) seat))
              (conj result (compute-seat-index seat))
              result))
          '()
          adjacents))

(defn create-seats1
  "Processes the input returned by parse() and creates the appropriate structure
  that represents the seats and their adjacent seats (problem 1)"
  ([] (create-seats1 [] 0 0))
  ([result row-index col-index]
   (if (> row-index max-row)
     result
     (if (> col-index max-col)
       (recur result (inc row-index) 0)
       (let [adjacents-pos (map #(% [row-index col-index]) (map #(second %) directional-funcs))
             adjacents (keys-from-adjacents-pos adjacents-pos)
             seat-state (get (get parsed-input row-index) col-index)
             result (conj result [seat-state adjacents])]
         (recur result row-index (inc col-index)))))))

(defn seats1
  []
  (create-seats1))

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

(defn create-seats2
  "Processes the input returned by parse() and creates the appropriate structure
  that represents the seats and their adjacent seats (problem 2)"
  ([] (create-seats2 [] 0 0))
  ([result row-index col-index]
   (if (> row-index max-row)
     result
     (if (> col-index max-col)
       (recur result (inc row-index) 0)
       (let [seat-state (get (get parsed-input row-index) col-index)
             result (conj result [seat-state row-index col-index])]
         (recur result row-index (inc col-index)))))))

(defn seats2
  []
  (create-seats2))

(defn get-seat-state
  "Returns the state of seat pos. Seat is a 2 element vector [row-index, col-index]."
  [current-seats pos]
  (first (get current-seats (compute-seat-index pos))))

(defn first-visible-pos
  "Returns the first visible position (problem 2)"
  [key current-seats pos]
  (if (= '\. (get-seat-state current-seats pos))
    nil
    (let [restrictions (key directional-restrictions)
          directional-func (key directional-funcs)
          directional-positions (iterate directional-func (directional-func pos))
          visible-positions (take-while #(and (restrictions %) (= (get-seat-state current-seats %) '\.)) directional-positions)
          found-pos (if (empty? visible-positions)
                      (directional-func pos)
                      (directional-func (last visible-positions)))]
      (if (not (restrictions found-pos))
        nil
        found-pos))))

(defn advance-seat-rules2
  "Returns the seat in its next state (problem 2)."
  [current-seats [seat-state row-index col-index :as seat]]
  (let [directional-positions (map #(partial first-visible-pos (first %)) directional-funcs)
        visible-seats (filter identity (map #(% current-seats [row-index col-index]) directional-positions))
        visible-states (map #(get-seat-state current-seats %) visible-seats)
        occupied-seat-states (filter #(= '\# %) visible-states)]
    (cond
      (and (= seat-state '\L) (= (count occupied-seat-states) 0)) ['\# row-index col-index]
      (and (= seat-state '\#) (>= (count occupied-seat-states) 5)) ['\L row-index col-index]
      :else seat)))

; ---------------------------------------
; results

(defn day11-1
  []
  (count-occupied-seats (run-simulation (seats1) advance-seat-rules1)))

;slow, needs optimization
(defn day11-2
  []
  (count-occupied-seats (run-simulation (seats2) advance-seat-rules2)))

(defn -main
  []
  (println (day11-1))                                         ;2299
  (println (day11-2)))                                        ;2047
