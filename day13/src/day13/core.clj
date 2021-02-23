(ns day13.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn str->Long
  "Converts a string to Long."
  [^String s]
  (Long. s))

(defn parse
  "Splits the input string by \n."
  [s]
  (clojure.string/split s #"\n"))

(def parsed-input (parse (slurp input-file)))

; ---------------------------------------
; problem 1

(def timestamp (str->Long (first parsed-input)))
(def bus-ids1 (map str->Long (filter #(not= "x" %) (clojure.string/split (second parsed-input) #","))))

(defn calculate-bus-wait-time
  "Returns the bus ids along with the time we'll have to wait each bus."
  []
  (let [minutes-wait (map #(- % (mod timestamp %)) bus-ids1)]
    (map #(vector %1 %2) bus-ids1 minutes-wait)))

(defn find-earliest-bus
  "Finds the earliest bus id we can take, along with the waiting time for that bus."
  [bus-wait-times]
  (reduce (fn [result bus-wait-time]
            (if (< (second bus-wait-time) (second result))
              bus-wait-time
              result))
          (first bus-wait-times)
          bus-wait-times))

; ---------------------------------------
; problem 2

(def bus-ids2 (clojure.string/split (second parsed-input) #","))

; a list of known bus ids along with their required relative timestamps
(def relative-timestamps
  (let [timestamps (take (count bus-ids2) (iterate inc 0))
        bus-timestamps (map #(vector %1 %2) bus-ids2 timestamps)]
    (filter #(not= "x" (first %)) bus-timestamps)))

; remove from timestamps the bus ids 577 and 601
(def final-relative-timestamps
  (let [filtered-timestamps (filter #(and (not= "601" (first %)) (not= "577" (first %))) relative-timestamps)]
    (map #(vector (str->Long (first %)) (second %)) filtered-timestamps)))

(defn check-t?
  "Returns true if t satisfies the requirements for problem 2."
  [t relative-timestamps]
  (if (empty? relative-timestamps)
    true
    (let [timestamp (first relative-timestamps)]
      (if (= 0 (mod (+ t (second timestamp)) (first timestamp)))
        (check-t? t (rest relative-timestamps))
        false))))

(def A (Long. "-447204"))
(def B (Long. "346777"))
(def starting-timestamp (+ A (* 2 B)))

(defn find-min-timestamp
  "Returns the min timestamp t that satisfies the requirements for problem 2."
  ([] (find-min-timestamp starting-timestamp))
  ([timestamp]
   (if (check-t? timestamp final-relative-timestamps)
     timestamp
     (recur (+ timestamp B)))))

; ---------------------------------------
; results

(defn day13-1
  []
  (let [bus-wait-times (calculate-bus-wait-time)
        min-bus (find-earliest-bus bus-wait-times)]
    (* (first min-bus) (second min-bus))))

;slow, needs optimization
(defn day13-2
  []
  (find-min-timestamp))

(defn -main
  []
  (println (day13-1))
  (println (day13-2)))
