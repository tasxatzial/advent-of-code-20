(ns day13.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn str->int
  "Converts a string to integer."
  [^String s]
  (Integer. s))

(defn parse
  "Splits the input string by \n."
  [s]
  (clojure.string/split s #"\n"))

(def parsed-input (parse (slurp input-file)))

; ---------------------------------------
; problem 1

(def timestamp (str->int (first parsed-input)))
(def bus-ids1 (map str->int (filter #(not= "x" %) (clojure.string/split (second parsed-input) #","))))

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

; ---------------------------------------
; results

(def day13-1 (let [bus-wait-times (calculate-bus-wait-time)
                   min-bus (find-earliest-bus bus-wait-times)]
               (* (first min-bus) (second min-bus))))

(defn -main
  []
  (println day13-1)                                         ;174
  (println relative-timestamps))
