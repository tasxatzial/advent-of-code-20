(ns day13.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn str->BigInt
  "Converts a string to Big integer."
  [^String s]
  (BigInteger. s))

(defn parse
  "Splits the input string by \n."
  [s]
  (clojure.string/split s #"\n"))

(def parsed-input (parse (slurp input-file)))

; ---------------------------------------
; problem 1

(def timestamp (str->BigInt (first parsed-input)))
(def bus-ids1 (map str->BigInt (filter #(not= "x" %) (clojure.string/split (second parsed-input) #","))))

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
(def final-timestamps
  (let [filtered-timestamps (filter #(and (not= "601" (first %)) (not= "577" (first %))) relative-timestamps)]
    (map #(vector (str->BigInt (first %)) (second %)) filtered-timestamps)))

(defn check-t?
  "Returns true if t satisfies the requirements for problem 2."
  [t timestamps]
  (if (empty? timestamps)
    true
    (let [timestamp (first timestamps)]
      (if (= 0 (/ (+ t (second timestamp)) (first timestamp)))
        (check-t? t (rest timestamps))
        false))))

; ---------------------------------------
; results

(def day13-1 (let [bus-wait-times (calculate-bus-wait-time)
                   min-bus (find-earliest-bus bus-wait-times)]
               (* (first min-bus) (second min-bus))))

(defn -main
  []
  (println day13-1)                                         ;174
  (println final-timestamps))
