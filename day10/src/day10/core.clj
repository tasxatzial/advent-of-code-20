(ns day10.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn str->Int
  "Converts a string to int."
  [^String s]
  (Integer. s))

(defn parse
  "Splits the input string by \n and converts the strings to numbers."
  [s]
  (map #(str->Int %) (clojure.string/split s #"\n")))

(def parsed-input (parse (slurp input-file)))

; ---------------------------------------
; problem 1

; in order to find the joltage differences, we should first sort the joltages
(def diffs
  (let [sorted-col (sort parsed-input)
        shifted-col (conj (butlast sorted-col) 0)]
    (map #(- %1 %2) sorted-col shifted-col)))

; ---------------------------------------
; results

(def day10-1
  (let [count-1jolts (count (filter #(= 1 %) diffs))
        count-3jolts (count (filter #(= 3 %) diffs))]
    (* count-1jolts (inc count-3jolts))))    ;+1 because our device always has diff 3

(defn -main
  []
  (println day10-1))                                        ;2277
