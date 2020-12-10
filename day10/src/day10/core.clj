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
; problem 2

(defn count-trees
  "Returns the number of binary trees that satisfy:
  1. Node values are either 0 or 1.
  2. There are no 3 consecutive nodes that have value 0."
  ([num] (count-trees num 0))
  ([num count-zero]
   (if (= num 0)
     1
     (if (or (= count-zero 0) (= count-zero 1))
       (let [count-left (count-trees (dec num) (inc count-zero))
             count-right (count-trees (dec num) 0)]
         (+ count-left count-right))
       (count-trees (dec num) 0)))))


; Create a list of the number of elements in each block of ones in the joltage
; differences list from problem 1 and subtract 1 from each element.
; Only non-zero values are collected.
(def ones-sizes
  (let [partition-by-3 (partition-by #(= 3 %) diffs)]
    (reduce (fn [result sublist]
              (if (= (first sublist) 3)
                result
                (if (not= (count sublist) 1)
                  (conj result (dec (count sublist)))
                  result)))
            []
            partition-by-3)))

; ---------------------------------------
; results

(def day10-1
  (let [count-1jolts (count (filter #(= 1 %) diffs))
        count-3jolts (count (filter #(= 3 %) diffs))]
    (* count-1jolts (inc count-3jolts))))    ;+1 because our device always has diff 3

(defn -main
  []
  (println day10-1))                                        ;2277
