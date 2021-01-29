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

(defn calculate-joltage-diffs
  []
  "Sorts the joltages and calculates the adjacent joltage differences."
  (let [sorted-col (sort parsed-input)
        shifted-col (conj (butlast sorted-col) 0)]
    (map #(- %1 %2) sorted-col shifted-col)))

(def memoized-diffs (memoize calculate-joltage-diffs))

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

(defn ones-block-sizes
  "Creates a list of the number of elements in each block of ones in the joltage
  differences list from problem 1 and subtract 1 from each element.
  Only non-zero values are collected."
  []
  (let [partition-by-3 (partition-by #(= 3 %) (memoized-diffs))]
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

(defn day10-1
  []
  (let [count-1jolts (count (filter #(= 1 %) (memoized-diffs)))
        count-3jolts (count (filter #(= 3 %) (memoized-diffs)))]
    (* count-1jolts (inc count-3jolts))))    ;+1 because our device always has diff 3

; Efficiently process the list ones-block-sizes. For each number in that list
; we use the count-trees then each result is multiplied by the previous one. The final
; result is the total number of distinct ways we can arrange the adapters.
(defn day10-2
  []
  (let [partition&sort-ones-block-sizes (partition-by identity (sort (ones-block-sizes)))]
    (reduce (fn [result ones-size-block]
              (let [ones-size-factor (count-trees (first ones-size-block))
                    ones-size-factors (take (count ones-size-block) (repeat ones-size-factor))]
                (* result (apply * ones-size-factors))))
            1
            partition&sort-ones-block-sizes)))

(defn -main
  []
  (println (day10-1))                                         ;2277
  (println (day10-2)))                                        ;37024595836928
