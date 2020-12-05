(ns day05.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  [s]
  (clojure.string/split s #"\n"))

; ---------------------------------------
; problem 1

(defn find-row
  "Finds the row number given a seat code."
  [code-string]
  (let [row-code (take 7 code-string)]
    (first (reduce (fn [[front back] letter]
                     (if (= letter '\F)
                       [front (dec (/ (inc (+ front back)) 2))]
                       [(/ (inc (+ front back)) 2) back]))
                   [0 127]
                   row-code))))

(defn find-col
  "Finds the col number given a seat code."
  [code-string]
  (let [col-code (drop 7 code-string)]
    (first (reduce (fn [[right left] letter]
                     (if (= letter '\L)
                       [right (dec (/ (inc (+ right left)) 2))]
                       [(/ (inc (+ right left)) 2) left]))
                   [0 7]
                   col-code))))

(defn find-seat-id
  "Finds the seat ID given a seat code."
  [code-string]
  (+ (find-col code-string) (* 8 (find-row code-string))))

; ---------------------------------------
; results

(def day05-1
  (apply max (map find-seat-id (parse (slurp input-file)))))

(defn -main
  []
  (println day05-1))
