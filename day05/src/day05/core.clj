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

(def seat-ids (map find-seat-id (parse (slurp input-file))))

; ---------------------------------------
; problem 2

(defn find-my-seat
  "Finds the missing seat-id."
  [seat-ids]
  (let [sorted-ids (sort seat-ids)]
    (loop [ids sorted-ids seat (first sorted-ids)]
      (if (not= seat (first ids))
        seat
        (recur (rest ids) (inc seat))))))

; ---------------------------------------
; results

(def day05-1
  (apply max seat-ids))

(def day05-2
  (find-my-seat seat-ids))

(defn -main
  []
  (println day05-1)                                         ;806
  (println day05-2))                                        ;562
