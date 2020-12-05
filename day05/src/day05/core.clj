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

(defn -main
  []
  (println (parse (slurp input-file))))
