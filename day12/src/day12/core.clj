(ns day12.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn str->int
  [^String s]
  (Integer. s))

(defn parse
  "Splits the input string by \n."
  [s]
  (clojure.string/split s #"\n"))

(def parsed-input (parse (slurp input-file)))

; create the appropriate structure for the instructions
(def instructions
  (reduce (fn [result instruction]
            (conj result [(keyword (str (first instruction))) (str->int (apply str (drop 1 instruction)))]))
          []
          parsed-input))

; for each key, the vector corresponds to a 90 rotation to the right
(def directions {:N [:E :S :W]
                 :E [:S :W :N]
                 :S [:W :N :E]
                 :W [:N :E :S]})

; ---------------------------------------
; problem 1

(defn new-direction
  [old-direction [cmd value]]
  (case value
    90 (case cmd
         :R (first (old-direction directions))
         :L (last (old-direction directions))
         old-direction)
    180 (second (old-direction directions))
    270 (case cmd
          :R (last (old-direction directions))
          :L (first (old-direction directions))
          old-direction)
    old-direction))

(defn new-pos
  [direction pos [cmd value]]
  (case cmd
    :F (case direction
         :S (assoc pos :N (- (:N pos) value))
         :W (assoc pos :E (- (:E pos) value))
         (assoc pos direction (+ (direction pos) value)))
    :N (assoc pos :N (+ (:N pos) value))
    :E (assoc pos :E (+ (:E pos) value))
    :S (assoc pos :N (- (:N pos) value))
    :W (assoc pos :E (- (:E pos) value))
    pos))

(defn -main
  []
  (println instructions))
