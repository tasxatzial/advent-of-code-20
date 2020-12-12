(ns day12.core
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

; create the appropriate structure for the instructions
(def instructions
  (reduce (fn [result instruction]
            (conj result [(keyword (str (first instruction))) (str->int (apply str (drop 1 instruction)))]))
          []
          parsed-input))

; for each key, the elements in the vector correspond to a 90 right rotation
; of the previous direction
(def directions {:N [:E :S :W]
                 :E [:S :W :N]
                 :S [:W :N :E]
                 :W [:N :E :S]})

(def init-pos {:N 0 :E 0})
(def init-direction :E)

; ---------------------------------------
; problem 1

(defn new-direction1
  "Returns the new direction (problem 1)"
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

(defn new-pos1
  "Returns the new position (problem 1)"
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

; steer ship to the final position
(def final1 (reduce (fn [[pos direction] instruction]
                      [(new-pos1 direction pos instruction)
                       (new-direction1 direction instruction)])
                    [init-pos init-direction]
                    instructions))

; compute manhattan distance of the final position
(def day12-1
  (+ (Math/abs ^Integer (:N (first final1))) (Math/abs ^Integer (:E (first final1)))))

; ---------------------------------------
; problem 2

(defn rotate-right-90
  "Clockwise rotation by 90 degrees."
  [x y]
  [y (* -1 x)])

(defn rotate-180
  "Clockwise rotation by 180 degrees."
  [x y]
  [(* -1 x) (* -1 y)])

(defn rotate-right-270
  "Clockwise rotation by 270 degrees."
  [x y]
  [(* -1 y ) x])

; ---------------------------------------
; results

(defn -main
  []
  (println day12-1))
