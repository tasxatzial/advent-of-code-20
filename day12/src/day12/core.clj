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

; for each key, the elements in the vector correspond to a 90 clockwise rotation
; of the previous direction
(def directions {:N [:E :S :W]
                 :E [:S :W :N]
                 :S [:W :N :E]
                 :W [:N :E :S]})

(def ship-init-pos {:N 0 :E 0})

(defn compute-manhattan
  "Returns the manhattan distance of [x, y]."
  [x y]
  (+ (Math/abs ^Integer x) (Math/abs ^Integer y)))

; ---------------------------------------
; problem 1

(def ship-init-direction :E)

(defn new-ship-direction1
  "Returns the new direction of the ship (problem 1)"
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

(defn new-ship-pos1
  "Returns the new position of the ship (problem 1)"
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
(defn destination1
  []
  (reduce (fn [[pos direction] instruction]
                      [(new-ship-pos1 direction pos instruction)
                       (new-ship-direction1 direction instruction)])
                          [ship-init-pos ship-init-direction]
                          instructions))

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
  [(* -1 y) x])

(defn move-ship2
  "Returns the new position of the ship (problem 2)"
  [waypoint-pos ship-pos [cmd value]]
  (let [waypoint-N (:N waypoint-pos)
        waypoint-E (:E waypoint-pos)
        ship-N (:N ship-pos)
        ship-E (:E ship-pos)]
    (case cmd
      :F (assoc ship-pos :N (+ ship-N (* waypoint-N value))
                         :E (+ ship-E (* waypoint-E value)))
      ship-pos)))

(defn move-waypoint2
  "Returns the new position of the waypoint (problem 2)"
  [{:keys [N E] :as pos} [cmd value]]
  (let [new-pos (case cmd
                  :N [E (+ N value) ]
                  :E [(+ E value) N]
                  :S [E (- N value)]
                  :W [(- E value) N]
                  :R (case value
                       90 (rotate-right-90 E N)
                       180 (rotate-180 E N)
                       270 (rotate-right-270 E N))
                  :L (case value
                       90 (rotate-right-270 E N)
                       180 (rotate-180 E N)
                       270 (rotate-right-90 E N))
                  [E N])]
    (assoc pos :E (first new-pos) :N (second new-pos))))

; steer ship and waypoint to their final positions
(defn destination2
  []
  (reduce (fn [[ship-pos waypoint-pos] instruction]
            [(move-ship2 waypoint-pos ship-pos instruction)
             (move-waypoint2 waypoint-pos instruction)])
          [ship-init-pos {:N 1 :E 10}]
          instructions))

; ---------------------------------------
; results

(defn day12-1
  []
  (let [destination1 (destination1)]
    (compute-manhattan (:N (first destination1)) (:E (first destination1)))))

(defn day12-2
  []
  (let [destination2 (destination2)]
    (compute-manhattan (:N (first destination2)) (:E (first destination2)))))

(defn -main
  []
  (println (day12-1))                                         ;1838
  (println (day12-2)))                                        ;89936
