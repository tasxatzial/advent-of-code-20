(ns day15.core
  (:gen-class))

; ---------------------------------------
; problem 1

(defn init-turns
  "Initializes a map of numbers (key), each one has value that is a list
  of the turns this number has appeared."
  ([input] (init-turns input {} 0))
  ([input turns-map turn]
   (if (empty? input)
     turns-map
     (let [key (keyword (str (first input)))
           turns (key turns-map)
           new-turns (conj turns turn)
           new-map (assoc turns-map key new-turns)]
       (init-turns (rest input) new-map (inc turn))))))

(def input '(8 0 17 4 1 12))
(def starting-turn (count input))
(def starting-num 12)
(def starting-turns-map (init-turns input))

(defn play-game1
  "Returns 2020th spoken number (problem 1)"
  ([] (play-game1 starting-turn starting-num starting-turns-map))
  ([turn num turns-map]
   (if (= turn 2020)
     num
     (let [key (keyword (str num))
           turns (key turns-map)]
       (if (= 1 (count turns))
         (let [turns-0 (:0 turns-map)
               new-turns-0 (conj turns-0 turn)
               new-map (assoc turns-map :0 new-turns-0)]
           (play-game1 (inc turn) 0 new-map))
         (let [diff (- (first turns) (second turns))
               new-key (keyword (str diff))
               new-key-turns (conj (new-key turns-map) turn)
               new-map (assoc turns-map new-key new-key-turns)]
           (play-game1 (inc turn) diff new-map)))))))

; ---------------------------------------
; results

(def day15-1 (play-game1))

(defn -main
  []
  (println day15-1))
