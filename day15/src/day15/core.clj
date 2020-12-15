(ns day15.core
  (:gen-class))

(defn init-turns
  "Initializes a map of numbers (keys), each one is mapped to a list
  of the positions of this number in the input."
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
(def starting-num (last input))
(def starting-turns-map (init-turns input))

(defn play-game
  "Returns nth spoken number"
  ([nth] (play-game nth starting-turn starting-num starting-turns-map))
  ([nth turn num turns-map]
   (if (= turn nth)
     num
     (let [key (keyword (str num))
           turns (key turns-map)]
       (if (= 1 (count turns))
         (let [turns-0 (:0 turns-map)
               new-turns-0 (if (= 1 (count turns-0))  ;keep only the first two elements
                             (conj turns-0 turn)
                             (conj (butlast turns-0) turn))
               new-turn-map (assoc turns-map :0 new-turns-0)]
           (recur nth (inc turn) 0 new-turn-map))
         (let [diff (- (first turns) (second turns))
               key (keyword (str diff))
               new-key-turns (let [key-turns (key turns-map)]  ;keep only the first two elements
                               (if (= 1 (count key-turns))
                                 (conj key-turns turn)
                                 (conj (butlast key-turns) turn)))
               new-turn-map (assoc turns-map key new-key-turns)]
           (recur nth (inc turn) diff new-turn-map)))))))

; ---------------------------------------
; results

(def day15-1 (play-game 2020))
(def day15-2 (play-game 30000000))

(defn -main
  []
  (println day15-1)                                         ;981
  (println day15-2))                                        ;164878
