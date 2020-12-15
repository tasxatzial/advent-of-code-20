(ns day15.core
  (:gen-class))

; ---------------------------------------
; problem 1

(def input '(8 0 17 4 1 12))
(def input-count (count input))

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

(def init-turns-map (init-turns input))

(defn -main
  []
  (println init-turns-map))
