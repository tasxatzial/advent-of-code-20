(ns day23.core
  (:gen-class))

(def input 418976235)

(defn num-to-seq
  "Returns a list of the digits of the integer num."
  [num]
  (->> num
       str
       (map (comp read-string str))))

(defn -main
  []
  (println (num-to-seq input)))
