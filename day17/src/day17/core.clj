(ns day17.core
  (:gen-class))

(def input-file "resources\\input.txt")

(defn str->int
  "Convert a string to integer."
  [^String s]
  (Integer. s))

(defn parse
  "Splits the input string by \n."
  [s]
  (clojure.string/split s #"\n"))

(def input (parse (slurp input-file)))

(defn gen-key
  "Generates a keyword from given integers x, y, z."
  [x y z]
  (keyword (str x "-" y "-" z)))

(defn gen-coordinates
  "Generates the integer x,y,z coordinates from the specified keyword."
  [keyword]
  (map str->int (rest (clojure.string/split (str keyword) #":|-"))))

(defn -main
  []
  (println input))
