(ns day17.core
  (:gen-class))

; ---------------------------------------
; common

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
  (keyword (str x "." y "." z)))

(defn gen-coordinates
  "Generates the integer x,y,z coordinates from the specified keyword."
  [keyword]
  (map str->int (rest (clojure.string/split (str keyword) #".|-"))))

(defn enable-neighbors
  "Inserts into cube (map of coordinates to either . or #) all non-existing
  neighbors of coordinate [x,y,z] with a default value of ."
  [cube [x y z]]
  (let [all-keys [(gen-key x (inc y) z)
        (gen-key (inc x) y z)
        (gen-key (inc x) (inc y) z)
        (gen-key (dec x) y z)
        (gen-key (dec x) (dec y) z)
        (gen-key x (dec y) z)
        (gen-key (dec x) (inc y) z)
        (gen-key (inc x) (dec y) z)
        (gen-key x (inc y) (inc z))
        (gen-key (inc x) y (inc z))
        (gen-key (inc x) (inc y) (inc z))
        (gen-key (dec x) y (inc z))
        (gen-key (dec x) (dec y) (inc z))
        (gen-key x (dec y) (inc z))
        (gen-key (dec x) (inc y) (inc z))
        (gen-key (inc x) (dec y) (inc z))
        (gen-key x (inc y) (dec z))
        (gen-key (inc x) y (dec z))
        (gen-key (inc x) (inc y) (dec z))
        (gen-key (dec x) y (dec z))
        (gen-key (dec x) (dec y) (dec z))
        (gen-key x (dec y) (dec z))
        (gen-key (dec x) (inc y) (dec z))
        (gen-key (inc x) (dec y) (dec z))
        (gen-key x y (inc z))
        (gen-key x y (dec z))]]
    (reduce (fn [result key]
              (if (contains? cube key)
                cube
                (conj result [key '\.])))
            cube
            all-keys)))

; ---------------------------------------
; results

(defn -main
  []
  (println input))
