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

(defn generate-neighbor-diffs
  "Generate a list of all difference vectors between
  point [0 0 0] and its 26 neighbors"
  ([] (generate-neighbor-diffs '() -1 -1 -1))
  ([result x y z]
   (if (and (= x 0) (= y 0) (= z 0))
     (recur result 0 0 1)
     (if (= z 2)
       (recur result x (inc y) -1)
       (if (= y 2)
         (recur result (inc x) -1 -1)
         (if (= x 2)
           result
           (recur (conj result (list x y z)) x y (inc z))))))))

(def neighbor-diffs (generate-neighbor-diffs))

(defn generate-neighbors-keys
  "Generate all neighbors keys of point [x y z]"
  [x y z]
  (let [neighbors (map #(list (+ x (first %)) (+ y (second %)) (+ z (last %))) neighbor-diffs)]
    (map #(apply gen-key %) neighbors)))

(defn add-neighbors-keys
  "Adds neighbors-keys (default value of '.') to all-points.
  Existing neighbors-keys are not added."
  [all-points neighbors-keys]
  (reduce (fn [result key]
            (if (contains? all-points key)
              all-points
              (conj result [key '\.])))
          all-points
          neighbors-keys))

; ---------------------------------------
; results

(defn -main
  []
  (println input))
