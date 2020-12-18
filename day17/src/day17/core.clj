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
(def rows (count input))
(def columns (count (first input)))

(defn gen-key
  "Generates a keyword from given integers x, y, z."
  [x y z]
  (keyword (str x "." y "." z)))

(defn gen-coordinates
  "Generates the integer x,y,z coordinates from the specified keyword."
  [keyword]
  (map str->int (rest (clojure.string/split (str keyword) #"[.|:]"))))

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
  [[x y z]]
  (let [neighbors (map #(list (+ x (first %)) (+ y (second %)) (+ z (last %))) neighbor-diffs)]
    (map #(apply gen-key %) neighbors)))

(defn add-neighbors-keys
  "Adds neighbors-keys (default value of '.') to all-points.
  Existing neighbors-keys are not added."
  [all-points neighbors-keys]
  (reduce (fn [result key]
            (if (contains? all-points key)
              result
              (conj result [key \.])))
          all-points
          neighbors-keys))

(defn next-state
  "Returns the new state based on the current state and the number
  of active neighbors."
  [state count-active]
  (case state
    \# (if (or (= count-active 2) (= count-active 3))
          \#
          \.)
    \. (if (= 3 count-active)
          \#
          \.)))

(defn advance-state
  "Computes the new state of point [key state] and updates the map
  of all-points."
  [all-points [key state]]
  (let [neighbors-keys (generate-neighbors-keys (gen-coordinates key))
        neighbors-state (map #(% all-points) neighbors-keys)
        count-active (count (filter #(= % \#) neighbors-state))
        new-state (next-state state count-active)
        new-all-points (assoc all-points key new-state)]
    (case new-state
      \# (add-neighbors-keys new-all-points neighbors-keys)
      \. new-all-points)))

(def joined-input (apply str input))
(def joined-input-count (count joined-input))

(defn gen-init-points
  "Generate the initial state from the given input."
  ([] (gen-init-points {} 0))
  ([all-points index]
   (if (= index joined-input-count)
     all-points
     (let [point-state (get joined-input index)
           y (Math/round (Math/floor (/ index columns)))
           x (mod index columns)
           point-key (gen-key x y 0)
           new-all-points (conj all-points [point-key point-state])]
       (recur new-all-points (inc index))))))

(def init-points (gen-init-points))

; expand the initial points so that their neighbors are also included
(def all-init-points
  (reduce (fn [points [key state :as point]]
            (let [coordinates (gen-coordinates key)
                  neighbors-keys (generate-neighbors-keys coordinates)]
              (if (= state \#)
                (add-neighbors-keys points neighbors-keys)
                points)))
          init-points
          init-points))

; ---------------------------------------
; results

(defn -main
  []
  (println (count all-init-points)))
