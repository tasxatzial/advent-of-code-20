(ns day17.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n."
  [s]
  (clojure.string/split s #"\n"))

(def input (parse (slurp input-file)))

(defn gen-keyword
  "Generates keyword :x:y:z from [x y z]."
  [[x y z]]
  (keyword (str x "." y "." z)))

(defn gen-point
  "Generates [x y z] from keyword :x:y:z"
  [keyword]
  (map #(Integer. ^String %) (rest (clojure.string/split (str keyword) #"[.|:]"))))

(defn generate-adjacent-0
  "Generate the adjacent points of [0 0 0]."
  ([] (generate-adjacent-0 '() -1 -1 -1))
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

(def neighbor-diffs (generate-adjacent-0))

(defn gen-adjacent-keywords
  "Generate all adjacent keywords of :x:y:z"
  [keyword]
  (let [[x y z] (gen-point keyword)
        neighbors (map #(list (+ x (first %)) (+ y (second %)) (+ z (last %))) neighbor-diffs)]
    (map gen-keyword neighbors)))

(defn gen-row-points
  "Parses a row from the input and returns a set of keywords :x:y:z, those
  keywords correspond to points [x y z] with initial state #"
  [y row]
  (loop [result #{}
         row-tmp row
         index-x 0]
    (if (empty? row-tmp)
      result
      (let [new-result (if (= \# (first row-tmp))
                         (conj result (gen-keyword [index-x y 0]))
                         result)]
        (recur new-result (rest row-tmp) (inc index-x))))))

(defn gen-init-points
  "Generate the initial state from the given input. Returns a set of keywords
  :x:y:z, those keywords correspond to points with initial state #"
  []
  (loop [result #{}
         input-tmp input
         index-y 0]
    (if (empty? input-tmp)
      result
      (let [row-result (gen-row-points index-y (first input-tmp))]
        (recur (into result row-result) (rest input-tmp) (inc index-y))))))


(def init-points (gen-init-points))

; ---------------------------------------
; problem 1


; ---------------------------------------
; results

(defn -main
  []
  (println 1))