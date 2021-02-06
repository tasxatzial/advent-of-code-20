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
  "Generates keyword :x.y.z. ... from (x y z ...)"
  [point]
  (keyword (apply str (map #(str % ".") point))))

(defn gen-point
  "Generates (x y z ...) from keyword :x.y.z. ..."
  [keyword]
  (map #(Integer. ^String %) (rest (clojure.string/split (str keyword) #"[.|:]"))))

(def diffs [-1 0 1])
(def diffs-count 3)
(def diffs-range (vec (range diffs-count)))

(defn gen-adjacent
  "Returns a list of differences between point 0 (in the specified dimension)
   and its adjacent points."
  ([dimension-size]
   (->> (gen-adjacent dimension-size 0 (vec (take dimension-size (repeat 0))))
        flatten
        (partition dimension-size)
        (filter #(not= [0 0 0] %))))
  ([dimension-size dimension-index arr]
   (if (= dimension-index dimension-size)
     arr
     (for [i diffs-range
           :let [arr (assoc arr dimension-index (get diffs i))]]
       (gen-adjacent dimension-size (inc dimension-index) arr)))))

(defn gen-adjacent-keywords
  "Generate all adjacent keywords of :x.y.z. ..."
  [keyword neighbor-diffs]
  (let [point (gen-point keyword)
        neighbors (reduce (fn [result diff]
                            (conj result (map #(+ %1 %2) point diff)))
                          '()
                          neighbor-diffs)]
    (map gen-keyword neighbors)))

(defn gen-row-points
  "Parses a row from the input and returns a set of keywords :x.y.z. ..., those
  keywords correspond to points (x y z ...) with initial state #.
  Dimension-size must be >=2"
  [dimension-size y row]
  (loop [result #{}
         row-tmp row
         index-x 0]
    (if (empty? row-tmp)
      result
      (let [point (concat [index-x y] (take (- dimension-size 2) (repeat 0)))
            new-result (if (= \# (first row-tmp))
                         (conj result (gen-keyword point))
                         result)]
        (recur new-result (rest row-tmp) (inc index-x))))))

(defn gen-init-points
  "Generates the initial state from the given input. Returns a set of keywords
  :x.y.z. ..., those keywords correspond to points (x y z ...) with initial state #."
  [dimension-size]
  (loop [result #{}
         input-tmp input
         index-y 0]
    (if (empty? input-tmp)
      result
      (let [row-result (gen-row-points dimension-size index-y (first input-tmp))]
        (recur (into result row-result) (rest input-tmp) (inc index-y))))))

; ---------------------------------------
; problem 1

(def init-points1 (gen-init-points 3))
(def neighbor-diffs1 (gen-adjacent 3))

; ---------------------------------------
; results

(defn -main
  []
  (println init-points1))