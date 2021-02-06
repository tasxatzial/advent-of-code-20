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
        (filter #(not= (take dimension-size (repeat 0)) %))))
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

(defn change-adjacent-to-active
  "Processes a list of adjacent keywords and returns a set of only those that
  correspond to points that will be changed to active.
  Points that are already active are skipped."
  [keywords current-active neighbor-diffs]
  (reduce (fn [result keyword]
            (let [adjacent-keywords (gen-adjacent-keywords keyword neighbor-diffs)
                  adjacent-active (filter #(contains? current-active %) adjacent-keywords)]
              (if (contains? current-active keyword)
                (if (or (= 2 (count adjacent-active)) (= 3 (count adjacent-active)))
                  (conj result keyword)
                  result)
                (if (= 3 (count adjacent-active))
                  (conj result keyword)
                  result))))
          #{}
          keywords))

(defn change-to-active
  "Processes a keyword & its adjacent keywords and returns a set of only those
  that correspond to points that will be changed to active."
  ([keyword current-active neighbor-diffs]
   (let [adjacent-keywords (gen-adjacent-keywords keyword neighbor-diffs)
         adjacent-active (filter #(contains? current-active %) adjacent-keywords)
         new-adjacent-active (change-adjacent-to-active adjacent-keywords current-active neighbor-diffs)]
     (if (contains? current-active keyword)
       (if (or (= 2 (count adjacent-active)) (= 3 (count adjacent-active)))
         (conj new-adjacent-active keyword)
         new-adjacent-active)
       (if (= 3 (count adjacent-active))
         (conj new-adjacent-active keyword)
         new-adjacent-active)))))

(defn simulate
  "Runs the simulation max-iter times. Returns the set of only the keywords
  that correspond to active points."
  [keywords neighbor-diffs max-iter]
  (if (= 0 max-iter)
    keywords
    (let [new-keywords (reduce (fn [result keyword]
                                 (let [new-active (change-to-active keyword keywords neighbor-diffs)]
                                   (into result new-active)))
                               #{}
                               keywords)]
      (recur new-keywords neighbor-diffs (dec max-iter)))))

; ---------------------------------------
; problem 1

(def init-points1 (gen-init-points 3))
(def neighbor-diffs1 (gen-adjacent 3))

; ---------------------------------------
; problem 2

(def init-points2 (gen-init-points 4))
(def neighbor-diffs2 (gen-adjacent 4))

; ---------------------------------------
; results

(defn day17-1
  []
  (count (simulate init-points1 neighbor-diffs1 6)))

;slow, needs optimization
(defn day17-2
  []
  (count (simulate init-points2 neighbor-diffs2 6)))

(defn -main
  []
  (println (day17-1))                                       ;230
  (println (day17-2)))                                      ;1600
