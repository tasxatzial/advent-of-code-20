(ns day03.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n."
  [s]
  (clojure.string/split s #"\n"))

; ---------------------------------------
; problem 1

(defn count-trees1
  "counts the number of trees encountered when we move 3 steps right
  and one step down. Result is the first element in the returned vector."
  [seq]
  (let [part-length (count (first seq))
        tree (.charAt "#" 0)
        right 3]
    (reduce (fn [[trees index] part]
              (let [next-index (mod (+ index right) part-length)]
                (if (= tree (get part index))
                  [(inc trees) next-index]
                  [trees next-index])))
            [0 0]
            seq)))

; ---------------------------------------
; results

(def day03-1
  (first (count-trees1 (parse (slurp input-file)))))

(defn -main
  []
  (println day03-1)) ;276
