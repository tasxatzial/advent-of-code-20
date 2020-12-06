(ns day06.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n\n and then splits again by \n.
  Returns a list of vectors, each element of the vector represents
  the answers of one user of the group."
  [input]
  (map #(clojure.string/split % #"\n") (clojure.string/split input #"\n\n")))

; ---------------------------------------
; problem 1

(defn count-yes
  "Counts the yes answers of a group."
  [group-answers]
  (count (reduce (fn [result answer]
                   (into result (set answer)))
                 #{}
                 group-answers)))

; ---------------------------------------
; results

(def day06-1
  (apply + (map count-yes (parse (slurp input-file)))))

(defn -main
  []
  (println day06-1))
