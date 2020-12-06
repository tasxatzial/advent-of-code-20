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

(def parsed-input (parse (slurp input-file)))

; Parses the strings from the parsed input into sets.
(def all-answers-set
  (reduce (fn [result group-answers]
            (conj result (map set group-answers)))
          '()
          parsed-input))

; ---------------------------------------
; problem 1

(defn count-yes1
  "Counts the yes answers of a group for problem 1."
  [group-answers]
  (count (reduce (fn [result answer]
                   (into result answer))
                 #{}
                 group-answers)))

; ---------------------------------------
; problem 2

(defn find-min-index
  "Searches the answers of a group and finds the index of the answer
  that has the minimum yes."
  [answer-set]
  (let [answer-count (map count answer-set)
        max-count (apply max answer-count)]
    (second (reduce (fn [[current-index min-index min-count] this-count]
                      (if (< this-count min-count)
                        [(inc current-index) current-index this-count]
                        [(inc current-index) min-index min-count]))
                    [0 0 max-count]
                    answer-count))))

; ---------------------------------------
; results

(def day06-1
  (apply + (map count-yes1 all-answers-set)))

(defn -main
  []
  (println day06-1))
