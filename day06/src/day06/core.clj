(ns day06.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n\n and then splits again by \n.
  Returns a list of vectors, each element of the vector represents
  the answers of a user of the group."
  [input]
  (map #(clojure.string/split % #"\n") (clojure.string/split input #"\n\n")))

; Converts each string from the parsed input into a set of characters
; this is now the new parsed input
(def parsed-input
  (reduce (fn [result group-answers]
            (conj result (map set group-answers)))
          '()
          (parse (slurp input-file))))

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
  [group-answers]
  (let [counted-group-answers (map count group-answers)
        max-count (apply max counted-group-answers)]
    (second (reduce (fn [[current-index min-index min-count] this-count]
                      (if (< this-count min-count)
                        [(inc current-index) current-index this-count]
                        [(inc current-index) min-index min-count]))
                    [0 0 max-count]
                    counted-group-answers))))

(defn count-yes2
  "Counts the yes answers of a group for problem 2."
  [group-answers]
  (let [min-set (nth group-answers (find-min-index group-answers))]
    (reduce (fn [yes-count answer]
              (if (some false? (map #(contains? % answer) group-answers))
                yes-count
                (inc yes-count)))
            0
            min-set)))

; ---------------------------------------
; results

(defn day06-1
  []
  (apply + (map count-yes1 parsed-input)))

(defn day06-2
  []
  (apply + (map count-yes2 parsed-input)))

(defn -main
  []
  (println (day06-1))
  (println (day06-2)))
