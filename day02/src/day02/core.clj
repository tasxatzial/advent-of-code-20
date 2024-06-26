(ns day02.core
  (:gen-class))

; ---------------------------------------
; common

(def input "resources\\input.txt")

(defn str->int
  "Converts a string to integer."
  [^String s]
  (Integer. s))

(defn parse
  "Parses the input string into a list of vectors.
  Each vector is one line of the input string split by space."
  [s]
  (map #(clojure.string/split % #" ") (clojure.string/split s #"\n")))

(defn get-letter
  "Returns the letter in the password policy.
  Input is a vector in the list returned by parse()."
  [seq]
  (get (get seq 1) 0))

(defn get-password
  "Returns the password in the password policy.
  Input is a vector in the list returned by parse()."
  [seq]
  (get seq 2))

; ---------------------------------------
; problem 1

(defn get-minmax
  "Returns a vector of the minimum and maximum numbers in the password policy.
   Input is a vector in the list returned by parse()."
  [seq]
  (let [[min max] (clojure.string/split (get seq 0) #"-")]
    (map str->int [min max])))

(defn isValid1?
  "Returns true of false depending on whether the password is valid according
  to the password policy."
  [seq]
  (let [[min max] (get-minmax seq)
        letter (get-letter seq)
        password (get-password seq)
        occurences (re-seq (re-pattern (str letter)) password)]
    (and (>= (count occurences) min) (<= (count occurences) max))))

; ---------------------------------------
; problem 2

(defn get-positions
  "Returns a vector of the first and second position in the password policy.
   Input is a vector in the list returned by parse()"
  [seq]
  (let [[min max] (clojure.string/split (get seq 0) #"-")]
    (map str->int [min max])))

(defn isValid2?
  "Returns true of false depending on whether the password is valid according
  to the password policy."
  [seq]
  (let [[first-pos second-pos] (get-positions seq)
        letter (get-letter seq)
        password (get-password seq)]
    (or (and (= letter (get password (dec first-pos))) (not= letter (get password (dec second-pos))))
        (and (= letter (get password (dec second-pos))) (not= letter (get password (dec first-pos)))))))

; ---------------------------------------
; results

(defn day02-1
  []
  (count (filter true? (map isValid1? (parse (slurp input))))))

(defn day02-2
  []
  (count (filter true? (map isValid2? (parse (slurp input))))))

(defn -main
  []
  (println (day02-1))
  (println (day02-2)))
