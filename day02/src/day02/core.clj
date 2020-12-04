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

(defn -main
  []
  )
