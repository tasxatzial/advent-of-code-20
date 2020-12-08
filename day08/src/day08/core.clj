(ns day08.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn str->int
  "Converts a string to integer."
  [^String s]
  (Integer. s))

(defn parse
  "Splits the input string by \n and then again by space. Returns
  a list of vectors of two elements, each vector describes a command."
  [s]
  (map #(clojure.string/split % #" ") (clojure.string/split s #"\n")))

; convert the input file to the appropriate structure, that is a
; vector of vectors, each vector is a [key, value] where
; key is a command keyword
(def parsed-input
  (let [input (parse (slurp input-file))]
    (reduce (fn [result [command value]]
              (conj result [(keyword command) (str->int value)]))
            []
            input)))

(defn -main
  []
  (println parsed-input))
