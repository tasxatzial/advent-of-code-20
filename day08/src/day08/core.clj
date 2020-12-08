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

; --------------------------
; problem 1

(defn execute1
  "Executes the instructions from the parsed-input and returns
  the value of the accumulator before a command is about to be executed
  for a second time."
  ([] (execute1 0 0 #{}))
  ([accumulator current-command-index command-history]
   (if (contains? command-history current-command-index)
     accumulator
     (let [[command value] (get parsed-input current-command-index)
           command-history (conj command-history current-command-index)]
       (case command
         :nop (execute1 accumulator (inc current-command-index) command-history)
         :acc (execute1 (+ accumulator value) (inc current-command-index) command-history)
         :jmp (execute1 accumulator (+ value current-command-index) command-history))))))

; --------------------------
; results

(def day08-1 (execute1))

(defn -main
  []
  (println day08-1))                                     ;1818
