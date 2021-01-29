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

(def command-count (count parsed-input))

(defn execute1
  "Executes the instructions from the parsed-input and returns a vector that
  contains:
  1) If the program does not terminate: [acc_value, false] where acc_value is the
   accumulator value before any instruction is executed a second time.
  2) If the program terminates (meaning we reached the next to last command):
   [acc_value, true] where acc_value is the accumulator value at that point."
  ([] (execute1 0 0 #{}))
  ([accumulator current-command-index command-history]
   (if (= current-command-index command-count)
     [accumulator true]
     (if (contains? command-history current-command-index)
       [accumulator false]
       (let [[command value] (get parsed-input current-command-index)
             command-history (conj command-history current-command-index)]
         (case command
           :nop (recur accumulator (inc current-command-index) command-history)
           :acc (recur (+ accumulator value) (inc current-command-index) command-history)
           :jmp (recur accumulator (+ value current-command-index) command-history)))))))

; --------------------------
; problem 2

(defn execute2
  "Executes the instructions from the parsed-input and modifies a single :nop to :jmp
  or vice versa so that they program terminates.
  Returns a vector that contains:
  1) If the program does not terminate: [acc_value, false] where acc_value is the
   accumulator value before any instruction is executed a second time.
  2) If the program terminates (meaning we reached the next to last command):
   [acc_value, true] where acc_value is the accumulator value at that point."
  ([] (execute2 0 0 #{}))
  ([accumulator current-command-index command-history]
   (if (= current-command-index command-count)
     [accumulator true]
     (let [[command value] (get parsed-input current-command-index)
           command-history (conj command-history current-command-index)]
       (case command
         :nop (let [[new-accumulator terminated?] (execute1 accumulator (+ value current-command-index) command-history)]
                (if terminated?
                  [new-accumulator true]
                  (recur accumulator (inc current-command-index) command-history)))
         :jmp (let [[new-accumulator terminated?] (execute1 accumulator (inc current-command-index) command-history)]
                (if terminated?
                  [new-accumulator true]
                  (recur accumulator (+ value current-command-index) command-history)))
         :acc (recur (+ accumulator value) (inc current-command-index) command-history))))))

; --------------------------
; results

(defn day08-1
  []
  (execute1))

(defn day08-2
  []
  (execute2))

(defn -main
  []
  (println (day08-1))                                         ;[1818 false]
  (println (day08-2)))                                        ;[631 true]
