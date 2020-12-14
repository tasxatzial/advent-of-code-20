(ns day14.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n."
  [s]
  (clojure.string/split s #"\n"))

(def parsed-input (parse (slurp input-file)))

(defn parse-mask
  "Parses a mask line from the input file and returns a mask string."
  [mask-line]
  (second (clojure.string/split mask-line #" = ")))

(defn parse-mem
  "Parses a mem line from the input file and returns a two element
  vector, first one is the mem address (int), second one is the value in binary
  format (string)"
  [mem-line]
  (let [split-line (filter #(boolean (seq %)) (clojure.string/split mem-line #"[^\d]+"))
        integer-line (map #(Integer. ^String %) split-line)
        binary-value (Integer/toBinaryString (second integer-line))
        padding (apply str (take (- 36 (count binary-value)) (repeat "0")))]
    [(first integer-line) (str padding binary-value)]))

(defn mask?
  "Returns true if the line is a mask."
  [line]
  (= '\a (second line)))

(defn apply-mask
  "Applies a mask to a given value."
  ([mask value] (apply-mask mask value []))
  ([mask value result]
   (if (empty? mask)
     (apply str result)
     (let [new-value (if (= (first mask) '\X)
                   (first value)
                   (first mask))]
       (apply-mask (rest mask) (rest value) (conj result new-value))))))

(def partitioned-input (partition-by mask? parsed-input))

; ---------------------------------------
; problem 1

(defn process-mem-block
  "Creates a map of memory addresses to value from a block of memory instructions,
  these are all the instructions between two masks in the input file."
  [block]
  (reduce (fn [result mem-string]
            (conj result (parse-mem mem-string)))
          []
          block))

; the vector of the masks in the input file
(def masks
  (reduce (fn [result line]
            (if (mask? (first line))
              (conj result (parse-mask (first line)))
              result))
          []
          partitioned-input))

; a vector of maps, each map has the instructions of the corresponding mask from the above masks
(def mems
  (reduce (fn [result line]
            (if (mask? (first line))
              result
              (conj result (process-mem-block line))))
          []
          partitioned-input))

; the final docking instructions
(def docking-instructions
  (map #(vector %1 %2) masks mems))

; ---------------------------------------
; results

(defn -main
  []
  (println parsed-input))
