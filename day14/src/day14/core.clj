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

(def partitioned-input (partition-by mask? parsed-input))

(defn process-mem-block
  "Creates a map of memory addresses to value from a block of memory instructions,
  this is a list of strings with all the instructions between two masks in the input file."
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

; a vector of maps, each map has the memory instructions for the corresponding mask
; from the above masks
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
; problem 1

(defn apply-mask1
  "Applies a mask to a given value (problem 1)"
  ([mask value] (apply-mask1 mask value []))
  ([mask value result]
   (if (empty? mask)
     (apply str result)
     (let [new-value (if (= (first mask) '\X)
                       (first value)
                       (first mask))]
       (apply-mask1 (rest mask) (rest value) (conj result new-value))))))

(defn apply-mask-mem-block1
  "Applies a mask to a block of memory instructions,
  these are all the instructions between two masks in the input file (problem 1)"
  [mask mem-block]
  (reduce (fn [result mem]
            (assoc result (keyword (str (first mem))) (apply-mask1 mask (second mem))))
          {}
          mem-block))

; apply all masks in the final docking instructions (problem 1)
(def apply-all-masks1
  (reduce (fn [result block]
            (let [mask (first block)
                  mem-block (second block)]
              (conj result (apply-mask-mem-block1 mask mem-block))))
          []
          docking-instructions))

; collect all memory address/values after the instructions have been applied (problem 1)
(def final-mem-values1
  (reduce (fn [result block]
            (into result block))
          {}
          apply-all-masks1))

; ---------------------------------------
; results

(def day14-1 (apply + (map #(Long/parseLong (second %) 2) final-mem-values1)))

(defn -main
  []
  (println day14-1))
