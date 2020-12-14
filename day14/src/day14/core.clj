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

(defn to-binary
  "Covert an integer to a 36-bit binary string."
  [num]
  (let [binary-value (Integer/toBinaryString num)
        padding (apply str (take (- 36 (count binary-value)) (repeat "0")))]
    (str padding binary-value)))

(defn parse-mem
  "Parses a mem line from the input file and returns a two element
  vector, first one is the mem address (int), second one is the value in binary
  format (string)"
  [mem-line]
  (let [split-line (filter #(boolean (seq %)) (clojure.string/split mem-line #"[^\d]+"))
        integer-line (map #(Integer. ^String %) split-line)]
    [(first integer-line) (second integer-line)]))

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

; apply all masks in the final docking instructions (problem 1)
(defn apply-all-masks
  [mask-func]
  (reduce (fn [result block]
            (let [mask (first block)
                  mem-block (second block)]
              (conj result (mask-func mask mem-block))))
          []
          docking-instructions))

; collect all memory address/values after the instructions have been applied (problem 1)
(defn final-mem-values
  [updated-mem-blocks]
  (reduce (fn [result block]
            (into result block))
          {}
          updated-mem-blocks))

; ---------------------------------------
; problem 1

(defn apply-mask1
  "Applies a mask to a given value and returns the new value (problem 1)"
  ([mask value] (apply-mask1 mask (to-binary value) []))
  ([mask value result]
   (if (empty? mask)
     (Long/parseLong (apply str result) 2)
     (let [new-value (if (= (first mask) '\X)
                       (first value)
                       (first mask))]
       (apply-mask1 (rest mask) (rest value) (conj result new-value))))))

(defn apply-mask-mem-block1
  "Applies a mask to a block of memory instructions,
  these are all the instructions between two masks in the input file (problem 1)"
  [mask mem-block]
  (reduce (fn [result mem]
            (let [new-value (apply-mask1 mask (second mem))]
              (assoc result (keyword (str (first mem))) new-value)))
          {}
          mem-block))

; ---------------------------------------
; problem 2

(defn apply-mask2
  "Applies a mask to a given value and returns the new value (problem 2)"
  ([mask value] (apply-mask2 mask (to-binary value) []))
  ([mask value result]
   (if (empty? mask)
     (apply str result)
     (let [new-value (cond
                       (= (first mask) '\0) (first value)
                       (= (first mask) '\1) '\1
                       :else '\X)]
       (apply-mask2 (rest mask) (rest value) (conj result new-value))))))

(defn create-all-values
  "Returns all possible values given a value with floating bits."
  ([value] (create-all-values value [] []))
  ([value address result]
   (if (empty? value)
     (apply str address)
     (if (not= '\X (first value))
       (recur (rest value) (conj address (first value)) result)
       (let [address0 (create-all-values (rest value) (conj address '\0) result)
             address1 (create-all-values (rest value) (conj address '\1) result)]
         (conj result address0 address1))))))

(defn create-all-addresses
  "Take the collection of values returned by create-all-values()
  and returns a list with keys of the corresponding addresses."
  [values]
  (map #(keyword (str (Long/parseLong % 2))) (flatten values)))

(defn apply-mask-mem-block2
  "Applies a mask to a block of memory instructions,
  these are all the instructions between two masks in the input file (problem 2)"
  [mask mem-block]
  (reduce (fn [final-result mem]
            (let [floating-address-value (apply-mask2 mask (first mem))
                  floating-addresses-values (create-all-values floating-address-value)
                  all-addresses (create-all-addresses floating-addresses-values)
                  updated-addresses-values (reduce (fn [result address]
                                                     (assoc result address (second mem)))
                                                   {}
                                                   all-addresses)]
              (into final-result updated-addresses-values)))
          {}
          mem-block))

; ---------------------------------------
; results

(def day14-1 (apply + (map second (final-mem-values (apply-all-masks apply-mask-mem-block1)))))

(def day14-2 (apply + (map second (final-mem-values (apply-all-masks apply-mask-mem-block2)))))

(defn -main
  []
  (println day14-1)                                         ;7997531787333
  (println day14-2))                                        ;3564822193820
