(ns day09.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn str->BigInt
  "Converts a string to BigInteger."
  [^String s]
  (BigInteger. s))

(defn parse
  "Splits the input string by \n."
  [s]
  (clojure.string/split s #"\n"))

; convert the input file to a list of BigInteger
(def parsed-input (apply vector (map str->BigInt (parse (slurp input-file)))))

(def preamble-size 25)

; ---------------------------------------
; problem 1

(defn sorted-insert
  "Adds num to a sorted col so that they new col is also sorted.
  Runs in linear time."
  ([sorted-col num]
   (sorted-insert sorted-col num '()))
  ([sorted-col num result]
   (if (empty? sorted-col)
     (concat result [num])
     (if (> (first sorted-col) num)
       (concat result [num] sorted-col)
       (recur (rest sorted-col) num (concat result [(first sorted-col)]))))))

(defn sumOfTwo?
  "Returns true if num is the sum of two numbers in col, false otherwise.
  Assumes col is sorted therefore takes advantage of the appropriate optimizations."
  [col num]
  (if (empty? col)
    false
    (let [first-num (first col)]
      (if (>= first-num num)
        false
        (loop [rest-col (rest col)]
          (if (empty? rest-col)
            (sumOfTwo? (rest col) num)
            (if (= num (+ (first rest-col) first-num))
              true
              (if (> num (+ (first rest-col) first-num))
                (recur (rest rest-col))
                (sumOfTwo? (rest col) num)))))))))

(defn find-index
  "Finds the index of num in col. Returns -1 if num is not present in col."
  ([col num]
   (find-index col num 0))
  ([col num index]
   (if (empty? col)
     -1
     (if (= (first col) num)
       index
       (find-index (rest col) num (inc index))))))

(defn remove-num
  "Removes the first instance of num from the col and returns a new col."
  [col num]
  (let [index (find-index col num)]
    (concat (take index col) (drop (inc index) col))))

(defn -main
  []
  (println parsed-input))
