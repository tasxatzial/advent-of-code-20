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

(def count-col (count parsed-input))

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
       (recur (rest col) num (inc index))))))

(defn remove-num
  "Removes the first instance of num from the col and returns a new col."
  [col num]
  (let [index (find-index col num)]
    (concat (take index col) (drop (inc index) col))))

(defn find-first
  "Finds the first number in the parsed input that is not the sum
  of any of the previous 25 numbers."
  ([]
   (let [init-col (take preamble-size parsed-input)
         sorted-init-col (sort init-col)]
     (find-first sorted-init-col preamble-size)))
  ([sub-col index]
   (if (>= index count-col)
     nil
     (let [checked-num (nth parsed-input index)]
       (if (sumOfTwo? sub-col checked-num)
         (let [to-remove (nth parsed-input (- index preamble-size))
               new-col (remove-num sub-col to-remove)
               new-sub-col (sorted-insert new-col checked-num)]
           (recur new-sub-col (inc index)))
         checked-num)))))

; ---------------------------------------
; problem 2

(declare memoized-day09-1)

(defn find-smallest-largest
  "Finds a contiguous list of numbers in the input file such that the sum of
  all numbers in the list equals to the solution to problem 1."
  ([] (find-smallest-largest 0 0 0))
  ([prev-sum first-index second-index]
   (if (= first-index count-col)
     nil
     (if (or (>= second-index count-col) (< (memoized-day09-1) prev-sum))
       (let [next-first-index (inc first-index)]
         (recur (nth parsed-input next-first-index) next-first-index (inc next-first-index)))
       (let [new-sum (+ prev-sum (nth parsed-input second-index))]
         (if (= (memoized-day09-1) new-sum)
           (concat (take (inc (- second-index first-index)) (drop first-index parsed-input)))
           (recur new-sum first-index (inc second-index))))))))

; ---------------------------------------
; results

(defn day09-1
  []
  (find-first))

(def memoized-day09-1 (memoize day09-1))

(defn day09-2
  []
  (let [sorted-sublist (sort (find-smallest-largest))]
    (+ (first sorted-sublist) (last sorted-sublist))))

(defn -main
  []
  (println (memoized-day09-1))
  (println (day09-2)))
