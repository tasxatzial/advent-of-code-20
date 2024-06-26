(ns day25.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n"
  [s]
  (clojure.string/split s #"\n"))

(def input (parse (slurp input-file)))

(def key1 (Integer. ^String (first input)))
(def key2 (Integer. ^String (second input)))
(def subject-number 7)
(def modulo 20201227)

; ---------------------------------------
; problem 1

(defn find-loop-size
  "Finds the loop size."
  [key]
  (loop [value 1
         iter 0]
    (if (= value key)
      iter
      (let [next-number (* value subject-number)]
        (if (< next-number modulo)
          (recur next-number (inc iter))
          (recur (mod next-number modulo) (inc iter)))))))

(defn transform-public-key
  "Finds the encryption key."
  [key loop-size]
  (loop [value 1
         iter 0]
    (if (= iter loop-size)
      value
      (let [next-number (* value key)]
        (if (< next-number modulo)
          (recur next-number (inc iter))
          (recur (mod next-number modulo) (inc iter)))))))

(defn find-encryption-key
  "Finds the encryption key when the loop size is unknown."
  [key1 key2]
  (loop [value1 1
         value2 1]
    (if (= value2 key2)
      value1
      (let [next-number1 (* value1 key1)
            next-number2 (* value2 subject-number)]
        (recur (mod next-number1 modulo) (mod next-number2 modulo))))))

; ---------------------------------------
; results

(defn day25-1
  []
  (let [loop-size (find-loop-size key1)]
    (transform-public-key key2 loop-size)))

(defn -main
  []
  (println (day25-1)))
