(ns day01.core
  (:gen-class))

; --------------------------
; common

(def input "resources\\input.txt")

(defn str->int
  "Converts a string to integer."
  [^String s]
  (Integer. s))

(defn parse
  "Splits the input string by \n and converts it into a list of numbers."
  [s]
  (map str->int (clojure.string/split s #"\n")))

; --------------------------
; problem 1

(defn add-to-first-element
  "Picks the first element of seq and adds it to the rest elements.
  Returns the first pair that sums to 2020 or nil if such pair cannot
  be found. Assumes seq is sorted in increasing order."
  ([seq] (add-to-first-element seq 1))
  ([seq i]
   (if (or (= i (count seq)) (> (+ (first seq) i) 2020))    ;optimization
     nil
     (let [f (first seq) g (first (drop i seq)) sum (+ f g)]
       (if (= sum 2020)
         [f g]
         (if (< sum 2020)                                   ;optimization
           (add-to-first-element seq (inc i))))))))

(defn sum2020-1
  "Returns the first pair of numbers from seq that sums to 2020 or nil if such pair
  cannot be found. Assumes seq is sorted in increasing order."
  [seq]
  (if (or (empty? seq) (> (first seq) 1010))                ;optimization
    nil
    (let [res (add-to-first-element seq)]
      (or res (sum2020-1 (rest seq))))))

; --------------------------
; problem 2

(defn add-to-two-elements
  "Uses two fixed numbers f & i and finds a number from seq such that the sum of all
  numbers is 2020. Returns the first triplet that sums to 2020 or nil if such triplet
  cannot be found."
  ([seq f i] (add-to-two-elements seq f i (inc i)))
  ([seq f i j]
   (if (= j (count seq))
     nil
     (let [g (first (drop i seq)) h (first (drop j seq)) sum (+ f g h)]
       (if (= sum 2020)
         [f g h]
         (if (< sum 2020)                                   ;optimization
           (add-to-two-elements seq f i (inc j))))))))

(defn add-to-one-element
  "Uses a fixed number f and finds two numbers from seq such that the sum of all
  numbers is 2020. Returns the first triplet that sums to 2020 or nil if such triplet
  cannot be found. Assumes seq is sorted in increasing order."
  ([seq f] (add-to-one-element seq f 0))
  ([seq f i]
   (if (or (= i (- (count seq) 1)) (> (+ f i) 2020))                 ;optimization
     nil
     (let [res (add-to-two-elements seq f i)]
       (or res (add-to-one-element seq f (inc i)))))))

(defn sum2020-2
  "Returns the first triplet from seq that sums to 2020 or nil if such triplet
  cannot be found. Assumes seq is sorted in increasing order."
  [seq]
  (if (or (= 2 (count seq)) (> (first seq) 673))            ;optimization
    nil
    (let [res (add-to-one-element seq (first seq))]
      (or res (sum2020-2 (rest seq))))))

; ---------------------------------------
; results

(def day01-1
  (sum2020-1 (sort (parse (slurp input)))))

(def day01-2
  (sum2020-2 (sort (parse (slurp input)))))

(defn -main
  []
  (println day01-1) ;[279 1741]
  (println day01-2)) ;[257 494 1269]
