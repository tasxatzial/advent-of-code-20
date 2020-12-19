(ns day18.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n."
  [s]
  (clojure.string/split s #"\n"))

(def input (parse (slurp input-file)))

(defn find-subexpr
  "Expr must start with (. Everything until the matching ) will be returned."
  ([expr] (find-subexpr (rest expr) [] 1))
  ([expr result count]
   (if (or (empty? expr) (= 0 count))
     (butlast result)
     (let [char (first expr)]
       (case char
         \( (recur (rest expr) (conj result \() (inc count))
         \) (recur (rest expr) (conj result \)) (dec count))
         (recur (rest expr) (conj result char) count))))))

; ---------------------------------------
; problem 1

(defn calculate-expr1
  "Calculates an expression."
  ([expr] (calculate-expr1 expr [1 (eval *)]))
  ([expr result]
   (if (empty? expr)
     result
     (let [char (first expr)]
       (case char
         \( (let [subexpr (find-subexpr expr)
                  subexpr-value (calculate-expr1 subexpr)
                  op (second result)
                  value1 (first subexpr-value)
                  value2 (first result)
                  new-result (vector (op value1 value2) op)
                  new-expr (drop (+ 2 (count subexpr)) expr)]
              (calculate-expr1 new-expr new-result))
         \+ (let [new-expr (rest expr)
                  value (first result)
                  new-result (vector value (eval +))]
              (calculate-expr1 new-expr new-result))
         \* (let [new-expr (rest expr)
                  res (first result)
                  new-result (vector res (eval *))]
              (calculate-expr1 new-expr new-result))
         \space (let [new-expr (rest expr)]
                  (calculate-expr1 new-expr result))
         (let [new-expr (rest expr)
               op (second result)
               value1 (first result)
               value2 (Integer. (str char))
               new-result (vector (op value1 value2) \space)]
           (calculate-expr1 new-expr new-result)))))))

; ---------------------------------------
; results

(def day18-1
  (apply + (map (comp first calculate-expr1) input)))

(defn -main
  []
  (println day18-1))                                        ;98621258158412
