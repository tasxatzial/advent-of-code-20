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

(defn calculate-expr
  "Calculates an expression."
  ([expr] (calculate-expr expr [1 (eval *)]))
  ([expr result]
   (if (empty? expr)
     result
     (let [char (first expr)]
       (case char
         \( (let [subexpr (find-subexpr expr)
                  subexpr-value (calculate-expr subexpr)
                  op (second result)
                  value1 (first subexpr-value)
                  value2 (first result)
                  new-result (vector (op value1 value2) op)
                  new-expr (drop (+ 2 (count subexpr)) expr)]
              (calculate-expr new-expr new-result))
         \+ (let [new-expr (rest expr)
                  value (first result)
                  new-result (vector value (eval +))]
              (calculate-expr new-expr new-result))
         \* (let [new-expr (rest expr)
                  res (first result)
                  new-result (vector res (eval *))]
              (calculate-expr new-expr new-result))
         \space (let [new-expr (rest expr)]
                  (calculate-expr new-expr result))
         (let [new-expr (rest expr)
               op (second result)
               value1 (first result)
               value2 (Integer. (str char))
               new-result (vector (op value1 value2) \space)]
           (calculate-expr new-expr new-result)))))))

; ---------------------------------------
; results

(defn -main
  []
  (println (calculate-expr "6 + (4 * 8)")))
