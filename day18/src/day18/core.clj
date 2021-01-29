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

(defn parse-expr
  "Parses a string expression into the appropriate list structure."
  [expr-string]
  (let [no-spaces-expr (filter #(not= \space %) expr-string)]
    (map #(cond
            (or (= \+ %) (= \* %)) (eval (read-string (str %)))
            (or (= \( %) (= \) %)) %
            :else (Integer. (str %)))
         no-spaces-expr)))

; ---------------------------------------
; problem 1

(defn calculate-expr1
  "Calculates an expression according to problem 1 rules."
  ([expr] (calculate-expr1 expr [* 1]))
  ([expr [op result]]
   (if (empty? expr)
     result
     (let [sym (first expr)]
       (cond
         (or (= + sym) (= * sym)) (recur (rest expr) [sym result])
         (= \( sym) (let [[_ res1 expr1] (calculate-expr1 (rest expr) [* 1])]
                      (recur (rest expr1) [op (op res1 result)]))
         (= \) sym) [op result expr]
         :else (recur (rest expr) [op (op result sym)]))))))

; ---------------------------------------
; problem 2

(defn char-to-int
  "Converts a character to integer."
  [ch]
  (Integer. (str ch)))

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

(defn calculate-expr2-no-parens
  "Evaluate an expression that does not have any parentheses (problem 2)"
  [expr]
  (->> expr
       (filter #(not= \space %))
       (partition-by #(= \* %))
       (filter #(not= \* (first %)))
       (reduce (fn [result list]
                 (if (> (count list) 1)
                   (conj result (->> list
                                     (take-nth 2)
                                     (map char-to-int)
                                     (apply +)))
                   (conj result (->> list
                                     first
                                     char-to-int))))
               [])
       (apply *)))

(defn calculate-expr2
  "Calculates an expression (problem 2)"
  [expr]
  (let [before-parens (take-while #(not= \( %) expr)
        before-parens-count (count before-parens)]
    (if (= before-parens-count (count expr))
      (calculate-expr2-no-parens expr)
      (let [after-parens (drop before-parens-count expr)
            subexpr (find-subexpr after-parens)
            subexpr-value (list (calculate-expr2 subexpr))
            first-part (take before-parens-count expr)
            last-part (drop (+ before-parens-count (count subexpr) 2) expr)]
        (recur (concat first-part subexpr-value last-part))))))

; ---------------------------------------
; results

(def day18-1
  (apply + (map (comp calculate-expr1 parse-expr) input)))
(def day18-2
  (apply + (map calculate-expr2 input)))

(defn -main
  []
  (println day18-1)                                         ;98621258158412
  (println day18-2))                                        ;241216538527890
