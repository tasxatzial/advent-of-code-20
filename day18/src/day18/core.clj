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

(defn -main
  []
  (println ))
