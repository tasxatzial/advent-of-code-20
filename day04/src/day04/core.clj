(ns day04.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(def required-keys '(:byr :iyr :eyr :hgt :hcl :ecl :pid))

(defn parse
  "Splits the input string by \n\n and then splits again by space.
  Returns a list of vectors, each element in the vector is
  one field (key:value) of the passport."
  [s]
  (let [password-strings (clojure.string/split s #"\n\n")]
    (map #(clojure.string/split % #"[ \n]") password-strings)))

(defn get-passport-field
  "Creates a (key value) map from a passport field."
  [x]
  (let [[key val] (clojure.string/split x #":")]
    {(keyword key) val}))

(defn create-passport
  "Creates a passport map from a passport string."
  [passport]
  (reduce (fn [passport field]
            (into passport (get-passport-field field)))
          {}
          passport))

; put all passports in a list
(def passports (map create-passport (parse (slurp input-file))))

; ---------------------------------------
; problem 1

(defn hasRequiredKeys?
  [passport]
  (every? true? (map #(contains? passport %) required-keys)))

(defn isPassportValid1?
  [passport]
  (hasRequiredKeys? passport))

; ---------------------------------------
; results

(def day04-1
  (count (filter true? (map isPassportValid1? passports))))

(defn -main
  []
  (println day04-1))                                        ;216
